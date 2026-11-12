# Host tables for borers and defoliators, built from the cleaned EU-ForDAM data
#
# What comes from the data:
#   guild (m3 records = borer, ha records = defoliator), host tree(s) in the data file,
#   host type (conifers / broadleaves), number of regions and countries recorded.
# What comes from the reference files in reference/ (expert knowledge, edit there):
#   insects.csv               insect name per code, optional host-amount override
#   supervisor_host_matrix.csv host trees and host amount from the supervisor's matrix,
#                              and the group column used to count its regions
#   host_trees.csv            Latin -> common host name, tree type, whether the forest
#                              data has an own column for that tree
#
# Counting rule: a region or country counts when it has at least one record for the
# insect (zeros count, because a zero means monitored). Blank / missing records do not.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

read_host_references <- function(ref_dir = "reference") {
  list(
    insects    = read_csv(file.path(ref_dir, "insects.csv"), show_col_types = FALSE,
                          col_types = cols(.default = "c")),
    supervisor = read_csv(file.path(ref_dir, "supervisor_host_matrix.csv"),
                          show_col_types = FALSE, col_types = cols(.default = "c")),
    hosts      = read_csv(file.path(ref_dir, "host_trees.csv"), show_col_types = FALSE)
  )
}

# Aggregate codes combine several agents (e.g. "CambXyl_con&BB_con&SM", "Def_dec").
is_aggregate_code <- function(code) str_detect(code, "&|^(CambXyl|Def)_")

# One row per insect with everything shown in the "insects" sheet.
build_insect_table <- function(cleaned, refs) {
  d <- cleaned$data |>
    left_join(refs$hosts |> select(host_tree, host, host_type = tree_type, host_order = order),
              by = "host_tree")

  unknown_hosts <- setdiff(unique(d$host_tree[is.na(d$host)]), NA)
  if (length(unknown_hosts))
    warning("Host trees missing from reference/host_trees.csv: ",
            paste(unknown_hosts, collapse = ", "))

  species <- d |> filter(!is_aggregate_code(sp_code))
  unknown <- setdiff(unique(species$sp_code), refs$insects$code)
  if (length(unknown))
    warning("Insect codes missing from reference/insects.csv (shown by code): ",
            paste(unknown, collapse = ", "))

  # Host trees as recorded in the data file
  data_hosts <- species |>
    distinct(code = sp_code, host, host_type, host_order) |>
    arrange(code, host_order) |>
    group_by(code) |>
    summarise(host_tree_in_data_file = paste(host, collapse = ", "),
              host_type = first(host_type),
              primary_order = first(host_order), .groups = "drop")

  guild <- species |>
    distinct(code = sp_code, unit) |>
    group_by(code) |>
    summarise(guild = case_when(all(unit == "m3") ~ "borer",
                                all(unit == "ha") ~ "defoliator",
                                TRUE ~ "check: mixed units"),
              measure = if_else(guild == "borer", "growing stock (m3)", "area (ha)"),
              .groups = "drop")

  # Regions / countries: from the supervisor's group column where given, else by code
  lk <- cleaned$groups
  sup_counts <- refs$supervisor |>
    filter(!is.na(group)) |>
    left_join(lk |> select(group, column), by = "group") |>
    rowwise() |>
    mutate(rows = list(cleaned$data[cleaned$data[[column]], c("nuts", "country")]),
           regions = n_distinct(rows$nuts), countries = n_distinct(rows$country)) |>
    ungroup() |>
    select(code, regions, countries)

  code_counts <- species |>
    group_by(code = sp_code) |>
    summarise(regions = n_distinct(nuts), countries = n_distinct(country), .groups = "drop")

  counts <- code_counts |>
    rows_update(sup_counts |> filter(code %in% code_counts$code), by = "code")

  host_cols <- refs$hosts |> distinct(host, has_forest_column)

  default_scope <- function(hosts, host_type) {
    h <- str_split(hosts, ",\\s*")[[1]]
    own <- host_cols$has_forest_column[match(h, host_cols$host)]
    if (all(own %in% TRUE)) paste(h, collapse = " + ") else paste("all", host_type)
  }

  tibble(code = unique(species$sp_code)) |>
    left_join(refs$insects, by = "code") |>
    mutate(insect = coalesce(insect, code)) |>
    left_join(guild, by = "code") |>
    left_join(data_hosts, by = "code") |>
    left_join(refs$supervisor |> select(code, sup_hosts = host_trees, sup_amount = host_amount),
              by = "code") |>
    left_join(counts, by = "code") |>
    rowwise() |>
    mutate(
      host_trees = coalesce(sup_hosts, host_tree_in_data_file),
      scope = coalesce(host_amount_override, sup_amount, default_scope(host_trees, host_type)),
      host_amount_from = paste0(measure, ": ", scope),
      host_amount_source = if_else(is.na(sup_hosts), "data-file host", "supervisor's matrix")
    ) |>
    ungroup() |>
    arrange(guild, host_type, primary_order, desc(regions), code) |>
    select(insect, code, guild, host_type, host_trees, host_tree_in_data_file,
           host_amount_from, host_amount_source, regions, countries)
}

# Long table insect x host tree: the tidy form for modelling.
build_insect_hosts <- function(insects) {
  insects |>
    select(code, insect, guild, host_type, host_trees, host_amount_source) |>
    separate_longer_delim(host_trees, delim = ", ") |>
    rename(host = host_trees)
}

# 0/1 matrix: rows = every host tree of that type, columns = insects.
build_host_matrix <- function(insects, refs, guild_name, host_type_name) {
  hosts <- refs$hosts |> filter(tree_type == host_type_name) |>
    distinct(host, order) |> arrange(order) |> pull(host)
  ins <- insects |> filter(guild == guild_name, host_type == host_type_name)

  m <- tibble(`host tree` = hosts)
  for (i in seq_len(nrow(ins))) {
    h <- str_split(ins$host_trees[i], ",\\s*")[[1]]
    m[[ins$insect[i]]] <- as.numeric(hosts %in% h)
  }
  m
}

build_host_tables <- function(cleaned, refs = read_host_references()) {
  insects <- build_insect_table(cleaned, refs)
  list(
    insects      = insects,
    insect_hosts = build_insect_hosts(insects),
    matrices = list(
      borers_on_conifers         = build_host_matrix(insects, refs, "borer", "conifers"),
      borers_on_broadleaves      = build_host_matrix(insects, refs, "borer", "broadleaves"),
      defoliators_on_conifers    = build_host_matrix(insects, refs, "defoliator", "conifers"),
      defoliators_on_broadleaves = build_host_matrix(insects, refs, "defoliator", "broadleaves")
    )
  )
}

host_tables_how_to_read <- function() {
  tibble(`How to read` = c(
    "One table per box of the 2 x 2 grid: borers / defoliators on conifers / broadleaves.",
    "Rows: host trees. Columns: insects (species and genus-level taxa; aggregate group codes left out). 1 = the insect lives on that tree.",
    "Host tree: the host recorded in the EU-ForDAM database, plus trees the supervisor's host matrix adds (reference/supervisor_host_matrix.csv).",
    "insects: every insect with its host trees, the forest data used for its host amount (supervisor's matrix, otherwise the data-file host),",
    "and the number of regions and countries with at least one record (zeros count; blank or missing records do not).",
    "Larch, birch, elm, horse chestnut and poplar/willow have no own column in the forest data, so their host amount is all conifers or all broadleaves.",
    "Generated automatically by R/host_tables.R; do not edit by hand. Change reference/*.csv instead."
  ))
}

write_host_tables <- function(ht, path) {
  writexl::write_xlsx(c(list(`how to read` = host_tables_how_to_read()),
                        ht$matrices,
                        list(insects = ht$insects, insect_hosts = ht$insect_hosts)),
                      path)
}

# Damage records joined with insect metadata: one table ready for modelling.
build_model_data <- function(cleaned, ht, refs = read_host_references()) {
  cleaned$data |>
    mutate(is_aggregate = is_aggregate_code(sp_code)) |>
    left_join(refs$hosts |> select(host_tree, host, host_type = tree_type), by = "host_tree") |>
    left_join(ht$insects |> select(code, insect, guild, host_amount_from),
              by = c("sp_code" = "code")) |>
    relocate(country, nuts, region, year, sp_code, insect, guild, is_aggregate,
             host_tree, host, host_type, unit, value, value_alt, unit_alt)
}
