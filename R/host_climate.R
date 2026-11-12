# Host amount (forest area / growing stock per tree group) and climate per spatial unit
#
# Source: Data_Klim_Tree_NUTS_*.xlsx, sheet DATA_CLEAN (one row per country / NUTS
# region / Polish forestry district), documented in Appendix_NUTS_Forests (Appendix D).
#
# Fixes applied so the units match the damage data (all logged in the QC report):
#   * "-" in host columns = not available -> NA (never 0)
#   * Poland: code and name columns are swapped in the source -> swapped back
#   * Slovenia: country code "SL" -> "SI"
#   * Czechia: host data are NUTS3 (CZ010...), damage data NUTS2 (CZ01...) -> host
#     amounts summed to NUTS2; climate averaged weighted by forest area
#   * Germany: Berlin (DE3) is lumped with Brandenburg (DE4) in the source; Bremen (DE5)
#     and Hamburg (DE6) have no host data -> these units get NA host amounts

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

.host_cols <- c("spruce_ha", "spruce_m3", "pine_ha", "pine_m3", "oak_ha", "oak_m3",
                "fir_ha", "fir_m3", "coniferous_ha", "coniferous_m3",
                "deciduous_ha", "deciduous_m3")

read_host_climate <- function(path, sheet = "DATA_CLEAN") {
  raw <- read_excel(path, sheet = sheet) |> filter(!is.na(NUTS_CODE))
  clim <- names(raw)[str_detect(names(raw), "_vpd_|_tas_|_pr_")]
  names(raw)[match(clim, names(raw))] <- case_when(
    str_detect(clim, "_vpd_") ~ "vpd", str_detect(clim, "_tas_") ~ "tas",
    str_detect(clim, "_pr_") ~ "pr")

  num <- c(.host_cols, "vpd", "tas", "pr")
  d <- raw |>
    mutate(across(all_of(num), ~ suppressWarnings(as.numeric(.x))),
           country = if_else(COUNTRY == "SL", "SI", COUNTRY),
           nuts    = if_else(country == "PL", NUTS_region, NUTS_CODE),
           unit_name = if_else(country == "PL", NUTS_CODE, NUTS_region),
           nuts    = if_else(country == "CZ", str_sub(nuts, 1, 4), nuts))

  d |>
    mutate(.w = coalesce(coniferous_ha, 0) + coalesce(deciduous_ha, 0)) |>
    group_by(country, nuts) |>
    summarise(unit_name = paste(unique(unit_name), collapse = "; "),
              n_source_units = n(),
              across(all_of(.host_cols), ~ if (all(is.na(.x))) NA_real_ else sum(.x, na.rm = TRUE)),
              across(c(vpd, tas, pr), ~ if (sum(.w) > 0) weighted.mean(.x, .w) else mean(.x)),
              .groups = "drop")
}

# Host columns used for an insect, from its host_amount_from text
# e.g. "growing stock (m3): spruce + pine" -> c("spruce_m3", "pine_m3")
host_columns_from_text <- function(txt) {
  suffix <- if_else(str_detect(txt, "m3"), "_m3", "_ha")
  scope  <- str_remove(txt, "^.*:\\s*")
  lapply(seq_along(txt), function(i) {
    s <- scope[i]
    trees <- if (s == "all conifers") "coniferous"
             else if (s == "all broadleaves") "deciduous"
             else if (s == "all forest") c("coniferous", "deciduous")
             else str_split(s, "\\s*\\+\\s*")[[1]]
    paste0(trees, suffix[i])
  })
}

# Sum of the given host columns for each unit; NA if any of them is not available
unit_host_amount <- function(hc, cols) {
  if (length(cols) == 0 || any(is.na(cols))) return(rep(NA_real_, nrow(hc)))
  rowSums(as.matrix(hc[cols]))
}

# Add host amount, relative damage and climate to the modelling table.
#   host_amount   area (ha) or growing stock (m3) of the insect's host in that unit
#   damage_rel    value / host_amount (fraction of host damaged that year)
# Aggregate codes use all conifers / all broadleaves in the record's unit.
add_host_climate <- function(model_data, hc) {
  md <- model_data |>
    mutate(.txt = coalesce(host_amount_from,
                           paste0(if_else(unit == "m3", "growing stock (m3)", "area (ha)"),
                                  ": all ", host_type)))
  keys <- unique(md$.txt)
  cols <- setNames(host_columns_from_text(keys), keys)

  amounts <- bind_rows(lapply(keys, function(k)
    tibble(.txt = k, nuts = hc$nuts, host_amount = unit_host_amount(hc, cols[[k]]),
           host_amount_columns = paste(cols[[k]], collapse = " + "))))

  md |>
    left_join(amounts, by = c(".txt", "nuts")) |>
    left_join(hc |> select(nuts, vpd, tas, pr), by = "nuts") |>
    mutate(host_amount_from = .txt,
           damage_rel = if_else(!is.na(host_amount) & host_amount > 0,
                                value / host_amount, NA_real_)) |>
    select(-.txt)
}

# Table 2, last column: % of host growing stock / area affected annually.
# Computed over the regions where the group has records (paper series filter applied):
#   100 * total damage / 23 years / host amount in those regions
compare_paper_host_share <- function(cleaned, hc, ref_dir = "reference", n_years = 23) {
  paper <- readr::read_csv(file.path(ref_dir, "paper_table2.csv"), show_col_types = FALSE)
  ghc   <- readr::read_csv(file.path(ref_dir, "group_host_columns.csv"), show_col_types = FALSE)
  d <- add_series_info(cleaned$data) |> filter(in_paper_series)
  lk <- cleaned$groups

  paper |>
    rowwise() |>
    mutate(
      host_columns = paste(ghc$host_column[ghc$group == group], collapse = " + "),
      .x = list(d[d[[lk$column[lk$group == group]]], ]),
      regions_with_host = sum(hc$nuts %in% unique(.x$nuts)),
      host_total = sum(unit_host_amount(hc[hc$nuts %in% unique(.x$nuts), ],
                                        ghc$host_column[ghc$group == group]), na.rm = TRUE),
      pct_host_per_year = round(100 * sum(.x$value) / n_years / host_total, 3)
    ) |>
    ungroup() |>
    select(level, group, unit, host_columns, regions_with_host, host_total,
           pct_host_per_year, paper_pct_host_per_year)
}

# Records where damage is larger than the host amount (damage_rel > 1): either a host
# data problem or the damage is attributed to the wrong host (e.g. L. monacha on pine).
qc_damage_exceeds_host <- function(model_data) {
  model_data |>
    filter(!is.na(damage_rel), damage_rel > 1) |>
    group_by(country, nuts, sp_code, insect, host_amount_columns) |>
    summarise(n_years = n(), max_damage_rel = max(damage_rel),
              host_amount = first(host_amount), max_value = max(value), .groups = "drop") |>
    arrange(desc(max_damage_rel))
}

# Spatial units in the damage data without host or climate data
qc_host_climate <- function(cleaned, hc) {
  du <- cleaned$data |> distinct(country, nuts)
  list(
    units_without_host_data = anti_join(du, hc, by = "nuts"),
    host_units_without_damage = anti_join(hc, du, by = "nuts") |> select(country, nuts, unit_name),
    # Growing stock per hectare; typical European values are ~100-700 m3/ha.
    # Values below 20 suggest m3 given in thousands (e.g. BG, LT species columns).
    host_density_suspicious = hc |>
      transmute(country, nuts,
                spruce = spruce_m3 / spruce_ha, pine = pine_m3 / pine_ha, oak = oak_m3 / oak_ha,
                fir = fir_m3 / fir_ha, coniferous = coniferous_m3 / coniferous_ha,
                deciduous = deciduous_m3 / deciduous_ha) |>
      pivot_longer(-c(country, nuts), names_to = "tree", values_to = "m3_per_ha") |>
      filter(is.finite(m3_per_ha), m3_per_ha < 20 | m3_per_ha > 1000),
    host_not_available = hc |>
      pivot_longer(all_of(.host_cols), names_to = "host_column", values_to = "v") |>
      filter(is.na(v)) |> select(country, nuts, host_column)
  )
}
