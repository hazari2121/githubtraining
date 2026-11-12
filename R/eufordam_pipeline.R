# EU-ForDAM cleaning and summary pipeline
#
# Data rules (agreed with the data owner):
#   * value1 == 0   -> monitored, no damage found. Real zero, kept as is.
#   * value1 blank  -> not recorded. Row is dropped, never filled with 0.
#   * missing row   -> not recorded. Never created / filled with 0.
#   * value2        -> the same damage as value1, expressed in the other unit
#                      (m3 <-> ha).

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

# Columns of the raw sheet that are not group-membership flags
.id_cols <- c("SP_CATEGORY", "SP_SUBCATEGORY", "HOSTTREE", "TREETYPE", "d_year",
              "Country", "Region", "NUTS", "sp_code", "sp_unit", "value1", "value2")

# Tidy a group label: remove non-breaking / stray spaces, fix spelling.
clean_label <- function(x) {
  x |>
    str_replace_all(" ", " ") |>
    str_squish() |>
    str_replace_all("(?i)decidous", "deciduous")
}

# Turn a group label into a snake_case column name, e.g.
# "Bark borers on conifers" -> "grp_bark_borers_on_conifers"
group_col_name <- function(label) {
  paste0("grp_", str_to_lower(label) |> str_replace_all("[^a-z0-9]+", "_") |>
           str_remove("^_|_$"))
}

# ---------------------------------------------------------------------------
# 1. Read
# ---------------------------------------------------------------------------
read_eufordam <- function(path, sheet = "EUForDam_REW") {
  raw <- read_excel(path, sheet = sheet, guess_max = 1e6)
  missing <- setdiff(.id_cols, names(raw))
  if (length(missing)) stop("Missing expected columns: ", paste(missing, collapse = ", "))
  raw
}

# Lookup table: raw flag column -> clean label -> clean column name
group_lookup <- function(raw) {
  flag_cols <- setdiff(names(raw), .id_cols)
  tibble(raw_name = flag_cols,
         group    = clean_label(flag_cols),
         column   = group_col_name(group))
}

# ---------------------------------------------------------------------------
# 2. Clean
# ---------------------------------------------------------------------------
# Returns a list with the clean long table and the rows that were dropped.
clean_eufordam <- function(raw) {
  lk <- group_lookup(raw)

  d <- raw |>
    rename(any_of(setNames(lk$raw_name, lk$column))) |>
    mutate(across(all_of(lk$column), ~ !is.na(.x) & .x == 1)) |>
    transmute(
      country        = str_squish(Country),
      nuts           = str_squish(NUTS),
      region         = str_squish(Region),
      year           = as.integer(d_year),
      tree_type      = str_squish(TREETYPE),
      host_tree      = str_squish(HOSTTREE),
      sp_category    = str_squish(SP_CATEGORY),
      sp_subcategory = str_squish(SP_SUBCATEGORY),
      sp_code        = str_squish(sp_code),
      unit           = str_squish(sp_unit),
      value          = as.numeric(value1),
      value_alt      = as.numeric(value2),
      unit_alt       = case_when(!is.na(value_alt) & unit == "m3" ~ "ha",
                                 !is.na(value_alt) & unit == "ha" ~ "m3",
                                 TRUE ~ NA_character_),
      across(all_of(lk$column))
    )

  # Rule: blank value1 = not recorded -> drop (never fill with 0)
  dropped <- d |> filter(is.na(value))
  d <- d |> filter(!is.na(value))

  list(data = d, dropped = dropped, groups = lk)
}

# ---------------------------------------------------------------------------
# 3. Quality checks (report only; nothing is changed here)
# ---------------------------------------------------------------------------
qc_eufordam <- function(cleaned) {
  d <- cleaned$data
  key <- c("sp_code", "host_tree", "year", "nuts", "unit")

  dup <- d |>
    add_count(across(all_of(key)), name = "n_rows") |>
    filter(n_rows > 1) |>
    arrange(across(all_of(key)))

  list(
    overview = tibble(
      metric = c("rows_kept", "rows_dropped_blank_value1", "zero_values_kept",
                 "positive_values", "rows_with_value_alt",
                 "rows_missing_unit", "duplicate_key_groups", "rows_in_duplicate_groups",
                 "negative_values"),
      value = c(nrow(d), nrow(cleaned$dropped), sum(d$value == 0),
                sum(d$value > 0), sum(!is.na(d$value_alt)),
                sum(is.na(d$unit)),
                n_distinct(dup[key]), nrow(dup),
                sum(d$value < 0))
    ),
    dropped_blank = cleaned$dropped |> count(country, sp_code, unit, name = "n_dropped"),
    duplicates = dup,
    nuts_levels = d |>
      mutate(nuts_chars = nchar(nuts)) |>
      count(country, nuts_chars, name = "n_rows") |>
      left_join(d |> group_by(country) |> summarise(n_regions = n_distinct(nuts)),
                by = "country"),
    coverage = d |>
      group_by(country, sp_code, unit) |>
      summarise(first_year = min(year), last_year = max(year),
                n_years = n_distinct(year), n_regions = n_distinct(nuts),
                n_zero = sum(value == 0), n_positive = sum(value > 0),
                .groups = "drop"),
    unit_ratio = d |>
      filter(!is.na(value_alt), value > 0) |>
      mutate(ratio = value_alt / value) |>
      group_by(country, unit, unit_alt) |>
      summarise(n = n(), ratio_min = min(ratio), ratio_median = median(ratio),
                ratio_max = max(ratio), .groups = "drop"),
    category_conflicts = d |>
      distinct(sp_code, sp_category, tree_type) |>
      group_by(sp_code) |>
      filter(n_distinct(sp_category) > 1 | n_distinct(tree_type) > 1) |>
      summarise(categories = paste(sort(unique(sp_category)), collapse = ", "),
                tree_types = paste(sort(unique(tree_type)), collapse = ", "),
                .groups = "drop"),
    odd_labels = d |>
      distinct(host_tree) |>
      filter(str_detect(host_tree, "\\?"))
  )
}

# ---------------------------------------------------------------------------
# 4. Summaries (replace the manual Excel pivot and List1)
# ---------------------------------------------------------------------------
# Long table: one row per record x group it belongs to.
long_by_group <- function(cleaned) {
  lk <- cleaned$groups
  cleaned$data |>
    pivot_longer(all_of(lk$column), names_to = "column", values_to = "in_group") |>
    filter(in_group) |>
    left_join(lk |> select(column, group), by = "column") |>
    select(-column, -in_group)
}

# Totals per group and unit (Europe-wide), plus each country's share.
# This is the automated version of the List1 sheet.
group_totals <- function(cleaned, by = character()) {
  long_by_group(cleaned) |>
    group_by(group, unit, across(all_of(by))) |>
    summarise(n_records = n(), total = sum(value),
              total_alt = sum(value_alt, na.rm = TRUE), .groups = "drop")
}

country_shares <- function(cleaned) {
  cleaned$data |>
    group_by(unit, country) |>
    summarise(total = sum(value), .groups = "drop_last") |>
    mutate(share = total / sum(total)) |>
    ungroup() |>
    arrange(unit, desc(total))
}

# Totals by country x unit x tree type x region.
# This is the automated version of the kotg_EUForDam_REW pivot (for every country).
region_totals <- function(cleaned, sp_codes = NULL) {
  d <- cleaned$data
  if (!is.null(sp_codes)) d <- d |> filter(sp_code %in% sp_codes)
  d |>
    group_by(country, unit, tree_type, nuts) |>
    summarise(total = sum(value),
              total_alt = if (all(is.na(value_alt))) NA_real_ else sum(value_alt, na.rm = TRUE),
              .groups = "drop")
}

# ---------------------------------------------------------------------------
# 5. Run everything and write outputs
# ---------------------------------------------------------------------------
run_pipeline <- function(path, out_dir = "output") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  raw     <- read_eufordam(path)
  cleaned <- clean_eufordam(raw)
  qc      <- qc_eufordam(cleaned)

  summaries <- list(
    group_totals          = group_totals(cleaned),
    group_totals_country  = group_totals(cleaned, by = "country"),
    group_totals_year     = group_totals(cleaned, by = "year"),
    country_shares        = country_shares(cleaned),
    region_totals         = region_totals(cleaned)
  )

  readr::write_csv(cleaned$data, file.path(out_dir, "eufordam_clean.csv"), na = "")
  writexl::write_xlsx(c(list(clean_data = cleaned$data), summaries),
                      file.path(out_dir, "eufordam_summaries.xlsx"))
  writexl::write_xlsx(qc, file.path(out_dir, "eufordam_qc.xlsx"))

  invisible(list(cleaned = cleaned, qc = qc, summaries = summaries))
}
