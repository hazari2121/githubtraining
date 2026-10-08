# Step 1: inspection of the raw EU-ForDAM workbook.
# Nothing here alters the raw file; all derived objects go to data/processed/
# and outputs/tables/.

# Read the main data sheet. The workbook also has a Czech pivot sheet
# ("kotg_EUForDam_REW") and a scratch sheet ("List1"); neither holds raw data.
read_raw <- function(path) {
  readxl::read_excel(path, sheet = "EUForDam_REW", guess_max = 50000) |>
    # Some indicator-column headers carry trailing spaces / non-breaking spaces
    dplyr::rename_with(~ stringr::str_squish(stringr::str_replace_all(.x, " ", " ")))
}

# Long format: one row per raw record. Nothing is aggregated or dropped yet:
#  - CZ has 1-3 rows per NUTS2-year-species (sub-regions, probably kraje,
#    that map to the same NUTS2 code). `sub_id` numbers them in file order.
#  - value2 is kept alongside value1 until its meaning is confirmed.
to_long <- function(raw) {
  raw |>
    dplyr::mutate(row_in_file = dplyr::row_number() + 1L) |>  # +1 = Excel header row
    dplyr::transmute(
      country  = Country,
      nuts_id  = NUTS,
      region   = Region,
      year     = as.integer(d_year),
      species  = sp_code,
      damage   = value1,
      unit     = sp_unit,
      value2,
      host     = HOSTTREE,
      treetype = TREETYPE,
      sp_category    = SP_CATEGORY,
      sp_subcategory = SP_SUBCATEGORY,
      row_in_file
    ) |>
    dplyr::group_by(country, nuts_id, year, species) |>
    dplyr::mutate(sub_id = dplyr::row_number(), n_sub = dplyr::n()) |>
    dplyr::ungroup()
}

# Headline counts
summarise_counts <- function(long) {
  tibble::tibble(
    n_rows      = nrow(long),
    n_countries = dplyr::n_distinct(long$country),
    n_species   = dplyr::n_distinct(long$species),
    n_regions   = dplyr::n_distinct(long$nuts_id),
    n_years     = dplyr::n_distinct(long$year),
    year_min    = min(long$year),
    year_max    = max(long$year),
    n_value1_na = sum(is.na(long$damage)),
    n_value1_zero = sum(long$damage == 0, na.rm = TRUE),
    n_value1_pos  = sum(long$damage > 0, na.rm = TRUE),
    n_value2_present = sum(!is.na(long$value2)),
    n_unit_na   = sum(is.na(long$unit)),
    n_dup_keys  = sum(long$sub_id == 2)  # keys (country-nuts-year-species) with >1 row
  )
}

# Which species each country reports, in which years, and how
# (NA / zero / positive counts). Rows exist for nearly every year, so
# "row present" is NOT evidence of monitoring; see the zero diagnostics.
country_species_coverage <- function(long) {
  long |>
    dplyr::group_by(country, species, unit) |>
    dplyr::summarise(
      n_regions = dplyr::n_distinct(nuts_id),
      year_min  = min(year),
      year_max  = max(year),
      n_years   = dplyr::n_distinct(year),
      n_rows    = dplyr::n(),
      n_na      = sum(is.na(damage)),
      n_zero    = sum(damage == 0, na.rm = TRUE),
      n_pos     = sum(damage > 0, na.rm = TRUE),
      # years in which every region of the country reports 0 or NA
      years_all_zero = {
        yz <- tapply(dplyr::coalesce(damage, 0) == 0, year, all)
        paste(names(yz)[yz], collapse = ",")
      },
      .groups = "drop"
    )
}

# Species lookup as given in the file (host, tree type, category columns)
species_lookup_raw <- function(long) {
  long |>
    dplyr::group_by(species) |>
    dplyr::summarise(
      unit      = paste(sort(unique(stats::na.omit(unit))), collapse = "/"),
      host      = paste(sort(unique(host)), collapse = "/"),
      treetype  = paste(sort(unique(treetype)), collapse = "/"),
      sp_category = paste(sort(unique(sp_category)), collapse = "/"),
      countries = paste(sort(unique(country)), collapse = ","),
      n_countries = dplyr::n_distinct(country),
      .groups = "drop"
    )
}

write_csv_out <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path)
  path
}
