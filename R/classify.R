# Step 2: classify every region-year-species cell.
#
# Grid = every NUTS region in the file x 2000-2022 x every species code.
#   observed_positive : value reported, > 0
#   observed_zero     : value reported, = 0  (treated as real; never filled)
#   not_monitored     : value NA, or no row for that country-species-year
#   impossible        : host tree absent from region. TODO: needs host-tree maps;
#                       column `host_absent` is a placeholder (all NA) for now.
#
# Coding confirmed by the data provider (Oct 2026): 0 = monitored, no damage;
# empty = not monitored.
classify_cells <- function(dmg, years = 2000:2022) {
  regions <- dplyr::distinct(dmg, country, nuts_id)
  grid <- tidyr::crossing(regions, year = years, species = sort(unique(dmg$species)))

  grid |>
    dplyr::left_join(dmg, by = c("country", "nuts_id", "year", "species")) |>
    dplyr::mutate(
      status = dplyr::case_when(
        is.na(damage) ~ "not_monitored",
        damage == 0   ~ "observed_zero",
        TRUE          ~ "observed_positive"
      ),
      host_absent = NA  # TODO: fill from host-tree maps -> status "impossible"
    )
}

# Share of cells in each class, by a grouping variable
coverage_summary <- function(cells, by) {
  g <- rlang::sym(by)
  cells |>
    dplyr::group_by(!!g) |>
    dplyr::summarise(
      n_cells = dplyr::n(),
      pct_observed_positive = 100 * mean(status == "observed_positive"),
      pct_observed_zero     = 100 * mean(status == "observed_zero"),
      pct_not_monitored     = 100 * mean(status == "not_monitored"),
      pct_impossible        = NA_real_,  # TODO: needs host-tree maps
      .groups = "drop"
    )
}

# Country x species coverage: share of region-years observed (positive or zero)
coverage_country_species <- function(cells) {
  cells |>
    dplyr::group_by(country, species) |>
    dplyr::summarise(
      n_regions = dplyr::n_distinct(nuts_id),
      years_observed = if (any(status != "not_monitored"))
        paste(range(year[status != "not_monitored"]), collapse = "-") else NA_character_,
      pct_observed = 100 * mean(status %in% c("observed_positive", "observed_zero")),
      pct_positive = 100 * mean(status == "observed_positive"),
      .groups = "drop"
    )
}
