# Species - NUTS occurrence ranking (suggested by T. Hlasny).
#
# "Occurs" in a region = at least one year with damage > 0.
# "Monitored" in a region = at least one year with a reported value (0 or > 0).
# Damage totals are only comparable within a unit (borers m3, defoliators ha),
# so shares and within-region ranks are computed per unit.
# Region counts mix NUTS levels: 11 countries report one national series, which
# counts as one region (column n_national_units).

nuts_level <- function(nuts_id) {
  dplyr::case_when(
    grepl(";", nuts_id)    ~ "NUTS1 (merged)",
    nchar(nuts_id) == 2    ~ "national",
    nchar(nuts_id) == 3    ~ "NUTS1",
    nchar(nuts_id) == 4    ~ "NUTS2",
    nchar(nuts_id) == 5    ~ "NUTS3",
    TRUE ~ NA_character_
  )
}

# Per region-species summary (observed cells only)
region_species_summary <- function(cells, min_nonzero = 6) {
  cells |>
    dplyr::filter(status != "not_monitored") |>
    dplyr::group_by(country, nuts_id, species, unit) |>
    dplyr::summarise(
      years_monitored   = dplyr::n(),
      years_with_damage = sum(damage > 0),
      first_year = min(year), last_year = max(year),
      total_damage = sum(damage),
      max_damage   = max(damage),
      peak_year    = if (max(damage) > 0) year[which.max(damage)] else NA_integer_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      nuts_level = nuts_level(nuts_id),
      occurs = years_with_damage > 0,
      usable_series = years_with_damage >= min_nonzero   # Hlasny et al. (2025) filter
    )
}

# Table 1: species ranked by number of NUTS units where they occur
species_occurrence_ranking <- function(rs, species_meta) {
  rs |>
    dplyr::group_by(species, unit) |>
    dplyr::summarise(
      n_countries_monitored = dplyr::n_distinct(country),
      n_countries_occurring = dplyr::n_distinct(country[occurs]),
      countries_occurring   = paste(sort(unique(country[occurs])), collapse = ","),
      n_regions_monitored   = dplyr::n(),
      n_regions_occurring   = sum(occurs),
      n_national_units      = sum(occurs & nuts_level == "national"),
      n_usable_series       = sum(usable_series),
      region_years_with_damage = sum(years_with_damage),
      total_damage          = sum(total_damage),
      .groups = "drop"
    ) |>
    dplyr::group_by(unit) |>
    dplyr::mutate(pct_of_unit_total = 100 * total_damage / sum(total_damage)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pct_regions_occurring = 100 * n_regions_occurring / n_regions_monitored,
      rank_by_regions = rank(-n_regions_occurring, ties.method = "min"),
      rank_by_damage_within_unit = NA_integer_
    ) |>
    dplyr::group_by(unit) |>
    dplyr::mutate(rank_by_damage_within_unit = as.integer(rank(-total_damage, ties.method = "min"))) |>
    dplyr::ungroup() |>
    dplyr::left_join(dplyr::select(species_meta, species, guild = guild_from_file,
                                   host_tree, is_aggregate), by = "species") |>
    dplyr::arrange(rank_by_regions, dplyr::desc(total_damage)) |>
    dplyr::relocate(rank_by_regions, species, guild, host_tree, unit)
}

# Table 2: within each region, species ranked by total damage (per unit)
region_species_ranking <- function(rs) {
  rs |>
    dplyr::group_by(country, nuts_id, unit) |>
    dplyr::mutate(
      rank_in_region = as.integer(rank(-total_damage, ties.method = "min")),
      pct_of_region_unit_total = if (sum(total_damage) > 0)
        100 * total_damage / sum(total_damage) else NA_real_
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(country, nuts_id, unit, rank_in_region) |>
    dplyr::select(country, nuts_id, nuts_level, unit, rank_in_region, species,
                  total_damage, pct_of_region_unit_total, years_with_damage, years_monitored,
                  first_year, last_year, peak_year, usable_series)
}

# Table 3: region x species matrix of years with damage (blank = not monitored)
occurrence_matrix <- function(rs, sp_rank) {
  rs |>
    dplyr::mutate(species = factor(species, sp_rank$species)) |>
    dplyr::arrange(species) |>
    dplyr::select(country, nuts_id, nuts_level, species, years_with_damage) |>
    tidyr::pivot_wider(names_from = species, values_from = years_with_damage) |>
    dplyr::arrange(country, nuts_id)
}

write_ranking_xlsx <- function(sp_rank, reg_rank, occ, path) {
  readme <- tibble::tibble(
    sheet = c("species_ranking", "region_ranking", "occurrence_matrix", "", "Definitions"),
    description = c(
      "Species ranked by the number of NUTS units where they occur (>= 1 year with damage > 0). Damage ranks and shares are within unit (m3 for borers, ha for defoliators).",
      "Within each NUTS unit, species ranked by total damage 2000-2022, separately per unit.",
      "Number of years with damage > 0 for each NUTS unit x species. Blank = not monitored. 0 = monitored, never any damage.",
      "",
      "Occurs = at least one year with damage > 0. usable_series = at least 6 years with damage > 0 (filter of Hlasny et al. 2025). NUTS levels differ by country: 11 countries report one national series. CZ spruce bark-borer group counted as Ips typographus; CZ sub-regions summed to NUTS2."
    )
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(list(README = readme, species_ranking = sp_rank,
                           region_ranking = reg_rank, occurrence_matrix = occ), path)
  path
}
