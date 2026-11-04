# Input tables for the predictability equation (Sx ~ IT x VPD x latitude x longitude)
#
#   regions      one row per region (NUTS code, PL forest directorate or country):
#                country, longitude, latitude, area, mean annual VPD (and temperature)
#   region_year  one row per region and year 2000-2022: the region's longitude,
#                latitude and VPD, I. typographus damage, and one column per
#                insect with its reported damage (empty = not recorded that year)
# Damage is value1 as reported (m3 for borers, ha for defoliators), CZ and DE
# spruce bark-borer group counted as I. typographus (as in the 2025 paper).

model_regions <- function(pos, clim) {
  dplyr::left_join(dplyr::select(pos, country, nuts_id, lon, lat, area_km2, position_source = source),
                   dplyr::select(clim, nuts_id, vpd_kPa = vpd, temp_C = temp), by = "nuts_id") |>
    dplyr::arrange(country, nuts_id)
}

model_region_year <- function(dmg, regions, meta) {
  w <- dmg |> dplyr::filter(!is.na(damage)) |>
    dplyr::select(nuts_id, year, species, damage) |>
    tidyr::pivot_wider(names_from = species, values_from = damage)
  codes <- setdiff(names(w), c("nuts_id", "year", "ips_typ"))
  # insects ordered: borers then defoliators, each by number of regions recorded
  n_reg <- vapply(codes, function(s) sum(tapply(!is.na(w[[s]]), w$nuts_id, any)), 0)
  gu <- meta$guild_from_file[match(codes, meta$species)]
  codes <- codes[order(gu != "borer", -n_reg)]
  tidyr::expand_grid(dplyr::select(regions, country, nuts_id, lon, lat, vpd_kPa), year = 2000:2022) |>
    dplyr::left_join(w, by = c("nuts_id", "year")) |>
    dplyr::select(country, nuts_id, year, lon, lat, vpd_kPa, ips_typ, dplyr::all_of(codes))
}

# Which insects can be paired with I. typographus: region-years and countries
# where both are recorded
model_insect_list <- function(ry, meta, ph_hier) {
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  codes <- setdiff(names(ry), c("country", "nuts_id", "year", "lon", "lat", "vpd_kPa", "ips_typ"))
  purrr::map_dfr(codes, function(s) {
    both <- !is.na(ry[[s]]) & !is.na(ry$ips_typ)
    tibble::tibble(code = s, insect = unname(nm[s]),
                   guild = meta$guild_from_file[match(s, meta$species)], unit = meta$unit[match(s, meta$species)],
                   aggregate_code = meta$is_aggregate[match(s, meta$species)],
                   region_years_with_ips = sum(both), regions_with_ips = dplyr::n_distinct(ry$nuts_id[both]),
                   countries_with_ips = dplyr::n_distinct(ry$country[both]),
                   countries = paste(sort(unique(ry$country[both])), collapse = " "),
                   share_zero_years = mean(ry[[s]][both] == 0),
                   meets_2_countries = countries_with_ips >= 2, meets_3_countries = countries_with_ips >= 3)
  }) |> dplyr::arrange(dplyr::desc(countries_with_ips), dplyr::desc(regions_with_ips))
}

write_model_tables <- function(regions, ry, ins, path) {
  how <- data.frame(`How to read` = c(
    "regions: one row per region (NUTS code, Polish forest directorate, or whole country where the data are national).",
    "  lon, lat: region position (NUTS 2021 label point; PL directorate city; FI forest centres share their NUTS2 point; IT = Friuli Venezia Giulia).",
    "  vpd_kPa: mean annual vapour pressure deficit, temp_C: mean annual air temperature, both WorldClim 2.1 (1970-2000), averaged in a circle of the region's area.",
    "region_year: one row per region and year 2000-2022; ips_typ = I. typographus; one column per insect code; empty = not recorded that year, 0 = recorded zero.",
    "  Damage: value1 as reported, m3 for borers, ha for defoliators. CZ and DE spruce bark-borer group counted as I. typographus.",
    "insects: every insect with the region-years and countries where it is recorded together with I. typographus.",
    "Host tree data not included yet."), check.names = FALSE)
  writexl::write_xlsx(list(regions = regions, region_year = ry, insects = ins, `how to read` = how), path)
  path
}
