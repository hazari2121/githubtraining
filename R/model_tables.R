# Input tables for the predictability equation (Sx ~ IT x VPD x latitude x longitude)
#
#   regions      one row per region (NUTS code, PL forest directorate or country):
#                country, longitude, latitude, area, mean annual VPD (and temperature)
#   region_year  one row per region and year 2000-2022: the region's longitude,
#                latitude and VPD, I. typographus damage, and one column per
#                insect with its reported damage (empty = not recorded that year)
# Damage is value1 as reported (m3 for borers, ha for defoliators), CZ and DE
# spruce bark-borer group counted as I. typographus (as in the 2025 paper).

model_regions <- function(pos, clim, hs = NULL, clim_paper = NULL) {
  out <- dplyr::left_join(dplyr::select(pos, country, nuts_id, lon, lat, area_km2, position_source = source),
                          dplyr::select(clim, nuts_id, vpd_worldclim_kPa = vpd, temp_worldclim_C = temp), by = "nuts_id")
  if (!is.null(clim_paper)) out <- dplyr::left_join(out, dplyr::select(clim_paper, nuts_id, vpd_kPa = vpd, temp_C = temp, precip_mm = precip), by = "nuts_id")
  else out <- dplyr::mutate(out, vpd_kPa = vpd_worldclim_kPa, temp_C = temp_worldclim_C)
  if (!is.null(hs)) out <- dplyr::left_join(out, model_host_shares(hs), by = "nuts_id")
  dplyr::arrange(out, country, nuts_id)
}

# Host tree per region from the NUTS forest appendix: share of the forest area
# (host ha / all forest ha) and growing stock (m3) of each host
model_host_shares <- function(hs) {
  w <- hs |> dplyr::filter(measure %in% c("ha", "m3")) |>
    tidyr::pivot_wider(id_cols = nuts_id, names_from = c(host_group, measure), values_from = value)
  hosts <- c("spruce", "pine", "fir", "oak", "conifers_all", "broadleaves_all")
  share <- lapply(hosts, function(h) w[[paste0(h, "_ha")]] / w$all_forest_ha)
  names(share) <- paste0("share_", hosts)
  stock <- lapply(hosts, function(h) w[[paste0(h, "_m3")]])
  names(stock) <- paste0("stock_m3_", hosts)
  area <- lapply(hosts, function(h) w[[paste0(h, "_ha")]])
  names(area) <- paste0("area_ha_", hosts)
  dplyr::bind_cols(tibble::tibble(nuts_id = w$nuts_id, forest_ha = w$all_forest_ha), tibble::as_tibble(share), tibble::as_tibble(area), tibble::as_tibble(stock))
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
model_insect_list <- function(ry, meta, ph_hier, lv = NULL, host_matrix = NULL) {
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
                   meets_2_countries = countries_with_ips >= 2 & !isTRUE(meta$is_aggregate[match(s, meta$species)]),
                   meets_3_countries = countries_with_ips >= 3 & !isTRUE(meta$is_aggregate[match(s, meta$species)]),
                   host = if (is.null(lv)) NA_character_ else lv$host_col[lv$level == "1_species" & lv$code == s][1])
  }) |>
    dplyr::mutate(host_columns = model_host_columns(code, guild, host, host_matrix),
                  host_source = dplyr::if_else(code %in% names(matrix_codes) & !is.null(host_matrix), "supervisor's matrix", "data-file host")) |> dplyr::arrange(dplyr::desc(countries_with_ips), dplyr::desc(regions_with_ips))
}

write_model_tables <- function(regions, ry, ins, path) {
  how <- data.frame(`How to read` = c(
    "regions: one row per region (NUTS code, Polish forest directorate, or whole country where the data are national).",
    "  lon, lat: region position (NUTS 2021 label point; PL directorate city; FI forest centres share their NUTS2 point; IT = Friuli Venezia Giulia).",
    "  vpd_kPa, temp_C, precip_mm: mean VPD (plantecophys), air temperature and precipitation (annual sum) from ERA5-Land (DKRZ portal), averaged over 2000-2022: the climate of the 2025 EU-ForDAM paper; empty for CZ01, DE3, DE5, DE6.",
    "  vpd_worldclim_kPa, temp_worldclim_C: the earlier WorldClim 2.1 (1970-2000) values, kept for comparison.",
    "  share_<host>: host forest area / all forest area; area_ha_<host>, stock_m3_<host>: host area and growing stock (NUTS forest appendix). Empty: no forest data (CZ01, DE3, DE5, DE6).",
    "region_year: one row per region and year 2000-2022; ips_typ = I. typographus; one column per insect code; empty = not recorded that year, 0 = recorded zero.",
    "  Damage: value1 as reported, m3 for borers, ha for defoliators. CZ and DE spruce bark-borer group counted as I. typographus.",
    "insects: every insect with the region-years and countries where it is recorded together with I. typographus.",
    "insects: host_columns = the forest columns that make up the insect's host amount: the supervisor's matrix where it names the insect, otherwise the host in the EU-ForDAM data file (m3 for borers, ha for defoliators)."), check.names = FALSE)
  writexl::write_xlsx(list(regions = regions, region_year = ry, insects = ins, `how to read` = how), path)
  path
}

# Codes of the insects named in the supervisor's host matrix
matrix_codes <- c(ips_typ = "Ips typographus", pha_cya = "Phaenops cyanea", tom_spp = "Tomicus minor and Tomicus piniperda",
                  ips_acu = "Ips acuminatus", pit_cha = "Pityogenes chalcographus", tor_vir = "Tortrix viridana",
                  geo_db = "Geometridae on oaks", lym_dis = "Lymantria dispar", lym_mon = "Lymantria monacha")

# Forest columns (of model_host_shares) that make up each insect's host amount:
# m3 of growing stock for borers, ha of area for defoliators
model_host_columns <- function(code, guild, host, host_matrix) {
  rename_col <- function(x) {
    x <- sub("^coniferous_", "conifers_all_", sub("^deciduous_", "broadleaves_all_", x))
    sub("^(.*)_(m3|ha)$", "\\2_\\1", x) |> sub(pattern = "^m3_", replacement = "stock_m3_") |> sub(pattern = "^ha_", replacement = "area_ha_")
  }
  vapply(seq_along(code), function(i) {
    if (!is.null(host_matrix) && code[i] %in% names(matrix_codes)) {
      cols <- strsplit(host_matrix$host_columns[trimws(host_matrix$group) == matrix_codes[[code[i]]]], " + ", fixed = TRUE)[[1]]
      return(paste(rename_col(cols), collapse = " + "))
    }
    if (is.na(host[i])) return(NA_character_)
    paste0(if (identical(guild[i], "borer")) "stock_m3_" else "area_ha_", host[i])
  }, "")
}
