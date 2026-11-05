# Where target and predictor species overlap
#
# Overlap = the same region (NUTS code, PL forest directorate or country) in
# the same year, both insects recorded (a value, 0 included). From the input
# table data/processed/model_region_year.csv (R/model_tables.R).
#
# Workbook sheets
#   how to read
#   pairs                one row per target x predictor pair: countries, regions and
#                        region-years of overlap, years when both had damage,
#                        the countries, meets 2 / 3 countries
#   countries_matrix     target (rows) x predictor (columns): number of countries of overlap
#   regions_matrix       same, number of regions
#   region_years_matrix  same, number of region-years
#   both_damage_matrix   same, number of region-years when both had damage (> 0)
#   ips_as_predictor     I. typographus as the predictor: every target with its overlap
#   insects_by_region    region (rows) x insect (columns): number of years recorded

overlap_codes <- function(ry) setdiff(names(ry), c("country", "nuts_id", "year", "lon", "lat", "vpd_kPa"))

overlap_pairs <- function(ry, ins = NULL, ph_hier = NULL) {
  codes <- overlap_codes(ry)
  rec <- lapply(codes, function(s) !is.na(ry[[s]])); names(rec) <- codes
  dmg <- lapply(codes, function(s) !is.na(ry[[s]]) & ry[[s]] > 0); names(dmg) <- codes
  nm <- if (is.null(ph_hier)) stats::setNames(codes, codes) else stats::setNames(ph_hier$name, ph_hier$species)
  out <- purrr::map_dfr(codes, function(t) purrr::map_dfr(setdiff(codes, t), function(p) {
    both <- rec[[t]] & rec[[p]]
    tibble::tibble(target = t, predictor = p,
                   countries = dplyr::n_distinct(ry$country[both]), regions = dplyr::n_distinct(ry$nuts_id[both]),
                   region_years = sum(both), region_years_both_damage = sum(dmg[[t]] & dmg[[p]]),
                   first_year = if (any(both)) min(ry$year[both]) else NA_integer_, last_year = if (any(both)) max(ry$year[both]) else NA_integer_,
                   country_list = paste(sort(unique(ry$country[both])), collapse = " "))
  }))
  out |> dplyr::mutate(target_name = unname(nm[target]), predictor_name = unname(nm[predictor]), .after = predictor) |>
    dplyr::mutate(meets_2_countries = countries >= 2, meets_3_countries = countries >= 3) |>
    dplyr::arrange(dplyr::desc(countries), dplyr::desc(region_years))
}

overlap_matrix <- function(pairs, value, codes) {
  m <- tidyr::pivot_wider(dplyr::select(pairs, target, predictor, v = dplyr::all_of(value)), names_from = predictor, values_from = v)
  m <- m[match(codes, m$target), c("target", codes)]
  dplyr::rename(m, `target \\ predictor` = target)
}

# Insects ordered: I. typographus first, then borers and defoliators by number of regions
overlap_order <- function(ry, meta) {
  codes <- overlap_codes(ry)
  n_reg <- vapply(codes, function(s) dplyr::n_distinct(ry$nuts_id[!is.na(ry[[s]])]), 0)
  gu <- meta$guild_from_file[match(codes, meta$species)]
  codes[order(codes != "ips_typ", gu != "borer", -n_reg)]
}

insects_by_region <- function(ry, codes) {
  ry |> dplyr::group_by(country, nuts_id) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(codes), ~ sum(!is.na(.x))), .groups = "drop")
}

write_overlap_xlsx <- function(pairs, ry, meta, ph_hier, path) {
  codes <- overlap_order(ry, meta)
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  names_tab <- tibble::tibble(code = codes, insect = dplyr::coalesce(unname(nm[codes]), codes),
                              guild = meta$guild_from_file[match(codes, meta$species)], unit = meta$unit[match(codes, meta$species)],
                              aggregate_code = meta$is_aggregate[match(codes, meta$species)],
                              regions_recorded = vapply(codes, function(s) dplyr::n_distinct(ry$nuts_id[!is.na(ry[[s]])]), 0),
                              countries_recorded = vapply(codes, function(s) dplyr::n_distinct(ry$country[!is.na(ry[[s]])]), 0))
  how <- data.frame(`How to read` = c(
    "Overlap = the same region (NUTS code, Polish forest directorate, or the whole country where data are national) in the same year, both insects recorded (0 counts as recorded).",
    "pairs: one row per target x predictor pair (every insect against every other insect).",
    "  countries / regions / region_years: how much the two overlap; region_years_both_damage: overlapping region-years with damage > 0 for both.",
    "  meets_2_countries / meets_3_countries: the pair overlaps in at least 2 / 3 countries (needed to test predictions in a country the model has not seen).",
    "countries_matrix, regions_matrix, region_years_matrix, both_damage_matrix: the same numbers as a grid, target in the rows, predictor in the columns.",
    "ips_as_predictor: I. typographus as the predictor, every target insect.",
    "insects_by_region: number of years each insect is recorded in each region.",
    "insects: code, name, guild, unit of every insect. EU-ForDAM value1, 2000-2022; CZ and DE spruce bark-borer group counted as I. typographus."), check.names = FALSE)
  ips <- dplyr::filter(pairs, predictor == "ips_typ") |> dplyr::select(-predictor, -predictor_name)
  sheets <- list(`how to read` = how, insects = names_tab, pairs = pairs, ips_as_predictor = ips,
                 countries_matrix = overlap_matrix(pairs, "countries", codes), regions_matrix = overlap_matrix(pairs, "regions", codes),
                 region_years_matrix = overlap_matrix(pairs, "region_years", codes), both_damage_matrix = overlap_matrix(pairs, "region_years_both_damage", codes),
                 insects_by_region = insects_by_region(ry, codes))
  wb <- openxlsx::createWorkbook()
  for (s in names(sheets)) {
    openxlsx::addWorksheet(wb, s)
    openxlsx::writeData(wb, s, sheets[[s]], headerStyle = openxlsx::createStyle(textDecoration = "bold"))
    openxlsx::freezePane(wb, s, firstRow = TRUE, firstCol = grepl("matrix|by_region", s))
    if (grepl("matrix", s)) {
      n <- nrow(sheets[[s]])
      openxlsx::conditionalFormatting(wb, s, cols = 2:(n + 1), rows = 2:(n + 1), type = "colourScale", style = c("#FFFFFF", "#9ec5e8", "#184f95"))
      openxlsx::addStyle(wb, s, openxlsx::createStyle(textRotation = 90, textDecoration = "bold"), rows = 1, cols = 2:(n + 1), gridExpand = TRUE)
      openxlsx::setColWidths(wb, s, cols = 2:(n + 1), widths = 4.5); openxlsx::setColWidths(wb, s, cols = 1, widths = 24)
    }
    if (s == "insects_by_region") {
      openxlsx::conditionalFormatting(wb, s, cols = 3:(ncol(sheets[[s]])), rows = 2:(nrow(sheets[[s]]) + 1), type = "colourScale", style = c("#FFFFFF", "#b1cf82", "#2B4334"))
      openxlsx::addStyle(wb, s, openxlsx::createStyle(textRotation = 90, textDecoration = "bold"), rows = 1, cols = 3:ncol(sheets[[s]]), gridExpand = TRUE)
      openxlsx::setColWidths(wb, s, cols = 3:ncol(sheets[[s]]), widths = 4.5)
    }
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}
