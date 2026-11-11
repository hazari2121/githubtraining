# Tables from the whiteboard (supervisor and student), built from our data
#
#   pairs            every target x predictor pair: overlap (countries, regions,
#                    region-years), coverage (% of the target's regions where the
#                    predictor is recorded), R2 of levels and R2 of ups and downs
#   coverage_matrix  target (rows) x predictor (columns): coverage %
#   r2_matrix        target x predictor: R2 of ups and downs (within regions)
#   gaps_by_region   for each target, the regions where it is NOT recorded and which
#                    predictors ARE recorded there (the "?" regions of the map)
#   gaps_count       target x region: number of predictors recorded where the target is missing
#   host_matrix      insect (rows) x host tree (columns): 1 = host of the insect
#   group_r2         borer and defoliator groups on pine, spruce, fir: R2 of ups and downs
#   training         one row per target x region x year where the target is recorded:
#                    target damage, I. typographus damage, VPD, temperature, precipitation,
#                    latitude, longitude, host amount (the equation's inputs)
#
# Overlap = same region and same year, both recorded (0 counts as recorded).
# R2_levels: squared correlation of log(damage + 1) of target and predictor over
#   the overlapping region-years (how much damage, region against region).
# R2_ups_downs: the same after standardising each region's series (z-score within
#   the region; regions with at least 3 overlapping years): do bad years coincide?

wb_codes <- function(ry, meta) {
  codes <- setdiff(names(ry), c("country", "nuts_id", "year", "lon", "lat", "vpd_kPa"))
  setdiff(codes, meta$species[meta$is_aggregate])
}

zscore_within <- function(x, g) {
  stats::ave(x, g, FUN = function(v) if (length(v) < 3 || stats::sd(v) == 0) rep(NA_real_, length(v)) else (v - mean(v)) / stats::sd(v))
}

wb_pairs <- function(ry, meta, nm) {
  codes <- wb_codes(ry, meta)
  reg_of <- lapply(codes, function(s) unique(ry$nuts_id[!is.na(ry[[s]])])); names(reg_of) <- codes
  purrr::map_dfr(codes, function(t) purrr::map_dfr(setdiff(codes, t), function(p) {
    both <- !is.na(ry[[t]]) & !is.na(ry[[p]])
    a <- log1p(ry[[t]][both]); b <- log1p(ry[[p]][both]); g <- ry$nuts_id[both]
    r_lev <- if (sum(both) >= 10 && stats::sd(a) > 0 && stats::sd(b) > 0) stats::cor(a, b) else NA_real_
    za <- zscore_within(a, g); zb <- zscore_within(b, g); ok <- !is.na(za) & !is.na(zb)
    r_ud <- if (sum(ok) >= 10) stats::cor(za[ok], zb[ok]) else NA_real_
    tibble::tibble(target = t, target_name = unname(nm[t]), predictor = p, predictor_name = unname(nm[p]),
                   countries = dplyr::n_distinct(ry$country[both]), regions = dplyr::n_distinct(g), region_years = sum(both),
                   target_regions = length(reg_of[[t]]),
                   coverage_pct = 100 * length(intersect(reg_of[[t]], reg_of[[p]])) / length(reg_of[[t]]),
                   r_levels = r_lev, R2_levels = r_lev^2, r_ups_downs = r_ud, R2_ups_downs = r_ud^2,
                   country_list = paste(sort(unique(ry$country[both])), collapse = " "))
  }))
}

wb_matrix <- function(pairs, value, codes, digits = 2) {
  m <- tidyr::pivot_wider(dplyr::select(pairs, target, predictor, v = dplyr::all_of(value)), names_from = predictor, values_from = v)
  m <- m[match(codes, m$target), c("target", codes)]
  m[-1] <- lapply(m[-1], round, digits)
  dplyr::rename(m, `target \\ predictor` = target)
}

wb_gaps <- function(ry, meta, nm) {
  codes <- wb_codes(ry, meta)
  regions <- dplyr::distinct(ry, country, nuts_id)
  purrr::map_dfr(codes, function(t) {
    missing <- setdiff(regions$nuts_id, unique(ry$nuts_id[!is.na(ry[[t]])]))
    purrr::map_dfr(missing, function(r) {
      rr <- ry[ry$nuts_id == r, ]
      yrs <- vapply(setdiff(codes, t), function(p) sum(!is.na(rr[[p]])), 0)
      yrs <- yrs[yrs > 0]
      tibble::tibble(target = t, target_name = unname(nm[t]), country = rr$country[1], nuts_id = r,
                     n_predictors_recorded = length(yrs), ips_recorded = "ips_typ" %in% names(yrs),
                     predictors_recorded = paste(names(sort(yrs, decreasing = TRUE)), collapse = " "))
    })
  })
}

wb_gaps_count <- function(gaps, ry, codes) {
  m <- tidyr::pivot_wider(dplyr::select(gaps, target, nuts_id, n_predictors_recorded), names_from = nuts_id, values_from = n_predictors_recorded)
  regs <- unique(ry$nuts_id)
  m <- m[match(codes, m$target), c("target", intersect(regs, names(m)))]
  dplyr::rename(m, `target \\ region (empty = target recorded there)` = target)
}

wb_host_matrix <- function(ins, nm) {
  hosts <- c("spruce", "pine", "fir", "oak", "conifers_all", "broadleaves_all")
  hc <- dplyr::coalesce(ins$host_columns, "")
  out <- tibble::tibble(insect = ins$code, name = unname(nm[ins$code]), guild = ins$guild, host_source = ins$host_source)
  for (h in hosts) out[[h]] <- as.integer(grepl(paste0("_", h, "($| )"), hc))
  out
}

# Groups of the 2025 paper's level 2 (borers on fir, spruce, pine; defoliators on spruce, pine)
wb_group_r2 <- function(dmg, ph_hier) {
  h <- dplyr::filter(ph_hier, !is.na(level2_paper), species != "xyl_lin")
  tree <- c(`A. alba` = "fir", `P. abies` = "spruce", `P. sylvestris` = "pine")
  h$group <- paste(sub(" on .*", "", h$level2_paper), "on", tree[sub("^.* on ", "", h$level2_paper)])
  g <- dmg |> dplyr::filter(!is.na(damage)) |> dplyr::inner_join(dplyr::select(h, species, group), by = "species") |>
    dplyr::group_by(group, country, nuts_id, year) |> dplyr::summarise(damage = sum(damage), .groups = "drop")
  groups <- sort(unique(g$group))
  purrr::map_dfr(groups, function(a) purrr::map_dfr(groups, function(b) {
    j <- dplyr::inner_join(dplyr::filter(g, group == a), dplyr::filter(g, group == b), by = c("country", "nuts_id", "year"), suffix = c("_a", "_b"))
    za <- zscore_within(log1p(j$damage_a), j$nuts_id); zb <- zscore_within(log1p(j$damage_b), j$nuts_id); ok <- !is.na(za) & !is.na(zb)
    tibble::tibble(group_a = a, group_b = b, regions = dplyr::n_distinct(j$nuts_id), region_years = nrow(j),
                   R2_ups_downs = if (a != b && sum(ok) >= 10) stats::cor(za[ok], zb[ok])^2 else NA_real_)
  }))
}

wb_training <- function(ry, regions, ins, meta, nm) {
  codes <- setdiff(wb_codes(ry, meta), "ips_typ")
  purrr::map_dfr(codes, function(t) {
    d <- ry[!is.na(ry[[t]]), ]
    hc <- ins$host_columns[ins$code == t]
    amount <- if (length(hc) && !is.na(hc)) Reduce(`+`, lapply(strsplit(hc, " + ", fixed = TRUE)[[1]], function(cn) regions[[cn]]))[match(d$nuts_id, regions$nuts_id)] else NA_real_
    tibble::tibble(target = t, target_name = unname(nm[t]), guild = meta$guild_from_file[match(t, meta$species)],
                   unit = meta$unit[match(t, meta$species)], country = d$country, nuts_id = d$nuts_id, year = d$year,
                   target_damage = d[[t]], ips_typ_damage = d$ips_typ,
                   vpd_kPa = d$vpd_kPa, temp_C = regions$temp_C[match(d$nuts_id, regions$nuts_id)],
                   precip_mm = regions$precip_mm[match(d$nuts_id, regions$nuts_id)], lat = d$lat, lon = d$lon,
                   host_columns = hc, host_amount = amount,
                   forest_ha = regions$forest_ha[match(d$nuts_id, regions$nuts_id)])
  })
}

# ---- The board's host table, for all four boxes of the 2 x 2 grid ---------------------
# BORER / DEFOLIATOR  x  CONIFERS / BROADLEAVES: host trees in the rows, insects in the
# columns, 1 = host tree of the insect.
#   host in the data file: HOSTTREE of the EU-ForDAM database (one tree per insect)
#   host used in the model: the supervisor's host matrix where it names the insect
#     (e.g. P. chalcographus on spruce and pine; L. dispar on all broadleaves),
#     otherwise the data-file host
# Extra rows: the forest-appendix column used for the host amount, regions and
# countries where the insect is recorded. Aggregate group codes are left out.

host_tree_rows <- list(
  conifers = c(pine = "Pinus sylvestris", spruce = "Picea abies", fir = "Abies alba", larch = "Larix decidua"),
  broadleaves = c(oak = "Quercus spp.", birch = "Betula spp.", elm = "Ulmus spp.", `horse chestnut` = "Aesculus spp.", `poplar, willow` = "Populus, Salix"))

wb_host_of_code <- function(raw_file = "data/raw/EUForDam_REW3.xlsx") {
  r <- readxl::read_excel(raw_file, "EUForDam_REW", guess_max = 50000)
  r |> dplyr::mutate(HOSTTREE = trimws(sub("\\?", "", HOSTTREE))) |>
    dplyr::filter(!is.na(HOSTTREE)) |> dplyr::count(sp_code, HOSTTREE, TREETYPE) |>
    dplyr::group_by(sp_code) |> dplyr::slice_max(n, n = 1, with_ties = FALSE) |> dplyr::ungroup() |>
    dplyr::select(code = sp_code, hosttree = HOSTTREE, treetype = TREETYPE)
}

# Host trees used in the model, as tree names (from the insect list's host_columns)
model_host_trees <- function(host_columns) {
  if (is.na(host_columns) || host_columns == "") return(character())
  cols <- sub("^(stock_m3|area_ha)_", "", strsplit(host_columns, " + ", fixed = TRUE)[[1]])
  unique(unlist(lapply(cols, function(c) switch(c, conifers_all = names(host_tree_rows$conifers),
                                                 broadleaves_all = names(host_tree_rows$broadleaves), c))))
}

wb_host_blocks <- function(ry, meta, nm, ins, hosts) {
  codes <- wb_codes(ry, meta)
  info <- tibble::tibble(code = codes, name = unname(nm[codes]), guild = meta$guild_from_file[match(codes, meta$species)]) |>
    dplyr::left_join(hosts, by = "code") |>
    dplyr::mutate(regions = vapply(code, function(s) dplyr::n_distinct(ry$nuts_id[!is.na(ry[[s]])]), 0),
                  countries = vapply(code, function(s) dplyr::n_distinct(ry$country[!is.na(ry[[s]])]), 0),
                  host_columns = dplyr::if_else(code == "ips_typ", "stock_m3_spruce", ins$host_columns[match(code, ins$code)]),
                  host_source = dplyr::if_else(code == "ips_typ", "supervisor's matrix", ins$host_source[match(code, ins$code)]))
  blocks <- list()
  for (g in c("borer", "defoliator")) for (ht in names(host_tree_rows)) {
    trees <- host_tree_rows[[ht]]
    x <- dplyr::filter(info, guild == g, hosttree %in% trees) |>
      dplyr::arrange(match(hosttree, trees), dplyr::desc(regions))
    if (!nrow(x)) next
    lab <- function(v) stats::setNames(as.list(v), x$name)
    file_rows <- lapply(names(trees), function(t) lab(as.integer(x$hosttree == trees[[t]])))
    model_rows <- lapply(names(trees), function(t) lab(as.integer(vapply(x$host_columns, function(h) t %in% model_host_trees(h), TRUE))))
    num <- dplyr::bind_rows(
      dplyr::bind_cols(tibble::tibble(row = paste("host in data file:", names(trees))), dplyr::bind_rows(file_rows)),
      dplyr::bind_cols(tibble::tibble(row = paste("host used in model:", names(trees))), dplyr::bind_rows(model_rows)))
    txt <- function(label, v) dplyr::bind_cols(tibble::tibble(row = label), tibble::as_tibble(lab(v)))
    inf <- dplyr::bind_rows(txt("forest column for the host amount", dplyr::coalesce(x$host_columns, "")),
                            txt("host from", dplyr::coalesce(x$host_source, "")), txt("code", x$code),
                            txt("regions recorded", as.character(x$regions)), txt("countries recorded", as.character(x$countries)))
    tab <- list(num = num, info = inf)
    blocks[[paste0(if (g == "borer") "borers" else "defoliators", "_on_", ht)]] <- tab
  }
  list(blocks = blocks, info = info)
}

write_host_blocks <- function(blocks, info, path) {
  wb <- openxlsx::createWorkbook()
  how <- data.frame(`How to read` = c(
    "The host table of the whiteboard for all four boxes of the 2 x 2 grid: borers / defoliators x conifers / broadleaves.",
    "Rows: host trees; columns: insects (species and genus-level taxa; aggregate group codes left out); 1 = host tree of the insect.",
    "host in data file: HOSTTREE of the EU-ForDAM database (one tree per insect).",
    "host used in model: the supervisor's host matrix where it names the insect (e.g. P. chalcographus on spruce and pine,",
    "  L. dispar on all broadleaves), otherwise the data-file host. Larch, birch, elm, horse chestnut and poplar have no",
    "  own column in the forest appendix, so their host amount is all conifers or all broadleaves.",
    "Further rows: the forest column used for the host amount, where the host comes from, the code, regions and countries recorded.",
    "all_insects: the same information as one list."), check.names = FALSE)
  sheets <- c(list(`how to read` = how), blocks, list(all_insects = info))
  for (s in names(sheets)) {
    openxlsx::addWorksheet(wb, s)
    if (s %in% names(blocks)) {
      num <- sheets[[s]]$num; inf <- sheets[[s]]$info; n <- ncol(num)
      openxlsx::writeData(wb, s, num, headerStyle = openxlsx::createStyle(textDecoration = "bold", textRotation = 90))
      openxlsx::writeData(wb, s, inf, startRow = nrow(num) + 3, colNames = FALSE)
      openxlsx::conditionalFormatting(wb, s, cols = 2:n, rows = 2:(nrow(num) + 1), rule = "==1", style = openxlsx::createStyle(bgFill = "#9ec5e8"))
      openxlsx::setColWidths(wb, s, cols = 1, widths = 36); openxlsx::setColWidths(wb, s, cols = 2:n, widths = 6)
      openxlsx::freezePane(wb, s, firstRow = TRUE, firstCol = TRUE)
    } else openxlsx::writeData(wb, s, sheets[[s]], headerStyle = openxlsx::createStyle(textDecoration = "bold"))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}
