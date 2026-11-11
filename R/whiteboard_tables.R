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
# BORER / DEFOLIATOR  x  CONIFERS / BROADLEAVES. One plain table per box: host trees
# in the rows, insects in the columns, 1 = the insect lives on that tree.
# Host tree = HOSTTREE of the EU-ForDAM database (one tree per insect), plus any
# single tree the supervisor's host matrix names for the insect (P. chalcographus:
# spruce and pine). Where the matrix uses a whole forest type for the host AMOUNT
# (e.g. L. dispar: all broadleaves), that is a choice of forest column, not a host
# tree, and is listed on the insects sheet instead.

host_tree_rows <- list(
  conifers = c(pine = "Pinus sylvestris", spruce = "Picea abies", fir = "Abies alba", larch = "Larix decidua"),
  broadleaves = c(oak = "Quercus spp.", birch = "Betula spp.", elm = "Ulmus spp.", `horse chestnut` = "Aesculus spp.", `poplar/willow` = "Populus, Salix"))

wb_host_of_code <- function(raw_file = "data/raw/EUForDam_REW3.xlsx") {
  r <- readxl::read_excel(raw_file, "EUForDam_REW", guess_max = 50000)
  r |> dplyr::mutate(HOSTTREE = trimws(sub("\\?", "", HOSTTREE))) |>
    dplyr::filter(!is.na(HOSTTREE)) |> dplyr::count(sp_code, HOSTTREE, TREETYPE) |>
    dplyr::group_by(sp_code) |> dplyr::slice_max(n, n = 1, with_ties = FALSE) |> dplyr::ungroup() |>
    dplyr::select(code = sp_code, hosttree = HOSTTREE, treetype = TREETYPE)
}

# Single trees the supervisor's matrix names (spruce, pine, fir, oak), not whole forest types
matrix_trees <- function(host_columns) {
  if (is.na(host_columns) || host_columns == "") return(character())
  cols <- sub("^(stock_m3|area_ha)_", "", strsplit(host_columns, " + ", fixed = TRUE)[[1]])
  intersect(cols, c("spruce", "pine", "fir", "oak"))
}

# "stock_m3_spruce + stock_m3_pine" -> "growing stock (m3): spruce + pine"
host_amount_label <- function(hc) {
  if (is.na(hc) || hc == "") return("")
  cols <- strsplit(hc, " + ", fixed = TRUE)[[1]]
  what <- if (all(startsWith(cols, "stock_m3_"))) "growing stock (m3)" else "area (ha)"
  trees <- sub("^(stock_m3|area_ha)_", "", cols)
  trees <- dplyr::recode(trees, conifers_all = "all conifers", broadleaves_all = "all broadleaves", all_forest = "all forest")
  paste0(what, ": ", paste(trees, collapse = " + "))
}

wb_host_blocks <- function(ry, meta, nm, ins, hosts) {
  codes <- wb_codes(ry, meta)
  tree_name <- stats::setNames(names(unlist(unname(host_tree_rows))), unlist(unname(host_tree_rows)))
  info <- tibble::tibble(code = codes, insect = unname(nm[codes]), guild = meta$guild_from_file[match(codes, meta$species)]) |>
    dplyr::left_join(hosts, by = "code") |>
    dplyr::mutate(host_type = dplyr::if_else(hosttree %in% host_tree_rows$conifers, "conifers", "broadleaves"),
                  host_columns = dplyr::if_else(code == "ips_typ", "stock_m3_spruce", ins$host_columns[match(code, ins$code)]),
                  host_trees = vapply(seq_along(code), function(i) paste(unique(c(tree_name[[hosttree[i]]], matrix_trees(host_columns[i]))), collapse = ", "), ""),
                  host_tree_in_data_file = unname(tree_name[hosttree]),
                  host_amount_from = vapply(host_columns, host_amount_label, ""),
                  host_amount_source = dplyr::if_else(code == "ips_typ", "supervisor's matrix", ins$host_source[match(code, ins$code)]),
                  regions = vapply(code, function(s) dplyr::n_distinct(ry$nuts_id[!is.na(ry[[s]])]), 0),
                  countries = vapply(code, function(s) dplyr::n_distinct(ry$country[!is.na(ry[[s]])]), 0)) |>
    dplyr::arrange(guild, host_type, match(hosttree, unlist(unname(host_tree_rows))), dplyr::desc(regions))
  blocks <- list()
  for (g in c("borer", "defoliator")) for (ht in names(host_tree_rows)) {
    x <- dplyr::filter(info, guild == g, host_type == ht)
    if (!nrow(x)) next
    trees <- names(host_tree_rows[[ht]])
    tab <- tibble::tibble(`host tree` = trees)
    for (k in seq_len(nrow(x))) tab[[x$insect[k]]] <- as.integer(trees %in% strsplit(x$host_trees[k], ", ")[[1]])
    blocks[[paste0(if (g == "borer") "borers_on_" else "defoliators_on_", ht)]] <- tab
  }
  info <- dplyr::select(info, insect, code, guild, host_type, host_trees, host_tree_in_data_file, host_amount_from, host_amount_source, regions, countries)
  list(blocks = blocks, info = info)
}

write_host_blocks <- function(blocks, info, path) {
  wb <- openxlsx::createWorkbook()
  how <- data.frame(`How to read` = c(
    "One table per box of the whiteboard's 2 x 2 grid: borers / defoliators on conifers / broadleaves.",
    "Rows: host trees. Columns: insects (species and genus-level taxa; aggregate group codes left out). 1 = the insect lives on that tree.",
    "Host tree: the host recorded in the EU-ForDAM database, plus a tree the supervisor's host matrix adds (P. chalcographus: spruce and pine).",
    "insects: every insect with its host trees, the forest data used for its host amount (supervisor's matrix, otherwise the data-file host),",
    "  and the regions and countries where it is recorded. Larch, birch, elm, horse chestnut and poplar/willow have no own column in the",
    "  forest data, so their host amount is all conifers or all broadleaves."), check.names = FALSE)
  sheets <- c(list(`how to read` = how), blocks, list(insects = info))
  hs <- openxlsx::createStyle(textDecoration = "bold")
  for (s in names(sheets)) {
    openxlsx::addWorksheet(wb, s)
    openxlsx::writeData(wb, s, sheets[[s]], headerStyle = hs)
    if (s %in% names(blocks)) {
      n <- ncol(sheets[[s]]); m <- nrow(sheets[[s]])
      openxlsx::conditionalFormatting(wb, s, cols = 2:n, rows = 2:(m + 1), rule = "==1", style = openxlsx::createStyle(bgFill = "#9ec5e8"))
      openxlsx::addStyle(wb, s, openxlsx::createStyle(textDecoration = "bold", textRotation = 90, valign = "bottom"), rows = 1, cols = 2:n)
      openxlsx::addStyle(wb, s, openxlsx::createStyle(halign = "center"), rows = 2:(m + 1), cols = 2:n, gridExpand = TRUE)
      openxlsx::setColWidths(wb, s, cols = 1, widths = 16); openxlsx::setColWidths(wb, s, cols = 2:n, widths = 5)
      openxlsx::setRowHeights(wb, s, rows = 1, heights = 150)
      openxlsx::freezePane(wb, s, firstRow = TRUE, firstCol = TRUE)
    } else openxlsx::setColWidths(wb, s, cols = seq_along(sheets[[s]]), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

# Shared host tree of target and predictor (host trees as in the host tables):
# shared_host = TRUE when the two live on at least one common tree
wb_add_shared_host <- function(pairs, info) {
  ht <- stats::setNames(strsplit(info$host_trees, ", ", fixed = TRUE), info$code)
  gu <- stats::setNames(info$guild, info$code)
  common <- mapply(function(a, b) paste(intersect(ht[[a]], ht[[b]]), collapse = ", "), pairs$target, pairs$predictor)
  pairs |> dplyr::mutate(target_host = vapply(ht[target], paste, "", collapse = ", "),
                         predictor_host = vapply(ht[predictor], paste, "", collapse = ", "),
                         shared_host = common != "", shared_host_trees = unname(common),
                         same_guild = unname(gu[target] == gu[predictor]), .after = predictor_name)
}

# ---- Source insect x target NUTS (the board's "TARGET NUTS / SOURCE" table) -----------
# For each target insect: the regions where it is NOT recorded (the "?" regions)
# in the columns, the source insects recorded there in the rows, cell = number of
# years the source is recorded in that region (empty = not recorded). Each source
# row also carries how it pairs with the target where both are recorded (shared
# host, R2 of ups and downs, R2 of levels, countries of overlap).
# all_sources_by_region: every insect x every region, years recorded.

wb_source_by_nuts <- function(ry, meta, nm, pairs) {
  codes <- wb_codes(ry, meta)
  regs <- dplyr::distinct(ry, country, nuts_id) |> dplyr::arrange(country, nuts_id)
  yrs <- sapply(codes, function(s) tapply(!is.na(ry[[s]]), ry$nuts_id, sum)[regs$nuts_id])
  rownames(yrs) <- regs$nuts_id
  per_target <- lapply(stats::setNames(codes, codes), function(t) {
    gap <- regs$nuts_id[yrs[, t] == 0]
    if (!length(gap)) return(NULL)
    src <- setdiff(codes, t)
    y <- t(yrs[gap, src, drop = FALSE]); y[y == 0] <- NA
    keep <- rowSums(!is.na(y)) > 0
    if (!any(keep)) return(NULL)
    p <- dplyr::filter(pairs, target == t) |> dplyr::select(predictor, shared_host, countries, R2_ups_downs, R2_levels)
    out <- tibble::tibble(source = src[keep], source_name = unname(nm[src[keep]])) |>
      dplyr::left_join(p, by = c("source" = "predictor")) |>
      dplyr::rename(overlap_countries = countries) |>
      dplyr::mutate(dplyr::across(c(R2_ups_downs, R2_levels), ~ round(.x, 2)),
                    target_regions_covered = rowSums(!is.na(y[keep, , drop = FALSE])))
    out <- dplyr::bind_cols(out, tibble::as_tibble(y[keep, , drop = FALSE]))
    dplyr::arrange(out, dplyr::desc(shared_host), dplyr::desc(R2_ups_downs), dplyr::desc(target_regions_covered))
  })
  per_target <- per_target[!vapply(per_target, is.null, TRUE)]
  summary <- purrr::map_dfr(names(per_target), function(t) {
    x <- per_target[[t]]; gap <- setdiff(names(x), c("source", "source_name", "shared_host", "overlap_countries", "R2_ups_downs", "R2_levels", "target_regions_covered"))
    tibble::tibble(target = t, target_name = unname(nm[t]), regions_recorded = sum(yrs[, t] > 0), regions_missing = length(gap),
                   sources_in_missing_regions = nrow(x),
                   ips_in_missing_regions = if ("ips_typ" %in% x$source) x$target_regions_covered[x$source == "ips_typ"] else 0L,
                   shared_host_sources = sum(x$shared_host, na.rm = TRUE))
  }) |> dplyr::arrange(dplyr::desc(regions_recorded))
  all_src <- dplyr::bind_cols(tibble::tibble(insect = codes, insect_name = unname(nm[codes])), tibble::as_tibble(t(yrs)))
  list(per_target = per_target, summary = summary, all_sources = all_src)
}

write_source_by_nuts <- function(sb, path) {
  how <- data.frame(`How to read` = c(
    "The whiteboard's TARGET NUTS x SOURCE table, from EU-ForDAM data (value1, 2000-2022).",
    "One sheet per target insect (sheet name = insect code): the columns after the source information are the regions where the TARGET is NOT recorded",
    "  (the regions we would like to predict); each row is a SOURCE insect recorded in at least one of them; cell = years the source is recorded there.",
    "  shared_host: the source lives on a tree of the target; overlap_countries, R2_ups_downs, R2_levels: how source and target relate where both are recorded",
    "  (see WHITEBOARD_tables, sheet pairs); target_regions_covered: in how many of the target's missing regions the source is recorded.",
    "  Rows sorted: shared host first, then by R2 of ups and downs.",
    "summary: one row per target: regions recorded / missing, sources available in the missing regions, regions where I. typographus is available.",
    "all_sources_by_region: every insect (rows) x every region (columns), years recorded.",
    "Aggregate group codes (e.g. CambXyl_con&MD) are left out: they are not species."), check.names = FALSE)
  wb <- openxlsx::createWorkbook(); hs <- openxlsx::createStyle(textDecoration = "bold")
  add <- function(name, x, shade_from = NULL) {
    openxlsx::addWorksheet(wb, name); openxlsx::writeData(wb, name, x, headerStyle = hs)
    openxlsx::freezePane(wb, name, firstRow = TRUE, firstCol = TRUE)
    if (!is.null(shade_from) && ncol(x) >= shade_from) {
      openxlsx::addStyle(wb, name, openxlsx::createStyle(textDecoration = "bold", textRotation = 90), rows = 1, cols = shade_from:ncol(x))
      openxlsx::setColWidths(wb, name, cols = shade_from:ncol(x), widths = 4.5)
      openxlsx::conditionalFormatting(wb, name, cols = shade_from:ncol(x), rows = 2:(nrow(x) + 1), type = "colourScale", style = c("#FFFFFF", "#b1cf82", "#2B4334"))
    }
  }
  add("how to read", how); add("summary", sb$summary)
  for (t in sb$summary$target) add(substr(gsub("[^A-Za-z0-9_]", "_", t), 1, 31), sb$per_target[[t]], 8)
  add("all_sources_by_region", sb$all_sources, 3)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}
