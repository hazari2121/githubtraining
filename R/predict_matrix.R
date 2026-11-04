# Predictability matrix (notes, pages 4-6): rows = predictor (I. typographus or
# a borer group), columns = target (species and groups), cell = how well the
# predictor predicts the target.
#
# Groups as in the 2025 paper (R/level_pairs.R paper_group_map, level 2):
# borers on fir, spruce, pine; defoliators on spruce, pine. A group's value in a
# region-year = sum of its recorded member codes. A target is never part of its
# own predictor: codes in both are taken out of the predictor (e.g. borers on
# spruce predicting P. chalcographus = borers on spruce without P. chalcographus).
#
# Each cell: Sx ~ P x VPD x latitude x longitude built up step by step from
# Sx ~ P (R/predict_all.R forward_terms), log(damage + 1), all region-years
# where both are recorded. Tested by hiding one NUTS region at a time.
# Cells only where the two overlap in at least 2 countries (notes: limit of
# 2-3 countries) and 30 region-years.
#   R2_tested  tested R2, hide a region (0 = no better than the overall mean)
#   r          plain correlation of the two log series (for reference)

matrix_series <- function(dmg, ph_hier) {
  gm <- paper_group_map(ph_hier) |> dplyr::filter(level == "level2")
  sp <- dplyr::filter(dmg, !is.na(damage)) |> dplyr::select(country, nuts_id, year, species, damage)
  groups <- split(gm$code, gm$group)
  list(members = c(stats::setNames(as.list(unique(sp$species)), unique(sp$species)), groups), data = sp)
}

series_value <- function(ms, codes, drop = character()) {
  keep <- setdiff(codes, drop)
  if (!length(keep)) return(NULL)
  ms$data |> dplyr::filter(species %in% keep) |>
    dplyr::group_by(country, nuts_id, year) |> dplyr::summarise(v = sum(damage), .groups = "drop")
}

matrix_cell <- function(ms, pred, target, pos, clim, min_countries = 2, min_n = 30) {
  tc <- ms$members[[target]]; pc <- ms$members[[pred]]
  p <- series_value(ms, pc, drop = tc); t <- series_value(ms, tc)
  if (is.null(p) || is.null(t)) return(NULL)
  d <- dplyr::inner_join(dplyr::rename(t, sx = v), dplyr::rename(p, it = v), by = c("country", "nuts_id", "year")) |>
    dplyr::mutate(sx = log1p(sx), it = log1p(it)) |>
    dplyr::inner_join(dplyr::select(clim, nuts_id, vpd), by = "nuts_id") |>
    dplyr::inner_join(dplyr::select(pos, nuts_id, lat, lon), by = "nuts_id")
  out <- tibble::tibble(predictor = pred, target = target, region_years = nrow(d), regions = dplyr::n_distinct(d$nuts_id),
                        countries = dplyr::n_distinct(d$country),
                        predictor_codes_removed = paste(intersect(pc, tc), collapse = " "))
  if (out$countries < min_countries || nrow(d) < min_n || stats::sd(d$sx) == 0 || stats::sd(d$it) == 0) return(out)
  fw <- forward_terms(d)
  dplyr::mutate(out, r = stats::cor(d$sx, d$it), R2_tested = loro_r2(d, fw$formula),
                equation = sub("^sx", "Sx", paste(deparse(fw$formula, width.cutoff = 200), collapse = "")))
}

predict_matrix <- function(dmg, ph_hier, pos, clim, predictors = c("ips_typ", "Borers on fir", "Borers on spruce", "Borers on pine")) {
  ms <- matrix_series(dmg, ph_hier)
  targets <- setdiff(names(ms$members), "ips_typ")
  purrr::map_dfr(predictors, function(p) purrr::map_dfr(setdiff(targets, p), function(t) matrix_cell(ms, p, t, pos, clim)))
}

plot_predict_matrix <- function(pm, ph_hier, meta) {
  nm <- c(stats::setNames(ph_hier$name, ph_hier$species), ips_typ = "Ips typographus")
  lab <- function(x) dplyr::coalesce(unname(nm[x]), x)
  gt <- c("Borers on fir", "Borers on spruce", "Borers on pine", "Defoliators on spruce", "Defoliators on pine")
  guild <- c(stats::setNames(meta$guild_from_file, meta$species), stats::setNames(c("borer", "borer", "borer", "defoliator", "defoliator"), gt))
  d <- dplyr::filter(pm, !is.na(R2_tested)) |>
    dplyr::mutate(kind = dplyr::case_when(target %in% gt ~ "Groups", guild[target] == "borer" ~ "Borer species", TRUE ~ "Defoliator species"),
                  kind = factor(kind, c("Groups", "Borer species", "Defoliator species")),
                  target_lab = lab(target), predictor_lab = factor(lab(predictor), lab(c("ips_typ", "Borers on fir", "Borers on spruce", "Borers on pine"))),
                  shown = pmax(R2_tested, -0.5), txt = formatC(R2_tested, format = "f", digits = 2))
  ord <- d |> dplyr::group_by(target_lab) |> dplyr::summarise(m = max(R2_tested)) |> dplyr::arrange(m)
  d$target_lab <- factor(d$target_lab, ord$target_lab)
  ggplot2::ggplot(d, ggplot2::aes(predictor_lab, target_lab, fill = shown)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = txt), size = 2.4) +
    ggplot2::scale_fill_gradient2(low = "#c9c8c3", mid = "white", high = "#184f95", midpoint = 0, limits = c(-0.5, 1),
                                  name = "Tested R-squared\n(below -0.5 shown as -0.5)") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::facet_grid(kind ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "Predictor", y = "Target",
                  title = "How well does each predictor predict each target?",
                  subtitle = paste("Cell = tested R-squared when one NUTS region is hidden and predicted from the others.",
                                   "\nModel per cell: target ~ predictor x VPD x latitude x longitude, built up step by step from target ~ predictor; log(damage + 1).",
                                   "\nPairs recorded together in at least 2 countries and 30 region-years. Empty = too little overlap."),
                  caption = paste("Groups as in the 2025 EU-ForDAM paper; a group = sum of its recorded member codes. A target is never part of its own predictor",
                                  "\n(e.g. borers on spruce without P. chalcographus when predicting P. chalcographus). EU-ForDAM value1, reported data. VPD: WorldClim 2.1.")) +
    theme_eu(9) + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold", hjust = 0), panel.grid = ggplot2::element_blank())
}

# Every pair (notes: "predictability of everything that has a meaningful pair"):
# every code and group predicting every other one, both directions
predict_matrix_all <- function(dmg, ph_hier, pos, clim) {
  ms <- matrix_series(dmg, ph_hier)
  all <- names(ms$members)
  purrr::map_dfr(all, function(p) purrr::map_dfr(setdiff(all, p), function(t) matrix_cell(ms, p, t, pos, clim)))
}

plot_predict_matrix_all <- function(pm, ph_hier, meta) {
  nm <- c(stats::setNames(ph_hier$name, ph_hier$species), ips_typ = "Ips typographus")
  lab <- function(x) dplyr::coalesce(unname(nm[x]), x)
  gt <- c("Borers on fir", "Borers on spruce", "Borers on pine", "Defoliators on spruce", "Defoliators on pine")
  guild <- c(stats::setNames(meta$guild_from_file, meta$species), stats::setNames(c("borer", "borer", "borer", "defoliator", "defoliator"), gt))
  kind <- function(x) factor(dplyr::case_when(x %in% gt ~ "Groups", guild[x] == "borer" ~ "Borers", TRUE ~ "Defoliators"), c("Groups", "Borers", "Defoliators"))
  d <- dplyr::filter(pm, !is.na(R2_tested))
  keep <- union(d$predictor, d$target)
  ord <- tibble::tibble(code = keep, k = kind(keep), lab = lab(keep)) |> dplyr::arrange(k, lab)
  d <- dplyr::mutate(d, p = factor(lab(predictor), ord$lab), t = factor(lab(target), rev(ord$lab)),
                     pk = kind(predictor), tk = kind(target), shown = pmax(pmin(R2_tested, 1), -0.5))
  ggplot2::ggplot(d, ggplot2::aes(p, t, fill = shown)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#c9c8c3", mid = "white", high = "#184f95", midpoint = 0, limits = c(-0.5, 1),
                                  name = "Tested R-squared\n(below -0.5 shown as -0.5)") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::facet_grid(tk ~ pk, scales = "free", space = "free") +
    ggplot2::labs(x = "Predictor", y = "Target", title = "Predictability of every insect and group from every other one",
                  subtitle = paste("Cell = tested R-squared when one NUTS region is hidden; target ~ predictor x VPD x latitude x longitude built up step by step.",
                                   "\nPairs recorded together in at least 2 countries and 30 region-years; empty = too little overlap. Read across a row: what predicts this target."),
                  caption = "Groups as in the 2025 EU-ForDAM paper (sum of recorded member codes; a target is taken out of its predictor). EU-ForDAM value1, reported data. VPD: WorldClim 2.1.") +
    theme_eu(8) + ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5, size = 6.5),
                                 axis.text.y = ggplot2::element_text(size = 6.5), panel.grid = ggplot2::element_blank(),
                                 strip.text = ggplot2::element_text(face = "bold"))
}
