# Predictability of every insect from Ips typographus, the bark beetle
# recorded in every region (brainstorm with the supervisor, before the PROLES
# methods note):
#
#   Sx ~ IT x VPD x latitude x longitude      (full interaction, 16 coefficients)
#
#   Sx   log(damage + 1) of the insect to predict, in a region-year
#   IT   log(damage + 1) of I. typographus in the same region-year
#   VPD  mean annual VPD of the region (R/climate.R), latitude and longitude
#        of the region position (R/gapfill_v2.R; Friuli Venezia Giulia for IT)
# All region-years where both insects are recorded, all regions (NUTS, PL
# forest directorates, countries). One ordinary least-squares model per insect.
# The host tree is not in the model yet (being worked out with the supervisor).
#
# Predictability = leave one country out: each country is predicted by the
# model fitted on the other countries only (so the insect must be recorded in
# at least 2 countries).
#   R2_fit     share of the insect's variation explained, fitted on all data
#   R2_tested  1 - sum(error^2) / sum((Sx - mean Sx)^2) over the held-out
#              predictions; 0 = no better than the overall mean, below 0 = worse
#   r_tested   correlation of held-out predictions and observations
# Baseline (as in the PROLES note): the European mean, i.e. each held-out
# region-year predicted by the mean Sx of the other countries in that year.

predict_all_formula <- sx ~ it * vpd * lat * lon

predict_all_data <- function(dmg, sx, pos, clim) {
  sp <- function(s) dplyr::filter(dmg, species == s, !is.na(damage)) |> dplyr::select(country, nuts_id, year, damage)
  dplyr::inner_join(dplyr::rename(sp(sx), sx = damage), dplyr::rename(sp("ips_typ"), it = damage),
                    by = c("country", "nuts_id", "year")) |>
    dplyr::mutate(sx = log1p(sx), it = log1p(it)) |>
    dplyr::inner_join(dplyr::select(clim, nuts_id, vpd), by = "nuts_id") |>
    dplyr::inner_join(dplyr::select(pos, nuts_id, lat, lon), by = "nuts_id")
}

# Leave-one-country-out predictions of the model and of the European mean
predict_all_cv <- function(d, by = "country") {
  purrr::map_dfr(unique(d[[by]]), function(cn) {
    tr <- d[d[[by]] != cn, ]; te <- d[d[[by]] == cn, ]
    m <- stats::lm(predict_all_formula, data = tr)
    eu <- dplyr::group_by(tr, year) |> dplyr::summarise(eu = mean(sx), .groups = "drop")
    te |> dplyr::left_join(eu, by = "year") |>
      dplyr::mutate(pred = suppressWarnings(stats::predict(m, newdata = te)),
                    eu = dplyr::coalesce(eu, mean(tr$sx)),
                    n_coef_train = sum(!is.na(stats::coef(m))))
  })
}

predict_all_one <- function(dmg, sx, pos, clim) {
  d <- predict_all_data(dmg, sx, pos, clim)
  base <- tibble::tibble(species = sx, region_years = nrow(d), regions = dplyr::n_distinct(d$nuts_id),
                         countries = dplyr::n_distinct(d$country),
                         countries_list = paste(sort(unique(d$country)), collapse = " "),
                         share_zero = mean(d$sx == 0))
  if (nrow(d) < 30 || stats::sd(d$sx) == 0) return(base)
  m <- stats::lm(predict_all_formula, data = d)
  r2t <- function(p, y) 1 - sum((y - p)^2) / sum((y - mean(y))^2)
  out <- dplyr::mutate(base, R2_fit = summary(m)$r.squared, n_coef = sum(!is.na(stats::coef(m))),
                       b_it_alone = unname(stats::coef(stats::lm(sx ~ it, d))[2]),
                       r_it_alone = stats::cor(d$sx, d$it))
  if (base$countries < 2) return(out)
  cv <- predict_all_cv(d)
  dplyr::mutate(out, R2_tested = r2t(cv$pred, cv$sx), r_tested = stats::cor(cv$pred, cv$sx),
                R2_tested_european_mean = r2t(cv$eu, cv$sx),
                worst_country = { e <- tapply((cv$sx - cv$pred)^2, cv$country, mean); names(which.max(e)) })
}

predict_all <- function(dmg, pos, clim, meta, ph_hier) {
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  codes <- setdiff(unique(dmg$species[!is.na(dmg$damage)]), "ips_typ")
  purrr::map_dfr(codes, function(s) predict_all_one(dmg, s, pos, clim)) |>
    dplyr::left_join(dplyr::select(meta, species, unit, guild = guild_from_file, is_aggregate), by = "species") |>
    dplyr::mutate(name = dplyr::coalesce(unname(nm[species]), species), .after = species) |>
    dplyr::arrange(dplyr::desc(R2_tested))
}

plot_predict_all <- function(pa) {
  d <- dplyr::filter(pa, !is.na(R2_tested)) |>
    dplyr::mutate(lab = paste0(name, "  (", countries, " countries, ", regions, " regions)"),
                  lab = forcats::fct_reorder(lab, R2_tested),
                  guild = dplyr::if_else(guild == "borer", "Bark and wood borers", "Defoliators"))
  lim <- c(max(-1, min(c(d$R2_tested, d$R2_tested_european_mean))), max(c(d$R2_tested, d$R2_fit, 0.1)))
  cl <- function(x) pmax(x, lim[1])
  ggplot2::ggplot(d, ggplot2::aes(y = lab)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(x = cl(R2_tested_european_mean)), shape = 124, size = 4, colour = "#5c5b57") +
    ggplot2::geom_point(ggplot2::aes(x = R2_fit), shape = 21, size = 2.2, colour = "#9c9b97", fill = NA) +
    ggplot2::geom_point(ggplot2::aes(x = cl(R2_tested), colour = guild), size = 3) +
    ggplot2::scale_colour_manual(values = c(`Bark and wood borers` = "#184f95", Defoliators = "#eb6834"), name = NULL) +
    ggplot2::coord_cartesian(xlim = lim) +
    ggplot2::labs(x = "R-squared", y = NULL,
                  title = "How well can I. typographus damage, VPD and location predict each insect?",
                  subtitle = paste("Model per insect: damage ~ I. typographus x VPD x latitude x longitude (log of damage + 1), all regions where both are recorded.",
                                   "\nFilled dot = tested R-squared, each country predicted from the others only. Open circle = R-squared fitted on all data.",
                                   "\nGrey bar = tested R-squared of the European mean of the other countries in the same year. Values below the left edge are drawn at the edge."),
                  caption = "EU-ForDAM value1, reported data. Insects recorded in at least 2 countries. VPD: WorldClim 2.1 (1970-2000). Host tree not yet in the model.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top")
}

# ---- Top five borers by total damage, I. typographus as the predictor ----------
# Same equation; tested both ways: hide a country (predict a country without
# records) and hide a region (fill a region inside the reporting countries).

top_borers <- function(dmg, n = 5) {
  dmg |> dplyr::filter(!is.na(damage), unit == "m3", species != "ips_typ") |>
    dplyr::group_by(species) |> dplyr::summarise(total_m3 = sum(damage), .groups = "drop") |>
    dplyr::mutate(share_of_borer_damage_pct = 100 * total_m3 / sum(dmg$damage[dmg$unit == "m3"], na.rm = TRUE)) |>
    dplyr::slice_max(total_m3, n = n)
}

top_borers_cv <- function(dmg, top, pos, clim) {
  purrr::map_dfr(top$species, function(s) {
    d <- predict_all_data(dmg, s, pos, clim)
    dplyr::bind_rows(
      if (dplyr::n_distinct(d$country) >= 2) dplyr::mutate(predict_all_cv(d, "country"), test = "hide a country"),
      dplyr::mutate(predict_all_cv(d, "nuts_id"), test = "hide a region")) |>
      dplyr::mutate(species = s, .before = 1)
  })
}

top_borers_scores <- function(cv, top, ph_hier) {
  r2t <- function(p, y) 1 - sum((y - p)^2) / sum((y - mean(y))^2)
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  cv |> dplyr::filter(!is.na(pred)) |> dplyr::group_by(species, test) |>
    dplyr::summarise(region_years = dplyr::n(), regions = dplyr::n_distinct(nuts_id), countries = dplyr::n_distinct(country),
                     R2_tested = r2t(pred, sx), r_tested = stats::cor(pred, sx),
                     R2_tested_european_mean = r2t(eu, sx), .groups = "drop") |>
    dplyr::left_join(top, by = "species") |>
    dplyr::mutate(name = unname(nm[species]), .after = species) |>
    dplyr::arrange(dplyr::desc(total_m3), test)
}

plot_top_borers <- function(cv, sc) {
  nm <- dplyr::distinct(sc, species, name, share_of_borer_damage_pct)
  d <- cv |> dplyr::filter(test == "hide a region", !is.na(pred)) |>
    dplyr::group_by(species, year) |> dplyr::summarise(observed = mean(sx), predicted = mean(pred), .groups = "drop") |>
    tidyr::pivot_longer(c(observed, predicted), names_to = "what", values_to = "v") |>
    dplyr::left_join(nm, by = "species") |>
    dplyr::left_join(dplyr::filter(sc, test == "hide a region") |> dplyr::select(species, R2_tested, R2_tested_european_mean), by = "species") |>
    dplyr::mutate(panel = paste0(name, " (", round(share_of_borer_damage_pct, 1), "% of borer damage)\ntested R2 = ", round(R2_tested, 2),
                                 "; European mean = ", round(R2_tested_european_mean, 2)),
                  panel = forcats::fct_reorder(panel, -share_of_borer_damage_pct),
                  what = factor(what, c("observed", "predicted"), c("observed", "predicted (region hidden)")))
  ggplot2::ggplot(d, ggplot2::aes(year, v, colour = what, linetype = what)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = c("#1f1e1c", "#eb6834"), name = NULL) +
    ggplot2::scale_linetype_manual(values = c("solid", "solid"), name = NULL) +
    ggplot2::facet_wrap(~ panel, ncol = 3, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "Mean log(damage + 1) over the regions",
                  title = "Top five borers predicted from Ips typographus, VPD and location",
                  subtitle = paste("Model per insect: damage ~ I. typographus x VPD x latitude x longitude. Each region predicted by the model fitted on all other regions;",
                                   "\nlines = yearly mean over the regions where both insects are recorded. European mean: tested R2 of predicting each region by the mean of the other regions that year."),
                  caption = "EU-ForDAM value1, reported data. VPD: WorldClim 2.1. Tested R2: 1 - squared error / variance over all held-out region-years (0 = no better than the overall mean).") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold"))
}
