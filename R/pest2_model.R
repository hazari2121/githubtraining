# Predicting pest 2 damage from pest 1: the hierarchical spatial model of the
# PROLES methods note "Predicting pest 2 forest damage from pest 1" (30 October
# 2026), implemented as written there.
#
#   log D2[r,t] = alpha_r + beta_r * log D1[r,t] + beta_lag * log D1[r,t-1] + delta_t + eps[r,t]
#   alpha_r     = a0 + a1 * vpd_r + a2 * host_r + u_alpha_r
#   beta_r      = b0 + b1 * vpd_r + b2 * host_r + u_beta_r
# The note's temperature is replaced by VPD, the climate variable of the 2025
# EU-ForDAM paper (temperature was left out there as it is correlated with
# VPD, Appendix E).
#   u ~ GP(lat, lon), delta_t ~ N(0, sd_delta), eps ~ AR(1)
#
# Our data in the note's layout (one row per region and year):
#   region  EU-ForDAM region (NUTS code or PL forest directorate)
#   d1, d2  reported damage of pest 1 and pest 2 (value1; m3 or ha)
#   vpd     mean annual VPD of the region (R/climate.R, data/external/region_climate.csv)
#   host    share of pest 2's host tree in the region's forest area (forest appendix: host ha / all forest ha)
#   lat/lon region position (R/gapfill_v2.R region_positions)
#   known   pest 2 recorded in the region (complete region); FALSE = prediction region
# Regions = all regions where pest 1 is recorded and the forest appendix has the host.

pest2_read_climate <- function(path = region_climate_file) {
  if (!file.exists(path)) stop("Climate missing: run R/climate.R (region_climate) to write ", path, call. = FALSE)
  dplyr::select(utils::read.csv(path), nuts_id, vpd)
}

# The note's input table: region, year, d1, d2, vpd, host, lat, lon, known
pest2_table <- function(dmg, pest1, pest2, host_col, pos, hs, clim) {
  sp <- function(s) dplyr::filter(dmg, species == s, !is.na(damage)) |> dplyr::select(country, nuts_id, year, damage)
  d1 <- sp(pest1); d2 <- sp(pest2); known_regions <- unique(d2$nuts_id)
  host <- hs |> dplyr::filter(measure == "ha", host_group %in% c(host_col, "all_forest")) |>
    tidyr::pivot_wider(id_cols = nuts_id, names_from = host_group, values_from = value) |>
    dplyr::transmute(nuts_id, host = .data[[host_col]] / all_forest) |>
    dplyr::filter(!is.na(host), is.finite(host))
  d1 |> dplyr::rename(d1 = damage) |>
    dplyr::left_join(dplyr::rename(d2, d2 = damage), by = c("country", "nuts_id", "year")) |>
    dplyr::mutate(known = nuts_id %in% known_regions) |>
    dplyr::inner_join(host, by = "nuts_id") |>
    dplyr::inner_join(clim, by = "nuts_id") |>
    dplyr::inner_join(dplyr::select(pos, nuts_id, lat, lon), by = "nuts_id") |>
    dplyr::transmute(region = factor(nuts_id), country, year, d1, d2, vpd, host, lat, lon, known) |>
    dplyr::arrange(region, year)
}

# Section 7, step 2 of the note. The lag is last year's pest 1 of the same
# region (year t - 1); a year without a pest 1 record the year before is
# dropped, as the note drops the first year.
pest2_prep <- function(d) {
  lag1 <- dplyr::transmute(d, region, year = year + 1, ld1_lag = log(d1 + 1))
  d |>
    dplyr::mutate(ld1 = log(d1 + 1), ld2 = log(d2 + 1)) |>
    dplyr::left_join(lag1, by = c("region", "year")) |>
    dplyr::mutate(vpd_s = as.numeric(scale(vpd)), host_s = as.numeric(scale(host)),
                  x_km = (lon - mean(lon)) * 60, y_km = (lat - mean(lat)) * 111,
                  year_f = factor(year)) |>
    dplyr::filter(!is.na(ld1_lag))
}

# Section 7, step 3: the model, priors and sampler settings of the note
pest2_formula <- function() {
  brms::bf(ld2 ~ ld1 + ld1_lag +
             vpd_s + host_s + vpd_s:ld1 + host_s:ld1 +
             gp(x_km, y_km, k = NA, scale = TRUE) +
             gp(x_km, y_km, by = ld1, k = NA, scale = TRUE) +
             (1 + ld1 | region) + (1 | year_f) +
             ar(time = year, gr = region, p = 1))
}

pest2_priors <- function() {
  c(brms::prior(normal(0, 1), class = b),
    brms::prior(normal(0, 2), class = Intercept),
    brms::prior(exponential(2), class = sd),
    brms::prior(exponential(2), class = sdgp),
    brms::prior(inv_gamma(3, 300), class = lscale),
    brms::prior(lkj(2), class = cor),
    brms::prior(exponential(2), class = sigma))
}

pest2_fit <- function(train, chains = 4, iter = 3000, warmup = 1500, seed = 42, file = NULL) {
  brms::brm(pest2_formula(), data = train, family = gaussian(), prior = pest2_priors(),
            chains = chains, iter = iter, warmup = warmup, cores = chains,
            control = list(adapt_delta = 0.95, max_treedepth = 12), seed = seed, file = file)
}

# Baseline (a) of the note: pooled slope, no region structure
pest2_fit_base <- function(train, seed = 42, file = NULL) {
  brms::brm(ld2 ~ ld1 + ld1_lag + vpd_s + host_s + (1 | year_f), data = train, family = gaussian(),
            chains = 2, iter = 2000, cores = 2, refresh = 0, seed = seed, file = file)
}

pest2_draw_summary <- function(te, pp) {
  tibble::tibble(region = as.character(te$region), year = te$year, obs = te$ld2,
                 pred = apply(pp, 2, stats::median),
                 lo90 = apply(pp, 2, stats::quantile, 0.05), hi90 = apply(pp, 2, stats::quantile, 0.95))
}

# Section 6: leave one region out. Hierarchical model and baseline (a) refitted
# without the region (chains = 2, iter = 2000 as in the note); baseline (b) =
# the European mean pest 2 (log scale) of the other regions in each year, with
# the 5-95 % range of the other regions as its interval.
pest2_loro_one <- function(r, train, fit, base) {
  tr <- dplyr::filter(train, region != r); te <- dplyr::filter(train, region == r)
  f <- stats::update(fit, newdata = tr, recompile = FALSE, refresh = 0, chains = 2, iter = 2000, warmup = 1000, cores = 2)
  b <- stats::update(base, newdata = tr, recompile = FALSE, refresh = 0, chains = 2, iter = 2000, cores = 2)
  h <- pest2_draw_summary(te, brms::posterior_predict(f, newdata = te, allow_new_levels = TRUE, sample_new_levels = "gaussian"))
  p <- pest2_draw_summary(te, brms::posterior_predict(b, newdata = te, allow_new_levels = TRUE, sample_new_levels = "gaussian"))
  eu <- tr |> dplyr::group_by(year) |>
    dplyr::summarise(pred = mean(ld2), lo90 = stats::quantile(ld2, 0.05), hi90 = stats::quantile(ld2, 0.95), .groups = "drop")
  m <- dplyr::left_join(dplyr::select(te, year, obs = ld2), eu, by = "year") |> dplyr::mutate(region = as.character(r), .before = 1)
  dplyr::bind_rows(dplyr::mutate(h, model = "hierarchical"), dplyr::mutate(p, model = "pooled regression"),
                   dplyr::mutate(m, model = "European mean"))
}

# Scores of the note, per held-out region and over all regions: RMSE and MAE on
# the log scale and on the original damage scale, 90 % coverage, series correlation
pest2_scores <- function(lo, by_region = FALSE) {
  g <- if (by_region) c("model", "region") else "model"
  lo |> dplyr::filter(!is.na(pred)) |> dplyr::group_by(dplyr::across(dplyr::all_of(g))) |>
    dplyr::summarise(n = dplyr::n(),
                     rmse_log = sqrt(mean((pred - obs)^2)), mae_log = mean(abs(pred - obs)),
                     rmse_damage = sqrt(mean((expm1(pred) - expm1(obs))^2)), mae_damage = mean(abs(expm1(pred) - expm1(obs))),
                     coverage90 = mean(obs >= lo90 & obs <= hi90),
                     cor_series = if (dplyr::n() > 2 && stats::sd(pred) > 0 && stats::sd(obs) > 0) stats::cor(pred, obs) else NA_real_,
                     .groups = "drop")
}

# Section 5: predictions for the prediction regions, back on the damage scale
pest2_predict_new <- function(fit, newreg) {
  pp <- brms::posterior_predict(fit, newdata = newreg, allow_new_levels = TRUE, sample_new_levels = "gaussian")
  q <- function(p) expm1(apply(pp, 2, stats::quantile, p))
  dplyr::select(newreg, region, country, year, d1) |>
    dplyr::mutate(d2_median = expm1(apply(pp, 2, stats::median)),
                  d2_lo50 = q(0.25), d2_hi50 = q(0.75), d2_lo90 = q(0.05), d2_hi90 = q(0.95))
}

# Section 7, step 6: plots in the PROLES palette
proles <- c(jehlici = "#2B4334", mech = "#B1CF82", briza = "#FAEED9", smola = "#E19230")

theme_proles <- function(base_size = 12) {
  fam <- if ("mulish" %in% sysfonts_families()) "mulish" else ""
  ggplot2::theme_minimal(base_size = base_size, base_family = fam) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = proles[["briza"]], colour = NA),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "#2B433422"),
                   text = ggplot2::element_text(colour = proles[["jehlici"]]),
                   strip.text = ggplot2::element_text(face = "bold", colour = proles[["jehlici"]]),
                   plot.title = ggplot2::element_text(face = "bold"))
}
sysfonts_families <- function() if (requireNamespace("sysfonts", quietly = TRUE)) sysfonts::font_families() else character()

plot_pest2_predictions <- function(pred_new, title, unit) {
  ggplot2::ggplot(pred_new, ggplot2::aes(year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = d2_lo90, ymax = d2_hi90), fill = proles[["mech"]], alpha = 0.35) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = d2_lo50, ymax = d2_hi50), fill = proles[["mech"]], alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = d2_median), colour = proles[["jehlici"]], linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = d1), colour = proles[["smola"]], linewidth = 0.6, linetype = "22") +
    ggplot2::facet_wrap(~ region, scales = "free_y", ncol = 6) +
    ggplot2::labs(title = title, subtitle = "Green: median with 50 % and 90 % intervals. Orange dashed: observed pest 1.",
                  x = NULL, y = paste0("Damage (", unit, ")")) +
    theme_proles(10)
}

plot_pest2_slope_map <- function(fit, train) {
  re <- brms::ranef(fit)$region[, , "ld1"] |> as.data.frame() |>
    tibble::rownames_to_column("region") |>
    dplyr::left_join(dplyr::distinct(dplyr::mutate(train, region = as.character(region)), region, lat, lon), by = "region")
  ggplot2::ggplot(re, ggplot2::aes(lon, lat, size = abs(Estimate), colour = Estimate > 0)) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::scale_colour_manual(values = c(`FALSE` = proles[["smola"]], `TRUE` = proles[["jehlici"]]),
                                 labels = c(`FALSE` = "weaker coupling", `TRUE` = "stronger coupling"), name = NULL) +
    ggplot2::coord_quickmap() +
    ggplot2::labs(title = "Region-specific deviation of the pest 1 → pest 2 slope", size = "|deviation|") +
    theme_proles(10)
}

plot_pest2_loro <- function(lo, title) {
  ggplot2::ggplot(dplyr::filter(lo, model == "hierarchical"), ggplot2::aes(year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo90, ymax = hi90), fill = proles[["mech"]], alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = pred), colour = proles[["jehlici"]], linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = obs), colour = proles[["smola"]], size = 1.1) +
    ggplot2::facet_wrap(~ region, ncol = 6) +
    ggplot2::labs(title = title, subtitle = "Each region hidden and predicted from the others. Green: median and 90 % interval. Orange: observed.",
                  x = NULL, y = "log(pest 2 damage + 1)") +
    theme_proles(10)
}
