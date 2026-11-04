# Predictability of every insect from Ips typographus with MuMIn::dredge
#
# Insects: recorded together with I. typographus in at least 3 countries
# (data/processed/model_region_year.csv, R/model_tables.R), only region-years
# where both are recorded, all regions.
# Global model:  Sx ~ IT x VPD x latitude x longitude   (log(damage + 1); VPD,
#   latitude, longitude standardised so the interactions are stable)
# dredge fits every sub-model that keeps IT and keeps the lower-order terms of
# each interaction it contains, and ranks them by AICc.
# The best model (lowest AICc) is then tested on data it has not seen:
#   R2_tested_region   hide one NUTS region at a time (fill a gap)
#   R2_tested_country  hide one country at a time (a country without records)
#   p_timing           I. typographus shuffled between years within each region
#                      (200 times): does its year-to-year timing improve the
#                      tested R2 beyond where it is common? q = FDR over insects.
# Meaningful pair: R2_tested_region > 0 and q < 0.05.

dredge_data <- function(ry, sx) {
  d <- ry[!is.na(ry[[sx]]) & !is.na(ry$ips_typ), ]
  tibble::tibble(country = d$country, nuts_id = d$nuts_id, year = d$year,
                 sx = log1p(d[[sx]]), it = log1p(d$ips_typ),
                 vpd = as.numeric(scale(d$vpd_kPa)), lat = as.numeric(scale(d$lat)), lon = as.numeric(scale(d$lon)))
}

tested_r2 <- function(d, fo, by) {
  p <- rep(NA_real_, nrow(d))
  for (k in unique(d[[by]])) { i <- d[[by]] == k; p[i] <- suppressWarnings(stats::predict(stats::lm(fo, d[!i, ]), d[i, ])) }
  1 - sum((d$sx - p)^2) / sum((d$sx - mean(d$sx))^2)
}

dredge_one <- function(ry, sx, n_perm = 200) {
  d <- dredge_data(ry, sx)
  global <- stats::lm(sx ~ it * vpd * lat * lon, data = d, na.action = "na.fail")
  dd <- suppressMessages(MuMIn::dredge(global, fixed = "it", rank = "AICc"))
  best_terms <- names(which(!is.na(unlist(dd[1, setdiff(names(dd), c("(Intercept)", "df", "logLik", "AICc", "delta", "weight"))]))))
  fo <- stats::reformulate(best_terms, response = "sx")
  best <- stats::lm(fo, data = d)
  r2_reg <- tested_r2(d, fo, "nuts_id")
  perm <- replicate(n_perm, { s <- dplyr::mutate(dplyr::group_by(d, nuts_id), it = sample(it)) |> dplyr::ungroup(); tested_r2(s, fo, "nuts_id") })
  imp <- MuMIn::sw(dd)
  list(
    summary = tibble::tibble(
      code = sx, region_years = nrow(d), regions = dplyr::n_distinct(d$nuts_id), countries = dplyr::n_distinct(d$country),
      models_compared = nrow(dd), best_equation = paste("Sx ~", paste(best_terms, collapse = " + ")),
      n_terms = length(best_terms), models_within_2_AICc = sum(dd$delta < 2), weight_best = dd$weight[1],
      R2_fit = summary(best)$r.squared,
      R2_tested_region = r2_reg,
      R2_tested_country = tested_r2(d, fo, "country"),
      R2_tested_region_it_alone = tested_r2(d, sx ~ it, "nuts_id"),
      R2_tested_region_full = tested_r2(d, sx ~ it * vpd * lat * lon, "nuts_id"),
      R2_tested_region_shuffled_it = stats::median(perm),
      p_timing = (sum(perm >= r2_reg) + 1) / (n_perm + 1)),
    importance = tibble::tibble(code = sx, term = names(imp), importance = as.numeric(imp)))
}

dredge_all <- function(ry, ins, min_countries = 3, n_perm = 200) {
  codes <- ins$code[ins$countries_with_ips >= min_countries]
  res <- lapply(codes, function(s) { cat(s, format(Sys.time(), "%H:%M:%S"), "\n"); dredge_one(ry, s, n_perm) })
  sm <- dplyr::bind_rows(lapply(res, `[[`, "summary")) |>
    dplyr::mutate(q_timing = stats::p.adjust(p_timing, "BH"),
                  meaningful = R2_tested_region > 0 & q_timing < 0.05) |>
    dplyr::left_join(dplyr::select(ins, code, insect, guild, unit, aggregate_code, countries_list = countries, share_zero_years), by = "code") |>
    dplyr::relocate(insect, guild, .after = code) |>
    dplyr::arrange(dplyr::desc(R2_tested_region))
  list(summary = sm, importance = dplyr::bind_rows(lapply(res, `[[`, "importance")))
}

plot_dredge <- function(sm) {
  d <- sm |> dplyr::mutate(lab = paste0(insect, "  (", countries, " countries, ", regions, " regions)"),
                           lab = forcats::fct_reorder(lab, R2_tested_region),
                           status = dplyr::if_else(meaningful, "meaningful pair", "not meaningful"))
  lo <- -1; cl <- function(x) pmax(x, lo)
  long <- tidyr::pivot_longer(d, c(R2_tested_region, R2_tested_region_it_alone, R2_tested_region_full), names_to = "model", values_to = "r2") |>
    dplyr::mutate(model = factor(model, c("R2_tested_region", "R2_tested_region_it_alone", "R2_tested_region_full"),
                                 c("best dredge model", "I. typographus alone", "full equation (all terms)")))
  ggplot2::ggplot(long, ggplot2::aes(cl(r2), lab)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(shape = model, colour = model, alpha = status), size = 2.6) +
    ggplot2::scale_shape_manual(values = c(16, 1, 4), name = NULL) +
    ggplot2::scale_colour_manual(values = c("#184f95", "#eb6834", "#9c9b97"), name = NULL) +
    ggplot2::scale_alpha_manual(values = c(`meaningful pair` = 1, `not meaningful` = 0.45), name = NULL) +
    ggplot2::coord_cartesian(xlim = c(lo, 1)) +
    ggplot2::labs(x = "Tested R-squared: each NUTS region predicted by the model fitted on all other regions (0 = no better than the mean)", y = NULL,
                  title = "How predictable is each insect from I. typographus? Best equation chosen by dredge",
                  subtitle = paste("Global model: damage ~ I. typographus x VPD x latitude x longitude (log of damage + 1); dredge keeps I. typographus and ranks all sub-models by AICc.",
                                   "\nInsects recorded together with I. typographus in at least 3 countries. Meaningful pair (solid): tested R-squared > 0 and the timing of",
                                   "\nI. typographus significant (shuffled between years within regions, FDR q < 0.05). Values below -1 drawn at -1."),
                  caption = "EU-ForDAM value1, reported data, region-years where both insects are recorded. VPD: WorldClim 2.1. Host tree not yet in the model.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top")
}
