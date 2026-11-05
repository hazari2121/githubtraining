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

dredge_one <- function(ry, sx, n_perm = 200, rank = "AICc") {
  d <- dredge_data(ry, sx)
  global <- stats::lm(sx ~ it * vpd * lat * lon, data = d, na.action = "na.fail")
  dd <- suppressMessages(MuMIn::dredge(global, fixed = "it", rank = rank))
  best_terms <- names(which(!is.na(unlist(dd[1, setdiff(attr(dd, "terms"), "(Intercept)")]))))
  fo <- stats::reformulate(best_terms, response = "sx")
  best <- stats::lm(fo, data = d)
  r2_reg <- tested_r2(d, fo, "nuts_id")
  perm <- replicate(n_perm, { s <- dplyr::mutate(dplyr::group_by(d, nuts_id), it = sample(it)) |> dplyr::ungroup(); tested_r2(s, fo, "nuts_id") })
  imp <- MuMIn::sw(dd)
  list(
    summary = tibble::tibble(
      code = sx, rank = rank, region_years = nrow(d), regions = dplyr::n_distinct(d$nuts_id), countries = dplyr::n_distinct(d$country),
      models_compared = nrow(dd), best_equation = paste("Sx ~", paste(best_terms, collapse = " + ")),
      n_terms = length(best_terms), models_within_2_AICc = sum(dd$delta < 2), weight_best = dd$weight[1],
      R2_fit = summary(best)$r.squared,
      R2_tested_region = r2_reg,
      R2_tested_country = if (dplyr::n_distinct(d$country) >= 2) tested_r2(d, fo, "country") else NA_real_,
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

# Top five borers by total damage (after I. typographus), from the input table,
# best equation chosen by AICc and by BIC (stricter about extra terms)
dredge_top_borers <- function(ry, ins, n = 5, n_perm = 200) {
  borers <- ins$code[ins$guild == "borer" & !ins$aggregate_code]
  tot <- vapply(borers, function(s) sum(ry[[s]], na.rm = TRUE), 0)
  top <- names(sort(tot, decreasing = TRUE))[seq_len(n)]
  sm <- dplyr::bind_rows(lapply(top, function(s) dplyr::bind_rows(lapply(c("AICc", "BIC"), function(rk) {
    cat(s, rk, format(Sys.time(), "%H:%M:%S"), "\n"); dredge_one(ry, s, n_perm, rk)$summary }))))
  sm |> dplyr::mutate(total_damage_m3 = tot[code], q_timing = stats::p.adjust(p_timing, "BH"),
                      meaningful = R2_tested_region > 0 & q_timing < 0.05) |>
    dplyr::left_join(dplyr::select(ins, code, insect, countries_list = countries), by = "code") |>
    dplyr::relocate(insect, total_damage_m3, .after = code) |>
    dplyr::arrange(dplyr::desc(total_damage_m3), rank)
}

# ---- Without dredge: every allowed equation, chosen by the honest test --------------
# All sub-models of Sx ~ IT x VPD x latitude x longitude that keep IT and keep
# the lower-order parts of every interaction, each scored by hiding one NUTS
# region at a time; the best is the one that predicts hidden regions best.
# (The winner is picked on the same test it is scored on, so its score is a
# little optimistic.)

all_hierarchical_models <- function() {
  tt <- setdiff(labels(stats::terms(~ it * vpd * lat * lon)), "it")
  parts <- function(t) strsplit(t, ":", fixed = TRUE)[[1]]
  ok <- function(set) all(vapply(set, function(t) {
    p <- parts(t); if (length(p) == 1) return(TRUE)
    subs <- unlist(lapply(seq_len(length(p) - 1), function(k) apply(utils::combn(p, k), 2, paste, collapse = ":")))
    all(subs %in% c("it", set) | vapply(subs, function(s) any(vapply(c("it", set), function(x) setequal(parts(x), parts(s)), TRUE)), TRUE))
  }, TRUE))
  sets <- unlist(lapply(0:length(tt), function(k) if (k == 0) list(character()) else utils::combn(tt, k, simplify = FALSE)), recursive = FALSE)
  Filter(ok, sets)
}

loro_fast <- function(X, y, g) {
  p <- numeric(length(y))
  for (k in unique(g)) { i <- g == k; b <- stats::lm.fit(X[!i, , drop = FALSE], y[!i])$coefficients; b[is.na(b)] <- 0; p[i] <- X[i, , drop = FALSE] %*% b }
  1 - sum((y - p)^2) / sum((y - mean(y))^2)
}

best_by_test <- function(ry, sx, models = all_hierarchical_models()) {
  d <- dredge_data(ry, sx)
  full <- stats::model.matrix(~ it * vpd * lat * lon, d)
  cols <- colnames(full)
  sc <- vapply(models, function(set) {
    keep <- c("(Intercept)", "it", set)
    loro_fast(full[, cols %in% keep | vapply(cols, function(cn) any(vapply(keep, function(k) setequal(strsplit(k, ":")[[1]], strsplit(cn, ":")[[1]]), TRUE)), TRUE), drop = FALSE], d$sx, d$nuts_id)
  }, 0)
  b <- which.max(sc)
  fo <- stats::reformulate(c("it", models[[b]]), response = "sx")
  tibble::tibble(code = sx, models_compared = length(models), best_equation = paste("Sx ~", paste(c("it", models[[b]]), collapse = " + ")),
                 n_terms = 1 + length(models[[b]]), R2_tested_region = sc[b],
                 R2_tested_country = if (dplyr::n_distinct(d$country) >= 2) tested_r2(d, fo, "country") else NA_real_)
}

# Fair version: for each hidden NUTS region the equation is chosen (all 148,
# by hiding regions) using only the other regions, then used to predict the
# hidden one. Its score is not flattered by the choice.
best_by_test_fair <- function(ry, sx, models = all_hierarchical_models()) {
  d <- dredge_data(ry, sx)
  full <- stats::model.matrix(~ it * vpd * lat * lon, d); cols <- colnames(full)
  same <- function(a, b) setequal(strsplit(a, ":")[[1]], strsplit(b, ":")[[1]])
  colsets <- lapply(models, function(set) { keep <- c("(Intercept)", "it", set); vapply(cols, function(cn) any(vapply(keep, same, TRUE, b = cn)), TRUE) })
  p <- numeric(nrow(d)); chosen <- character()
  for (k in unique(d$nuts_id)) {
    i <- d$nuts_id == k
    sc <- vapply(colsets, function(cs) loro_fast(full[!i, cs, drop = FALSE], d$sx[!i], d$nuts_id[!i]), 0)
    cs <- colsets[[which.max(sc)]]; chosen <- c(chosen, paste(c("it", models[[which.max(sc)]]), collapse = " + "))
    b <- stats::lm.fit(full[!i, cs, drop = FALSE], d$sx[!i])$coefficients; b[is.na(b)] <- 0
    p[i] <- full[i, cs, drop = FALSE] %*% b
  }
  tibble::tibble(code = sx, R2_tested_region_fair = 1 - sum((d$sx - p)^2) / sum((d$sx - mean(d$sx))^2),
                 most_chosen_equation = paste("Sx ~", names(sort(table(chosen), decreasing = TRUE))[1]),
                 times_chosen = max(table(chosen)), regions = length(unique(d$nuts_id)))
}

# ---- With the host tree --------------------------------------------------------------
# host = log(host amount + 1), standardised: growing stock (m3) for borers, area
# (ha) for defoliators, of the forest columns in the insect's host_columns
# (supervisor's matrix, otherwise the data-file host; R/model_tables.R). Each of the 148 equations is tried three ways:
# without host, + host, + host + IT x host (the IT link changes with the
# amount of host). Regions without forest data are left out of both the
# with-host and the without-host search, so the two compare on the same rows.

dredge_data_host <- function(ry, sx, regions, host_columns) {
  d <- dredge_data(ry, sx)
  cols <- strsplit(host_columns, " + ", fixed = TRUE)[[1]]
  amount <- Reduce(`+`, lapply(cols, function(cn) regions[[cn]]))[match(d$nuts_id, regions$nuts_id)]
  d$host_amount <- amount
  d <- d[!is.na(amount), ]
  d$host <- as.numeric(scale(log1p(d$host_amount)))
  d
}

host_models <- function(models = all_hierarchical_models()) {
  c(lapply(models, function(m) m), lapply(models, function(m) c(m, "host")), lapply(models, function(m) c(m, "host", "it:host")))
}

search_fair <- function(d, models) {
  full <- stats::model.matrix(~ it * vpd * lat * lon + host + it:host, d); cols <- colnames(full)
  same <- function(a, b) setequal(strsplit(a, ":")[[1]], strsplit(b, ":")[[1]])
  colsets <- lapply(models, function(set) { keep <- c("(Intercept)", "it", set); vapply(cols, function(cn) any(vapply(keep, same, TRUE, b = cn)), TRUE) })
  # flattered: best of all models on the test itself
  sc_all <- vapply(colsets, function(cs) loro_fast(full[, cs, drop = FALSE], d$sx, d$nuts_id), 0)
  # fair: model chosen without the hidden region
  p <- numeric(nrow(d)); chosen <- character()
  for (k in unique(d$nuts_id)) {
    i <- d$nuts_id == k
    sc <- vapply(colsets, function(cs) loro_fast(full[!i, cs, drop = FALSE], d$sx[!i], d$nuts_id[!i]), 0)
    j <- which.max(sc); chosen <- c(chosen, paste(c("it", models[[j]]), collapse = " + "))
    b <- stats::lm.fit(full[!i, colsets[[j]], drop = FALSE], d$sx[!i])$coefficients; b[is.na(b)] <- 0
    p[i] <- full[i, colsets[[j]], drop = FALSE] %*% b
  }
  tab <- sort(table(chosen), decreasing = TRUE)
  tibble::tibble(R2_best_flattered = max(sc_all), best_equation = paste("Sx ~", paste(c("it", models[[which.max(sc_all)]]), collapse = " + ")),
                 R2_fair = 1 - sum((d$sx - p)^2) / sum((d$sx - mean(d$sx))^2),
                 most_chosen_equation = paste("Sx ~", names(tab)[1]), times_chosen = as.integer(tab[1]),
                 rounds_with_host = sum(grepl("host", chosen)))
}

host_compare <- function(ry, regions, ins, codes) {
  base <- all_hierarchical_models(); withh <- host_models(base)
  dplyr::bind_rows(lapply(codes, function(s) {
    host <- ins$host_columns[ins$code == s]
    d <- dredge_data_host(ry, s, regions, host)
    cat(s, host, nrow(d), "rows", format(Sys.time(), "%H:%M:%S"), "\n")
    dplyr::bind_rows(
      dplyr::mutate(search_fair(d, base), with_host = FALSE),
      dplyr::mutate(search_fair(d, withh), with_host = TRUE)) |>
      dplyr::mutate(code = s, insect = ins$insect[ins$code == s], host = host, regions = dplyr::n_distinct(d$nuts_id),
                    region_years = nrow(d), R2_host_alone_with_it = tested_r2(d, sx ~ it + host + it:host, "nuts_id"),
                    R2_it_alone = tested_r2(d, sx ~ it, "nuts_id"), .before = 1)
  }))
}
