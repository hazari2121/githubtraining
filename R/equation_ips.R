# The equation (supervisor and student):  Sx ~ IT x VPD x latitude x longitude
#
# Fed from the two workbooks:
#   which targets: OVERLAP_target_and_predictor_species.xlsx, sheet pairs,
#                  predictor = I. typographus, overlap in at least 3 countries
#   the data:      data/processed/model_region_year.csv (MODEL_input_tables.xlsx,
#                  sheet region_year), the region-years where both are recorded
# Sx and IT are log(damage + 1); VPD (ERA5-Land 2000-2022, 2025 paper),
# latitude and longitude are standardised (mean 0, sd 1), so each main effect
# is the effect at average conditions. One ordinary least-squares fit per
# target, the full equation as written (16 coefficients), no selection.
#
# How well it predicts (each region-year predicted by the equation fitted
# without it):
#   R2_tested_region   hide one NUTS region at a time
#   R2_tested_country  hide one country at a time
# 1 = perfect, 0 = no better than the mean, below 0 = worse than the mean.

equation_formula <- sx ~ it * vpd * lat * lon

equation_targets <- function(overlap_xlsx, min_countries = 3) {
  p <- readxl::read_excel(overlap_xlsx, "pairs")
  dplyr::filter(p, predictor == "ips_typ", countries >= min_countries) |>
    dplyr::select(target, target_name, countries, regions, region_years, country_list)
}

equation_data <- function(ry, sx) {
  d <- ry[!is.na(ry[[sx]]) & !is.na(ry$ips_typ) & !is.na(ry$vpd_kPa), ]
  tibble::tibble(country = d$country, nuts_id = d$nuts_id, year = d$year,
                 sx = log1p(d[[sx]]), it = log1p(d$ips_typ),
                 vpd = as.numeric(scale(d$vpd_kPa)), lat = as.numeric(scale(d$lat)), lon = as.numeric(scale(d$lon)))
}

equation_tested <- function(d, by) {
  if (dplyr::n_distinct(d[[by]]) < 2) return(NA_real_)
  p <- rep(NA_real_, nrow(d))
  for (k in unique(d[[by]])) { i <- d[[by]] == k; p[i] <- suppressWarnings(stats::predict(stats::lm(equation_formula, d[!i, ]), d[i, ])) }
  1 - sum((d$sx - p)^2) / sum((d$sx - mean(d$sx))^2)
}

equation_one <- function(ry, sx) {
  d <- equation_data(ry, sx)
  m <- stats::lm(equation_formula, d)
  s <- summary(m)
  f <- s$fstatistic
  co <- as.data.frame(stats::coef(s)); co$term <- rownames(co)
  list(
    summary = tibble::tibble(target = sx, region_years = nrow(d), regions = dplyr::n_distinct(d$nuts_id), countries = dplyr::n_distinct(d$country),
                             share_zero_years = mean(d$sx == 0), coefficients_estimated = sum(!is.na(stats::coef(m))),
                             R2 = s$r.squared, R2_adjusted = s$adj.r.squared,
                             p_equation = if (is.null(f)) NA_real_ else stats::pf(f[1], f[2], f[3], lower.tail = FALSE),
                             R2_tested_region = equation_tested(d, "nuts_id"), R2_tested_country = equation_tested(d, "country")),
    coefficients = tibble::tibble(target = sx, term = co$term, estimate = co$Estimate, std_error = co$`Std. Error`,
                                  t = co$`t value`, p = co$`Pr(>|t|)`))
}

equation_all <- function(ry, targets) {
  res <- lapply(targets$target, function(s) equation_one(ry, s))
  sm <- dplyr::bind_rows(lapply(res, `[[`, "summary")) |>
    dplyr::left_join(dplyr::select(targets, target, target_name, country_list), by = "target") |>
    dplyr::relocate(target_name, .after = target) |>
    dplyr::arrange(dplyr::desc(R2_tested_region))
  co <- dplyr::bind_rows(lapply(res, `[[`, "coefficients")) |>
    dplyr::left_join(dplyr::select(targets, target, target_name), by = "target") |>
    dplyr::relocate(target_name, .after = target)
  list(summary = sm, coefficients = co)
}

write_equation_xlsx <- function(res, path) {
  how <- data.frame(`How to read` = c(
    "Equation: target damage ~ I. typographus x VPD x latitude x longitude (all terms, 16 coefficients), fitted once per target, nothing selected.",
    "Targets: insects that overlap with I. typographus in at least 3 countries (OVERLAP workbook, sheet pairs). Data: region-years where both are recorded (MODEL_input_tables, sheet region_year).",
    "Damage as log(damage + 1); VPD (ERA5-Land 2000-2022), latitude and longitude standardised: main effects are effects at average conditions.",
    "summary: R2 = share of the target's variation the equation explains in the data it was fitted on; p_equation = F-test of the whole equation.",
    "  R2_tested_region: each NUTS region predicted by the equation fitted without it; R2_tested_country: each country predicted without it.",
    "  1 = perfect, 0 = no better than the mean, below 0 = worse than the mean.",
    "coefficients: estimate, standard error, t and p of every term for every target. it = I. typographus; it:vpd = how the I. typographus effect changes with VPD, etc."), check.names = FALSE)
  writexl::write_xlsx(list(`how to read` = how, summary = res$summary, coefficients = res$coefficients), path)
  path
}

# ---- With the host tree -------------------------------------------------------------
# Sx ~ IT x VPD x latitude x longitude + host
# host = log(host amount + 1), standardised: growing stock (m3) for borers, area
# (ha) for defoliators, of the target's host_columns (supervisor's host matrix,
# otherwise the host in the data file; MODEL_insects_with_ips_typographus.csv)
# from the regions table. Regions without forest data are left out; the
# equation without host is refitted on the same rows so the two compare directly.

equation_formula_host <- sx ~ it * vpd * lat * lon + host

equation_data_host <- function(ry, regions, sx, host_columns) {
  d <- equation_data(ry, sx)
  cols <- strsplit(host_columns, " + ", fixed = TRUE)[[1]]
  amount <- Reduce(`+`, lapply(cols, function(cn) regions[[cn]]))[match(d$nuts_id, regions$nuts_id)]
  d$host_amount <- amount
  d <- d[!is.na(amount), ]
  d$host <- as.numeric(scale(log1p(d$host_amount)))
  d
}

equation_tested_fo <- function(d, fo, by) {
  if (dplyr::n_distinct(d[[by]]) < 2) return(NA_real_)
  p <- rep(NA_real_, nrow(d))
  for (k in unique(d[[by]])) { i <- d[[by]] == k; p[i] <- suppressWarnings(stats::predict(stats::lm(fo, d[!i, ]), d[i, ])) }
  1 - sum((d$sx - p)^2) / sum((d$sx - mean(d$sx))^2)
}

equation_host_all <- function(ry, regions, targets, ins) {
  res <- lapply(targets$target, function(s) {
    hc <- ins$host_columns[ins$code == s]
    d <- equation_data_host(ry, regions, s, hc)
    m0 <- stats::lm(equation_formula, d); m1 <- stats::lm(equation_formula_host, d)
    co <- as.data.frame(stats::coef(summary(m1))); co$term <- rownames(co)
    list(summary = tibble::tibble(
           target = s, host = hc, region_years = nrow(d), regions = dplyr::n_distinct(d$nuts_id), countries = dplyr::n_distinct(d$country),
           R2_without_host = summary(m0)$r.squared, R2_with_host = summary(m1)$r.squared,
           b_host = co$Estimate[co$term == "host"], p_host = co$`Pr(>|t|)`[co$term == "host"],
           b_it = co$Estimate[co$term == "it"], p_it = co$`Pr(>|t|)`[co$term == "it"],
           tested_region_without_host = equation_tested_fo(d, equation_formula, "nuts_id"),
           tested_region_with_host = equation_tested_fo(d, equation_formula_host, "nuts_id"),
           tested_country_without_host = equation_tested_fo(d, equation_formula, "country"),
           tested_country_with_host = equation_tested_fo(d, equation_formula_host, "country")),
         coefficients = tibble::tibble(target = s, term = co$term, estimate = co$Estimate, std_error = co$`Std. Error`, t = co$`t value`, p = co$`Pr(>|t|)`))
  })
  sm <- dplyr::bind_rows(lapply(res, `[[`, "summary")) |>
    dplyr::left_join(dplyr::select(targets, target, target_name), by = "target") |> dplyr::relocate(target_name, .after = target) |>
    dplyr::arrange(dplyr::desc(tested_region_with_host))
  co <- dplyr::bind_rows(lapply(res, `[[`, "coefficients")) |>
    dplyr::left_join(dplyr::select(targets, target, target_name), by = "target") |> dplyr::relocate(target_name, .after = target)
  list(summary = sm, coefficients = co)
}

write_equation_host_xlsx <- function(res, path) {
  how <- data.frame(`How to read` = c(
    "Equation: target damage ~ I. typographus x VPD x latitude x longitude + host (the equation of the supervisor and student with the host tree added as one more term).",
    "host = log(host amount + 1), standardised: m3 of growing stock for borers, ha of area for defoliators; host columns from the supervisor's host matrix, otherwise the data-file host (column host).",
    "Regions without forest data (CZ01, DE3, DE5, DE6) are left out; the equation without host is fitted on the same rows, so the two compare directly.",
    "summary: R2 without / with host (data the equation was fitted on); b_host, p_host = effect of the host amount; b_it, p_it = effect of I. typographus.",
    "  tested_region_*: each NUTS region predicted by the equation fitted without it; tested_country_*: each country predicted without it. 1 = perfect, 0 = no better than the mean.",
    "  p-values treat every region-year as independent; the same region in many years makes them too small.",
    "coefficients: every term of the equation with host, for every target."), check.names = FALSE)
  writexl::write_xlsx(list(`how to read` = how, summary = res$summary, coefficients = res$coefficients), path)
  path
}

# ---- Better ways to predict: mixed model and lasso ----------------------------------
# Same targets, same rows (region-years with both insects and forest data) as
# the equation with host. Compared by hiding one NUTS region (and one country)
# at a time:
#   full equation  Sx ~ IT x VPD x latitude x longitude + host   (least squares)
#   mixed model    Sx ~ IT + host + VPD + latitude + longitude + (1 | year) + (1 | region)
#                  lme4; a hidden region gets the year effects learned from the
#                  other regions and the average region (its own region effect is unknown)
#   lasso          the terms of the full equation with host, unhelpful terms shrunk
#                  to zero (glmnet); how strongly is chosen by hiding regions
#                  inside the training data only, so the hidden region is never seen
#   European mean  baseline: mean of the other regions in the same year

pred_mixed <- function(tr, te) {
  tr$year_f <- factor(tr$year); te$year_f <- factor(te$year, levels = levels(tr$year_f))
  m <- suppressMessages(lme4::lmer(sx ~ it + host + vpd + lat + lon + (1 | year_f) + (1 | nuts_id), data = tr))
  stats::predict(m, newdata = te, re.form = ~ (1 | year_f), allow.new.levels = TRUE)
}

# the same mixed model without I. typographus: how much of its skill is I. typographus
pred_mixed_no_it <- function(tr, te) {
  tr$year_f <- factor(tr$year); te$year_f <- factor(te$year, levels = levels(tr$year_f))
  m <- suppressMessages(lme4::lmer(sx ~ host + vpd + lat + lon + (1 | year_f) + (1 | nuts_id), data = tr))
  stats::predict(m, newdata = te, re.form = ~ (1 | year_f), allow.new.levels = TRUE)
}

pred_lasso <- function(tr, te) {
  X <- function(d) stats::model.matrix(equation_formula_host, d)[, -1, drop = FALSE]
  fold <- as.integer(factor(tr$nuts_id))
  if (max(fold) > 10) fold <- (fold %% 10) + 1
  # an inner training set with no variation (e.g. all zero years) leaves nothing to fit: predict its mean
  cv <- tryCatch(glmnet::cv.glmnet(X(tr), tr$sx, alpha = 1, foldid = fold), error = function(e) NULL)
  if (is.null(cv)) return(rep(mean(tr$sx), nrow(te)))
  as.numeric(stats::predict(cv, X(te), s = "lambda.min"))
}

pred_full <- function(tr, te) suppressWarnings(stats::predict(stats::lm(equation_formula_host, tr), te))

pred_eu <- function(tr, te) {
  m <- tapply(tr$sx, tr$year, mean)
  out <- unname(m[as.character(te$year)]); out[is.na(out)] <- mean(tr$sx); out
}

tested_by <- function(d, fun, by) {
  if (dplyr::n_distinct(d[[by]]) < 2) return(NA_real_)
  p <- rep(NA_real_, nrow(d))
  for (k in unique(d[[by]])) { i <- d[[by]] == k; p[i] <- fun(d[!i, ], d[i, ]) }
  1 - sum((d$sx - p)^2) / sum((d$sx - mean(d$sx))^2)
}

lasso_terms <- function(d) {
  X <- stats::model.matrix(equation_formula_host, d)[, -1, drop = FALSE]
  fold <- as.integer(factor(d$nuts_id)); if (max(fold) > 10) fold <- (fold %% 10) + 1
  cv <- tryCatch(glmnet::cv.glmnet(X, d$sx, alpha = 1, foldid = fold), error = function(e) NULL)
  if (is.null(cv)) return(NA_character_)
  b <- as.matrix(stats::coef(cv, s = "lambda.min"))[-1, 1]
  paste(names(b)[b != 0], collapse = " + ")
}

better_ways <- function(ry, regions, targets, ins) {
  set.seed(1)
  dplyr::bind_rows(lapply(targets$target, function(s) {
    d <- equation_data_host(ry, regions, s, ins$host_columns[ins$code == s])
    cat(s, format(Sys.time(), "%H:%M:%S"), "\n")
    mx <- suppressMessages(lme4::lmer(sx ~ it + host + vpd + lat + lon + (1 | year) + (1 | nuts_id), data = dplyr::mutate(d, year = factor(year))))
    cm <- summary(mx)$coefficients
    tibble::tibble(target = s, target_name = targets$target_name[targets$target == s], regions = dplyr::n_distinct(d$nuts_id),
                   countries = dplyr::n_distinct(d$country), region_years = nrow(d),
                   region_full_equation = tested_by(d, pred_full, "nuts_id"), region_mixed = tested_by(d, pred_mixed, "nuts_id"),
                   region_mixed_without_it = tested_by(d, pred_mixed_no_it, "nuts_id"),
                   region_lasso = tested_by(d, pred_lasso, "nuts_id"), region_european_mean = tested_by(d, pred_eu, "nuts_id"),
                   country_full_equation = tested_by(d, pred_full, "country"), country_mixed = tested_by(d, pred_mixed, "country"),
                   country_lasso = tested_by(d, pred_lasso, "country"), country_european_mean = tested_by(d, pred_eu, "country"),
                   mixed_b_it = cm["it", "Estimate"], mixed_t_it = cm["it", "t value"],
                   mixed_b_host = cm["host", "Estimate"], mixed_t_host = cm["host", "t value"],
                   lasso_terms_kept = lasso_terms(d))
  })) |> dplyr::arrange(dplyr::desc(pmax(region_mixed, region_lasso, region_full_equation)))
}

plot_better_ways <- function(bw) {
  d <- bw |> dplyr::mutate(lab = paste0(target_name, "  (", countries, " countries, ", regions, " regions)"),
                           lab = forcats::fct_reorder(lab, pmax(region_mixed, region_lasso, region_full_equation))) |>
    tidyr::pivot_longer(c(region_full_equation, region_mixed, region_lasso, region_european_mean), names_to = "way", values_to = "r2") |>
    dplyr::mutate(way = factor(way, c("region_mixed", "region_lasso", "region_full_equation", "region_european_mean"),
                               c("mixed model (IT + host + VPD + lat + lon, year and region effects)", "lasso on the full equation with host",
                                 "full equation with host", "European mean of the other regions that year")))
  ggplot2::ggplot(d, ggplot2::aes(pmax(r2, -1), lab, colour = way, shape = way)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_point(size = 2.6) +
    ggplot2::scale_colour_manual(values = c("#eb6834", "#184f95", "#9c9b97", "#1f1e1c"), name = NULL) +
    ggplot2::scale_shape_manual(values = c(16, 17, 4, 124), name = NULL) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2), shape = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::labs(x = "Tested R-squared: each NUTS region predicted from the other regions only (0 = no better than the overall mean; values below -1 drawn at -1)", y = NULL,
                  title = "Predicting each insect from I. typographus: which way predicts hidden NUTS regions best?",
                  subtitle = "Targets overlapping with I. typographus in at least 3 countries. log(damage + 1); host = log host amount (supervisor's host matrix). VPD: ERA5-Land 2000-2022.",
                  caption = "EU-ForDAM value1, region-years where both insects are recorded and the region has forest data.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top")
}

# ---- Simpler equations ---------------------------------------------------------------
# Few terms, an interaction only where biology suggests it (I. typographus
# mattering more in dry regions -> IT x VPD). Same targets and rows as above.
simple_formulas <- list(
  `IT alone`                 = sx ~ it,
  `IT + host + VPD`          = sx ~ it + host + vpd,
  `IT + host + VPD + IT x VPD` = sx ~ it + host + vpd + it:vpd,
  `host + VPD (no IT)`       = sx ~ host + vpd)

simple_equations <- function(ry, regions, targets, ins) {
  dplyr::bind_rows(lapply(targets$target, function(s) {
    d <- equation_data_host(ry, regions, s, ins$host_columns[ins$code == s])
    m <- stats::lm(sx ~ it + host + vpd + it:vpd, d); co <- stats::coef(summary(m))
    one <- function(fo, by) equation_tested_fo(d, fo, by)
    tibble::tibble(target = s, target_name = targets$target_name[targets$target == s], regions = dplyr::n_distinct(d$nuts_id),
                   countries = dplyr::n_distinct(d$country), region_years = nrow(d),
                   region_full_equation = one(equation_formula_host, "nuts_id")) |>
      dplyr::bind_cols(tibble::as_tibble(stats::setNames(lapply(simple_formulas, one, by = "nuts_id"), paste0("region_", names(simple_formulas))))) |>
      dplyr::bind_cols(tibble::as_tibble(stats::setNames(lapply(simple_formulas, one, by = "country"), paste0("country_", names(simple_formulas))))) |>
      dplyr::mutate(b_it = co["it", "Estimate"], p_it = co["it", "Pr(>|t|)"], b_host = co["host", "Estimate"], p_host = co["host", "Pr(>|t|)"],
                    b_vpd = co["vpd", "Estimate"], p_vpd = co["vpd", "Pr(>|t|)"], b_it_x_vpd = co["it:vpd", "Estimate"], p_it_x_vpd = co["it:vpd", "Pr(>|t|)"])
  }))
}
