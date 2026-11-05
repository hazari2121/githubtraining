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
