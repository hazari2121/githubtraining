# Step 3: transformed versions of the observed data (never imputed values).
#
# Per region-species series:
#   log_damage = log(damage + 1)
#   z_within   = log_damage standardised within the series
#   detrended  = residuals of lm(log_damage ~ year) within the series
#   diff       = log_damage[t] - log_damage[t-1], consecutive years only
# z_within / detrended are NA for series with < 3 observations or no variation
# (e.g. all-zero series), so such series drop out of correlations.

transform_versions <- tibble::tribble(
  ~version,     ~comparable_across_countries, ~note,
  "log_damage", FALSE, "Pooled reference only. Units are fixed per species, but levels depend on region size and on NUTS level (national vs NUTS1-3), so levels are not comparable across countries.",
  "z_within",   TRUE,  "Standardised within each region-species series; removes region size and scale.",
  "detrended",  TRUE,  "Residuals of a linear time trend per series, in log units; removes shared long-term trends.",
  "diff",       TRUE,  "Year-to-year change in log_damage; removes level and most trend."
)

detrend_series <- function(y, t) {
  if (length(y) < 3 || stats::sd(y) == 0) return(rep(NA_real_, length(y)))
  as.numeric(stats::resid(stats::lm(y ~ t)))
}

make_transforms <- function(cells, drop_suspect_zeros = FALSE) {
  obs <- cells |> dplyr::filter(status %in% c("observed_positive", "observed_zero"))
  if (drop_suspect_zeros) obs <- obs |> dplyr::filter(!suspect_zero)

  obs |>
    dplyr::arrange(country, nuts_id, species, year) |>
    dplyr::group_by(country, nuts_id, species) |>
    dplyr::mutate(
      log_damage = log1p(damage),
      n_obs = dplyr::n(),
      z_within = if (dplyr::first(n_obs) >= 3 && stats::sd(log_damage) > 0)
        (log_damage - mean(log_damage)) / stats::sd(log_damage) else NA_real_,
      detrended = detrend_series(log_damage, year),
      diff = dplyr::if_else(year - dplyr::lag(year) == 1,
                            log_damage - dplyr::lag(log_damage), NA_real_)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(country, nuts_id, year, species, unit, damage, status, suspect_zero,
                  log_damage, z_within, detrended, diff)
}
