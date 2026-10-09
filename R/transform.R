# Step 3: transformed versions of the observed data (never imputed values).
#
# Per region-species series:
#   log_damage = log(damage + 1)
#   z_within   = log_damage standardised within the series
#   detrended  = residuals of lm(log_damage ~ year) within the series
#   diff       = log_damage[t] - log_damage[t-1], consecutive years only
#   anomaly    = z_within minus the mean z_within of the same species in the same
#                year across all OTHER regions. Removes the Europe-wide year signal
#                (e.g. the 2003 and 2018-2020 droughts) so what remains is regional
#                co-variation beyond shared bad years.
# z_within / detrended are NA for series with < 3 observations or no variation
# (e.g. all-zero series), so such series drop out of correlations.
#
# Zeros are real observations (0 = monitored, no damage; confirmed Oct 2026), so
# the main analysis keeps every observed value (min_nonzero = 0). The series
# filter of Hlasny et al. (2025, Sect. 2.5), which drops region-species series
# with fewer than 6 non-zero values, is kept as a sensitivity check (min_nonzero = 6).

transform_versions <- tibble::tribble(
  ~version,     ~comparable_across_countries, ~note,
  "log_damage", FALSE, "Pooled reference only. Units are fixed per species, but levels depend on region size and on NUTS level (national vs NUTS1-3), so levels are not comparable across countries.",
  "z_within",   TRUE,  "Standardised within each region-species series; removes region size and scale.",
  "detrended",  TRUE,  "Residuals of a linear time trend per series, in log units; removes shared long-term trends.",
  "diff",       TRUE,  "Year-to-year change in log_damage; removes level and most trend.",
  "anomaly",    TRUE,  "z_within minus the same species' mean z_within in that year in all other regions; removes Europe-wide year effects."
)

detrend_series <- function(y, t) {
  if (length(y) < 3 || stats::sd(y) == 0) return(rep(NA_real_, length(y)))
  as.numeric(stats::resid(stats::lm(y ~ t)))
}

make_transforms <- function(cells, min_nonzero = 0) {
  obs <- cells |>
    dplyr::filter(status %in% c("observed_positive", "observed_zero")) |>
    dplyr::group_by(country, nuts_id, species) |>
    dplyr::filter(sum(damage > 0) >= min_nonzero) |>
    dplyr::ungroup()

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
    dplyr::group_by(species, year) |>
    dplyr::mutate(
      k = sum(!is.na(z_within)),
      other_mean = dplyr::if_else(k > 1, (sum(z_within, na.rm = TRUE) - z_within) / (k - 1), NA_real_),
      anomaly = z_within - other_mean
    ) |>
    dplyr::ungroup() |>
    dplyr::select(country, nuts_id, year, species, unit, damage, status,
                  log_damage, z_within, detrended, diff, anomaly)
}
