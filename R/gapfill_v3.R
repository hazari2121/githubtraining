# Gap-filling, aligned with the correlation metrics (Oct 2026).
#
# The earlier gap-filling (v1) used the insect codes as reported; the
# correlation metrics count the CZ spruce bark-borer group as I. typographus
# (as in Hlasny et al. 2025). The v3 targets rebuild the factor-model inputs
# from the same data as the correlation metrics (cb_cells / cb_tr), so both
# parts of the thesis rest on one dataset. Model settings (K, ridge penalties,
# group hint) are the ones chosen earlier; they are not re-tuned here.
#
# model_vs_correlation(): does the fitted factor model reproduce the
# correlation metrics? For every pair, the same within-region Pearson r is
# computed on the model's fitted values (expected log damage = P(any) x
# log amount) over the same region-years where both insects are recorded,
# and compared with the r measured on the data.

model_vs_correlation <- function(raw, verdict) {
  d <- raw |>
    dplyr::filter(!is.na(y)) |>
    dplyr::mutate(fit = p_any * amount_log) |>
    dplyr::select(country, nuts_id, year, species, fit)
  w <- tidyr::pivot_wider(d, names_from = species, values_from = fit)
  reg <- paste(w$country, w$nuts_id)
  zfun <- function(v) {   # standardise within each region series
    stats::ave(v, reg, FUN = function(x) {
      ok <- !is.na(x)
      if (sum(ok) < 3 || stats::sd(x[ok]) == 0) return(rep(NA_real_, length(x)))
      (x - mean(x[ok])) / stats::sd(x[ok])
    })
  }
  sp <- intersect(unique(c(verdict$species_a, verdict$species_b)), names(w))
  Z <- vapply(sp, function(s) zfun(w[[s]]), numeric(nrow(w)))
  r_model <- purrr::map2_dbl(verdict$species_a, verdict$species_b, function(a, b) {
    if (!a %in% sp || !b %in% sp) return(NA_real_)
    x <- Z[, a]; y <- Z[, b]; ok <- !is.na(x) & !is.na(y)
    if (sum(ok) < 20) NA_real_ else stats::cor(x[ok], y[ok])
  })
  verdict |>
    dplyr::select(species_a, species_b, verdict, r_data = r) |>
    dplyr::mutate(r_model = r_model)
}
