# The pairwise equation from the original notes, fitted for the usable pairs:
#   log(B + 1) = a + b * log(A + 1)
# on all region-years where both insects are recorded (value1, zeros kept,
# CZ spruce group counted as ips_typ). This is the equation the partner
# method uses for gap-filling (partner_predict(), R/gapfill_compare.R).
# Fitted in both directions, since predicting B from A is not the same line
# as predicting A from B. Ordinary least squares; no randomness.

partner_equations <- function(tr, verdict) {
  w <- tr |>
    dplyr::select(country, nuts_id, year, species, log_damage) |>
    tidyr::pivot_wider(names_from = species, values_from = log_damage)
  u <- dplyr::filter(verdict, verdict == "usable")
  fit_one <- function(x, y) {
    f <- stats::lm(y ~ x); s <- summary(f)
    c(a = unname(stats::coef(f)[1]), b = unname(stats::coef(f)[2]),
      b_se = unname(s$coefficients[2, 2]), r2 = s$r.squared)
  }
  purrr::pmap_dfr(u[, c("species_a", "species_b", "r")], function(species_a, species_b, r) {
    ok <- !is.na(w[[species_a]]) & !is.na(w[[species_b]])
    xa <- w[[species_a]][ok]; xb <- w[[species_b]][ok]
    ab <- fit_one(xa, xb)   # B from A
    ba <- fit_one(xb, xa)   # A from B
    tibble::tibble(species_a, species_b, n_region_years = sum(ok),
                   n_countries = dplyr::n_distinct(w$country[ok]),
                   r_within_region = r,
                   a_B_from_A = ab[["a"]], b_B_from_A = ab[["b"]], b_se_B_from_A = ab[["b_se"]],
                   a_A_from_B = ba[["a"]], b_A_from_B = ba[["b"]], b_se_A_from_B = ba[["b_se"]],
                   r2 = ab[["r2"]])
  }) |>
    dplyr::arrange(dplyr::desc(r_within_region))
}
