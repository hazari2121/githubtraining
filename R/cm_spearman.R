# Spearman (rank) version of the correlation metrics (Oct 2026).
#
# Same pipeline as the Pearson metrics, but each region-insect series is first
# turned into ranks (1 = that region's lowest year, ties averaged) and then
# standardised. A Pearson r on these within-region ranks is a within-region
# Spearman correlation: it asks whether two insects' years are ORDERED the
# same way, ignoring how extreme the outbreak years are. This makes the
# metric robust to a few huge outbreak years dominating a series.
# The Europe-wide-year removal (anomaly) is rebuilt on the rank scale exactly
# as in make_transforms(). Ranks are deterministic; the bootstrap inside
# correlation_metrics() sets its own seed.

rank_transforms <- function(tr) {
  tr |>
    dplyr::group_by(country, nuts_id, species) |>
    dplyr::mutate(
      rk = rank(log_damage, ties.method = "average"),
      z_within = if (dplyr::n() >= 3 && stats::sd(rk) > 0) (rk - mean(rk)) / stats::sd(rk) else NA_real_
    ) |>
    dplyr::group_by(species, year) |>
    dplyr::mutate(
      k = sum(!is.na(z_within)),
      other_mean = dplyr::if_else(k > 1, (sum(z_within, na.rm = TRUE) - z_within) / (k - 1), NA_real_),
      anomaly = z_within - other_mean
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-rk, -k, -other_mean)
}

# Pearson vs Spearman verdicts side by side, one row per pair
compare_rank_verdicts <- function(vp, vs) {
  dplyr::inner_join(
    dplyr::select(vp, species_a, species_b, verdict_pearson = verdict, r_pearson = r),
    dplyr::select(vs, species_a, species_b, verdict_spearman = verdict, r_spearman = r),
    by = c("species_a", "species_b"))
}
