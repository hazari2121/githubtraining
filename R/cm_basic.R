# Correlation metrics, back to basics (Oct 2026).
#
# Purpose: test the core premise of the thesis, "damage from different agents
# tends to fluctuate together, so the existing records carry information about
# the missing ones", insect code vs insect code (all 49 codes, 1,176 pairs).
#
# Data: value1 (value2 = the same damage in another unit, not used); 0 =
# monitored, no damage; empty / not reported = unknown (never treated as 0).
# Codes as reported, except the CZ spruce bark-borer group, counted as ips_typ
# as in Hlasny et al. (2025) (decision Oct 2026).
# Only region-years where both codes are recorded. Each region-code series is
# standardised against its own mean (z_within) so regions of any size compare.
# Pearson r with 95% ranges from resampling whole regions (999 times).
#
# Five questions per pair:
#   Q1 same_year   : do they have bad years together?
#   Q2 beyond_year : still linked after removing Europe-wide bad years?
#   Q3 lag_ab/ba   : does one's bad year come before the other's (t -> t+1),
#                    beyond the follower's own persistence?
#   Q4 transfer    : does the link hold in each country separately?
#   Q5 structure   : do linked codes share a broad group / host tree?
# Q1-Q3: correlation_metrics() (R/metrics.R); Q4: per_country_correlations(),
# loco_correlations(), transferability_summary() (R/correlate.R);
# Q5: hierarchy_pair_test() (R/hierarchy.R).


# One clean table, one row per pair
cm_basic_table <- function(cm, hier) {
  h <- dplyr::select(hier, species, level1, level2)
  cm |>
    dplyr::left_join(dplyr::rename_with(h, ~ paste0(.x, "_a")), by = c("species_a" = "species_a")) |>
    dplyr::left_join(dplyr::rename_with(h, ~ paste0(.x, "_b")), by = c("species_b" = "species_b")) |>
    dplyr::mutate(
      shared_group = dplyr::case_when(
        level2_a == level2_b ~ "same host-tree group",
        level1_a == level1_b ~ "same broad group",
        TRUE ~ "different broad groups"),
      lead = dplyr::case_when(
        lag_ab_sure & !lag_ba_sure ~ paste(species_a, "leads", species_b),
        lag_ba_sure & !lag_ab_sure ~ paste(species_b, "leads", species_a),
        lag_ab_sure & lag_ba_sure ~ "both directions",
        TRUE ~ "none clear")
    ) |>
    dplyr::transmute(
      species_a, species_b, group_a = level1_a, group_b = level1_b, shared_group,
      n_region_years = n, n_regions, n_countries,
      `Q1_same_year_r` = same_year, Q1_lo = same_year_lo, Q1_hi = same_year_hi,
      `Q2_beyond_shared_years_r` = beyond_year, Q2_lo = beyond_year_lo, Q2_hi = beyond_year_hi,
      Q3_lead = lead, Q3_r_a_then_b = lag_ab, Q3_r_b_then_a = lag_ba,
      Q4_holds_across_countries = transferability, Q4_countries = countries_n10,
      link)
}

# Order of codes for the matrix: by broad group, host-tree group, then name
cm_basic_order <- function(hier) {
  hier |>
    dplyr::arrange(factor(level1, c("borers on conifers", "borers on broadleaves",
                                    "defoliators on conifers", "defoliators on broadleaves")),
                   level2, species) |>
    dplyr::pull(species)
}
