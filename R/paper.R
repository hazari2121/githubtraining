# Comparison with Hlasny et al. (2025) Divergent Trends in Insect Disturbance Across
# Europe's Temperate and Boreal Forests. Global Change Biology 31: e70580.
#
# Table 6 of the paper: "Pearson correlations between the time series of total
# forest disturbance recorded within the investigated geographical domain".
# Values transcribed from the paper (lower triangle).

paper_table6 <- local({
  sp <- c("ips_typ", "pha_cya", "tom_spp", "ips_acu", "pit_cha",
          "tor_vir", "geo_db", "lym_dis", "lym_mon")
  low <- c(.73,
           .01, .15,
           .68, .36, .01,
           .34, .23, .20, .28,
           .07, .00, .44, .17, .01,
           .02, .00, .38, .07, .15, .19,
           .03, .00, .34, .06, .06, .31, .61,
           .01, .01, .20, .00, .39, .02, .56, .20)
  out <- list(); k <- 1
  for (i in 2:9) for (j in 1:(i - 1)) {
    out[[k]] <- tibble::tibble(species_a = sp[j], species_b = sp[i], paper_value = low[k]); k <- k + 1
  }
  dplyr::bind_rows(out)
})

# Europe-wide total per species and year (sum over all regions, untransformed),
# then Pearson r between species. Our data reproduce the paper's Table 6 when its
# values are read as r^2 (squared correlations), which also explains why all
# values in the paper's table are non-negative.
europe_total_correlations <- function(dmg) {
  sp <- unique(c(paper_table6$species_a, paper_table6$species_b))
  tot <- dmg |>
    dplyr::filter(species %in% sp) |>
    dplyr::group_by(species, year) |>
    dplyr::summarise(total = sum(damage, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = species, values_from = total)

  paper_table6 |>
    dplyr::rowwise() |>
    dplyr::mutate(
      ct = list(stats::cor.test(tot[[species_a]], tot[[species_b]])),
      r_ours = unname(ct$estimate),
      r2_ours = r_ours^2,
      p_ours = ct$p.value,
      # same series, linearly detrended (removes the shared long-term trend)
      r_ours_detrended = stats::cor(stats::resid(stats::lm(tot[[species_a]] ~ tot$year)),
                                    stats::resid(stats::lm(tot[[species_b]] ~ tot$year)))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-ct) |>
    dplyr::mutate(
      abs_diff_paper_vs_r2 = abs(paper_value - round(r2_ours, 2)),
      abs_diff_paper_vs_r  = abs(paper_value - round(r_ours, 2)),
      sign_hidden_in_paper = r_ours < 0 & p_ours < 0.001
    )
}
