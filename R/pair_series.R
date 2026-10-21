# Time series of the usable pairs (correlation metrics, R/cm_basic.R)
#
# For each pair, only the region-years where BOTH insects have a recorded value
# are used (the same data as the correlation). Each region's series is first
# standardised against its own mean (z_within: 0 = a normal year for that
# region, +1 = one standard deviation worse than normal), then averaged over the
# regions for each year. Two lines moving up and down together = the link the
# correlation r measures.

pair_series <- function(tr, usable) {
  z <- dplyr::filter(tr, !is.na(z_within)) |> dplyr::select(country, nuts_id, year, species, z_within)
  purrr::pmap_dfr(dplyr::select(usable, species_a, species_b, r, group, Pair), function(species_a, species_b, r, group, Pair) {
    dplyr::inner_join(dplyr::filter(z, species == species_a) |> dplyr::select(-species, z_a = z_within),
                      dplyr::filter(z, species == species_b) |> dplyr::select(-species, z_b = z_within),
                      by = c("country", "nuts_id", "year")) |>
      dplyr::group_by(year) |>
      dplyr::summarise(z_a = mean(z_a), z_b = mean(z_b), n_regions = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(species_a, species_b, r, group, Pair, .before = 1)
  }) |>
    dplyr::group_by(species_a, species_b) |>
    dplyr::mutate(n_regions_max = max(n_regions)) |>
    dplyr::ungroup()
}

plot_pair_series <- function(ps, grp) {
  d <- dplyr::filter(ps, group == grp) |>
    dplyr::mutate(panel = paste0(Pair, "\nr = ", formatC(r, format = "f", digits = 2), "  (", n_regions_max, " regions)"))
  d <- d |> dplyr::mutate(panel = factor(panel, unique(panel[order(-abs(r))])))
  long <- tidyr::pivot_longer(d, c(z_a, z_b), names_to = "which", values_to = "z") |>
    dplyr::mutate(which = dplyr::if_else(which == "z_a", "first insect in the title", "second insect in the title"))
  ggplot2::ggplot(long, ggplot2::aes(year, z, colour = which)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::scale_colour_manual(values = c(`first insect in the title` = "#184f95", `second insect in the title` = "#eb6834"), name = NULL) +
    ggplot2::facet_wrap(~ panel, ncol = 3) +
    ggplot2::labs(x = NULL, y = "Damage compared with the region's normal (0 = normal year)",
                  title = sub("^[0-9]+\\. ", "Usable pairs: ", grp),
                  subtitle = "Both insects year by year, in the regions where both are recorded.\nLines moving together = the correlation.",
                  caption = paste("Each region's log(1 + damage) series standardised against its own mean (+1 = one standard deviation above normal),",
                                  "\nthen averaged over the regions each year.",
                                  "\nr = same-year correlation from the correlation metrics (within region). Reported data, no gap-filling.")) +
    theme_eu(8.5) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold", size = 7.5))
}
