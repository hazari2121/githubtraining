# Pairs of broader categories: the usable-pairs graphs (time series and
# scatterplots) for groups instead of single insects, at three levels of
# aggregation (R/levels.R): host tree (e.g. borers on pine x borers on fir),
# host type (borers on gymnosperms x defoliators on angiosperms) and guild
# (all borers x all defoliators).
#
# Group value in a region-year = sum of all recorded member codes, aggregates
# included. Same metric as the species pairs: each region against its own
# normal (log, standardised within the region), pooled r over the region-years
# where both groups are recorded; 95% range from the region bootstrap; the
# regional part removes the Europe-wide year effect (R/transform.R).

level_transforms <- function(cells, lm, level) make_transforms(level_cells(cells, lm, level), min_nonzero = 0)

level_pair_metrics <- function(tr, lm, level, min_n = 50, min_regions = 5) {
  z <- dplyr::filter(tr, !is.na(z_within)) |> dplyr::select(country, nuts_id, year, species, z_within, anomaly)
  info <- dplyr::distinct(dplyr::filter(lm, level == !!level), group, guild, host_tree, host_is_proxy)
  grp <- sort(unique(z$species))
  pairs <- utils::combn(grp, 2, simplify = FALSE)
  purrr::map_dfr(pairs, function(p) {
    j <- dplyr::inner_join(dplyr::filter(z, species == p[1]), dplyr::filter(z, species == p[2]),
                           by = c("country", "nuts_id", "year"), suffix = c("_a", "_b"))
    if (nrow(j) < min_n || dplyr::n_distinct(j$nuts_id) < min_regions) return(NULL)
    b <- boot_r(j$z_within_a, j$z_within_b, j$nuts_id)
    ok <- !is.na(j$anomaly_a) & !is.na(j$anomaly_b)
    br <- boot_r(j$anomaly_a[ok], j$anomaly_b[ok], j$nuts_id[ok])
    tibble::tibble(level, group_a = p[1], group_b = p[2], r = b[["r"]], lo = b[["lo"]], hi = b[["hi"]],
                   r_regional_part = br[["r"]], n = nrow(j), n_regions = dplyr::n_distinct(j$nuts_id),
                   n_countries = dplyr::n_distinct(j$country), countries = paste(sort(unique(j$country)), collapse = ", "))
  }) |>
    dplyr::left_join(dplyr::rename_with(info, ~ paste0(.x, "_a")), by = "group_a") |>
    dplyr::left_join(dplyr::rename_with(info, ~ paste0(.x, "_b")), by = "group_b") |>
    dplyr::mutate(pair_type = dplyr::case_when(guild_a == "Borers" & guild_b == "Borers" ~ "both borers",
                                               guild_a == "Defoliators" & guild_b == "Defoliators" ~ "both defoliators",
                                               TRUE ~ "a borer group with a defoliator group"),
                  sure = lo > 0 | hi < 0) |>
    dplyr::arrange(dplyr::desc(abs(r)))
}

# Region-year points (scatter) and yearly means over regions (time series) for a set of pairs
level_pair_points <- function(tr, pm) {
  z <- dplyr::filter(tr, !is.na(z_within)) |> dplyr::select(country, nuts_id, year, species, z_within)
  purrr::pmap_dfr(dplyr::select(pm, level, group_a, group_b, r, r_regional_part, n_regions, n_countries, pair_type),
                  function(level, group_a, group_b, r, r_regional_part, n_regions, n_countries, pair_type) {
    dplyr::inner_join(dplyr::filter(z, species == group_a) |> dplyr::select(-species, z_a = z_within),
                      dplyr::filter(z, species == group_b) |> dplyr::select(-species, z_b = z_within),
                      by = c("country", "nuts_id", "year")) |>
      dplyr::mutate(level, group_a, group_b, r, r_regional_part, n_regions, n_countries, pair_type, .before = 1)
  })
}

level_panel <- function(d) {
  d |> dplyr::mutate(panel = paste0(group_a, "\nx ", group_b, "\nr = ", formatC(r, format = "f", digits = 2),
                                    ", regional part ", formatC(r_regional_part, format = "f", digits = 2),
                                    "  (", n_regions, " regions, ", n_countries, dplyr::if_else(n_countries == 1, " country)", " countries)")),
                     panel = factor(panel, unique(panel[order(-abs(r))])))
}

plot_level_series <- function(pts, title, subtitle_extra = "") {
  d <- pts |> dplyr::group_by(level, group_a, group_b, r, r_regional_part, n_regions, n_countries, year) |>
    dplyr::summarise(z_a = mean(z_a), z_b = mean(z_b), .groups = "drop") |> level_panel()
  long <- tidyr::pivot_longer(d, c(z_a, z_b), names_to = "which", values_to = "z")
  ggplot2::ggplot(long, ggplot2::aes(year, z, colour = which)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 0.9) +
    ggplot2::scale_colour_manual(values = c(z_a = "#184f95", z_b = "#eb6834"),
                                 labels = c(z_a = "first group in the title", z_b = "second group"), name = NULL) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2020, 5)) +
    ggplot2::facet_wrap(~ panel, ncol = 3) +
    ggplot2::labs(x = NULL, y = "Damage compared with the region's normal (0 = normal year)", title = title,
                  subtitle = paste0("Two groups year by year, in the regions where both are recorded. Lines moving together = the correlation.", subtitle_extra),
                  caption = paste("Group value = sum of all recorded member codes in a region-year (aggregates included). Each region's log(1 + damage) series",
                                  "\nstandardised against its own mean, then averaged over the regions each year. r = same-year correlation within regions;",
                                  "\nregional part = r after removing the Europe-wide year effect. Reported data, no gap-filling.")) +
    theme_eu(8.5) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold", size = 7.2))
}

plot_level_scatter <- function(pts, title, subtitle_extra = "") {
  d <- level_panel(pts)
  lim <- stats::quantile(abs(c(d$z_a, d$z_b)), 0.995)
  ggplot2::ggplot(d, ggplot2::aes(z_a, z_b)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#d9d8d3", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "#d9d8d3", linewidth = 0.3) +
    ggplot2::geom_point(colour = "#184f95", alpha = 0.22, size = 1, stroke = 0) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#eb6834", fill = "#eb6834", alpha = 0.18, linewidth = 0.9) +
    ggplot2::coord_cartesian(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::facet_wrap(~ panel, ncol = 3) +
    ggplot2::labs(x = "x: first group (0 = normal year for the region)", y = "y: second group (0 = normal year)", title = title,
                  subtitle = paste0("One dot = one region in one year where both groups are recorded. Orange line = straight-line fit, band = its 95% range.", subtitle_extra),
                  caption = paste("Group value = sum of all recorded member codes in a region-year (aggregates included), each region against its own normal.",
                                  "\nr = same-year correlation within regions; regional part = r after removing the Europe-wide year effect.",
                                  "\nReported data, no gap-filling. A few extreme dots beyond the plotted range are not shown but are in the fit.")) +
    theme_eu(8.5) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 7), aspect.ratio = 1)
}
