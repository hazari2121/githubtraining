# Pairs of broader categories: the usable-pairs graphs (time series and
# scatterplots) for the groups of the 2025 EU-ForDAM paper instead of single
# insects. Groups exactly as in the paper's Table 1 (data/lookup/paper_hierarchy_2025.csv):
#   level 2: borers on A. alba (fir), P. abies (spruce), P. sylvestris (pine);
#            defoliators on P. abies, P. sylvestris
#   level 1: borers / defoliators on gymnosperms / angiosperms
#   total:   all borers, all defoliators (every code)
# Trypodendron lineatum is left out: it is not in the paper's Table 1.
# Every pair within a level is shown; nothing is selected or filtered.
#
# Group value in a region-year = sum of all recorded member codes, aggregates
# included. Same metric as the species pairs: each region's log(1 + damage)
# series standardised against its own mean, r pooled over the region-years
# where both groups are recorded.

paper_tree_names <- c(`A. alba` = "fir", `P. abies` = "spruce", `P. sylvestris` = "pine")

# Codes in our hierarchy file that the paper's Table 1 does not list (its
# Table 2 totals also match only without them; R/paper_compare.R)
not_in_paper_groups <- c("xyl_lin")

paper_group_map <- function(ph_hier) {
  h <- dplyr::mutate(ph_hier, guild = sub(" on .*", "", level1)) |> dplyr::filter(!species %in% not_in_paper_groups)
  l2 <- dplyr::filter(h, !is.na(level2_paper)) |>
    dplyr::transmute(level = "level2", code = species, guild,
                     group = paste(guild, "on", paper_tree_names[sub("^.* on ", "", level2_paper)]))
  dplyr::bind_rows(
    l2,
    dplyr::transmute(h, level = "level1", code = species, guild, group = level1),
    dplyr::transmute(h, level = "total", code = species, guild, group = paste("All", tolower(guild))))
}

# Line and axis labels: the group name itself ("Borers on fir", "All borers")
short_group <- function(g) g

level_transforms <- function(cells, gm, level) make_transforms(level_cells(cells, gm, level), min_nonzero = 0)

# r for every pair of groups within one level (all pairs, no filter)
level_pair_metrics <- function(tr, level) {
  z <- dplyr::filter(tr, !is.na(z_within)) |> dplyr::select(country, nuts_id, year, species, z_within)
  pairs <- utils::combn(sort(unique(z$species)), 2, simplify = FALSE)
  purrr::map_dfr(pairs, function(p) {
    j <- dplyr::inner_join(dplyr::filter(z, species == p[1]), dplyr::filter(z, species == p[2]),
                           by = c("country", "nuts_id", "year"), suffix = c("_a", "_b"))
    tibble::tibble(level, group_a = p[1], group_b = p[2],
                   r = if (nrow(j) >= 3) stats::cor(j$z_within_a, j$z_within_b) else NA_real_,
                   n_region_years = nrow(j), n_regions = dplyr::n_distinct(j$nuts_id),
                   n_countries = dplyr::n_distinct(j$country), countries = paste(sort(unique(j$country)), collapse = ", "))
  }) |>
    dplyr::arrange(dplyr::desc(abs(r)))
}

# Region-year points of each pair (scatter); yearly means over regions give the time series
level_pair_points <- function(tr, pm) {
  z <- dplyr::filter(tr, !is.na(z_within)) |> dplyr::select(country, nuts_id, year, species, z_within)
  purrr::pmap_dfr(dplyr::select(pm, level, group_a, group_b, r, n_regions, n_countries),
                  function(level, group_a, group_b, r, n_regions, n_countries) {
    dplyr::inner_join(dplyr::filter(z, species == group_a) |> dplyr::select(-species, z_a = z_within),
                      dplyr::filter(z, species == group_b) |> dplyr::select(-species, z_b = z_within),
                      by = c("country", "nuts_id", "year")) |>
      dplyr::mutate(level, group_a, group_b, r, n_regions, n_countries, .before = 1)
  })
}

level_panel <- function(d) {
  d |> dplyr::mutate(panel = paste0(group_a, " vs\n", group_b, "\nr = ", formatC(r, format = "f", digits = 2)),
                     panel = factor(panel, unique(panel[order(-r)])))
}

level_caption <- paste(
  "Annual damage (bark and wood borers: m\u00b3 of trees killed; defoliators: ha defoliated) from the EU-ForDAM database, 2000-2022, reported values only.",
  "\nHost groups as defined in the 2025 EU-ForDAM study; a group's value is the sum of all agents recorded in that group in a region and year",
  "\n(Trypodendron lineatum excluded, as in that study). Each regional series was log(x + 1)-transformed and standardised to z-scores within the region.",
  "\nr = Pearson correlation of the regional z-scores, pooled over all region-years in which both groups were recorded.")

plot_level_series <- function(pts, title, subtitle) {
  d <- pts |> dplyr::group_by(level, group_a, group_b, r, n_regions, n_countries, year) |>
    dplyr::summarise(z_a = mean(z_a), z_b = mean(z_b), .groups = "drop") |> level_panel()
  long <- tidyr::pivot_longer(d, c(z_a, z_b), names_to = "which", values_to = "z") |>
    dplyr::mutate(name = short_group(dplyr::if_else(which == "z_a", group_a, group_b)))
  ends <- long |> dplyr::group_by(panel, which) |> dplyr::slice_max(year, n = 1) |> dplyr::ungroup()
  ggplot2::ggplot(long, ggplot2::aes(year, z, colour = which)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 0.9) +
    ggrepel::geom_text_repel(data = ends, ggplot2::aes(label = name), hjust = 0, nudge_x = 0.6, direction = "y",
                             segment.colour = NA, min.segment.length = Inf, size = 2.5, show.legend = FALSE, seed = 1) +
    ggplot2::scale_colour_manual(values = c(z_a = "#184f95", z_b = "#eb6834"), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(2000, 2029), breaks = seq(2000, 2020, 5)) +
    ggplot2::facet_wrap(~ panel, ncol = 3) +
    ggplot2::labs(x = NULL, y = "Damage anomaly (z-score)", title = title, subtitle = subtitle,
                  caption = paste0(level_caption, "\nLines show the mean z-score across those regions in each year.")) +
    theme_eu(8.5) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 7.2))
}

plot_level_scatter <- function(pts, title, subtitle) {
  d <- level_panel(pts)
  lim <- stats::quantile(abs(c(d$z_a, d$z_b)), 0.995)
  # each axis labelled inside its panel with the group it shows
  ax <- dplyr::distinct(d, panel, group_a, group_b)
  ggplot2::ggplot(d, ggplot2::aes(z_a, z_b)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#d9d8d3", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "#d9d8d3", linewidth = 0.3) +
    ggplot2::geom_point(colour = "#184f95", alpha = 0.22, size = 1, stroke = 0) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#eb6834", fill = "#eb6834", alpha = 0.18, linewidth = 0.9) +
    ggplot2::geom_text(data = ax, ggplot2::aes(x = lim, y = -lim, label = paste(short_group(group_a), "\u2192")),
                       hjust = 1, vjust = 0, size = 2.5, colour = pal$ink2, inherit.aes = FALSE) +
    ggplot2::geom_text(data = ax, ggplot2::aes(x = -lim, y = lim, label = paste(short_group(group_b), "\u2192")),
                       angle = 90, hjust = 1, vjust = 1, size = 2.5, colour = pal$ink2, inherit.aes = FALSE) +
    ggplot2::coord_cartesian(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::facet_wrap(~ panel, ncol = 3) +
    ggplot2::labs(x = "Damage anomaly (z-score)", y = "Damage anomaly (z-score)", title = title, subtitle = subtitle,
                  caption = paste0(level_caption, "\nPoints are region-years; lines are ordinary least-squares fits with 95% confidence bands.",
                                   " A few extreme points outside the plotted range are included in the fits.")) +
    theme_eu(8.5) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 7), aspect.ratio = 1)
}
