# Correlations that bring in more countries
#
# (1) Group level: the paper's groups (level 1: guild x host type; level 2:
#     gymnosperm host). Group value in a region-year = sum of all recorded
#     member codes (aggregates included). Countries that report only a few
#     insects still contribute, because their insects feed into the groups.
#     Same metric as the species level (each region against its own normal,
#     pooled r), plus the r in each country separately.
# (2) Europe-wide totals (the 2025 paper's approach): for each insect or group,
#     the yearly sum over ALL reporting regions of all countries, log(1 + x),
#     then Pearson r between the two yearly series (23 years).

group_transforms <- function(cells, ph_hier, level) {
  h <- dplyr::filter(ph_hier, !is.na(.data[[level]]))
  make_transforms(aggregate_cells(dplyr::filter(cells, species %in% h$species), h, level), min_nonzero = 0)
}

# r per country for every group pair (z_within, >= 10 region-years per country)
group_country_r <- function(gtr, level) {
  per_country_correlations(gtr, sort(unique(gtr$species)), versions = "z_within") |>
    dplyr::transmute(level = sub("_paper", "", level), group_a = species_a, group_b = species_b,
                     country, n, r = r_pearson)
}

# Yearly z_within averaged over regions, for group pairs (as for species pairs)
group_pair_series <- function(gtr, gcm, level) {
  z <- dplyr::filter(gtr, !is.na(z_within)) |> dplyr::select(country, nuts_id, year, species, z_within)
  d <- dplyr::filter(gcm, level == sub("_paper", "", !!level))
  purrr::pmap_dfr(dplyr::select(d, species_a, species_b, same_year), function(species_a, species_b, same_year) {
    dplyr::inner_join(dplyr::filter(z, species == species_a) |> dplyr::select(-species, z_a = z_within),
                      dplyr::filter(z, species == species_b) |> dplyr::select(-species, z_b = z_within),
                      by = c("country", "nuts_id", "year")) |>
      dplyr::group_by(year) |>
      dplyr::summarise(z_a = mean(z_a), z_b = mean(z_b), n_regions = dplyr::n(),
                       n_countries = dplyr::n_distinct(country), .groups = "drop") |>
      dplyr::mutate(level = sub("_paper", "", level), group_a = species_a, group_b = species_b, r = same_year, .before = 1)
  })
}

# Europe-wide yearly totals of every insect or group, log(1 + x)
europe_totals <- function(cells, ph_hier = NULL, level = NULL) {
  d <- if (is.null(level)) cells else {
    h <- dplyr::filter(ph_hier, !is.na(.data[[level]]))
    aggregate_cells(dplyr::filter(cells, species %in% h$species), h, level)
  }
  d |> dplyr::filter(status %in% c("observed_positive", "observed_zero")) |>
    dplyr::group_by(species, year) |>
    dplyr::summarise(total = sum(damage), n_regions = dplyr::n(), n_countries = dplyr::n_distinct(country), .groups = "drop") |>
    dplyr::mutate(log_total = log1p(total))
}

# Pearson r of the Europe-wide yearly series for a set of pairs
europe_r <- function(et, pairs) {
  w <- tidyr::pivot_wider(dplyr::select(et, species, year, log_total), names_from = species, values_from = log_total)
  pairs |> dplyr::mutate(r_europe = purrr::map2_dbl(a, b, function(x, y) {
    ok <- !is.na(w[[x]]) & !is.na(w[[y]])
    if (sum(ok) >= 10) stats::cor(w[[x]][ok], w[[y]][ok]) else NA_real_
  }))
}

# ---- Figures -------------------------------------------------------------------------

# Table as heatmap: one row per group pair, one column per country (+ all)
plot_group_country_r <- function(gcr, gcm, level) {
  lv <- sub("_paper", "", level)
  allr <- dplyr::filter(gcm, level == lv) |>
    dplyr::transmute(group_a = species_a, group_b = species_b, country = "All countries\n(pooled)", r = same_year, n)
  d <- dplyr::bind_rows(dplyr::filter(gcr, level == lv) |> dplyr::select(-level), allr) |>
    dplyr::mutate(pair = paste(group_a, "x", group_b))
  ord <- allr |> dplyr::arrange(r) |> dplyr::mutate(pair = paste(group_a, "x", group_b)) |> dplyr::pull(pair)
  cty <- c(sort(setdiff(unique(d$country), "All countries\n(pooled)")), "All countries\n(pooled)")
  d <- dplyr::mutate(d, pair = factor(pair, ord), country = factor(country, cty))
  ggplot2::ggplot(d, ggplot2::aes(country, pair)) +
    ggplot2::geom_tile(ggplot2::aes(fill = r), colour = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(formatC(r, format = "f", digits = 2), "\n(", n, ")")), size = 2.4, lineheight = 0.85) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", limits = c(-0.8, 0.8), oob = scales::squish,
                                  name = "r (same year,\nwithin region)") +
    ggplot2::geom_vline(xintercept = length(cty) - 0.5, colour = pal$ink, linewidth = 0.5) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = paste0(if (lv == "level1") "Level 1 groups" else "Level 2 groups (gymnosperm hosts)", ": correlation in each country"),
                  subtitle = "Number = r, in brackets the region-years. Blank = fewer than 10 region-years with both groups in that country.",
                  caption = paste("Group value = sum of all recorded member codes in a region-year, aggregates included; each region against its own normal.",
                                  "\nCountries reporting only one group (e.g. FI, SE, CH: only I. typographus) cannot form a pair.")) +
    theme_eu(8.5) + ggplot2::theme(panel.grid = ggplot2::element_blank())
}

plot_group_pair_series <- function(gps, level) {
  lv <- sub("_paper", "", level)
  d <- dplyr::filter(gps, level == lv) |>
    dplyr::group_by(group_a, group_b) |>
    dplyr::mutate(panel = paste0(group_a, "\nx ", group_b, "\nr = ", formatC(r[1], format = "f", digits = 2),
                                 "  (", max(n_regions), " regions, ", max(n_countries), " countries)")) |>
    dplyr::ungroup()
  d$panel <- factor(d$panel, unique(d$panel[order(-d$r)]))
  long <- tidyr::pivot_longer(d, c(z_a, z_b), names_to = "which", values_to = "z")
  ggplot2::ggplot(long, ggplot2::aes(year, z, colour = which)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 0.9) +
    ggplot2::scale_colour_manual(values = c(z_a = "#184f95", z_b = "#eb6834"), labels = c(z_a = "first group in the title", z_b = "second group"), name = NULL) +
    ggplot2::facet_wrap(~ panel, ncol = 3) +
    ggplot2::labs(x = NULL, y = "Damage compared with the region's normal (0 = normal year)",
                  title = paste0(if (lv == "level1") "Level 1 groups" else "Level 2 groups (gymnosperm hosts)", ": year by year, all countries"),
                  subtitle = "Region-years where both groups are recorded, each region against its own normal, averaged over regions.",
                  caption = "Group value = sum of all recorded member codes (aggregates included). r = within-region correlation. Reported data, no gap-filling.") +
    theme_eu(8.5) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold", size = 7.5))
}

# Europe-wide totals of two series (standardised over the 23 years so they share an axis)
plot_europe_pairs <- function(et, pairs, title, subtitle) {
  z <- et |> dplyr::group_by(species) |> dplyr::mutate(z = (log_total - mean(log_total)) / stats::sd(log_total)) |> dplyr::ungroup()
  d <- purrr::pmap_dfr(pairs, function(a, b, label, r_europe, ...) {
    dplyr::bind_rows(dplyr::filter(z, species == a) |> dplyr::mutate(which = "a"),
                     dplyr::filter(z, species == b) |> dplyr::mutate(which = "b")) |>
      dplyr::mutate(panel = paste0(label, "\nr (Europe totals) = ", formatC(r_europe, format = "f", digits = 2)))
  })
  d$panel <- factor(d$panel, unique(d$panel))
  ggplot2::ggplot(d, ggplot2::aes(year, z, colour = which)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 0.9) +
    ggplot2::scale_colour_manual(values = c(a = "#184f95", b = "#eb6834"), labels = c(a = "first in the title", b = "second"), name = NULL) +
    ggplot2::facet_wrap(~ panel, ncol = 3) +
    ggplot2::labs(x = NULL, y = "Europe-wide total compared with its 23-year mean (log scale, standardised)",
                  title = title, subtitle = subtitle,
                  caption = paste("Europe-wide total = sum over every reporting region of every country that year, log(1 + x), as in the 2025 EU-ForDAM paper.",
                                  "\nThe set of reporting countries changes over time (most start in 2003; FI 2012), which also moves these totals.")) +
    theme_eu(8.5) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold", size = 7.5))
}

# Usable species pairs: within-region r against the Europe-totals r
plot_within_vs_europe <- function(cmp) {
  ggplot2::ggplot(cmp, ggplot2::aes(r_within, r_europe)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#c9c8c3", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, colour = "#e2e1dd") + ggplot2::geom_vline(xintercept = 0, colour = "#e2e1dd") +
    ggplot2::geom_point(colour = "#184f95", size = 2.2) +
    ggrepel::geom_text_repel(ggplot2::aes(label = label), size = 2.3, colour = pal$ink2, max.overlaps = 30, seed = 1) +
    ggplot2::scale_x_continuous(limits = c(-0.5, 0.8)) + ggplot2::scale_y_continuous(limits = c(-1, 1)) +
    ggplot2::labs(x = "r within regions (correlation metrics; the regions where both are recorded)",
                  y = "r of Europe-wide yearly totals (all countries)",
                  title = "Usable pairs: correlation within regions vs correlation of Europe-wide totals",
                  subtitle = "Dashed line = the same value both ways. Above the line = Europe totals make the link look stronger.",
                  caption = "Europe totals: sum over every reporting region and country each year, log(1 + x), Pearson r over 23 years (the paper's approach).") +
    theme_eu(9)
}
