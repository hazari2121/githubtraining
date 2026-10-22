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

# ---- B: scatter per pair (exactly what r measures) -------------------------------
# One dot per region-year where both insects are recorded: x = insect A compared
# with that region's normal, y = insect B. r = the correlation of these dots.
pair_points <- function(tr, usable) {
  z <- dplyr::filter(tr, !is.na(z_within)) |> dplyr::select(country, nuts_id, year, species, z_within)
  purrr::pmap_dfr(dplyr::select(usable, species_a, species_b, r, group, Pair), function(species_a, species_b, r, group, Pair) {
    dplyr::inner_join(dplyr::filter(z, species == species_a) |> dplyr::select(-species, z_a = z_within),
                      dplyr::filter(z, species == species_b) |> dplyr::select(-species, z_b = z_within),
                      by = c("country", "nuts_id", "year")) |>
      dplyr::mutate(species_a, species_b, r, group, Pair, .before = 1)
  })
}

plot_pair_scatter <- function(pp, grp) {
  d <- dplyr::filter(pp, group == grp) |>
    dplyr::group_by(Pair) |>
    dplyr::mutate(panel = paste0(Pair, "\nr = ", formatC(r[1], format = "f", digits = 2), "  (", dplyr::n(), " region-years)")) |>
    dplyr::ungroup()
  d <- d |> dplyr::mutate(panel = factor(panel, unique(panel[order(-abs(r))])))
  ggplot2::ggplot(d, ggplot2::aes(z_a, z_b)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#d9d8d3", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "#d9d8d3", linewidth = 0.3) +
    ggplot2::geom_point(colour = "#184f95", alpha = 0.25, size = 0.9) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#eb6834", linewidth = 0.8) +
    ggplot2::facet_wrap(~ panel, ncol = 3) +
    ggplot2::labs(x = "First insect in the title (0 = normal year for the region)", y = "Second insect (0 = normal year)",
                  title = sub("^[0-9]+\\. ", "Usable pairs, every region-year: ", grp),
                  subtitle = "One dot = one region in one year where both insects are recorded. Line = straight-line fit.\nThe tighter the dots around the line, the stronger the link (r).",
                  caption = "Each region's log(1 + damage) series standardised against its own mean. Reported data, no gap-filling.") +
    theme_eu(8.5) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 7.5))
}

# ---- C: real damage over time for the strongest pairs -----------------------------
# Damage summed over the region-years where BOTH insects are recorded (the same
# data as r), each insect on its own scale and in its own unit.
pair_damage <- function(tr, usable, n = 6) {
  top <- usable |> dplyr::slice_max(abs(r), n = n, with_ties = FALSE)
  d <- dplyr::filter(tr, !is.na(damage)) |> dplyr::select(country, nuts_id, year, species, unit, damage)
  purrr::pmap_dfr(dplyr::mutate(dplyr::select(top, species_a, species_b, r, Pair), k = dplyr::row_number()),
                  function(species_a, species_b, r, Pair, k) {
    a <- dplyr::filter(d, species == species_a); b <- dplyr::filter(d, species == species_b)
    keys <- dplyr::inner_join(dplyr::select(a, country, nuts_id, year), dplyr::select(b, country, nuts_id, year),
                              by = c("country", "nuts_id", "year"))
    dplyr::bind_rows(dplyr::semi_join(a, keys, by = c("country", "nuts_id", "year")) |> dplyr::mutate(role = "A"),
                     dplyr::semi_join(b, keys, by = c("country", "nuts_id", "year")) |> dplyr::mutate(role = "B")) |>
      dplyr::group_by(role, species, unit, year) |>
      dplyr::summarise(damage = sum(damage), n_regions = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(k, Pair, r)
  })
}

plot_pair_damage <- function(pd, ph_hier) {
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  d <- pd |> dplyr::mutate(panel = paste0(k, ". ", Pair, " (r = ", formatC(r, format = "f", digits = 2), ")\n", nm[species], ", ", unit,
                                          dplyr::if_else(unit == "m3", " killed", " defoliated"), " per year"))
  d$panel <- factor(d$panel, unique(d$panel[order(d$k, d$role)]))
  ggplot2::ggplot(d, ggplot2::aes(year, damage, fill = role)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::scale_fill_manual(values = c(A = "#184f95", B = "#eb6834"), guide = "none") +
    ggplot2::scale_y_continuous(labels = function(x) short_number(x)) +
    ggplot2::facet_wrap(~ panel, ncol = 2, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "Reported damage per year",
                  title = "The 6 strongest usable pairs: real damage per year",
                  subtitle = "Each row = one pair: first insect left (blue), second right (orange). Each panel has its own scale.\nOnly the regions and years where both insects are recorded (the same data as r).",
                  caption = "EU-ForDAM value1, reported data, no gap-filling. CZ and DE spruce bark-borer group counted as I. typographus.") +
    theme_eu(8.5) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 7.5))
}
