# Figures for the analyses on the 2025 paper's hierarchy (R/paper_hier.R)

paper_level1_cols <- c(`Borers on gymnosperms` = "#184f95", `Borers on angiosperms` = "#5b8fd6",
                       `Defoliators on gymnosperms` = "#eb6834", `Defoliators on angiosperms` = "#f2a33a")

short_number <- function(x) scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)(x)

# Group level: one bar per paper group (levels 1 and 2), aggregates included
plot_paper_group_ranking <- function(gr, n_units = 77) {
  d <- gr |>
    dplyr::mutate(level_lab = dplyr::if_else(level == "level1", "Level 1: feeding guild x host type",
                                             "Level 2: gymnosperm host"),
                  l1 = dplyr::if_else(level == "level1", group,
                                      dplyr::if_else(grepl("^Borers", group), "Borers on gymnosperms", "Defoliators on gymnosperms")),
                  lab = paste0(n_units_with_damage, "   (", short_number(cumulative_damage), " ", unit,
                               dplyr::if_else(from_aggregate_codes > 0,
                                              paste0("; ", formatC(100 * share_from_aggregates, format = "f", digits = 1), "% from aggregate codes"), ""),
                               ")")) |>
    dplyr::arrange(n_units_with_damage) |>
    dplyr::mutate(group = factor(group, unique(group)))
  ggplot2::ggplot(d, ggplot2::aes(n_units_with_damage, group)) +
    ggplot2::geom_col(ggplot2::aes(fill = l1), width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = lab), hjust = 0, nudge_x = 1, size = 2.7, colour = pal$ink2) +
    ggplot2::scale_fill_manual(values = paper_level1_cols, guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, n_units + 45), breaks = seq(0, 70, 10), expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::facet_grid(level_lab ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "NUTS units with damage (at least one year > 0, 2000-2022)", y = NULL,
                  title = "A. Groups of the 2025 EU-ForDAM paper (aggregate codes included)",
                  subtitle = "In brackets: cumulative reported damage 2000-2022 (borers m3, defoliators ha) and the part reported under aggregate codes") +
    theme_eu(8.5) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# Species level: single insects only, coloured by the paper's level-1 group
plot_paper_species_ranking <- function(sr, n_units = 77) {
  d <- sr |>
    dplyr::mutate(lab = paste0(species, "  ", name, dplyr::if_else(in_paper_table1, "", "  [not in Table 1]"))) |>
    dplyr::arrange(n_units_with_damage, dplyr::desc(species)) |>
    dplyr::mutate(lab = factor(lab, lab), level1 = factor(level1, paper_level1_order))
  ggplot2::ggplot(d, ggplot2::aes(n_units_with_damage, lab)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = n_units_with_damage, yend = lab, colour = level1),
                          linewidth = 0.5, alpha = 0.35) +
    ggplot2::geom_point(ggplot2::aes(colour = level1), size = 2.4) +
    ggplot2::geom_text(ggplot2::aes(label = n_units_with_damage), hjust = 0, nudge_x = 1.2, size = 2.5, colour = pal$ink2) +
    ggplot2::scale_colour_manual(values = paper_level1_cols, name = "Level 1 group", drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, n_units), breaks = seq(0, 70, 10), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = "NUTS units with damage (at least one year > 0, 2000-2022)", y = NULL,
                  title = "B. Single insects (level 3; aggregate reporting codes left out)",
                  caption = paste("77 NUTS units in total; 11 countries report one national unit; PL district codes are NUTS2.",
                                  "\nAs in the 2025 EU-ForDAM paper, the spruce bark-borer group (CambXyl_con&BB_con&SM, CZ and DE) is counted as ips_typ.",
                                  "\nAggregate codes (borers on pine / larch, defoliators on angiosperms / oaks) lump several insects and are counted only in A.")) +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

plot_paper_ranking <- function(gr, sr) {
  gridExtra::arrangeGrob(plot_paper_group_ranking(gr), plot_paper_species_ranking(sr), ncol = 1, heights = c(0.36, 1))
}

# Group-level correlations: lower-triangle heatmap of the same-year r
# (within region) with the Europe-total r of the paper's approach underneath
plot_paper_group_heatmap <- function(gc, level) {
  d <- dplyr::filter(gc, level == !!level)
  ord <- if (level == "level1") paper_level1_order else paper_level2_order
  both <- dplyr::bind_rows(d, dplyr::rename(d, species_a = species_b, species_b = species_a)) |>
    dplyr::mutate(ia = match(species_a, ord), ib = match(species_b, ord)) |>
    dplyr::filter(ia > ib) |>
    dplyr::mutate(a = factor(species_a, rev(ord)), b = factor(species_b, ord),
                  lab = paste0(formatC(same_year, format = "f", digits = 2),
                               "\n(", formatC(same_year_lo, format = "f", digits = 2), " to ",
                               formatC(same_year_hi, format = "f", digits = 2), ")",
                               "\nEurope: ", formatC(r_europe_total, format = "f", digits = 2)))
  ggplot2::ggplot(both, ggplot2::aes(b, a)) +
    ggplot2::geom_tile(ggplot2::aes(fill = same_year, alpha = same_year_sure), colour = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = lab), size = 2.6, lineheight = 0.9) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", limits = c(-0.8, 0.8),
                                  oob = scales::squish, name = "r (same year,\nwithin region)") +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
    ggplot2::scale_x_discrete(labels = function(x) sub(" on ", "\non ", x)) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = if (level == "level1") "Level 1 groups: do they have bad years together?" else "Level 2 groups (gymnosperm hosts)",
                  subtitle = "Top: r within regions, with 95% range (faded = range includes 0). Bottom: r of the Europe-wide yearly totals (the paper's approach)",
                  caption = "Group value = sum of all recorded member codes in a region-year, aggregate codes included; borers m3, defoliators ha (never mixed within a group).") +
    theme_eu(8.5) + ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# Group totals: reported vs reported + filled, one panel per group
plot_paper_group_totals <- function(gt) {
  d <- gt |>
    dplyr::mutate(group = factor(group, c(paper_level1_order, paper_level2_order)),
                  lab = paste0(group, " (", unit, ")")) |>
    dplyr::arrange(group) |> dplyr::mutate(lab = factor(lab, unique(lab)))
  ggplot2::ggplot(d, ggplot2::aes(year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = total_lo80, ymax = total_hi80), fill = "#86b6ef", alpha = 0.45) +
    ggplot2::geom_line(ggplot2::aes(y = total_mid, colour = "reported + filled"), linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = reported, colour = "reported only"), linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = c(`reported only` = pal$ink, `reported + filled` = "#1c5cab"), name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    ggplot2::facet_wrap(~ lab, scales = "free_y", ncol = 3) +
    ggplot2::labs(x = NULL, y = "Europe-wide damage per year",
                  title = "Europe-wide damage by group of the 2025 EU-ForDAM paper: reported vs reported + gap-filled",
                  subtitle = "Reported = every code, aggregates included. Filled = single insects only (aggregates are never filled). Band = 80% range from the hide-and-predict test",
                  caption = "Filled insects tested in >= 2 countries only; T. pityocampa (tha_pit) filled only in BG, CH, HR, IT, SI; other insects in reporting countries and their land neighbours.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold"))
}
