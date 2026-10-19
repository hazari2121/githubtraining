# Figures for the analyses on the 2025 paper's hierarchy (R/paper_hier.R)

paper_level1_cols <- c(`Borers on gymnosperms` = "#184f95", `Borers on angiosperms` = "#5b8fd6",
                       `Defoliators on gymnosperms` = "#eb6834", `Defoliators on angiosperms` = "#f2a33a")

short_number <- function(x) scales::label_number(scale_cut = scales::cut_short_scale(), accuracy = 0.1)(x)

# Group level: one bar per paper group (levels 1 and 2), aggregates included
plot_paper_group_ranking <- function(gr, n_units = 77,
                                     title = "A. Groups of the 2025 EU-ForDAM paper (aggregate codes included)",
                                     caption = NULL) {
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
                  title = title, caption = caption,
                  subtitle = "In brackets: cumulative reported damage 2000-2022 (borers m3, defoliators ha) and the part reported under aggregate codes") +
    theme_eu(8.5) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# Species level: single insects only, coloured by the paper's level-1 group
plot_paper_species_ranking <- function(sr, n_units = 77,
                                       title = "B. Single insects (level 3; aggregate reporting codes left out)") {
  d <- sr |>
    dplyr::mutate(lab = paste0(species, "  ", name, dplyr::if_else(in_paper_table1, "", "  [not in Table 1]"))) |>
    dplyr::arrange(n_units_with_damage, dplyr::desc(species)) |>
    dplyr::mutate(lab = factor(lab, lab), level1 = factor(level1, paper_level1_order))
  ggplot2::ggplot(d, ggplot2::aes(n_units_with_damage, lab)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = n_units_with_damage, yend = lab, colour = level1),
                          linewidth = 0.5, alpha = 0.35) +
    ggplot2::geom_point(ggplot2::aes(colour = level1), size = 2.4) +
    ggplot2::geom_text(ggplot2::aes(label = n_units_with_damage), hjust = 0, nudge_x = 1.2, size = 2.5, colour = pal$ink2) +
    ggplot2::scale_colour_manual(values = paper_level1_cols, name = "Level 1 group", drop = FALSE,
                                 guide = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::scale_x_continuous(limits = c(0, n_units), breaks = seq(0, 70, 10), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = "NUTS units with damage (at least one year > 0, 2000-2022)", y = NULL,
                  title = title,
                  caption = paste("77 NUTS units in total; 11 countries report one national unit; PL district codes are NUTS2.",
                                  "\nAs in the 2025 EU-ForDAM paper, the spruce bark-borer group (CambXyl_con&BB_con&SM, CZ and DE) is counted as ips_typ.",
                                  "\nAggregate codes (borers on pine / larch, defoliators on angiosperms / oaks) lump several insects and are counted only in A.")) +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# The three levels as three separate figures
rank_caption <- paste("77 NUTS units in total; 11 countries report one national unit; PL district codes are NUTS2.",
                      "\nAggregate codes (several insects reported together, e.g. borers on pine) are counted in the groups they belong to.",
                      "\nAs in the 2025 EU-ForDAM paper, the spruce bark-borer group (CZ and DE) is counted as I. typographus.")

plot_rank_level1 <- function(gr) {
  plot_paper_group_ranking(dplyr::filter(gr, level == "level1"),
                           title = "Level 1: borers and defoliators on gymnosperms and angiosperms",
                           caption = rank_caption) +
    ggplot2::facet_null()
}

plot_rank_level2 <- function(gr) {
  plot_paper_group_ranking(dplyr::filter(gr, level == "level2"),
                           title = "Level 2: borers and defoliators by gymnosperm host tree",
                           caption = paste(rank_caption, "\nLevel 2 covers spruce, pine and fir only, as in the paper (larch and angiosperm insects have no level-2 group).")) +
    ggplot2::facet_null()
}

# Individual species only: genus-level codes (spp. / sp.), Siricinae and the
# species group Geometridae on oaks are left out, as are aggregate codes
is_single_species <- function(sr) {
  sr$code_type == "species" & !grepl("spp\\.|sp\\.|Siricinae", sr$name)
}

plot_rank_level3 <- function(sr) {
  left_out <- sr$species[!is_single_species(sr)]
  plot_paper_species_ranking(sr[is_single_species(sr), ], title = "Level 3: individual species") +
    ggplot2::labs(caption = paste0(
      "77 NUTS units in total; 11 countries report one national unit; PL district codes are NUTS2.",
      "\nOnly codes that name one species. Left out (they count in the level 1-2 groups): aggregate reporting codes,",
      "\nand genus-level codes or species groups: ", paste(left_out, collapse = ", "), ".",
      "\nAs in the 2025 EU-ForDAM paper, the spruce bark-borer group (CZ and DE) is counted as I. typographus."))
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
                  title = if (level == "level1") "Level 1: do the broad groups have bad years together?" else "Level 2: do the host-tree groups have bad years together?",
                  subtitle = "Top: r within regions, with 95% range (faded = range includes 0).\nBottom: r of the Europe-wide yearly totals (the paper's approach)",
                  caption = "Group value = sum of all recorded member codes in a region-year, aggregate codes included.\nBorers m3, defoliators ha (never mixed within a group).") +
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
    ggplot2::geom_line(ggplot2::aes(y = reported + filled, colour = "reported + filled"), linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = reported, colour = "reported only"), linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = c(`reported only` = pal$ink, `reported + filled` = "#1c5cab"), name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    ggplot2::facet_wrap(~ lab, scales = "free_y", ncol = 3) +
    ggplot2::labs(x = NULL, y = "Europe-wide damage per year",
                  title = "Europe-wide damage by group of the 2025 EU-ForDAM paper: reported vs reported + gap-filled",
                  subtitle = "Reported = every code, aggregates included. Filled = single insects only (aggregates are never filled). Band = 80% range from the hide-and-predict test.\nLine = reported + the model's filled values (middles of the range are not used here: they do not add up across insects)",
                  caption = "Filled insects tested in >= 2 countries only; T. pityocampa (tha_pit) filled only in BG, CH, HR, IT, SI; other insects in reporting countries and their land neighbours.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold"))
}

# ---- Share of the host affected (R/host_share.R) ---------------------------------

share_cols <- c(`reported, all host (paper's way)` = "#9c9b97",
                `reported + filled, all host` = "#1c5cab",
                `reported, only where recorded` = "#eb6834")

share_long <- function(st) {
  st |>
    dplyr::transmute(level, group, unit, host_group, paper_share_pct,
                     `reported, all host (paper's way)` = share_pct_paper,
                     `reported + filled, all host` = share_pct_paper_filled,
                     `reported, only where recorded` = share_pct_where_recorded) |>
    tidyr::pivot_longer(c(`reported, all host (paper's way)`, `reported + filled, all host`, `reported, only where recorded`),
                        names_to = "version", values_to = "share") |>
    dplyr::mutate(version = factor(version, names(share_cols)))
}

share_caption <- paste(
  "Borers: % of host growing stock (m3) killed per year. Defoliators: % of host forest area (ha) defoliated per year (defoliated, not killed).",
  "\nHost: level 1 = all conifers / all broadleaves; level 2 and species = spruce, pine, fir or oak (larch and other broadleaf hosts: all conifers / broadleaves).",
  "\nAll host = host of all 77 regions, as in the 2025 EU-ForDAM paper (diamond = its Table 2). Only where recorded = host of the region-years where the insect has a record.",
  "\nBG and LT species growing stocks read as 1000 m3. Filled = factor model, tested insects only.")

plot_share_groups <- function(st) {
  d <- share_long(dplyr::filter(st, level %in% c("level1", "level2"))) |>
    dplyr::mutate(level_lab = dplyr::if_else(level == "level1", "Level 1", "Level 2"),
                  group = factor(group, rev(c(paper_level1_order, paper_level2_order))))
  p <- dplyr::distinct(d, level_lab, group, paper_share_pct)
  ggplot2::ggplot(d, ggplot2::aes(share, group)) +
    ggplot2::geom_col(ggplot2::aes(fill = version), position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(share, format = "f", digits = 2), group = version),
                       position = ggplot2::position_dodge(width = 0.8), hjust = -0.15, size = 2.4, colour = pal$ink2) +
    ggplot2::geom_point(data = p, ggplot2::aes(paper_share_pct, group), shape = 23, size = 2.8, fill = "white", colour = pal$ink,
                        position = ggplot2::position_nudge(y = 0.27)) +
    ggplot2::scale_fill_manual(values = share_cols, name = NULL, guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::facet_grid(level_lab ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "% of host affected per year (average 2000-2022)", y = NULL,
                  title = "How much of the host is hit each year? Paper groups",
                  caption = share_caption) +
    theme_eu(8.5) + ggplot2::theme(legend.position = "top", strip.text.y = ggplot2::element_text(angle = 0, face = "bold"))
}

plot_share_species <- function(st, species_keep) {
  d <- dplyr::filter(st, level == "species", group %in% species_keep) |>
    dplyr::arrange(share_pct_paper_filled) |>
    dplyr::mutate(lab = paste0(group, " (", host_group, ", ", unit, ")"), lab = factor(lab, lab))
  dl <- share_long(d) |> dplyr::left_join(dplyr::select(d, group, lab), by = "group")
  ggplot2::ggplot(dl, ggplot2::aes(share, lab)) +
    ggplot2::geom_point(ggplot2::aes(colour = version), size = 2.2) +
    ggplot2::geom_point(data = dplyr::filter(d, !is.na(paper_share_pct)), ggplot2::aes(paper_share_pct, lab),
                        shape = 23, size = 2.8, fill = "white", colour = pal$ink) +
    ggplot2::scale_colour_manual(values = share_cols, name = NULL) +
    ggplot2::scale_x_log10(labels = function(x) formatC(x, format = "fg", digits = 2)) +
    ggplot2::labs(x = "% of host affected per year (average 2000-2022, log scale)", y = NULL,
                  title = "How much of the host is hit each year? Individual species",
                  caption = share_caption) +
    theme_eu(8.5) + ggplot2::theme(legend.position = "top")
}

plot_share_by_year <- function(sy, sy_filled) {
  d <- dplyr::bind_rows(
    dplyr::mutate(sy, version = "reported, all host (paper's way)", share = share_pct_paper),
    dplyr::mutate(sy, version = "reported, only where recorded", share = share_pct_where_recorded),
    dplyr::mutate(sy_filled, version = "reported + filled, all host", share = share_pct_paper)) |>
    dplyr::filter(level %in% c("level1", "level2")) |>
    dplyr::mutate(group = factor(group, c(paper_level1_order, paper_level2_order)),
                  version = factor(version, names(share_cols)))
  ggplot2::ggplot(d, ggplot2::aes(year, share, colour = version)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = share_cols, name = NULL) +
    ggplot2::facet_wrap(~ group, scales = "free_y", ncol = 3) +
    ggplot2::labs(x = NULL, y = "% of host affected that year",
                  title = "Share of the host affected, year by year (paper groups)",
                  caption = share_caption) +
    theme_eu(8.5) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold"))
}
