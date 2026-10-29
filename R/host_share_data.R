# Proportion of the host killed (borers) or defoliated (defoliators) per year,
# from the two datasets only:
#   damage : EU-ForDAM value1 (borers m3 of killed trees, defoliators ha)
#   host   : NUTS forest appendix (growing stock m3 and forest area ha of
#            spruce, pine, fir, oak, all conifers, all broadleaves)
#
#   share in a region-year = damage / host in that region x 100
#
# Only region-years where the insect has a recorded value (0 included) count;
# regions that do not record an insect say nothing about it, so they are left
# out of both damage and host. No gap-filling, nothing taken from the paper.
#
#   average per year = all recorded damage / all host of the recorded
#                      region-years (each region-year's host counted once)
#   per year         = that year's recorded damage / host of the regions
#                      recording it that year
#
# Host of each code = what it feeds on (species_host in R/host_share.R;
# otherwise the host tree in the data file). Host groups put together every
# code (aggregates included) that is divided by the same host. Growing stock of
# BG and LT single tree species is read as 1000 m3 (see R/host_share.R).

host_label <- c(spruce = "spruce", pine = "pine", fir = "fir", oak = "oak",
                conifers_all = "all conifers", broadleaves_all = "all broadleaves", all_forest = "all forest")

# Host of every code (single insects and aggregates)
code_hosts <- function(ph_hier, species_meta) {
  ph_hier |>
    dplyr::transmute(species, name, code_type, unit, guild = sub(" on .*", "", level1),
                     host_tree = host_tree_of(species, species_meta),
                     host_group = unname(forest_column_of[host_tree]),
                     group = paste(guild, "on", host_label[host_group]))
}

# Region-year table for one set of codes divided by one host
share_region_years <- function(d, codes, host_group, unit, hs) {
  hv <- hs |> dplyr::filter(host_group == !!host_group, measure == unit) |> dplyr::select(nuts_id, host = value)
  d |>
    dplyr::filter(species %in% codes, !is.na(damage)) |>
    dplyr::group_by(country, nuts_id, year) |>
    dplyr::summarise(damage = sum(damage), .groups = "drop") |>
    dplyr::inner_join(hv, by = "nuts_id") |>
    dplyr::filter(!is.na(host), host > 0)
}

# What is measured: host groups (all codes with that guild and host) and the
# single insects
share_units_data <- function(ch) {
  grp <- ch |> dplyr::group_by(level = "host group", group, guild, unit, host_group) |>
    dplyr::summarise(codes = list(species), .groups = "drop") |>
    dplyr::mutate(host_tree = unname(host_label[host_group]))
  sp <- ch |> dplyr::filter(code_type != "aggregate") |>
    dplyr::transmute(level = "species", group = species, guild, unit, host_group, host_tree, codes = as.list(species))
  dplyr::bind_rows(grp, sp)
}

# host = host tree label (same as the ranking); host_group = forest-file column
# actually divided by
host_share_average <- function(d, units, hs) {
  purrr::pmap_dfr(units, function(level, group, guild, unit, host_group, host_tree, codes) {
    ry <- share_region_years(d, codes, host_group, unit, hs)
    yr <- ry |> dplyr::group_by(year) |> dplyr::summarise(s = 100 * sum(damage) / sum(host), .groups = "drop")
    tibble::tibble(level, group, guild, unit, host = host_tree, divided_by = host_label[[host_group]],
                   n_regions = dplyr::n_distinct(ry$nuts_id), n_region_years = nrow(ry),
                   damage_total = sum(ry$damage),
                   share_pct_per_year = 100 * sum(ry$damage) / sum(ry$host),
                   worst_year = if (nrow(yr)) yr$year[which.max(yr$s)] else NA_integer_,
                   share_pct_worst_year = if (nrow(yr)) max(yr$s) else NA_real_)
  })
}

host_share_years <- function(d, units, hs) {
  purrr::pmap_dfr(dplyr::filter(units, level == "host group"), function(level, group, guild, unit, host_group, host_tree, codes) {
    share_region_years(d, codes, host_group, unit, hs) |>
      dplyr::group_by(year) |>
      dplyr::summarise(damage = sum(damage), host = sum(host), n_regions = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(share_pct = 100 * damage / host, group, guild, unit, host_tree = host_tree, .before = 1)
  })
}

# ---- Figures ----------------------------------------------------------------------

share_data_caption <- paste(
  "Borers: % of the host's growing stock (m3) killed. Defoliators: % of the host's forest area (ha) defoliated (not necessarily killed).",
  "\nDamage = EU-ForDAM value1; host = NUTS forest appendix. Only regions and years where the insect has a record (0 included); no gap-filling.",
  "\nHost tree as in the ranking. Larch, birch, elm, poplar and several-tree hosts are divided by all conifers / all broadleaves (not separate in the forest file).",
  "\nBG and LT single-species growing stocks read as 1000 m3.")

guild_cols <- c(Borers = "#184f95", Defoliators = "#eb6834")

plot_share_avg_groups <- function(sa) {
  d <- dplyr::filter(sa, level == "host group", damage_total > 0) |>
    dplyr::arrange(guild, share_pct_per_year) |>
    dplyr::mutate(lab = factor(paste(group, guild), paste(group, guild)),
                  txt = paste0(formatC(share_pct_per_year, format = "f", digits = 2), "% per year  (worst: ",
                               formatC(share_pct_worst_year, format = "f", digits = 2), "% in ", worst_year, "; ", n_regions, " regions)"))
  ggplot2::ggplot(d, ggplot2::aes(share_pct_per_year, lab)) +
    ggplot2::geom_col(ggplot2::aes(fill = guild), width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = txt), hjust = 0, nudge_x = 0.01, size = 2.7, colour = pal$ink2) +
    ggplot2::scale_fill_manual(values = guild_cols, guide = "none") +
    ggplot2::scale_y_discrete(labels = function(x) sub(" (Borers|Defoliators)$", "", x)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.9))) +
    ggplot2::facet_grid(guild ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "% of host affected per year (average 2000-2022)", y = NULL,
                  title = "Share of the host killed (borers) or defoliated (defoliators) per year, by host tree",
                  caption = share_data_caption) +
    theme_eu(8.5) + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold"))
}

plot_share_avg_species <- function(sa, ph_hier, species_keep) {
  d <- dplyr::filter(sa, level == "species", group %in% species_keep, damage_total > 0) |>
    dplyr::left_join(dplyr::select(ph_hier, group = species, name), by = "group") |>
    dplyr::arrange(guild, share_pct_per_year) |>
    dplyr::mutate(lab = factor(paste0(name, " (", host, ")"), unique(paste0(name, " (", host, ")"))),
                  txt = paste0(formatC(share_pct_per_year, format = "fg", digits = 2), "%"))
  ggplot2::ggplot(d, ggplot2::aes(share_pct_per_year, lab)) +
    ggplot2::geom_point(ggplot2::aes(colour = guild), size = 2.4) +
    ggplot2::geom_text(ggplot2::aes(label = txt), hjust = 0, nudge_x = 0.08, size = 2.5, colour = pal$ink2) +
    ggplot2::scale_colour_manual(values = guild_cols, guide = "none") +
    ggplot2::scale_x_log10(labels = function(x) paste0(formatC(x, format = "fg", digits = 1), "%"),
                           expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::facet_grid(guild ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "% of host affected per year (average 2000-2022, log scale)", y = NULL,
                  title = "Share of the host killed or defoliated per year, individual species",
                  caption = share_data_caption) +
    theme_eu(8.5) + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold"))
}

plot_share_years_data <- function(sy) {
  d <- sy |> dplyr::group_by(group, guild) |> dplyr::filter(sum(damage) > 0) |> dplyr::ungroup() |>
    dplyr::mutate(panel = paste(group, "-", guild))
  ggplot2::ggplot(d, ggplot2::aes(year, share_pct, colour = guild)) +
    ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 1) +
    ggplot2::scale_colour_manual(values = guild_cols, guide = "none") +
    ggplot2::facet_wrap(~ paste(guild, sub("^(Borers|Defoliators) ", "", group)), scales = "free_y", ncol = 3) +
    ggplot2::labs(x = NULL, y = "% of host affected that year",
                  title = "Share of the host killed or defoliated, year by year",
                  caption = share_data_caption) +
    theme_eu(8.5) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
}

# ---- Species as the unit ------------------------------------------------------------

# Year-by-year share for single insects
host_share_years_species <- function(d, units, hs, species_keep) {
  purrr::pmap_dfr(dplyr::filter(units, level == "species", group %in% species_keep),
                  function(level, group, guild, unit, host_group, host_tree, codes) {
    share_region_years(d, codes, host_group, unit, hs) |>
      dplyr::group_by(year) |>
      dplyr::summarise(damage = sum(damage), host = sum(host), n_regions = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(share_pct = 100 * damage / host, species = group, guild, unit,
                    host_tree = host_tree, .before = 1)
  })
}

# Average % of its host affected per year, one bar per species (normal scale,
# borers and defoliators in separate panels with their own scale)
plot_share_species_avg <- function(sa, ph_hier, species_keep) {
  pc <- function(x, k) dplyr::if_else(x < 10^-k, paste0("<", formatC(10^-k, format = "f", digits = k)), formatC(x, format = "f", digits = k))
  d <- dplyr::filter(sa, level == "species", group %in% species_keep) |>
    dplyr::left_join(dplyr::select(ph_hier, group = species, name), by = "group") |>
    dplyr::mutate(panel = dplyr::if_else(guild == "Borers", "Borers: % of host growing stock killed per year",
                                         "Defoliators: % of host forest area defoliated per year"),
                  lab = paste0(name, " (", host, ")"),
                  txt = paste0(pc(share_pct_per_year, 3), "%   worst ", worst_year, ": ", pc(share_pct_worst_year, 2), "%   (",
                               n_regions, dplyr::if_else(n_regions == 1, " region)", " regions)"))) |>
    dplyr::arrange(panel, share_pct_per_year) |>
    dplyr::mutate(key = factor(paste(panel, lab), paste(panel, lab)))
  ggplot2::ggplot(d, ggplot2::aes(share_pct_per_year, key)) +
    ggplot2::geom_col(ggplot2::aes(fill = guild), width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = txt), hjust = 0, nudge_x = 0.005, size = 2.5, colour = pal$ink2) +
    ggplot2::scale_fill_manual(values = guild_cols, guide = "none") +
    ggplot2::scale_y_discrete(labels = function(x) sub("^.*per year ", "", x)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.75))) +
    ggplot2::facet_wrap(~ panel, scales = "free", ncol = 1) +
    ggplot2::labs(x = "Average % of the host affected per year, 2000-2022", y = NULL,
                  title = "How much of its host tree does each species kill or defoliate per year?",
                  caption = share_data_caption) +
    theme_eu(8.5) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# Year by year for the species with the largest average share in one guild
# Plain titles for the host-share graphs (sets 1 and 2): borers "proportion of
# host plants killed", defoliators "proportion of host area defoliated".
share_title <- function(guild) if (guild == "Borers") "proportion of host plants killed" else "proportion of host area defoliated"
# For defoliators the measure is the host forest area defoliated, said in the subtitle.
killed_note <- function(guild) {
  if (guild == "Borers") "Measured as % of the host's growing stock (m3) killed."
  else "For defoliators: % of the host's forest area (ha) defoliated (stripped of leaves, not always killed)."
}

# "Spruce killed by Ips typographus": the tree is what is measured, the insect what did it
killed_label <- function(host, insect, guild) {
  paste0(toupper(substr(host, 1, 1)), substring(host, 2), dplyr::if_else(guild == "Borers", " killed by ", " defoliated by "), insect)
}

plot_share_species_years <- function(sys, sa, ph_hier, guild, n = 9) {
  top <- dplyr::filter(sa, level == "species", guild == !!guild, group %in% unique(sys$species)) |>
    dplyr::slice_max(share_pct_per_year, n = n) |> dplyr::pull(group)
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  d <- dplyr::filter(sys, species %in% top) |>
    dplyr::mutate(panel = killed_label(host_tree, nm[species], guild))
  hs <- dplyr::distinct(sys, species, host_tree)
  d$panel <- factor(d$panel, killed_label(hs$host_tree[match(top, hs$species)], nm[top], guild))
  what <- if (guild == "Borers") "% of host growing stock killed" else "% of host forest area defoliated"
  ggplot2::ggplot(d, ggplot2::aes(year, share_pct)) +
    ggplot2::geom_line(colour = guild_cols[[guild]], linewidth = 0.8) +
    ggplot2::geom_point(colour = guild_cols[[guild]], size = 1) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 3) +
    ggplot2::labs(x = NULL, y = paste(what, "that year"),
                  title = paste0(guild, ": ", share_title(guild), " per year, by insect species"),
                  subtitle = paste("The", n, "species with the largest average share, 2000-2022. Each panel has its own scale.", killed_note(guild)),
                  caption = share_data_caption) +
    theme_eu(8.5) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
}

# One guild, one number per species: the average % of its host affected per year
plot_share_species_avg_guild <- function(sa, ph_hier, species_keep, guild) {
  what <- if (guild == "Borers") "killed" else "defoliated"
  base <- if (guild == "Borers") "growing stock (m3)" else "forest area (ha)"
  d <- dplyr::filter(sa, level == "species", group %in% species_keep, guild == !!guild) |>
    dplyr::left_join(dplyr::select(ph_hier, group = species, name), by = "group") |>
    dplyr::arrange(share_pct_per_year) |>
    dplyr::mutate(lab = killed_label(host, name, guild), lab = factor(lab, lab),
                  txt = dplyr::if_else(share_pct_per_year < 0.001, "<0.001%",
                                       paste0(formatC(share_pct_per_year, format = "f", digits = 3), "%")))
  ggplot2::ggplot(d, ggplot2::aes(share_pct_per_year, lab)) +
    ggplot2::geom_col(fill = guild_cols[[guild]], width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = txt), hjust = 0, nudge_x = max(d$share_pct_per_year) * 0.01,
                       size = 2.8, colour = pal$ink2) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = paste0("Average % of the host's ", base, " ", what, " per year (2000-2022)"), y = NULL,
                  title = paste0(guild, ": ", share_title(guild), " per year on average, by insect species"),
                  subtitle = killed_note(guild),
                  caption = paste("Average = all recorded damage / all host forest of the regions and years where the species is recorded (0 included).",
                                  "\nDamage = EU-ForDAM value1; host = NUTS forest appendix. No gap-filling. BG and LT single-species growing stocks read as 1000 m3.")) +
    theme_eu(9)
}

# ---- Guild level: share of the host per year and on average ------------------------
# Guild x host type (the paper's level 1): all codes of the group, aggregates
# included, divided by all conifers (gymnosperms) or all broadleaves
# (angiosperms) of the regions and years where the group has a record.
# Only codes whose own host belongs to the group's host type are counted:
# Melolontha (host = all forest, larvae mostly in pine stands) would otherwise
# be divided by broadleaves only and push some Polish regions above 100%.
guild_share_units <- function(ph_hier, hosts) {
  conifer_hosts <- c("spruce", "pine", "fir", "conifers_all")
  broadleaf_hosts <- c("oak", "broadleaves_all")
  ph_hier |>
    dplyr::left_join(dplyr::select(hosts, species, host_group), by = "species") |>
    dplyr::filter(dplyr::if_else(grepl("gymnosperms$", level1), host_group %in% conifer_hosts, host_group %in% broadleaf_hosts)) |>
    dplyr::group_by(group = level1, unit) |>
    dplyr::summarise(codes = list(species), .groups = "drop") |>
    dplyr::mutate(guild = sub(" on .*", "", group), host_type = sub("^.* on ", "", group),
                  host_group = dplyr::if_else(host_type == "gymnosperms", "conifers_all", "broadleaves_all"))
}

guild_share_years <- function(d, gu, hs) {
  purrr::pmap_dfr(gu, function(group, unit, codes, guild, host_type, host_group) {
    share_region_years(d, codes, host_group, unit, hs) |>
      dplyr::group_by(year) |>
      dplyr::summarise(damage = sum(damage), host = sum(host), .groups = "drop") |>
      dplyr::mutate(share_pct = 100 * damage / host, group, guild, host_type, unit, .before = 1)
  })
}

guild_share_average <- function(gy) {
  gy |> dplyr::group_by(group, guild, host_type, unit) |>
    dplyr::summarise(share_pct_per_year = 100 * sum(damage) / sum(host),
                     worst_year = year[which.max(share_pct)], share_pct_worst_year = max(share_pct), .groups = "drop")
}

guild_share_caption <- paste(
  "All codes of the guild, aggregates included, except Melolontha (feeds in all forest). Gymnosperms divided by all conifers, angiosperms by all broadleaves (NUTS forest appendix),",
  "\nonly in regions and years with a record (0 included). EU-ForDAM value1, no gap-filling. BG and LT single-species growing stocks read as 1000 m3.")

plot_guild_share_years <- function(gy, guild) {
  what <- if (guild == "Borers") "% of host growing stock killed" else "% of host forest area defoliated"
  d <- dplyr::filter(gy, guild == !!guild) |> dplyr::mutate(panel = paste(guild, "on", host_type))
  ggplot2::ggplot(d, ggplot2::aes(year, share_pct)) +
    ggplot2::geom_line(colour = guild_cols[[guild]], linewidth = 0.8) +
    ggplot2::geom_point(colour = guild_cols[[guild]], size = 1.2) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA)) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = NULL, y = paste(what, "that year"), title = paste0(guild, ": ", share_title(guild), " per year (2000-2022)"),
                  subtitle = paste("Each panel has its own scale.", killed_note(guild)), caption = guild_share_caption) +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
}

plot_guild_share_avg <- function(ga, guild) {
  what <- if (guild == "Borers") "killed" else "defoliated"
  base <- if (guild == "Borers") "growing stock (m3)" else "forest area (ha)"
  d <- dplyr::filter(ga, guild == !!guild) |>
    dplyr::mutate(lab = factor(paste(guild, "on", host_type)),
                  txt = paste0(formatC(share_pct_per_year, format = "f", digits = 3), "% per year   (worst ", worst_year, ": ",
                               formatC(share_pct_worst_year, format = "f", digits = 2), "%)"))
  ggplot2::ggplot(d, ggplot2::aes(share_pct_per_year, lab)) +
    ggplot2::geom_col(fill = guild_cols[[guild]], width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = txt), hjust = 0, nudge_x = max(d$share_pct_per_year) * 0.02, size = 3, colour = pal$ink2) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.8))) +
    ggplot2::labs(x = paste0("Average % of the host's ", base, " ", what, " per year (2000-2022)"), y = NULL,
                  title = paste0(guild, ": ", share_title(guild), " per year on average (2000-2022)"),
                  subtitle = killed_note(guild), caption = guild_share_caption) +
    theme_eu(9.5)
}

# ---- Comparison with the 2025 paper's Table 2 (share of host per year) ------------------
# Three numbers per row: the paper's, ours done the paper's way (R/paper_compare.R:
# its series filter, its hosts, damage / host of all regions), and ours as in the
# graphs (damage / host of the regions where the insect is recorded, shared host table).
paper_share_comparison <- function(g_avg, hd_avg, t2s, ph_hier, species_meta) {
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  ours <- dplyr::bind_rows(
    dplyr::transmute(g_avg, group, ours_graph = share_pct_per_year, our_host = dplyr::if_else(host_type == "gymnosperms", "all conifers", "all broadleaves")),
    dplyr::transmute(hd_avg, group, ours_graph = share_pct_per_year, our_host = divided_by))
  paper_host <- c(conifers_all = "all conifers", broadleaves_all = "all broadleaves", spruce = "spruce", pine = "pine", fir = "fir", oak = "oak")
  t2s |>
    dplyr::filter(level != "level2") |>
    dplyr::left_join(ours, by = "group") |>
    dplyr::mutate(paper_host = unname(paper_host[host_group]),
                  name = dplyr::if_else(level == "species", nm[group], group),
                  why = dplyr::case_when(
                    our_host != paper_host ~ paste0("Divided by ", our_host, " (paper: ", paper_host, "), because it feeds on more than one tree"),
                    abs(ours_graph / paper_share_pct - 1) <= 0.15 ~ "Same within rounding",
                    TRUE ~ "Divided only by the host of the regions that record this insect")) |>
    dplyr::transmute(level, name, paper = paper_share_pct, ours_paper_way = ours_23_years, ours_graph, why)
}

plot_paper_share_comparison <- function(cmp) {
  f <- function(x) paste0(formatC(x, format = "f", digits = 2), "%")
  d <- cmp |> dplyr::transmute(`Insect or group` = name, `2025 paper` = f(paper), `Ours, the paper's way` = f(ours_paper_way),
                               `Ours, in the graphs` = f(ours_graph), `Why the graphs differ` = why)
  fill <- dplyr::if_else(cmp$why == "Same within rounding", "#e6f2e6", "#fdf6e3")
  th <- gridExtra::ttheme_minimal(base_size = 9,
    core = list(bg_params = list(fill = fill, col = "white"), fg_params = list(hjust = 0, x = 0.03)),
    colhead = list(bg_params = list(fill = "#16302B"), fg_params = list(col = "white", fontface = "bold", hjust = 0, x = 0.03)))
  g <- gridExtra::tableGrob(d, rows = NULL, theme = th)
  ttl <- grid::textGrob("Share of the host affected per year: our numbers vs the 2025 EU-ForDAM paper (Table 2)", x = 0.01, hjust = 0,
                        gp = grid::gpar(fontsize = 13, fontface = "bold"))
  note <- grid::textGrob(paste(
    "Paper's way = the paper's data preparation (series with fewer than 6 non-zero years removed), its host for each insect, and damage / host of ALL regions.",
    "\nIn the graphs = all reported data; damage / host of only the regions where the insect is recorded; insects feeding on several trees divided by all of them.",
    "\nGreen = same as the paper; yellow = differs for the reason given. Remaining small gaps in 'the paper's way': the paper's percentages fit dividing by 22 years, not 23.",
    "\nBorers: % of host growing stock (m3) killed; defoliators: % of host forest area (ha) defoliated. Average per year, 2000-2022."),
    x = 0.01, hjust = 0, gp = grid::gpar(fontsize = 8, col = "#555555"))
  gridExtra::arrangeGrob(ttl, g, note, ncol = 1, heights = grid::unit.c(grid::unit(0.45, "in"), sum(g$heights), grid::unit(0.85, "in")))
}
