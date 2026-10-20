# Total damage, from the EU-ForDAM data as reported (value1; no gap-filling)
#   A: total per guild (borers m3, defoliators ha; never added together)
#   B: total per individual species
#   C: total per year
# Every reported value is added up once: zeros count as 0, missing values are
# left out, aggregate codes count in their guild totals (A, C) but are not
# species (B). CZ sub-rows summed; CZ and DE spruce bark-borer group counted
# as I. typographus (R/paper_hier.R).

guild_of <- function(level1) sub(" on .*", "", level1)
host_type_of <- function(level1) sub("^.* on ", "", level1)

# A: per guild, split by host type (gymnosperms / angiosperms)
total_by_guild <- function(dmg, ph_hier) {
  dmg |>
    dplyr::filter(!is.na(damage)) |>
    dplyr::inner_join(dplyr::select(ph_hier, species, level1), by = "species") |>
    dplyr::group_by(guild = guild_of(level1), host_type = host_type_of(level1), unit) |>
    dplyr::summarise(total = sum(damage), .groups = "drop") |>
    dplyr::group_by(guild) |>
    dplyr::mutate(share_of_guild = total / sum(total)) |>
    dplyr::ungroup()
}

# B: per individual species
total_by_species <- function(dmg, ph_hier, species_keep) {
  dmg |>
    dplyr::filter(!is.na(damage), species %in% species_keep) |>
    dplyr::group_by(species) |>
    dplyr::summarise(total = sum(damage), .groups = "drop") |>
    dplyr::left_join(dplyr::select(ph_hier, species, name, level1, unit), by = "species") |>
    dplyr::mutate(guild = guild_of(level1))
}

# C: per year, split by host type
total_by_year <- function(dmg, ph_hier) {
  dmg |>
    dplyr::filter(!is.na(damage)) |>
    dplyr::inner_join(dplyr::select(ph_hier, species, level1), by = "species") |>
    dplyr::group_by(year, guild = guild_of(level1), host_type = host_type_of(level1), unit) |>
    dplyr::summarise(total = sum(damage), .groups = "drop")
}

# Check: damage of all insects on the same host in a region-year must not
# exceed that host (borers: growing stock m3; defoliators: area ha)
damage_vs_host_check <- function(dmg, hosts, hs) {
  dmg |>
    dplyr::filter(!is.na(damage)) |>
    dplyr::inner_join(dplyr::select(hosts, species, guild, host_group), by = "species") |>
    dplyr::group_by(country, nuts_id, year, guild, host_group, unit) |>
    dplyr::summarise(damage = sum(damage), insects = paste(unique(species[damage > 0]), collapse = ", "), .groups = "drop") |>
    dplyr::left_join(dplyr::select(hs, nuts_id, host_group, unit = measure, host = value),
                     by = c("nuts_id", "host_group", "unit")) |>
    dplyr::mutate(pct_of_host = 100 * damage / host,
                  problem = dplyr::case_when(damage > 0 & !is.na(host) & host == 0 ~ "damage but host is 0 in the forest file",
                                             pct_of_host > 100 ~ "damage above host",
                                             pct_of_host > 50 ~ "above 50% of host",
                                             TRUE ~ NA_character_)) |>
    dplyr::filter(!is.na(problem)) |>
    dplyr::arrange(dplyr::desc(pct_of_host))
}

# ---- Figures ------------------------------------------------------------------------

host_type_cols <- c(gymnosperms = "#184f95", angiosperms = "#86b6ef")
total_caption <- paste("All reported damage 2000-2022 (EU-ForDAM value1), no gap-filling. Borers in m3 of trees killed, defoliators in ha defoliated: never added together.",
                       "\nAggregate codes counted in the totals; CZ and DE spruce bark-borer group counted as I. typographus.")

plot_total_by_guild <- function(tg, word = "total") {
  d <- tg |> dplyr::mutate(panel = paste0(guild, " (", unit, ")"),
                           host_type = factor(host_type, c("angiosperms", "gymnosperms")))
  lab <- d |> dplyr::group_by(panel, unit) |> dplyr::summarise(total = sum(total), .groups = "drop")
  ggplot2::ggplot(d, ggplot2::aes(total, panel)) +
    ggplot2::geom_col(ggplot2::aes(fill = host_type), width = 0.55) +
    ggplot2::geom_text(data = lab, ggplot2::aes(label = paste(" ", word, short_number(total), unit)),
                       hjust = 0, nudge_x = 0, size = 3.2, colour = pal$ink) +
    ggplot2::geom_text(data = dplyr::filter(d, share_of_guild > 0.04),
                       ggplot2::aes(label = paste0(host_type, " ", round(100 * share_of_guild), "%"), group = host_type),
                       position = ggplot2::position_stack(vjust = 0.5), size = 2.8, colour = "white") +
    ggplot2::scale_fill_manual(values = host_type_cols, name = NULL, guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_x_continuous(labels = function(x) short_number(x), expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ggplot2::facet_wrap(~ panel, scales = "free", ncol = 1) +
    ggplot2::labs(x = "Total damage 2000-2022", y = NULL, title = "Total damage 2000-2022, borers and defoliators",
                  caption = total_caption) +
    theme_eu(9) + ggplot2::theme(legend.position = "top", axis.text.y = ggplot2::element_blank(),
                                 strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

plot_total_by_species <- function(ts, guild) {
  d <- dplyr::filter(ts, guild == !!guild, total > 0) |>
    dplyr::mutate(share = total / sum(total)) |>
    dplyr::arrange(total) |>
    dplyr::mutate(name = factor(name, name),
                  txt = paste0(short_number(total), " ", unit, "  (",
                               dplyr::if_else(share < 0.001, "<0.1", formatC(100 * share, format = "f", digits = 1)), "%)"))
  what <- if (guild == "Borers") "m3 of trees killed" else "ha defoliated"
  ggplot2::ggplot(d, ggplot2::aes(total, name)) +
    ggplot2::geom_col(fill = guild_cols[[guild]], width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = txt), hjust = 0, nudge_x = max(d$total) * 0.01, size = 2.8, colour = pal$ink2) +
    ggplot2::scale_x_continuous(labels = function(x) short_number(x), expand = ggplot2::expansion(mult = c(0, 0.22))) +
    ggplot2::labs(x = paste0("Total ", what, ", 2000-2022"), y = NULL,
                  title = paste0(guild, ": total damage per species, 2000-2022"),
                  subtitle = "In brackets: share of the total of these species",
                  caption = paste("Individual species only (genus-level codes, species groups and aggregate codes left out).",
                                  "\nAll reported damage (EU-ForDAM value1), no gap-filling. CZ and DE spruce bark-borer group counted as I. typographus.")) +
    theme_eu(9)
}

plot_total_by_year <- function(ty, guild) {
  d <- dplyr::filter(ty, guild == !!guild) |>
    dplyr::mutate(host_type = factor(host_type, c("angiosperms", "gymnosperms")))
  what <- if (guild == "Borers") "m3 of trees killed" else "ha defoliated"
  ggplot2::ggplot(d, ggplot2::aes(year, total)) +
    ggplot2::geom_col(ggplot2::aes(fill = host_type), width = 0.75) +
    ggplot2::scale_fill_manual(values = host_type_cols, name = NULL, guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_y_continuous(labels = function(x) short_number(x), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = paste(what, "per year"),
                  title = paste0(guild, ": total damage per year, 2000-2022"),
                  caption = paste(total_caption,
                                  "\nThe number of reporting regions changes over time (most countries start in 2003)."))+
    theme_eu(9) + ggplot2::theme(legend.position = "top")
}

# ---- Data checks for the totals ------------------------------------------------------
# Lists entries that look like recording problems rather than real damage, for
# the data providers. Nothing is changed; this only flags.
#   all zero        : every insect reported in a region-year is exactly 0
#                     (likely "not reported", not "no damage", in large forests)
#   split values    : values ending in .333 / .667 (look like 3-year totals
#                     divided evenly)
#   sudden drop     : a value below 1% of the median of the 2 years before and
#                     after, where those are large (> 100,000)
suspicious_values <- function(dmg) {
  d <- dplyr::filter(dmg, !is.na(damage))
  zero <- d |> dplyr::group_by(country, nuts_id, year) |>
    dplyr::summarise(n_insects = dplyr::n(), all0 = all(damage == 0), .groups = "drop") |>
    dplyr::filter(all0, n_insects >= 5) |>
    dplyr::group_by(country, nuts_id) |>
    dplyr::summarise(check = "all insects 0 in these years", years = paste(year, collapse = ", "),
                     detail = paste(max(n_insects), "insects reported, all 0"), .groups = "drop")
  thirds <- d |> dplyr::filter(abs(damage %% 1 - 1 / 3) < 1e-6 | abs(damage %% 1 - 2 / 3) < 1e-6) |>
    dplyr::group_by(country, nuts_id) |>
    dplyr::summarise(check = "values ending in .333 / .667", years = paste(range(year), collapse = "-"),
                     detail = paste(dplyr::n(), "values, e.g.", formatC(damage[1], format = "f", digits = 2), species[1]), .groups = "drop")
  drop <- d |> dplyr::arrange(country, nuts_id, species, year) |>
    dplyr::group_by(country, nuts_id, species) |>
    dplyr::mutate(around = purrr::map_dbl(year, function(y) stats::median(damage[abs(year - y) %in% 1:2]))) |>
    dplyr::ungroup() |>
    dplyr::filter(around > 1e5, damage < 0.01 * around) |>
    dplyr::transmute(country, nuts_id, check = "sudden drop", years = as.character(year),
                     detail = paste0(species, ": ", formatC(damage, format = "f", digits = 0), " vs about ",
                                     formatC(around, format = "f", digits = 0), " in the years around"))
  dplyr::bind_rows(zero, thirds, drop) |> dplyr::arrange(check, country, nuts_id)
}

# ---- Average per year ----------------------------------------------------------------
# Average damage per year = total 2000-2022 / 23 years (every species has
# records, 0 included, in all 23 years)
average_per_year <- function(tot, n_years = 23) dplyr::mutate(tot, total = total / n_years)

plot_avg_by_guild <- function(tg) {
  plot_total_by_guild(average_per_year(tg), word = "average") +
    ggplot2::labs(x = "Average damage per year (2000-2022)",
                  title = "Average damage per year, borers and defoliators (2000-2022)",
                  caption = paste("Average = total reported damage 2000-2022 / 23 years (EU-ForDAM value1), no gap-filling.",
                                  "\nBorers in m3 of trees killed, defoliators in ha defoliated: never added together. Aggregate codes included; CZ and DE spruce bark-borer group counted as I. typographus."))
}

plot_avg_by_species <- function(ts, guild) {
  what <- if (guild == "Borers") "m3 of trees killed" else "ha defoliated"
  plot_total_by_species(average_per_year(ts), guild) +
    ggplot2::labs(x = paste0("Average ", what, " per year (2000-2022)"), subtitle = "In brackets: share of the average of these species",
                  title = paste0(guild, ": average damage per year per species (2000-2022)"),
                  caption = paste("Average = total reported damage 2000-2022 / 23 years. Individual species only (genus-level codes, species groups and aggregate codes left out).",
                                  "\nEU-ForDAM value1, no gap-filling. CZ and DE spruce bark-borer group counted as I. typographus."))
}

# ---- Average per year with the typical year and the range ------------------------
# Per species (or guild) and year: the yearly total; then the average (mean),
# the typical year (median) and the lowest / highest year over 2000-2022.
yearly_stats <- function(dmg, ph_hier, species_keep = NULL) {
  d <- dplyr::filter(dmg, !is.na(damage)) |>
    dplyr::inner_join(dplyr::select(ph_hier, species, name, level1), by = "species") |>
    dplyr::mutate(guild = guild_of(level1))
  if (!is.null(species_keep)) d <- dplyr::filter(d, species %in% species_keep)
  key <- if (is.null(species_keep)) c("guild", "unit") else c("species", "name", "guild", "unit")
  d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key)), year) |>
    dplyr::summarise(t = sum(damage), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(average = mean(t), typical_year = stats::median(t),
                     lowest = min(t), lowest_year = year[which.min(t)],
                     highest = max(t), highest_year = year[which.max(t)],
                     top3_share = sum(sort(t, decreasing = TRUE)[1:3]) / sum(t), .groups = "drop")
}

# Bar = average year, dot = typical (median) year, line = lowest to highest year
plot_yearly_stats <- function(st, guild = NULL) {
  by_guild <- is.null(guild)
  d <- if (by_guild) dplyr::mutate(st, name = paste0(guild, " (", unit, ")")) else dplyr::filter(st, guild == !!guild, highest > 0)
  d <- d |> dplyr::arrange(average) |>
    dplyr::mutate(name = factor(name, name),
                  txt = paste0("average ", short_number(average), "  |  typical year ", short_number(typical_year),
                               "  |  ", lowest_year, ": ", short_number(lowest), " to ", highest_year, ": ", short_number(highest)))
  col <- if (by_guild) pal$ink else guild_cols[[guild]]
  what <- if (by_guild) "damage per year" else if (guild == "Borers") "m3 of trees killed per year" else "ha defoliated per year"
  p <- ggplot2::ggplot(d, ggplot2::aes(y = name)) +
    ggplot2::geom_col(ggplot2::aes(x = average), fill = col, alpha = 0.35, width = 0.7) +
    ggplot2::geom_segment(ggplot2::aes(x = lowest, xend = highest, yend = name), colour = col, linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = typical_year), colour = col, size = 2.4) +
    ggplot2::geom_text(ggplot2::aes(x = highest, label = txt), hjust = 0, nudge_x = max(d$highest) * 0.015,
                       size = 2.5, colour = pal$ink2) +
    ggplot2::scale_x_continuous(labels = function(x) short_number(x), expand = ggplot2::expansion(mult = c(0, 0.9))) +
    ggplot2::labs(x = what, y = NULL,
                  subtitle = "Bar = average year (total / 23). Dot = typical year (median). Line = lowest to highest year.",
                  caption = paste("Reported damage 2000-2022 (EU-ForDAM value1), no gap-filling. Borers in m3 of trees killed, defoliators in ha defoliated: never added together.",
                                  if (by_guild) "\nAll codes, aggregates included." else "\nIndividual species only. CZ and DE spruce bark-borer group counted as I. typographus.")) +
    theme_eu(9)
  if (by_guild) {
    p + ggplot2::facet_wrap(~ name, scales = "free", ncol = 1) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(), strip.text = ggplot2::element_text(face = "bold", hjust = 0)) +
      ggplot2::labs(title = "Damage per year: average, typical year and range (2000-2022)")
  } else {
    p + ggplot2::labs(title = paste0(guild, ": damage per year per species (2000-2022)"))
  }
}
