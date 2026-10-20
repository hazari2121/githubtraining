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

plot_total_by_guild <- function(tg) {
  d <- tg |> dplyr::mutate(panel = paste0(guild, " (", unit, ")"),
                           host_type = factor(host_type, c("angiosperms", "gymnosperms")))
  lab <- d |> dplyr::group_by(panel, unit) |> dplyr::summarise(total = sum(total), .groups = "drop")
  ggplot2::ggplot(d, ggplot2::aes(total, panel)) +
    ggplot2::geom_col(ggplot2::aes(fill = host_type), width = 0.55) +
    ggplot2::geom_text(data = lab, ggplot2::aes(label = paste("  total", short_number(total), unit)),
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
