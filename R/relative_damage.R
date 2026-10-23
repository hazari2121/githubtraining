# Relative damage: host volume and area linked to every insect and group
#
# For every group at every level (R/levels.R) and every region-year where the
# group is recorded (0 included):
#   damage          m3 killed (borers) or ha defoliated (defoliators)
#   host_area_ha    area of the group's host in the region (forest appendix)
#   host_stock_m3   growing stock of the host in the region
#   relative_pct    damage / host stock (borers) or damage / host area (defoliators) x 100
#   m3_per_ha_host  borers only: m3 killed per ha of host forest
# Host = the forest-appendix column of the group's host tree. Codes whose host
# lies outside a host type are left out of that host type's relative damage
# (Melolontha, feeding in all forest, under defoliators on angiosperms), so
# the damage is never divided by the wrong forest. Regions without forest data
# are dropped. BG and LT single-species stocks read as 1000 m3.

relative_damage <- function(dmg, lm, hs) {
  host <- hs |> dplyr::filter(measure %in% c("ha", "m3")) |>
    tidyr::pivot_wider(id_cols = c(nuts_id, host_group), names_from = measure, values_from = value) |>
    dplyr::rename(host_col = host_group, host_area_ha = ha, host_stock_m3 = m3)
  inside <- function(level, host_col, code_col) {
    level != "3_host_type" | code_col %in% if (host_col[1] == "conifers_all") c("spruce", "pine", "fir", "conifers_all") else c("oak", "broadleaves_all")
  }
  code_col <- dplyr::filter(lm, level == "2_host_tree") |> dplyr::distinct(code, code_host_col = host_col)
  memb <- lm |> dplyr::left_join(code_col, by = "code") |>
    dplyr::group_by(level, group) |>
    dplyr::filter(inside(level, host_col, code_host_col)) |>
    dplyr::ungroup()
  dmg |> dplyr::filter(!is.na(damage)) |>
    dplyr::inner_join(dplyr::select(memb, level, group, species = code), by = "species", relationship = "many-to-many") |>
    dplyr::group_by(level, group, country, nuts_id, year) |>
    dplyr::summarise(damage = sum(damage), n_codes = dplyr::n(), .groups = "drop") |>
    dplyr::left_join(dplyr::distinct(lm, level, group, guild, unit, host_tree, host_col, host_is_proxy), by = c("level", "group")) |>
    dplyr::left_join(host, by = c("nuts_id", "host_col")) |>
    dplyr::mutate(host_used = dplyr::if_else(unit == "m3", host_stock_m3, host_area_ha),
                  relative_pct = dplyr::if_else(!is.na(host_used) & host_used > 0, 100 * damage / host_used, NA_real_),
                  m3_per_ha_host = dplyr::if_else(unit == "m3" & !is.na(host_area_ha) & host_area_ha > 0, damage / host_area_ha, NA_real_)) |>
    dplyr::select(level, group, guild, unit, host_tree, host_col, host_is_proxy, country, nuts_id, year, damage, n_codes,
                  host_area_ha, host_stock_m3, relative_pct, m3_per_ha_host)
}

# Summaries: per group per year (Europe, recorded region-years), and averages
relative_damage_years <- function(rd) {
  rd |> dplyr::filter(!is.na(relative_pct)) |>
    dplyr::mutate(host_used = dplyr::if_else(unit == "m3", host_stock_m3, host_area_ha)) |>
    dplyr::group_by(level, group, guild, unit, host_tree, host_col, host_is_proxy, year) |>
    dplyr::summarise(damage = sum(damage), host_used = sum(host_used), host_area_ha = sum(host_area_ha),
                     n_regions = dplyr::n(), n_countries = dplyr::n_distinct(country), .groups = "drop") |>
    dplyr::mutate(relative_pct = 100 * damage / host_used,
                  m3_per_ha_host = dplyr::if_else(unit == "m3", damage / host_area_ha, NA_real_))
}

relative_damage_summary <- function(rd, ry) {
  avg <- ry |> dplyr::group_by(level, group, guild, unit, host_tree, host_col, host_is_proxy) |>
    dplyr::summarise(damage_total = sum(damage), relative_pct_per_year = 100 * sum(damage) / sum(host_used),
                     m3_per_ha_host_per_year = dplyr::if_else(unit[1] == "m3", sum(damage) / sum(host_area_ha), NA_real_),
                     worst_year = year[which.max(relative_pct)], relative_pct_worst_year = max(relative_pct),
                     .groups = "drop")
  reg <- rd |> dplyr::filter(!is.na(relative_pct)) |>
    dplyr::group_by(level, group) |>
    dplyr::summarise(n_regions = dplyr::n_distinct(nuts_id), n_countries = dplyr::n_distinct(country),
                     n_region_years = dplyr::n(), max_region_year_pct = max(relative_pct),
                     region_years_over_100pct = sum(relative_pct > 100), .groups = "drop")
  dplyr::left_join(avg, reg, by = c("level", "group")) |> dplyr::arrange(level, guild, dplyr::desc(relative_pct_per_year))
}

relative_damage_country <- function(rd) {
  rd |> dplyr::filter(!is.na(relative_pct)) |>
    dplyr::mutate(host_used = dplyr::if_else(unit == "m3", host_stock_m3, host_area_ha)) |>
    dplyr::group_by(level, group, unit, country) |>
    dplyr::summarise(n_years = dplyr::n_distinct(year), damage = sum(damage), relative_pct_per_year = 100 * sum(damage) / sum(host_used),
                     .groups = "drop")
}

# Figure: average relative damage per group at levels 2-4 (species are in sets 1-2)
plot_relative_damage_levels <- function(rs, guild) {
  d <- rs |> dplyr::filter(level != "1_species", guild == !!guild) |>
    dplyr::mutate(level_lab = factor(level_names[level], level_names),
                  lab = paste0(group, dplyr::if_else(host_is_proxy, " *", "")),
                  txt = paste0(formatC(relative_pct_per_year, format = "f", digits = 3), "%")) |>
    dplyr::group_by(level_lab) |> dplyr::arrange(relative_pct_per_year, .by_group = TRUE) |> dplyr::ungroup() |>
    dplyr::mutate(key = factor(paste(level_lab, lab), unique(paste(level_lab, lab))))
  what <- if (guild == "Borers") "% of host growing stock (m3) killed" else "% of host forest area (ha) defoliated"
  ggplot2::ggplot(d, ggplot2::aes(relative_pct_per_year, key)) +
    ggplot2::geom_col(fill = guild_cols[[guild]], width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = txt), hjust = 0, nudge_x = max(d$relative_pct_per_year) * 0.01, size = 2.7, colour = pal$ink2) +
    ggplot2::scale_y_discrete(labels = function(k) sub("^.*?\\. [^ ]+( \\(.*?\\))? ", "", k)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::facet_grid(level_lab ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = paste("Average", what, "per year, 2000-2022"), y = NULL,
                  title = paste0(guild, ": proportion of host plants killed per year, at each level of aggregation"),
                  subtitle = "Damage of the group / host of the regions and years where the group is recorded (0 included). * = no own host column: all conifers or all broadleaves used.",
                  caption = paste("Host = NUTS forest appendix column of the group's host tree. Host type: codes feeding outside the host type left out (Melolontha).",
                                  "\nEU-ForDAM value1, reported data, no gap-filling. BG and LT single-species growing stocks read as 1000 m3.")) +
    theme_eu(8.5) + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold", hjust = 0))
}
