# Share of the host affected per year, as in the last column of Table 2 of the
# 2025 EU-ForDAM paper:
#   borers      : % of host growing stock killed   = damage m3 / host growing stock m3
#   defoliators : % of host area defoliated        = damage ha / host forest area ha
#
# Host values come from the NUTS forest appendix (area ha and growing stock m3
# for spruce, pine, fir, oak, all conifers, all broadleaves). Which host goes
# with which insect or group:
#   level 1: gymnosperms -> all conifers, angiosperms -> all broadleaves
#   level 2: P. abies -> spruce, P. sylvestris -> pine, A. alba -> fir
#   level 3: the species' level-2 host, otherwise its host tree in the file
#            (oak -> oak; larch -> all conifers; other broadleaves -> all broadleaves)
#
# A region-year counts when the insect or group has a recorded value there
# (0 included) and the region has a host value > 0. Damage in region-years
# without a usable host value is left out of both sides and reported as
# "share of damage covered".
#
# Assumption: in the appendix the species growing stocks of BG and LT are
# about 1000x too small compared with their areas (e.g. BG spruce 31,157 m3 on
# 133,737 ha), while their conifer/broadleaf totals look right. They are read
# as 1000 m3 and multiplied by 1000 here (stock_x1000 = TRUE). Areas are used
# as given.

host_group_of_level <- c(
  `Borers on gymnosperms` = "conifers_all", `Defoliators on gymnosperms` = "conifers_all",
  `Borers on angiosperms` = "broadleaves_all", `Defoliators on angiosperms` = "broadleaves_all",
  `Borers on P. abies` = "spruce", `Defoliators on P. abies` = "spruce",
  `Borers on P. sylvestris` = "pine", `Defoliators on P. sylvestris` = "pine",
  `Borers on A. alba` = "fir")

host_stock_fix_countries <- c("BG", "LT")

# Host of each single insect, by what it actually feeds on. Specialists get
# their tree; insects feeding on several trees get the wider group, otherwise
# their damage is divided by the wrong forest (e.g. L. monacha in Polish pine
# forests divided by spruce area gave up to 4,000% of the spruce defoliated).
# The appendix has spruce, pine, fir, oak, all conifers and all broadleaves;
# all_forest = all conifers + all broadleaves. Insects not listed keep their
# paper level-2 host or the host tree in the data file.
species_host <- c(
  # generalists or other hosts than the ones in the appendix
  lym_mon = "conifers_all",       # nun moth: spruce and pine (mostly pine in PL, Brandenburg)
  mel_spp = "all_forest",         # cockchafer: larvae on roots in any young stand
  lym_dis = "broadleaves_all",    # gypsy moth: oak first, but many broadleaves
  geo_db = "broadleaves_all",     # winter moths and other geometrids: many broadleaves
  ope_spp = "broadleaves_all", era_def = "broadleaves_all",
  agr_vir = "broadleaves_all",    # Agrilus viridis: beech, oak, birch, willow
  xyl_spp = "broadleaves_all",    # ambrosia beetles on broadleaves
  xyl_lin = "conifers_all",       # Trypodendron lineatum: spruce, fir, pine
  uro_sir_spp = "conifers_all",   # woodwasps: all conifers
  zei_gri = "conifers_all",       # larch bud moth (larch has no column)
  # specialists (stated for clarity)
  ips_typ = "spruce", pit_cha = "spruce", ips_dup = "spruce", ips_ami = "spruce", den_mic = "spruce",
  pol_pol = "spruce", tet_spp = "spruce", pri_abi = "spruce", cep_spp = "spruce",
  ips_acu = "pine", ips_sex = "pine", pha_cya = "pine", tom_spp = "pine", pis_pin = "pine", pit_bid = "pine",
  bup_pin = "pine", den_pin = "pine", dip_sp = "pine", neo_ser = "pine", pan_fla = "pine", tha_pit = "pine",
  pit_spp = "fir",
  tor_vir = "oak", tha_pro = "oak", agr_big = "oak", sco_int = "oak")

# ---- One host table for every graph ------------------------------------------------
# Host tree of each code = the host tree in the data file (HOSTTREE), except
# for insects that feed on several trees, which get the wider group. The same
# label is used in the ranking (level 2), the host-share graphs and the checks.
# Larch, birch, elm and poplar are not in the forest appendix, so their damage
# is divided by all conifers / all broadleaves (forest_column_of).
host_tree_override <- c(
  lym_mon = "spruce and pine", mel_spp = "all forest",
  lym_dis = "broadleaves", geo_db = "broadleaves", ope_spp = "broadleaves", era_def = "broadleaves",
  agr_vir = "broadleaves", xyl_spp = "broadleaves", Def_dec = "broadleaves",
  xyl_lin = "conifers", uro_sir_spp = "conifers", zei_gri = "larch")

forest_column_of <- c(spruce = "spruce", pine = "pine", fir = "fir", oak = "oak",
                      larch = "conifers_all", `spruce and pine` = "conifers_all", conifers = "conifers_all",
                      birch = "broadleaves_all", elm = "broadleaves_all", `poplar / willow` = "broadleaves_all",
                      broadleaves = "broadleaves_all", `all forest` = "all_forest")

host_tree_of <- function(species, species_meta) {
  dplyr::coalesce(unname(host_tree_override[species]), host_tree_in_file(species, species_meta))
}

# Host tree as written in the data file (HOSTTREE), without the overrides
host_tree_in_file <- function(species, species_meta) {
  file_host <- species_meta$host_tree[match(species, species_meta$species)]
  dplyr::case_when(
    grepl("^Picea", file_host) ~ "spruce", grepl("^Pinus", file_host) ~ "pine",
    grepl("^Abies", file_host) ~ "fir", grepl("^Larix", file_host) ~ "larch",
    grepl("^Quercus", file_host) ~ "oak", grepl("^Betula", file_host) ~ "birch",
    grepl("^Ulmus", file_host) ~ "elm", grepl("^Populus", file_host) ~ "poplar / willow",
    TRUE ~ "broadleaves")
}

# Codes left out of a group's share because the group's host is not what they
# feed on (shown on their own at species level instead)
group_share_exclude <- list(`Defoliators on P. abies` = "lym_mon")

# Host area (ha) and growing stock (m3) per region, long format
host_stock_long <- function(appx, dash_means_absent = TRUE, stock_x1000 = host_stock_fix_countries) {
  appx |>
    dplyr::select(code, dplyr::matches("_(ha|m3)$")) |>
    tidyr::pivot_longer(-code, names_to = c("host_group", "measure"), names_pattern = "(.*)_(ha|m3)$",
                        values_to = "raw") |>
    dplyr::mutate(
      dash = trimws(raw) %in% c("-", "–"),
      value = suppressWarnings(as.numeric(raw)),
      value = dplyr::if_else(dash & dash_means_absent, 0, value),
      code = dplyr::recode(code, CR = "HR", SL = "SI", DEB = "DEB; DEC", DEC = "DEB; DEC"),
      stock_x1000 = measure == "m3" & code %in% stock_x1000 & !host_group %in% c("conifers_all", "broadleaves_all"),
      value = dplyr::if_else(stock_x1000, value * 1000, value)) |>
    dplyr::group_by(nuts_id = code, host_group, measure) |>
    dplyr::summarise(value = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE),
                     stock_x1000 = any(stock_x1000), .groups = "drop") |>
    (\(x) dplyr::bind_rows(x, x |>
      dplyr::filter(host_group %in% c("conifers_all", "broadleaves_all")) |>
      dplyr::group_by(nuts_id, measure) |>
      dplyr::summarise(value = sum(value), stock_x1000 = FALSE, .groups = "drop") |>
      dplyr::mutate(host_group = "all_forest")))()
}

# What is measured: one row per level-group or single insect, with its codes
# and its host
share_units <- function(ph_hier, species_meta) {
  grp <- function(level) {
    h <- dplyr::filter(ph_hier, !is.na(.data[[level]]))
    h |> dplyr::group_by(group = .data[[level]], unit) |>
      dplyr::summarise(codes = list(setdiff(species, group_share_exclude[[dplyr::cur_group()$group]])), .groups = "drop") |>
      dplyr::mutate(level = sub("_paper", "", level), host_group = unname(host_group_of_level[group]))
  }
  sp <- ph_hier |>
    dplyr::filter(code_type != "aggregate") |>
    dplyr::left_join(dplyr::select(species_meta, species, host_tree), by = "species") |>
    dplyr::transmute(level = "species", group = species, unit, codes = as.list(species),
                     host_group = unname(forest_column_of[host_tree_of(species, species_meta)]))
  dplyr::bind_rows(grp("level1"), grp("level2_paper"), sp)
}

# Yearly share for one unit. d: country, nuts_id, year, species, damage
share_one <- function(d, codes, host_group, unit, hs) {
  hv <- hs |> dplyr::filter(host_group == !!host_group, measure == unit) |> dplyr::select(nuts_id, host = value)
  ry <- d |>
    dplyr::filter(species %in% codes, !is.na(damage)) |>
    dplyr::group_by(country, nuts_id, year) |>
    dplyr::summarise(damage = sum(damage), .groups = "drop") |>
    dplyr::left_join(hv, by = "nuts_id")
  ok <- !is.na(ry$host) & ry$host > 0
  list(region_year = ry[ok, ],
       damage_covered = if (sum(ry$damage) > 0) sum(ry$damage[ok]) / sum(ry$damage) else NA_real_)
}

# Main table: average share of the host affected per year (2000-2022) for
# every group and insect, two ways:
#   share_pct_paper          = damage per year / host of ALL regions in the data
#                              (the paper's way; reproduces its Table 2). Low
#                              for insects monitored in few countries, because
#                              host where nobody looks still counts.
#   share_pct_where_recorded = damage / host, only region-years where the insect
#                              or group has a recorded value (0 included):
#                              the share of the MONITORED host affected.
host_share_table <- function(d, units, hs, regions, n_years = 23) {
  purrr::pmap_dfr(units, function(level, group, unit, codes, host_group) {
    r <- share_one(d, codes, host_group, unit, hs)
    ry <- r$region_year
    host_all <- hs |> dplyr::filter(host_group == !!host_group, measure == unit, nuts_id %in% regions$nuts_id) |>
      dplyr::pull(value) |> sum(na.rm = TRUE)
    by_year <- ry |> dplyr::group_by(year) |> dplyr::summarise(damage = sum(damage), .groups = "drop")
    tibble::tibble(level, group, unit, host_group,
                   n_regions_recorded = dplyr::n_distinct(ry$nuts_id), n_region_years = nrow(ry),
                   damage_total = sum(ry$damage),
                   share_pct_paper = 100 * sum(ry$damage) / n_years / host_all,
                   share_pct_where_recorded = 100 * sum(ry$damage) / sum(ry$host),
                   worst_year = by_year$year[which.max(by_year$damage)],
                   share_pct_paper_worst_year = 100 * max(by_year$damage) / host_all,
                   damage_with_host_value = r$damage_covered)
  })
}

# Europe-wide share per year (both ways), for the figures
host_share_by_year <- function(d, units, hs, regions) {
  purrr::pmap_dfr(units, function(level, group, unit, codes, host_group) {
    host_all <- hs |> dplyr::filter(host_group == !!host_group, measure == unit, nuts_id %in% regions$nuts_id) |>
      dplyr::pull(value) |> sum(na.rm = TRUE)
    share_one(d, codes, host_group, unit, hs)$region_year |>
      dplyr::group_by(year) |>
      dplyr::summarise(damage = sum(damage), host_recorded = sum(host), n_regions = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(level, group, unit, host_group, host_all = host_all,
                    share_pct_paper = 100 * damage / host_all,
                    share_pct_where_recorded = 100 * damage / host_recorded, .before = 1)
  })
}

# Per region: average yearly share (for maps and drivers later)
host_share_by_region <- function(d, units, hs) {
  purrr::pmap_dfr(units, function(level, group, unit, codes, host_group) {
    share_one(d, codes, host_group, unit, hs)$region_year |>
      dplyr::group_by(country, nuts_id) |>
      dplyr::summarise(n_years = dplyr::n(), damage = sum(damage), host = dplyr::first(host), .groups = "drop") |>
      dplyr::mutate(level, group, unit, host_group, share_pct = 100 * damage / (host * n_years), .before = 1)
  })
}

# Reported + tested filled values, laid out like the damage table
filled_as_damage <- function(gd) {
  gd |>
    dplyr::filter(value_type %in% c("reported damage", "reported zero") |
                    (value_type == "filled" & confidence %in% c("reliable", "weak"))) |>
    dplyr::select(country, nuts_id, year, species, damage)
}

paper_table2_share <- tibble::tribble(
  ~level, ~group, ~paper_share_pct,
  "level1", "Borers on gymnosperms", 0.27, "level1", "Borers on angiosperms", 0.01,
  "level1", "Defoliators on gymnosperms", 0.46, "level1", "Defoliators on angiosperms", 0.95,
  "level2", "Borers on A. alba", 0.07, "level2", "Borers on P. abies", 0.53,
  "level2", "Borers on P. sylvestris", 0.03, "level2", "Defoliators on P. abies", 0.52,
  "level2", "Defoliators on P. sylvestris", 0.46,
  "species", "ips_typ", 0.49, "species", "pha_cya", 0.02, "species", "tom_spp", 0.01,
  "species", "ips_acu", 0.01, "species", "pit_cha", 0.02, "species", "tor_vir", 0.46,
  "species", "geo_db", 0.53, "species", "lym_dis", 1.70, "species", "lym_mon", 0.43)


# Sanity check: region-years where the damage is more than a given share of
# the host in that region (impossible above 100%; very unusual above 50%)
host_share_check <- function(d, units, hs, over = c(50, 100)) {
  purrr::pmap_dfr(dplyr::filter(units, level == "species"), function(level, group, unit, codes, host_group) {
    share_one(d, codes, host_group, unit, hs)$region_year |>
      dplyr::mutate(species = group, host_group, unit, pct = 100 * damage / host) |>
      dplyr::filter(pct > min(over))
  }) |>
    dplyr::mutate(over_100 = pct > 100) |>
    dplyr::arrange(dplyr::desc(pct))
}
