# Gap-filled dataset v4: v3 (model M2 + draft range rule) rebuilt on the
# shared host table used by the host-share, totals and correlation work.
#
# What changes from v3 is only the host information each insect gets:
#   - host tree from host_tree_of() (R/host_share.R): e.g. L. monacha "spruce
#     and pine", Melolontha "all forest", Zeiraphera "larch", A. viridis and
#     L. dispar "broadleaves", X. lineatus "conifers";
#   - host area from the matching forest-appendix column (forest_column_of):
#     larch -> all conifers, birch / elm / poplar -> all broadleaves.
# Rule for "host absent" (filled as 0): the region's area of that forest column
# is 0 in the appendix AND no damage on that host was ever recorded there.
# Recorded damage counts as evidence for the forest column of the insect's host
# tree (both the shared label and the data file's HOSTTREE), and for the wider
# column it sits in: spruce / pine / fir -> all conifers, oak -> all broadleaves
# (e.g. spruce bark-beetle damage in Vienna shows conifers are there although
# the appendix gives 0). In v3 only spruce, pine, oak and fir could be absent; now any column
# can (a region without conifers cannot have larch or conifer insects), and
# "all forest" never is.

host_status_v4 <- function(cells, species_meta, host_long) {
  hl <- dplyr::bind_rows(host_long,
                         host_long |> dplyr::filter(host_group %in% c("conifers_all", "broadleaves_all")) |>
                           dplyr::group_by(nuts_id) |>
                           dplyr::summarise(area_ha = if (all(is.na(area_ha))) NA_real_ else sum(area_ha, na.rm = TRUE), .groups = "drop") |>
                           dplyr::mutate(host_group = "all_forest"))
  sp <- dplyr::distinct(cells, species) |>
    dplyr::mutate(host_tree = host_tree_of(species, species_meta), host_group = unname(forest_column_of[host_tree]))
  parent <- c(spruce = "conifers_all", pine = "conifers_all", fir = "conifers_all", oak = "broadleaves_all")
  ev_cols <- dplyr::bind_rows(
    dplyr::transmute(sp, species, col = host_group),
    dplyr::transmute(sp, species, col = unname(forest_column_of[host_tree_in_file(species, species_meta)])))
  ev_cols <- dplyr::bind_rows(ev_cols, dplyr::transmute(ev_cols, species, col = unname(parent[col]))) |>
    dplyr::filter(!is.na(col)) |> dplyr::distinct()
  evidence <- cells |>
    dplyr::filter(status == "observed_positive") |>
    dplyr::distinct(nuts_id, species) |>
    dplyr::inner_join(ev_cols, by = "species", relationship = "many-to-many") |>
    dplyr::distinct(nuts_id, host_group = col) |>
    dplyr::mutate(damage_recorded_on_host = TRUE)
  tidyr::crossing(dplyr::distinct(cells, country, nuts_id), sp) |>
    dplyr::left_join(dplyr::select(hl, nuts_id, host_group, area_ha), by = c("nuts_id", "host_group")) |>
    dplyr::left_join(evidence, by = c("nuts_id", "host_group")) |>
    dplyr::mutate(
      damage_recorded_on_host = dplyr::coalesce(damage_recorded_on_host, FALSE),
      host_status = dplyr::case_when(
        !nuts_id %in% host_long$nuts_id ~ "no host data",
        damage_recorded_on_host ~ "present",
        is.na(area_ha) ~ "no host data",
        area_ha > 0 ~ "present",
        TRUE ~ "absent"),
      host_area_ha = area_ha) |>
    dplyr::select(country, nuts_id, species, host_tree, host_group, host_status, host_area_ha, damage_recorded_on_host)
}

# Which insect x region host answers changed between v3 and v4
host_status_change <- function(old, new) {
  dplyr::inner_join(dplyr::select(old, nuts_id, species, v3_host = host_group, v3_status = host_status),
                    dplyr::select(new, nuts_id, species, v4_host = host_tree, v4_column = host_group, v4_status = host_status),
                    by = c("nuts_id", "species")) |>
    dplyr::filter(v3_status != v4_status) |>
    dplyr::count(species, v3_host, v4_host, v3_status, v4_status, name = "regions")
}

# Check after filling: damage (reported + filled) never above the host.
# Borers: m3 / growing stock (m3); defoliators: ha / forest area (ha) of the host column.
gapfill_host_check <- function(gd, hs4, stock, species_keep) {
  st <- dplyr::filter(stock, measure %in% c("ha", "m3")) |> dplyr::select(nuts_id, host_group, measure, host = value)
  gd |>
    dplyr::filter(species %in% species_keep, value_type %in% c("reported damage", "filled"), !is.na(damage)) |>
    dplyr::left_join(dplyr::select(hs4, nuts_id, species, host_group), by = c("nuts_id", "species")) |>
    dplyr::left_join(st, by = c("nuts_id", "host_group", unit = "measure")) |>
    dplyr::filter(!is.na(host), host > 0) |>
    dplyr::mutate(share = damage / host) |>
    dplyr::group_by(value_type) |>
    dplyr::summarise(cells = dplyr::n(), max_share = max(share), above_10pct = sum(share > 0.10),
                     above_100pct = sum(share > 1), .groups = "drop")
}

# Filled totals per insect, v3 vs v4 (single insects)
gapfill_v3_v4 <- function(t3, t4) {
  f <- function(t) t$europe |> dplyr::group_by(species) |> dplyr::summarise(reported = sum(reported), filled = sum(filled), .groups = "drop")
  dplyr::full_join(f(t3), f(t4), by = "species", suffix = c("_v3", "_v4")) |>
    dplyr::mutate(filled_vs_reported_v3 = filled_v3 / reported_v3, filled_vs_reported_v4 = filled_v4 / reported_v4,
                  change_in_filled = filled_v4 / filled_v3)
}
