# Levels of aggregation, used by relative damage, pairs at all levels and
# predictability. One table says, for every insect code, which group it falls
# into at each level and which forest-appendix column is its host:
#
#   1 species     single insects only (genus codes and aggregates excluded)
#   2 host tree   guild x host tree (e.g. borers on pine, defoliators on oak);
#                 every code, aggregates included, host from the shared host table
#   3 host type   guild x gymnosperms / angiosperms (the paper's level 1)
#   4 guild       all borers, all defoliators
#
# Host columns: spruce, pine, fir, oak, conifers_all, broadleaves_all,
# all_forest. Larch, birch, elm and poplar have no column of their own; they
# use all conifers / all broadleaves and are flagged (host_is_proxy).

level_names <- c(`1_species` = "1. Species", `2_host_tree` = "2. Host tree", `3_host_type` = "3. Host type (gymnosperms / angiosperms)",
                 `4_guild` = "4. Guild (all borers / all defoliators)")

levels_map <- function(ph_hier, species_meta, species_keep) {
  base <- ph_hier |>
    dplyr::transmute(code = species, code_type, unit, guild = sub(" on .*", "", level1), level1,
                     host_tree = host_tree_of(species, species_meta),
                     host_col = unname(forest_column_of[host_tree]))
  host_type_col <- function(g) dplyr::if_else(grepl("gymnosperms$", g), "conifers_all", "broadleaves_all")
  dplyr::bind_rows(
    base |> dplyr::filter(code %in% species_keep) |>
      dplyr::transmute(level = "1_species", group = code, code, guild, unit, host_tree, host_col,
                       host_is_proxy = host_tree %in% proxy_hosts),
    base |> dplyr::transmute(level = "2_host_tree", group = paste(guild, "on", host_tree), code, guild, unit, host_tree, host_col,
                             host_is_proxy = host_tree %in% proxy_hosts),
    base |> dplyr::transmute(level = "3_host_type", group = level1, code, guild, unit, host_tree = sub("^.* on ", "", level1),
                             host_col = host_type_col(level1), host_is_proxy = FALSE),
    base |> dplyr::transmute(level = "4_guild", group = paste("All", tolower(guild)), code, guild, unit, host_tree = "all forest",
                             host_col = "all_forest", host_is_proxy = FALSE))
}

# Region-year cells of every group at one level (sum of the recorded member
# codes; a group is recorded in a region-year when any member code is)
level_cells <- function(cells, lm, level) {
  h <- dplyr::filter(lm, level == !!level) |> dplyr::distinct(species = code, group)
  aggregate_cells(dplyr::filter(cells, species %in% h$species), h, "group")
}
