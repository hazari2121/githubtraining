# Hierarchy of the 2025 EU-ForDAM paper (Table 1), used for the ranking, the
# correlation metrics and the gap-filling.
#
#   level 1: feeding guild x host type (4 groups)
#            Borers / Defoliators on gymnosperms / angiosperms
#   level 2: gymnosperm host genus (5 groups)
#            Borers on P. abies / P. sylvestris / A. alba,
#            Defoliators on P. abies / P. sylvestris
#            (larch insects and all angiosperm insects have no level-2 group)
#   level 3: single insects (species, genus, or the species group geo_db)
#
# Aggregate reporting codes (e.g. CambXyl_con&BO = "borers on pine") lump
# several insects together. As in the paper they count only at levels 1-2:
# they are added into the group totals, never ranked or correlated as if they
# were one insect, and never gap-filled. The spruce bark-borer group
# (CambXyl_con&BB_con&SM, reported by CZ and DE) is dominated by I. typographus
# and is counted as ips_typ in both countries; this reproduces the paper's
# I. typographus total (Table 2) to within 0.01%.

paper_level1_order <- c("Borers on gymnosperms", "Borers on angiosperms",
                        "Defoliators on gymnosperms", "Defoliators on angiosperms")
paper_level2_order <- c("Borers on P. abies", "Borers on P. sylvestris", "Borers on A. alba",
                        "Defoliators on P. abies", "Defoliators on P. sylvestris")

# Aggregate codes recoded to one species (paper rule: dominated by one species)
group_to_species_paper <- tibble::tribble(
  ~country, ~from,                   ~to,       ~reason,
  "CZ",     "CambXyl_con&BB_con&SM", "ips_typ", "Spruce bark-borer group, dominated by I. typographus (paper rule)",
  "DE",     "CambXyl_con&BB_con&SM", "ips_typ", "Spruce bark-borer group, dominated by I. typographus (paper rule; matches the paper's Table 2 total)"
)

read_paper_hierarchy <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

# One row per code: level1, level2_paper (NA where the paper has no level-2
# group) and level2 (level2_paper, or level1 where missing; used only to class
# pairs as "same host-tree group" in the permutation test, as before).
paper_hier_table <- function(ph, species_meta) {
  ph |>
    dplyr::transmute(species = code, name, code_type,
                     level1 = paper_level1,
                     level2_paper = dplyr::na_if(paper_level2, ""),
                     level2 = dplyr::coalesce(level2_paper, level1),
                     in_paper_table1 = in_paper_table1 == "yes") |>
    dplyr::left_join(dplyr::select(species_meta, species, unit), by = "species") |>
    (\(h) dplyr::mutate(h, level2_all = host_tree_group(h, species_meta)))()
}

# Level 2 extended to every host tree (the paper has level 2 for spruce, pine
# and fir only): guild x host tree from the data file's HOSTTREE column, with
# the paper's names for the gymnosperm groups. Def_dec is a generic
# "defoliators on angiosperms" code, so it gets an unspecified-host group.
host_tree_group <- function(ph_hier, species_meta) {
  # same host table as the host-share graphs (host_tree_of in R/host_share.R)
  paste(sub(" on .*", "", ph_hier$level1), "on", host_tree_of(ph_hier$species, species_meta))
}

# Codes that are single insects (everything except aggregate reporting codes)
paper_species_codes <- function(ph_hier) {
  ph_hier$species[ph_hier$code_type != "aggregate"]
}

# ---- Ranking -------------------------------------------------------------------

# Species level: single insects only; units = NUTS units with damage > 0 in at
# least one year
paper_species_ranking <- function(dmg, ph_hier) {
  dmg |>
    dplyr::filter(!is.na(damage), species %in% paper_species_codes(ph_hier)) |>
    dplyr::group_by(species, country, nuts_id) |>
    dplyr::summarise(occurs = any(damage > 0), total = sum(damage), .groups = "drop") |>
    dplyr::group_by(species) |>
    dplyr::summarise(n_units_with_damage = sum(occurs), n_units_reported = dplyr::n(),
                     n_countries_with_damage = dplyr::n_distinct(country[occurs]),
                     cumulative_damage = sum(total), .groups = "drop") |>
    dplyr::left_join(dplyr::select(ph_hier, species, name, code_type, level1, level2_paper, unit,
                                   in_paper_table1), by = "species") |>
    dplyr::arrange(dplyr::desc(n_units_with_damage), species) |>
    dplyr::mutate(rank = rank(-n_units_with_damage, ties.method = "min"), .before = 1)
}

# Group level (paper levels 1 and 2): a unit has damage from a group when any
# member code, aggregates included, reports damage > 0 in any year
paper_group_ranking <- function(dmg, ph_hier) {
  one <- function(level) {
    h <- dplyr::filter(ph_hier, !is.na(.data[[level]]))
    d <- dmg |> dplyr::filter(!is.na(damage)) |>
      dplyr::inner_join(dplyr::transmute(h, species, group = .data[[level]], code_type, level1), by = "species")
    d |>
      dplyr::group_by(group, unit, level1) |>
      dplyr::summarise(
        n_units_with_damage = dplyr::n_distinct(paste(country, nuts_id)[damage > 0]),
        n_units_reported = dplyr::n_distinct(paste(country, nuts_id)),
        n_countries_with_damage = dplyr::n_distinct(country[damage > 0]),
        n_codes = dplyr::n_distinct(species),
        cumulative_damage = sum(damage),
        from_aggregate_codes = sum(damage[code_type == "aggregate"]),
        .groups = "drop") |>
      dplyr::mutate(level = sub("_paper", "", level), .before = 1)
  }
  dplyr::bind_rows(one("level1"), one("level2_paper"), one("level2_all")) |>
    dplyr::mutate(share_from_aggregates = from_aggregate_codes / cumulative_damage)
}

# Aggregate reporting codes: who reports them and where they are counted
paper_aggregate_table <- function(dmg, ph_hier) {
  dmg |>
    dplyr::filter(!is.na(damage)) |>
    dplyr::inner_join(dplyr::filter(ph_hier, code_type == "aggregate") |>
                        dplyr::select(species, name, level1, level2_paper), by = "species") |>
    dplyr::group_by(species, name, level1, level2_paper, unit) |>
    dplyr::summarise(countries = paste(sort(unique(country)), collapse = ", "),
                     n_units_with_damage = dplyr::n_distinct(paste(country, nuts_id)[damage > 0]),
                     cumulative_damage = sum(damage), .groups = "drop") |>
    dplyr::mutate(counted_in = dplyr::if_else(is.na(level2_paper), "level 1 only", "levels 1 and 2"))
}

# ---- Check against Table 2 of the paper ----------------------------------------

paper_table2 <- tibble::tribble(
  ~level, ~group, ~paper_total,
  "level1", "Borers on gymnosperms", 673169401,
  "level1", "Borers on angiosperms", 10869383,
  "level1", "Defoliators on gymnosperms", 5566820,
  "level1", "Defoliators on angiosperms", 6393628,
  "level2", "Borers on A. alba", 7845375,
  "level2", "Borers on P. abies", 631243712,
  "level2", "Borers on P. sylvestris", 30553861,
  "level2", "Defoliators on P. abies", 2916474,
  "level2", "Defoliators on P. sylvestris", 2650346,
  "species", "ips_typ", 586715446,
  "species", "pha_cya", 14865369,
  "species", "tom_spp", 5432690,
  "species", "ips_acu", 6845704,
  "species", "pit_cha", 28145602,
  "species", "tor_vir", 641455,
  "species", "geo_db", 746724,
  "species", "lym_dis", 2390823,
  "species", "lym_mon", 2402944
)

# Our cumulative reported damage 2000-2022 next to the paper's. Of the three
# codes not listed in Table 1, sap_car and xyl_spp are inside the paper's
# Borers on angiosperms total (exact match), while xyl_lin (DE only) is not:
# leaving it out reproduces Borers on P. abies to 0.01%.
paper_table2_check <- function(dmg, ph_hier) {
  d <- dplyr::filter(dmg, !is.na(damage)) |>
    dplyr::left_join(dplyr::select(ph_hier, species, level1, level2 = level2_paper, in_paper_table1), by = "species")
  tot <- function(x) tibble::tibble(ours = sum(x$damage), ours_without_xyl_lin = sum(x$damage[x$species != "xyl_lin"]))
  paper_table2 |>
    dplyr::mutate(res = purrr::map2(level, group, function(lv, g) {
      key <- if (lv == "species") d$species else d[[lv]]
      tot(d[!is.na(key) & key == g, ])
    })) |>
    tidyr::unnest(res) |>
    dplyr::mutate(ours_vs_paper_pct = round(100 * (ours / paper_total - 1), 2),
                  without_xyl_lin_vs_paper_pct = round(100 * (ours_without_xyl_lin / paper_total - 1), 2))
}

# ---- Group-level correlations (levels 1 and 2, aggregates included) ------------

# Same metrics as the species level, on group series (sum of all recorded
# member codes in a region-year, aggregates included)
paper_group_correlations <- function(cells, ph_hier, level) {
  h <- dplyr::filter(ph_hier, !is.na(.data[[level]]))
  level_correlations(dplyr::filter(cells, species %in% h$species), h, level) |>
    dplyr::mutate(level = sub("_paper", "", level))
}

# Paper-style check (as in its Tables 4-6): Pearson r of the Europe-wide yearly
# totals, log(1 + x)
paper_group_europe_correlations <- function(cells, ph_hier, level) {
  h <- dplyr::filter(ph_hier, !is.na(.data[[level]]))
  hier_europe_correlations(dplyr::filter(cells, species %in% h$species), h, level) |>
    dplyr::mutate(level = sub("_paper", "", level))
}

# ---- Gap-filling: group totals with ranges ---------------------------------------

# Europe-wide yearly totals per paper group: reported values of every code
# (aggregates included) + the tested filled values of the member insects.
# The 80% range resamples each insect's test errors exactly as in
# gapfill_totals_v2(), then adds the draws up within the group.
gapfill_group_totals <- function(gd, bt, ph_hier, n_draw = 2000, min_n = 30, seed = 20261019, count = c("reliable", "weak")) {
  set.seed(seed)
  ct <- gd |>
    dplyr::group_by(country, species, unit, year) |>
    dplyr::summarise(
      reported = sum(damage[value_type %in% c("reported damage", "reported zero")]),
      filled_tested = sum(damage[value_type == "filled" & confidence %in% count]),
      filled_weak_not_counted = sum(damage[value_type == "filled" & confidence == "weak" & !"weak" %in% count]),
      .groups = "drop")
  pool_all <- bt$lr
  pools <- split(bt$lr, bt$species)
  pool_of <- function(s) if (!is.null(pools[[s]]) && length(pools[[s]]) >= min_n) pools[[s]] else pool_all
  draws_of <- function(f, s) {
    f <- f[f > 0]
    if (length(f) == 0) return(rep(0, n_draw))
    e <- matrix(sample(pool_of(s), n_draw * length(f), replace = TRUE), n_draw)
    rowSums(pmax(exp(e) * matrix(f + 1, n_draw, length(f), byrow = TRUE) - 1, 0))
  }
  one <- function(level) {
    h <- dplyr::filter(ph_hier, !is.na(.data[[level]]))
    ct |>
      dplyr::inner_join(dplyr::select(h, species, group = dplyr::all_of(level)), by = "species") |>
      dplyr::group_by(group, unit, year) |>
      dplyr::group_modify(function(d, key) {
        draws <- rep(0, n_draw)
        for (s in unique(d$species)) draws <- draws + draws_of(d$filled_tested[d$species == s], s)
        r <- sum(d$reported)
        tibble::tibble(reported = r, filled = sum(d$filled_tested),
                       filled_weak_not_counted = sum(d$filled_weak_not_counted),
                       total_lo80 = r + stats::quantile(draws, 0.1, names = FALSE),
                       total_mid = r + stats::quantile(draws, 0.5, names = FALSE),
                       total_hi80 = r + stats::quantile(draws, 0.9, names = FALSE))
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate(level = sub("_paper", "", level), .before = 1)
  }
  dplyr::bind_rows(one("level1"), one("level2_paper"))
}
