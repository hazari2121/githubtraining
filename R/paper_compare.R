# Comparison with the 2025 EU-ForDAM paper, table by table
#
# Every number in the paper that can be recomputed from the database is
# recomputed here and put next to the paper's value, to catch bugs on either
# side. The paper's own way of preparing the data, worked out from these checks:
#   - a "time series" is one region x insect code x sub-row (Czech and German
#     sheets have several rows per region; they are separate series);
#   - series with fewer than 6 non-zero years are removed before everything
#     (Methods 2.5), also before the totals of Table 2;
#   - the CZ and DE spruce bark-borer group is counted as I. typographus;
#   - Trypodendron (Xyloterus) lineatum is not among the conifer borers
#     (Table 1 does not list it);
#   - Tables 4-6 are squared Pearson correlations (r^2) of Europe-wide yearly
#     totals on the original scale (not log), although they are called
#     "Pearson correlations".

# ---- The paper's values ------------------------------------------------------------

paper_tab1 <- tibble::tribble(
  ~level, ~group, ~paper_series, ~paper_series_kept, ~paper_countries,
  "level1", "Borers on gymnosperms", 616, 328, "AT BG CH CZ DE FI HR HU IT LT PL RO RS SE SI SK",
  "level1", "Borers on angiosperms", 167, 73, "CZ DE LT PL",
  "level1", "Defoliators on gymnosperms", 320, 125, "AT BG CZ DE HR LT PL",
  "level1", "Defoliators on angiosperms", 247, 118, "AT BG CZ DE HR HU LT PL RO RS SK",
  "level2_paper", "Borers on A. alba", 40, 21, "AT CZ DE HR SI",
  "level2_paper", "Borers on P. abies", 232, 130, "AT BG CH CZ DE FI HR HU IT LT PL RO RS SE SI SK",
  "level2_paper", "Defoliators on P. abies", 119, 53, "CZ DE LT PL",
  "level2_paper", "Borers on P. sylvestris", 263, 144, "AT BG CZ DE HR LT PL SI SK",
  "level2_paper", "Defoliators on P. sylvestris", 201, 72, "AT BG CZ DE LT PL",
  "species", "ips_typ", 98, 74, "AT BG CH CZ DE FI HR HU IT LT PL RO RS SE SI SK",
  "species", "pha_cya", 56, 42, "AT CZ DE LT PL",
  "species", "tom_spp", 56, 42, "AT CZ DE LT PL",
  "species", "ips_acu", 49, 26, "BG CZ DE LT PL SK",
  "species", "pit_cha", 28, 23, "AT DE IT LT SI SK",
  "species", "tor_vir", 35, 25, "BG DE LT PL RO",
  "species", "geo_db", 36, 22, "BG DE HR LT PL RO",
  "species", "lym_dis", 45, 17, "AT BG CZ DE HR HU LT RO RS SK",
  "species", "lym_mon", 32, 26, "CZ DE LT PL")

paper_tab2_total <- tibble::tribble(
  ~level, ~group, ~paper_total,
  "level1", "Borers on gymnosperms", 673169401, "level1", "Borers on angiosperms", 10869383,
  "level1", "Defoliators on gymnosperms", 5566820, "level1", "Defoliators on angiosperms", 6393628,
  "level2_paper", "Borers on A. alba", 7845375, "level2_paper", "Borers on P. abies", 631243712,
  "level2_paper", "Borers on P. sylvestris", 30553861, "level2_paper", "Defoliators on P. abies", 2916474,
  "level2_paper", "Defoliators on P. sylvestris", 2650346,
  "species", "ips_typ", 586715446, "species", "pha_cya", 14865369, "species", "tom_spp", 5432690,
  "species", "ips_acu", 6845704, "species", "pit_cha", 28145602, "species", "tor_vir", 641455,
  "species", "geo_db", 746724, "species", "lym_dis", 2390823, "species", "lym_mon", 2402944)

paper_tab45 <- tibble::tribble(
  ~table, ~a, ~b, ~paper,
  "Table 4", "Borers on gymnosperms", "Borers on angiosperms", 0.21,
  "Table 4", "Borers on gymnosperms", "Defoliators on gymnosperms", 0.00,
  "Table 4", "Borers on gymnosperms", "Defoliators on angiosperms", 0.00,
  "Table 4", "Borers on angiosperms", "Defoliators on gymnosperms", 0.02,
  "Table 4", "Borers on angiosperms", "Defoliators on angiosperms", 0.04,
  "Table 4", "Defoliators on gymnosperms", "Defoliators on angiosperms", 0.20,
  "Table 5", "Borers on A. alba", "Borers on P. abies", 0.63,
  "Table 5", "Borers on A. alba", "Borers on P. sylvestris", 0.73,
  "Table 5", "Borers on P. abies", "Borers on P. sylvestris", 0.88,
  "Table 5", "Borers on A. alba", "Defoliators on P. abies", 0.04,
  "Table 5", "Borers on P. abies", "Defoliators on P. abies", 0.00,
  "Table 5", "Borers on P. sylvestris", "Defoliators on P. abies", 0.00,
  "Table 5", "Borers on A. alba", "Defoliators on P. sylvestris", 0.03,
  "Table 5", "Borers on P. abies", "Defoliators on P. sylvestris", 0.04,
  "Table 5", "Borers on P. sylvestris", "Defoliators on P. sylvestris", 0.03,
  "Table 5", "Defoliators on P. abies", "Defoliators on P. sylvestris", 0.02)

# Numbers given in the Results text (section 3.1-3.2)
paper_text <- tibble::tribble(
  ~item, ~paper,
  "Borers, total 2000-2022 (million m3)", 684,
  "Borer damage on gymnosperms (%)", 98.4,
  "Borer damage on P. abies (%)", 92.3,
  "I. typographus, share of borer damage (%)", 85.8,
  "P. chalcographus, share of borer damage (%)", 4.1,
  "I. duplicatus, share of borer damage (%)", 2.2,
  "P. cyanea, share of borer damage (%)", 2.2,
  "Agrilus spp., share of borer damage (%)", 1.3,
  "Defoliators, total 2000-2022 (million ha)", 11.9,
  "Defoliation on angiosperms (%)", 53.5,
  "L. monacha, share of defoliation (%)", 20.1,
  "L. dispar, share of defoliation (%)", 20.0,
  "Melolontha spp., share of defoliation (%)", 13.5,
  "Geometridae on oaks, share of defoliation (%)", 6.2)

# ---- The paper's data preparation -----------------------------------------------------

# One row per series (region x code x sub-row) with its number of non-zero years
paper_series <- function(long, recode = group_to_species_paper) {
  long |>
    dplyr::left_join(dplyr::select(recode, country, species = from, to), by = c("country", "species")) |>
    dplyr::mutate(species = dplyr::coalesce(to, species)) |>
    dplyr::select(-to) |>
    dplyr::filter(!is.na(damage))
}

paper_series_table <- function(ps) {
  ps |> dplyr::group_by(country, nuts_id, species, sub_id) |>
    dplyr::summarise(nz = sum(damage > 0), .groups = "drop")
}

# Damage after the paper's filter (series with >= 6 non-zero years), summed to region-years
paper_filtered <- function(ps, st, min_nonzero = 6) {
  keep <- dplyr::filter(st, nz >= min_nonzero)
  ps |> dplyr::semi_join(keep, by = c("country", "nuts_id", "species", "sub_id")) |>
    dplyr::group_by(country, nuts_id, year, species) |>
    dplyr::summarise(damage = sum(damage), .groups = "drop")
}

codes_of <- function(ph_hier, level, group) if (level == "species") group else ph_hier$species[ph_hier[[level]] %in% group]

# ---- Table 1: number of series and countries -----------------------------------------------

cmp_table1 <- function(st, ph_hier) {
  purrr::pmap_dfr(paper_tab1, function(level, group, paper_series, paper_series_kept, paper_countries) {
    s <- dplyr::filter(st, species %in% codes_of(ph_hier, level, group))
    ours_c <- paste(sort(unique(s$country[s$nz > 0])), collapse = " ")
    tibble::tibble(level = sub("_paper", "", level), group, paper_series, ours_series = nrow(s),
                   paper_series_kept, ours_series_kept = sum(s$nz >= 6),
                   countries_same = ours_c == paper_countries,
                   countries_only_ours = paste(setdiff(strsplit(ours_c, " ")[[1]], strsplit(paper_countries, " ")[[1]]), collapse = " "),
                   countries_only_paper = paste(setdiff(strsplit(paper_countries, " ")[[1]], strsplit(ours_c, " ")[[1]]), collapse = " "))
  })
}

# ---- Table 2: totals and share of host -----------------------------------------------------

cmp_table2_totals <- function(ps, pf, ph_hier) {
  all <- ps |> dplyr::group_by(country, nuts_id, year, species) |> dplyr::summarise(damage = sum(damage), .groups = "drop")
  purrr::pmap_dfr(paper_tab2_total, function(level, group, paper_total) {
    cd <- codes_of(ph_hier, level, group)
    a <- sum(all$damage[all$species %in% cd]); f <- sum(pf$damage[pf$species %in% cd])
    f2 <- sum(pf$damage[pf$species %in% setdiff(cd, "xyl_lin")])
    tibble::tibble(level = sub("_paper", "", level), group, paper_total,
                   ours_all_data = a, ours_paper_filter = f, ours_paper_filter_no_xyl_lin = f2,
                   diff_all_pct = 100 * (a / paper_total - 1), diff_filter_pct = 100 * (f / paper_total - 1),
                   diff_filter_no_xyl_lin_pct = 100 * (f2 / paper_total - 1))
  })
}

# Share of host per year, the paper's way: damage / host of all regions in the
# data, per year. Hosts as the paper assigns them (main host genus in the data
# file, L. monacha on spruce). Shown dividing by 23 years (2000-2022) and by 22.
cmp_table2_share <- function(pf, ph_hier, species_meta, hs, regions) {
  pf <- dplyr::filter(pf, species != "xyl_lin")
  units <- share_units(ph_hier, species_meta)
  sp <- units$level == "species"
  units$host_group[sp] <- unname(forest_column_of[host_tree_in_file(units$group[sp], species_meta)])
  for (g in names(group_share_exclude)) units$codes[units$level != "species" & units$group == g] <- list(codes_of(ph_hier, "level2_paper", g))
  host_of <- function(hg, unit) hs |> dplyr::filter(host_group == hg, measure == unit, nuts_id %in% regions$nuts_id) |>
    dplyr::pull(value) |> sum(na.rm = TRUE)
  purrr::pmap_dfr(units, function(level, group, unit, codes, host_group) {
    tibble::tibble(level, group, host_group, damage = sum(pf$damage[pf$species %in% codes]), host = host_of(host_group, unit))
  }) |>
    dplyr::inner_join(paper_table2_share, by = c("level", "group")) |>
    dplyr::transmute(level, group, host_group, paper_share_pct,
                     ours_23_years = 100 * damage / 23 / host, ours_22_years = 100 * damage / 22 / host,
                     match_23 = round(ours_23_years, 2) == paper_share_pct, match_22 = round(ours_22_years, 2) == paper_share_pct)
}

# ---- Tables 4-5: group correlations ----------------------------------------------------------

cmp_table45 <- function(dmg, ph_hier) {
  tot <- function(level) {
    h <- dplyr::filter(ph_hier, !is.na(.data[[level]]))
    dmg |> dplyr::inner_join(dplyr::select(h, species, g = dplyr::all_of(level)), by = "species") |>
      dplyr::group_by(g, year) |> dplyr::summarise(t = sum(damage, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = g, values_from = t)
  }
  w <- dplyr::bind_cols(tot("level1"), dplyr::select(tot("level2_paper"), -year))
  paper_tab45 |> dplyr::mutate(
    r = purrr::map2_dbl(a, b, ~ stats::cor(w[[.x]], w[[.y]])),
    r_squared = r^2,
    p = purrr::map2_dbl(a, b, ~ stats::cor.test(w[[.x]], w[[.y]])$p.value),
    r_log = purrr::map2_dbl(a, b, ~ stats::cor(log1p(w[[.x]]), log1p(w[[.y]]))),
    paper_matches_r_squared = abs(round(r_squared, 2) - paper) <= 0.01 + 1e-9,
    paper_matches_r = abs(round(r, 2) - paper) <= 0.01 + 1e-9,
    sign_hidden = r < 0)
}

# ---- Numbers in the text ---------------------------------------------------------------------

cmp_text <- function(pf, ph_hier) {
  pf <- dplyr::filter(pf, species != "xyl_lin")
  s <- function(level, g) sum(pf$damage[pf$species %in% codes_of(ph_hier, level, g)])
  bor <- s("level1", "Borers on gymnosperms") + s("level1", "Borers on angiosperms")
  def <- s("level1", "Defoliators on gymnosperms") + s("level1", "Defoliators on angiosperms")
  sp <- function(x) sum(pf$damage[pf$species %in% x])
  ours <- c(bor / 1e6, 100 * s("level1", "Borers on gymnosperms") / bor, 100 * s("level2_paper", "Borers on P. abies") / bor,
            100 * sp("ips_typ") / bor, 100 * sp("pit_cha") / bor, 100 * sp("ips_dup") / bor, 100 * sp("pha_cya") / bor,
            100 * sp(c("agr_big", "agr_vir")) / bor,
            def / 1e6, 100 * s("level1", "Defoliators on angiosperms") / def,
            100 * sp("lym_mon") / def, 100 * sp("lym_dis") / def, 100 * sp("mel_spp") / def, 100 * sp("geo_db") / def)
  paper_text |> dplyr::mutate(ours = ours, match = abs(round(ours, 1) - paper) <= 0.15 | abs(ours / paper - 1) <= 0.01)
}

# ---- Table 6 summary (species correlations, R/paper.R) ------------------------------------------

cmp_table6 <- function(t6) {
  t6 |> dplyr::transmute(species_a, species_b, paper = paper_value, r = r_ours, r_squared = r2_ours,
                         paper_matches_r_squared = abs_diff_paper_vs_r2 <= 0.01 + 1e-9, paper_matches_r = abs_diff_paper_vs_r <= 0.01 + 1e-9,
                         sign_hidden = r_ours < 0)
}

# ---- One summary table ---------------------------------------------------------------------------

cmp_summary <- function(t1, t2, t2s, t45, t6, tx) {
  tibble::tribble(
    ~check, ~result, ~meaning,
    "Table 1: countries per group",
    paste(sum(t1$countries_same), "of", nrow(t1), "groups list exactly the same countries"),
    "Same countries in our data as in the paper.",
    "Table 1: number of time series",
    paste(sum(t1$paper_series == t1$ours_series), "of", nrow(t1), "totals and", sum(t1$paper_series_kept == t1$ours_series_kept), "of", nrow(t1), "kept series match exactly"),
    "The paper counts Czech and German sub-rows as separate series; kept = series with at least 6 non-zero years.",
    "Table 2: total damage",
    paste(sum(abs(t2$diff_filter_no_xyl_lin_pct) < 0.1), "of", nrow(t2), "within 0.1% with the paper's filter and without T. lineatum (without the filter:",
          sum(abs(t2$diff_all_pct) < 0.1), "of", nrow(t2), ")"),
    "The paper's totals are after removing series with fewer than 6 non-zero years; T. lineatum is not in its conifer borers.",
    "Table 2: share of host per year",
    paste(sum(t2s$match_23), "of", nrow(t2s), "match dividing by 23 years;", sum(t2s$match_22), "of", nrow(t2s), "dividing by 22 years"),
    "Same damage, so the difference is in the division: 2000-2022 is 23 years, but the paper's numbers fit 22 better (question for Roman).",
    "Tables 4-5: group correlations",
    paste(sum(t45$paper_matches_r_squared), "of", nrow(t45), "match r squared;", sum(t45$paper_matches_r), "match r;", sum(t45$sign_hidden), "are negative r shown as positive"),
    "The tables are called Pearson correlations, but they are r squared of untransformed Europe totals.",
    "Table 6: species correlations",
    paste(sum(t6$paper_matches_r_squared), "of", nrow(t6), "match r squared;", sum(t6$paper_matches_r), "match r;", sum(t6$sign_hidden), "are negative r shown as positive"),
    "Same as Tables 4-5.",
    "Numbers in the text",
    paste(sum(tx$match), "of", nrow(tx), "match"),
    "The text is consistent with the paper's own Table 2, and with our data.")
}
