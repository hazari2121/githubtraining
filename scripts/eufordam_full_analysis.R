# =============================================================================
#  EU-ForDAM: species ranking, correlation metrics and gap-filling
#  One self-contained R script that reproduces the analyses we kept.
# =============================================================================
#
#  What it does (in order)
#    PART 1  Read the data, apply the shared rules
#    PART 2  Species-NUTS occurrence ranking
#    PART 3  Correlation metrics (insect vs insect), checks, figures
#    PART 4  Gap-filling with the latent factor model
#              4a  hide-and-predict test of the methods
#              4b  model versions M0 / M1 / M2
#              4c  final filled dataset (M2 + draft range rule) with 80% ranges
#
#  Shared rules
#    - value1 only (value2 is the same damage in another unit)
#    - 0 = monitored, no damage (kept); not reported = unknown (never set to 0)
#    - the Czech spruce bark-borer group is counted as Ips typographus,
#      as in the 2025 EU-ForDAM paper
#    - raw data are read only; outputs go to OUT (tables, figures) and DATA_OUT
#
#  How to run
#    1. Put EUForDam_REW3.xlsx and Appendix_NUTS_Forests_12.xlsx in data/raw/
#    2. Set the working directory to the project folder
#    3. source("scripts/eufordam_full_analysis.R")   (about 15-20 minutes)
#
#  Packages: dplyr, tidyr, readr, readxl, stringr, tibble, purrr, ggplot2,
#            scales, forcats, gridExtra, ggrepel, writexl
#  All random steps (bootstraps, permutations, model starts) use fixed seeds.
#
#  Model settings: the sizes of the factor model (K1 = 2 factors, ridge 10 for
#  "any damage?"; K2 = 2 factors, ridge 20 for "how much?") and the group hint
#  (tau = 50) were chosen earlier by the same hide-and-predict test over a grid
#  of candidates; they are fixed here so the script runs in reasonable time.
#  The function definitions follow PART 4 (R needs them defined before the
#  analysis runs, so the analysis is wrapped in run_all() and called at the end).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(readxl); library(stringr)
  library(tibble); library(purrr); library(ggplot2)
})

RAW_FILE  <- "data/raw/EUForDam_REW3.xlsx"
HOST_FILE <- "data/raw/Appendix_NUTS_Forests_12.xlsx"
OUT       <- "outputs"            # tables and figures
DATA_OUT  <- "data/processed"     # full filled dataset (large; keep out of public repos)

run_all <- function() {
  dir.create(file.path(OUT, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(OUT, "figures"), recursive = TRUE, showWarnings = FALSE)
  dir.create(DATA_OUT, recursive = TRUE, showWarnings = FALSE)
  tab <- function(x, name) write_csv_out(x, file.path(OUT, "tables", name))
  fig <- function(name) file.path(OUT, "figures", name)

  # ---------------------------------------------------------------------------
  # PART 1  Data and shared rules
  # ---------------------------------------------------------------------------
  message("PART 1: reading data")
  raw          <- read_raw(RAW_FILE)               # the EU-ForDAM sheet, as is
  long         <- to_long(raw)                     # one row per region x year x insect
  species_meta <- species_meta_from_raw(raw)       # unit, host tree, borer / defoliator
  dmg          <- prepare_damage(long, species_meta)   # value1; CZ sub-rows summed; CZ group -> ips_typ
  cells        <- classify_cells(dmg)              # observed damage / observed zero / not reported
  hier         <- hierarchy_table(species_meta)    # broad groups: borers/defoliators x conifers/broadleaves

  # ---------------------------------------------------------------------------
  # PART 2  Species-NUTS occurrence ranking
  #   number of NUTS units where each code caused damage (at least one year > 0)
  # ---------------------------------------------------------------------------
  message("PART 2: species-NUTS ranking")
  occ_rank <- occurrence_ranking_v2(long, species_meta)
  tab(occ_rank, "species_nuts_occurrence_ranking_v2.csv")
  save_fig(plot_occurrence_ranking_v2(occ_rank), fig("species_nuts_occurrence_ranking.png"), 8, 10)

  # ---------------------------------------------------------------------------
  # PART 3  Correlation metrics
  #   For every pair of codes (49 codes -> 1,176 pairs), on region-years where
  #   both are recorded: log(1 + damage), each region series standardised
  #   against its own mean (z_within), pooled Pearson r, 95% range from
  #   resampling whole regions 999 times.
  #   Q1 same-year r | Q2 r after removing Europe-wide years | Q3 one leads the
  #   other | Q4 holds in every country | Q5 same broad group / host tree
  # ---------------------------------------------------------------------------
  message("PART 3: correlation metrics (a few minutes)")
  tr      <- make_transforms(cells, min_nonzero = 0)          # log_damage, z_within, anomaly
  species <- sort(unique(cells$species))
  pw      <- pairwise_correlations(tr, species, versions = "z_within")
  pc      <- per_country_correlations(tr, species, versions = "z_within")      # Q4: r per country
  loco    <- loco_correlations(tr, species, "z_within")                         # Q4: leave one country out
  ts      <- transferability_summary(pw, pc, loco, "z_within")                  # consistent / inconsistent
  cm      <- correlation_metrics(tr, species, ts)                               # Q1-Q3 with bootstrap ranges
  q5      <- dplyr::bind_rows(hierarchy_pair_test(cm, hier, "same_year"),       # Q5: permutation test
                              hierarchy_pair_test(cm, hier, "beyond_year"))
  verdict <- cm_verdict(cm_basic_table(cm, hier))   # usable / promising / unreliable / weak / no link
  tab(verdict, "CM_pairs_verdict.csv"); tab(q5, "CM_basic_Q5_structure_test.csv"); tab(pc, "CM_basic_Q4_per_country.csv")

  save_fig(plot_cm_heatmap(verdict, hier, cm_basic_order(hier)), fig("CM_heatmap.png"), 11, 11)
  save_fig(plot_cm_verdict(verdict), fig("CM_verdict.png"), 8, 4.2)
  save_fig(plot_cm_usable(verdict), fig("CM_usable_pairs.png"), 8.5, 7)
  save_grob(plot_usable_table(usable_table(verdict, species_meta)), fig("CM_usable_pairs_table.png"), 10.5, 10.4)
  save_grob(plot_simple_table(cm_table_questions(), "Four questions per insect pair, and the objective each one serves"),
            fig("CM_table_questions.png"), 10, 3.4)
  save_grob(plot_simple_table(cm_table_verdicts(verdict), "One verdict per insect pair (1,176 pairs)",
                              "strong = sure link (95% range excludes 0) with r >= 0.3; checked = recorded together in >= 2 countries"),
            fig("CM_table_verdicts.png"), 8.5, 3.6)
  save_grob(plot_simple_table(cm_table_groups(verdict), "Feeding type matters: strong links within vs between broad groups",
                              "Broad groups: borers / defoliators on conifers / broadleaves. Permutation test (999 relabellings)."),
            fig("CM_table_groups.png"), 8.5, 2.2)

  # Check 1: Spearman (within-region ranks) instead of values
  sp_tr  <- rank_transforms(tr)
  sp_ts  <- transferability_summary(pairwise_correlations(sp_tr, species, versions = "z_within"),
                                    per_country_correlations(sp_tr, species, versions = "z_within"),
                                    loco_correlations(sp_tr, species, "z_within"), "z_within")
  sp_ver <- cm_verdict(cm_basic_table(correlation_metrics(sp_tr, species, sp_ts), hier))
  tab(compare_rank_verdicts(verdict, sp_ver), "CM_pearson_vs_spearman.csv")

  # Check 2: does a link learnt in other countries predict a hidden country?
  pred <- predictive_summary(predictive_by_country(tr, species), verdict)
  tab(pred, "CM_predictive_pairs.csv")
  save_fig(plot_cm_predictive(pred), fig("CM_predictive.png"), 9.5, 6.5)

  # The pairwise equation log(B+1) = a + b log(A+1) for the usable pairs
  peq <- partner_equations(tr, verdict)
  tab(peq, "CM_usable_pairs_equation.csv")
  save_grob(plot_partner_eq_table(peq), fig("CM_usable_pairs_equation_table.png"), 11, 9.4)
  save_fig(plot_partner_eq_scatter(tr, "ips_typ", "pit_spp"), fig("CM_equation_example_ips_typ_pit_spp.png"), 11, 5.6)

  # ---------------------------------------------------------------------------
  # PART 4  Gap-filling with the latent factor model
  #   Matrix Y: rows = region-years, columns = insects, values = log(1 + damage).
  #   Two parts per missing cell:
  #     "any damage?"  logit P = insect level + region-year intensity + scores x loadings
  #     "how much?"    log(1 + 1000 damage / host area) = same structure, damage > 0 only
  #   filled value = P(any) x expected amount (x host area / 1000); 0 if host absent.
  #   Tested by hiding whole country x insect blocks (10 folds) and predicting them.
  # ---------------------------------------------------------------------------
  message("PART 4: gap-filling (about 10 minutes)")
  host_appx   <- read_host_appendix(HOST_FILE)                       # NUTS forest table
  host_long   <- host_area_long(host_appx, dash_means_absent = TRUE)
  host_status <- host_status_table(cells, species_meta, host_long)    # host area per region x insect
  L      <- build_lfm_matrix(cells, tr)
  blocks <- make_cv_blocks(L, n_folds = 10)                           # hidden country x insect blocks
  group  <- hier$level1[match(L$species, hier$species)]
  K1 <- 2; lambda1 <- 10; K2 <- 2; lambda2 <- 20; tau <- 50

  # 4a  Methods head to head: simple average, partner method, factor model (+ forest area)
  gf_cv <- gapfill_compare_cv(L, blocks, group, K1, lambda1, K2, lambda2, tau = tau,
                              Lh = host_scaled_L(L, host_status), absent = host_absent_matrix(L, host_status))
  gf_scores <- gapfill_compare_scores(gf_cv)
  tab(gf_scores$overall, "GF_methods_overall.csv")
  save_fig(plot_gapfill_overall(gf_scores$overall), fig("GF_1_methods_overall.png"), 11, 7)

  # 4b  Model versions (M1: region + year intensity; M2: 3 factors + group-pulled links)
  base  <- list(K1 = K1, lambda1 = lambda1, K2 = K2, lambda2 = lambda2, tau = tau)
  specs <- list(
    `M0 current`               = c(base, list(tau_links = 0, lambda_rc = 0)),
    `M1 region+year (pull 5)`  = c(base, list(tau_links = 0, lambda_rc = 5)),
    `M2 K=3 + group links`     = modifyList(c(base, list(tau_links = 50, lambda_rc = 5)), list(K1 = 3, K2 = 3)))
  m_cv <- lapply(specs, function(s) variant_cv(L, blocks, host_status, group, s))
  m_scores <- dplyr::bind_rows(lapply(names(specs), function(nm) variant_scores(m_cv[[nm]], nm))) |>
    dplyr::left_join(dplyr::bind_rows(lapply(names(specs), function(nm)
      variant_links(L, host_status, group, specs[[nm]], verdict, nm))), by = "model")
  tab(m_scores, "GF_M0_M1_M2_comparison.csv")

  # 4c  Final filled dataset: M2 + draft range rule, ranges from the test
  spec   <- specs[["M2 K=3 + group links"]]
  fcv    <- m_cv[["M2 K=3 + group links"]]
  bt     <- cv_block_totals(fcv)                                    # hidden totals: real vs predicted
  simple <- gf_scores$by_species |> dplyr::filter(method == "simple average") |>
    dplyr::select(species, error_log_simple = error_log)
  conf   <- final_method_scores(fcv, simple)                        # reliable / weak per insect
  rng    <- range_table(bt)                                         # 80% error ranges per insect
  tab(range_calibration(bt), "GF3_range_calibration.csv")           # do 80% ranges cover 80%?
  rd     <- range_draft(L)                                          # country reports or neighbour reports
  writexl::write_xlsx(list(`in range (1 = yes)` = range_draft_wide(rd), reasons = rd),
                      file.path(OUT, "tables", "GF2_range_draft_for_correction.xlsx"))
  raw_fill <- gapfill_raw_rs(L, host_status, group, spec)           # fit M2 on all data
  filled <- apply_range(gapfill_dataset_v2(raw_fill, conf, dplyr::select(species_meta, species, unit), L$species), rd)
  tot    <- gapfill_totals_v2(filled, rng, bt)                      # country and Europe totals + 80% ranges
  write_csv_out(filled, file.path(DATA_OUT, "eufordam_gapfilled_v3.csv"))
  tab(conf, "GF3_confidence_by_insect.csv")
  tab(tot$europe, "GF3_europe_totals_with_ranges.csv"); tab(tot$country, "GF3_country_totals_with_ranges.csv")
  tab(dplyr::count(filled, value_type, confidence), "GF3_dataset_summary.csv")
  save_fig(plot_gf2_europe(tot$europe), fig("GF3_europe_totals_with_ranges.png"), 10, 10)

  # Scenario: tha_pit as Thaumetopoea pityocampa (filled only in BG, CH, HR, IT, SI)
  filled_p <- apply_range(gapfill_dataset_v2(raw_fill, conf, dplyr::select(species_meta, species, unit), L$species),
                          apply_range_overrides(rd, range_overrides_pityocampa))
  tab(range_effect(tot, gapfill_totals_v2(filled_p, rng, bt)), "GF3_scenario_pityocampa_vs_pinivora.csv")

  message("Done. Tables in ", file.path(OUT, "tables"), ", figures in ", file.path(OUT, "figures"))
  invisible(list(verdict = verdict, gap_scores = gf_scores$overall, model_versions = m_scores, filled = filled, totals = tot))
}

# =============================================================================
#  FUNCTIONS (from the project's R/ folder, file by file)
# =============================================================================


# -----------------------------------------------------------------------------
# helpers.R
# -----------------------------------------------------------------------------
# Helper finder: for each target species, which other species could be used to
# fill its gaps? Three questions per target-helper pair:
#   1. Link        : do they move together? pooled r (z_within), and r after
#                    removing Europe-wide year effects (anomaly).
#   2. Transfer    : does the link hold across countries? (transferability flag)
#   3. Availability: is the helper observed where the target is missing?
#                    gap_coverage = share of the target's not-monitored
#                    region-years in which the helper is observed.
#
# Grade (thresholds are choices, not rules; see report):
#   A  recommended   : consistent across >= 2 countries and |r| >= 0.3, and the
#                      link survives removal of Europe-wide year effects
#                      (anomaly r same sign, |r| >= 0.2). Links that vanish are
#                      mostly shared bad years or opposite long-term trends,
#                      which a year effect in the gap-filling model captures anyway.
#   B  possible      : consistent and 0.2 <= |r| < 0.3, or |r| >= 0.3 but tested
#                      in only one country (transfer untested)
#   C  not suitable  : everything else (weak, inconsistent or no data)
# A helper that is never observed where the target is missing (gap_coverage = 0)
# cannot fill gaps and is graded C. So is any link resting on thin evidence:
# fewer than 2 regions or fewer than 10 region-years where both species had
# damage (e.g. r = 0.96 from one region with 6 shared damage years is chance).

min_support_regions <- 2
min_support_both_pos <- 10

min_anomaly_r <- 0.2

helper_grade <- function(r, flag, coverage, n_regions, n_both_positive, r_anomaly) {
  beyond_year <- !is.na(r_anomaly) & sign(r_anomaly) == sign(r) & abs(r_anomaly) >= min_anomaly_r
  dplyr::case_when(
    is.na(r) | coverage == 0 ~ "C",
    n_regions < min_support_regions | n_both_positive < min_support_both_pos ~ "C",
    flag == "consistent" & abs(r) >= 0.3 & beyond_year ~ "A",
    flag == "consistent" & abs(r) >= 0.3 ~ "B",
    flag == "consistent" & abs(r) >= 0.2 ~ "B",
    flag == "insufficient data" & abs(r) >= 0.3 ~ "B",
    TRUE ~ "C"
  )
}

grade_label <- c(A = "A: recommended", B = "B: possible", C = "C: not suitable")

helper_finder <- function(cells, ts, pw, species_meta) {
  # Both directions of every pair: target <- helper
  tz <- ts |> dplyr::filter(version == "z_within")
  ta <- ts |> dplyr::filter(version == "anomaly") |>
    dplyr::select(species_a, species_b, r_anomaly = r_pooled, transfer_anomaly = transferability)
  pairs <- tz |>
    dplyr::left_join(ta, by = c("species_a", "species_b")) |>
    dplyr::select(species_a, species_b, n = n_pooled, r = r_pooled, r_anomaly,
                  transferability, transfer_anomaly, n_countries, countries = countries_n10,
                  r_min, r_max)
  support <- pw |>
    dplyr::filter(version == "z_within") |>
    dplyr::select(species_a, species_b, n_regions, n_both_positive)
  pairs <- pairs |> dplyr::left_join(support, by = c("species_a", "species_b"))
  both <- dplyr::bind_rows(
    dplyr::rename(pairs, target = species_a, helper = species_b),
    dplyr::rename(pairs, target = species_b, helper = species_a)
  )

  # Availability: target not monitored, helper observed, in the same region-year
  obs <- cells |>
    dplyr::mutate(observed = status != "not_monitored") |>
    dplyr::select(country, nuts_id, year, species, observed)
  gaps <- obs |> dplyr::filter(!observed) |> dplyr::select(country, nuts_id, year, target = species)
  avail <- obs |> dplyr::filter(observed) |> dplyr::select(country, nuts_id, year, helper = species)
  n_gaps <- gaps |> dplyr::count(target, name = "target_gap_region_years")
  cover <- gaps |>
    dplyr::inner_join(avail, by = c("country", "nuts_id", "year"), relationship = "many-to-many") |>
    dplyr::group_by(target, helper) |>
    dplyr::summarise(gap_region_years_filled = dplyr::n(),
                     gap_countries_reached = paste(sort(unique(country)), collapse = ","),
                     .groups = "drop")

  both |>
    dplyr::left_join(n_gaps, by = "target") |>
    dplyr::left_join(cover, by = c("target", "helper")) |>
    dplyr::mutate(
      gap_region_years_filled = dplyr::coalesce(gap_region_years_filled, 0L),
      gap_coverage = 100 * gap_region_years_filled / target_gap_region_years,
      grade = helper_grade(r, transferability, gap_coverage, n_regions, n_both_positive, r_anomaly),
      link_beyond_year_effects = !is.na(r_anomaly) & sign(r_anomaly) == sign(r) & abs(r_anomaly) >= min_anomaly_r
    ) |>
    dplyr::left_join(dplyr::select(species_meta, target = species, target_guild = guild_from_file),
                     by = "target") |>
    dplyr::left_join(dplyr::select(species_meta, helper = species, helper_guild = guild_from_file,
                                   helper_is_aggregate = is_aggregate), by = "helper") |>
    dplyr::arrange(target, grade, dplyr::desc(abs(r))) |>
    dplyr::relocate(target, target_guild, grade, helper, helper_guild, r, r_anomaly,
                    transferability, countries, gap_coverage)
}

# Best helpers per target: up to k helpers with grade A or B, ranked by grade,
# then by share of the target's gaps they reach, then |r|.
# Targets without any A/B helper are kept with helper = NA.
helper_best <- function(hf, k = 3) {
  best <- hf |>
    dplyr::filter(grade %in% c("A", "B")) |>
    dplyr::group_by(target) |>
    dplyr::arrange(grade, dplyr::desc(gap_coverage), dplyr::desc(abs(r)), .by_group = TRUE) |>
    dplyr::slice_head(n = k) |>
    dplyr::mutate(helper_rank = dplyr::row_number()) |>
    dplyr::ungroup()
  none <- hf |>
    dplyr::distinct(target, target_guild, target_gap_region_years) |>
    dplyr::anti_join(best, by = "target") |>
    dplyr::mutate(grade = "C", helper_rank = NA_integer_)
  dplyr::bind_rows(best, none) |>
    dplyr::select(target, target_guild, helper_rank, helper, grade, r, r_anomaly,
                  transferability, countries, n_regions, n_both_positive, gap_coverage,
                  gap_countries_reached, target_gap_region_years) |>
    dplyr::arrange(target, helper_rank)
}

# Graph: one row per target species; dot = its best helper, x = |r| of the link,
# colour = grade, label = helper name and share of gaps it reaches.
plot_helper_finder <- function(best, sp_rank) {
  d <- best |>
    dplyr::group_by(target) |>
    dplyr::slice_min(helper_rank, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::left_join(dplyr::select(sp_rank, target = species, n_regions_occurring), by = "target") |>
    dplyr::mutate(
      grade_lab = factor(grade_label[grade], grade_label),
      x = dplyr::coalesce(abs(r), 0),
      lab = dplyr::case_when(
        !is.na(helper) ~ paste0(helper, "  (reaches ", round(gap_coverage), "% of gaps)"),
        target_gap_region_years < 300 ~ "hardly any gaps: monitored almost everywhere",
        TRUE ~ "no suitable helper"),
      target_guild = dplyr::coalesce(target_guild, "other")
    ) |>
    dplyr::arrange(n_regions_occurring) |>
    dplyr::mutate(target = factor(target, unique(target)))

  ggplot2::ggplot(d, ggplot2::aes(x, target)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = x, yend = target), colour = "#e6e5e1", linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(colour = grade_lab, shape = grade_lab), size = 2.6) +
    ggplot2::geom_text(ggplot2::aes(label = lab), hjust = 0, nudge_x = 0.02, size = 2.4, colour = pal$ink2) +
    ggplot2::scale_colour_manual(values = c(`A: recommended` = "#008300", `B: possible` = "#eda100",
                                            `C: not suitable` = "#9c9b97"), name = NULL, drop = FALSE) +
    ggplot2::scale_shape_manual(values = c(`A: recommended` = 16, `B: possible` = 17,
                                           `C: not suitable` = 4), name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 1.25), breaks = seq(0, 1, 0.2)) +
    ggplot2::facet_grid(target_guild ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "Strength of the link with the best helper (|r|)", y = "Insect with gaps (target)",
                  title = "Helper finder: which insect can fill the gaps of each insect?",
                  subtitle = "Best helper per target. Targets ordered by how widespread they are (most widespread at top).",
                  caption = paste("A = link holds in >= 2 countries, |r| >= 0.3 and survives removing Europe-wide bad years.",
                                  "\nB = weaker, mostly shared bad years, or tested in one country only. C = none suitable.",
                                  "\nAll links need >= 2 regions and >= 10 years where both insects had damage.",
                                  "'% of gaps' = share of the target's missing region-years where the helper is recorded.")) +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}


# -----------------------------------------------------------------------------
# inspect.R
# -----------------------------------------------------------------------------
# Step 1: inspection of the raw EU-ForDAM workbook.
# Nothing here alters the raw file; all derived objects go to data/processed/
# and outputs/tables/.

# Read the main data sheet. The workbook also has a Czech pivot sheet
# ("kotg_EUForDam_REW") and a scratch sheet ("List1"); neither holds raw data.
read_raw <- function(path) {
  readxl::read_excel(path, sheet = "EUForDam_REW", guess_max = 50000) |>
    # Some indicator-column headers carry trailing spaces / non-breaking spaces
    dplyr::rename_with(~ stringr::str_squish(stringr::str_replace_all(.x, " ", " ")))
}

# Long format: one row per raw record. Nothing is aggregated or dropped yet:
#  - CZ has 1-3 rows per NUTS2-year-species (sub-regions, probably kraje,
#    that map to the same NUTS2 code). `sub_id` numbers them in file order.
#  - value2 is kept alongside value1 until its meaning is confirmed.
to_long <- function(raw) {
  raw |>
    dplyr::mutate(row_in_file = dplyr::row_number() + 1L) |>  # +1 = Excel header row
    dplyr::transmute(
      country  = Country,
      nuts_id  = NUTS,
      region   = Region,
      year     = as.integer(d_year),
      species  = sp_code,
      damage   = value1,
      unit     = sp_unit,
      value2,
      host     = HOSTTREE,
      treetype = TREETYPE,
      sp_category    = SP_CATEGORY,
      sp_subcategory = SP_SUBCATEGORY,
      row_in_file
    ) |>
    dplyr::group_by(country, nuts_id, year, species) |>
    dplyr::mutate(sub_id = dplyr::row_number(), n_sub = dplyr::n()) |>
    dplyr::ungroup()
}

# Headline counts
summarise_counts <- function(long) {
  tibble::tibble(
    n_rows      = nrow(long),
    n_countries = dplyr::n_distinct(long$country),
    n_species   = dplyr::n_distinct(long$species),
    n_regions   = dplyr::n_distinct(long$nuts_id),
    n_years     = dplyr::n_distinct(long$year),
    year_min    = min(long$year),
    year_max    = max(long$year),
    n_value1_na = sum(is.na(long$damage)),
    n_value1_zero = sum(long$damage == 0, na.rm = TRUE),
    n_value1_pos  = sum(long$damage > 0, na.rm = TRUE),
    n_value2_present = sum(!is.na(long$value2)),
    n_unit_na   = sum(is.na(long$unit)),
    n_dup_keys  = sum(long$sub_id == 2)  # keys (country-nuts-year-species) with >1 row
  )
}

# Which species each country reports, in which years, and how
# (NA / zero / positive counts). Rows exist for nearly every year, so
# "row present" is NOT evidence of monitoring; see the zero diagnostics.
country_species_coverage <- function(long) {
  long |>
    dplyr::group_by(country, species, unit) |>
    dplyr::summarise(
      n_regions = dplyr::n_distinct(nuts_id),
      year_min  = min(year),
      year_max  = max(year),
      n_years   = dplyr::n_distinct(year),
      n_rows    = dplyr::n(),
      n_na      = sum(is.na(damage)),
      n_zero    = sum(damage == 0, na.rm = TRUE),
      n_pos     = sum(damage > 0, na.rm = TRUE),
      # years in which every region of the country reports 0 or NA
      years_all_zero = {
        yz <- tapply(dplyr::coalesce(damage, 0) == 0, year, all)
        paste(names(yz)[yz], collapse = ",")
      },
      .groups = "drop"
    )
}

# Species lookup as given in the file (host, tree type, category columns)
species_lookup_raw <- function(long) {
  long |>
    dplyr::group_by(species) |>
    dplyr::summarise(
      unit      = paste(sort(unique(stats::na.omit(unit))), collapse = "/"),
      host      = paste(sort(unique(host)), collapse = "/"),
      treetype  = paste(sort(unique(treetype)), collapse = "/"),
      sp_category = paste(sort(unique(sp_category)), collapse = "/"),
      countries = paste(sort(unique(country)), collapse = ","),
      n_countries = dplyr::n_distinct(country),
      .groups = "drop"
    )
}

write_csv_out <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path)
  path
}


# -----------------------------------------------------------------------------
# prepare.R
# -----------------------------------------------------------------------------
# Data preparation decisions (agreed with SH, Oct 2026):
#  - value1 is the damage variable. value2 is kept in the raw long table only:
#    in most countries it is an exact constant fraction of value1 (a unit
#    conversion), and in DE it is a sub-part of value1; it is filled for ~12% of rows.
#  - CZ has 1-3 rows per NUTS2-year-species (sub-regions, probably kraje).
#    Damage (m3 / ha) is additive, so sub-rows are summed to NUTS2.
#  - Aggregate codes (containing "&", plus "Def_dec") are kept as their own units
#    and flagged; their content differs between countries. Exception: groups a
#    country reports in place of a dominant species (see group_to_species).

# Species metadata from the file itself. Guild is derived from the indicator
# columns ("Bark borers on ..." / "Defoliators on ..."), which are consistent
# across countries (unlike SP_CATEGORY, which is scrambled in the CZ rows).
species_meta_from_raw <- function(raw) {
  flag_cols <- names(raw)[13:ncol(raw)]
  flags <- raw |>
    dplyr::select(sp_code, dplyr::all_of(flag_cols)) |>
    tidyr::pivot_longer(-sp_code, names_to = "flag", values_to = "v") |>
    dplyr::filter(v == 1) |>
    dplyr::distinct(sp_code, flag) |>
    dplyr::group_by(species = sp_code) |>
    dplyr::summarise(flags = paste(sort(flag), collapse = "; "), .groups = "drop")

  raw |>
    dplyr::group_by(species = sp_code) |>
    dplyr::summarise(
      unit     = dplyr::first(stats::na.omit(sp_unit)),
      host_tree = paste(sort(unique(HOSTTREE)), collapse = "/"),
      treetype = dplyr::first(TREETYPE),
      .groups = "drop"
    ) |>
    dplyr::left_join(flags, by = "species") |>
    dplyr::mutate(
      is_aggregate = grepl("&", species) | species == "Def_dec",
      is_genus_level = grepl("_spp?$", species),
      guild_from_file = dplyr::case_when(
        grepl("Bark borers", flags) ~ "borer",
        grepl("Defoliators", flags) ~ "defoliator",
        TRUE ~ NA_character_
      ),
      host_from_file = dplyr::recode(treetype, con = "conifer", dec = "broadleaf")
    )
}

# Group codes treated as a single species, following Hlasny et al. (2025, GCB,
# Sect. 2.2): "if the national data providers indicated that a broader group was
# dominated by a single species (e.g. borers on gymnosperms is dominated by
# I. typographus), we considered this group in species-specific analyses".
# Only applied where the country does not report that species separately.
# This mapping reproduces the paper's Table 6 (see R/paper.R).
group_to_species <- tibble::tribble(
  ~country, ~from,                   ~to,       ~reason,
  "CZ",     "CambXyl_con&BB_con&SM", "ips_typ", "Spruce bark-borer group, dominated by I. typographus; CZ reports no separate ips_typ"
)

# One row per country-nuts-year-species with the damage value used downstream.
prepare_damage <- function(long, species_meta, recode = group_to_species) {
  long |>
    dplyr::left_join(dplyr::select(recode, country, species = from, to),
                     by = c("country", "species")) |>
    dplyr::mutate(species = dplyr::coalesce(to, species)) |>
    dplyr::select(-to) |>
    # BG/RS rows with missing value also miss the unit; units are fixed per species
    dplyr::select(-unit) |>
    dplyr::left_join(dplyr::select(species_meta, species, unit), by = "species") |>
    dplyr::group_by(country, nuts_id, year, species, unit) |>
    dplyr::summarise(
      # NA only if every sub-row is NA; otherwise sum of reported sub-rows
      damage = if (all(is.na(damage))) NA_real_ else sum(damage, na.rm = TRUE),
      n_sub_rows = dplyr::n(),
      .groups = "drop"
    )
}


# -----------------------------------------------------------------------------
# classify.R
# -----------------------------------------------------------------------------
# Step 2: classify every region-year-species cell.
#
# Grid = every NUTS region in the file x 2000-2022 x every species code.
#   observed_positive : value reported, > 0
#   observed_zero     : value reported, = 0  (treated as real; never filled)
#   not_monitored     : value NA, or no row for that country-species-year
#   impossible        : not monitored and host tree absent from the region; set
#                       afterwards by apply_host_status() (R/hosts.R).
#
# Coding confirmed by the data provider (Oct 2026): 0 = monitored, no damage;
# empty = not monitored.
classify_cells <- function(dmg, years = 2000:2022) {
  regions <- dplyr::distinct(dmg, country, nuts_id)
  grid <- tidyr::crossing(regions, year = years, species = sort(unique(dmg$species)))

  grid |>
    dplyr::left_join(dmg, by = c("country", "nuts_id", "year", "species")) |>
    dplyr::mutate(
      status = dplyr::case_when(
        is.na(damage) ~ "not_monitored",
        damage == 0   ~ "observed_zero",
        TRUE          ~ "observed_positive"
      )
    )
}

# Share of cells in each class, by a grouping variable
coverage_summary <- function(cells, by) {
  g <- rlang::sym(by)
  cells |>
    dplyr::group_by(!!g) |>
    dplyr::summarise(
      n_cells = dplyr::n(),
      pct_observed_positive = 100 * mean(status == "observed_positive"),
      pct_observed_zero     = 100 * mean(status == "observed_zero"),
      pct_not_monitored     = 100 * mean(status == "not_monitored"),
      pct_impossible        = 100 * mean(status == "impossible"),  # not monitored + host absent
      .groups = "drop"
    )
}

# Country x species coverage: share of region-years observed (positive or zero)
coverage_country_species <- function(cells) {
  cells |>
    dplyr::group_by(country, species) |>
    dplyr::summarise(
      n_regions = dplyr::n_distinct(nuts_id),
      years_observed = if (any(status != "not_monitored"))
        paste(range(year[status != "not_monitored"]), collapse = "-") else NA_character_,
      pct_observed = 100 * mean(status %in% c("observed_positive", "observed_zero")),
      pct_positive = 100 * mean(status == "observed_positive"),
      .groups = "drop"
    )
}


# -----------------------------------------------------------------------------
# transform.R
# -----------------------------------------------------------------------------
# Step 3: transformed versions of the observed data (never imputed values).
#
# Per region-species series:
#   log_damage = log(damage + 1)
#   z_within   = log_damage standardised within the series
#   detrended  = residuals of lm(log_damage ~ year) within the series
#   diff       = log_damage[t] - log_damage[t-1], consecutive years only
#   anomaly    = z_within minus the mean z_within of the same species in the same
#                year across all OTHER regions. Removes the Europe-wide year signal
#                (e.g. the 2003 and 2018-2020 droughts) so what remains is regional
#                co-variation beyond shared bad years.
# z_within / detrended are NA for series with < 3 observations or no variation
# (e.g. all-zero series), so such series drop out of correlations.
#
# Zeros are real observations (0 = monitored, no damage; confirmed Oct 2026), so
# the main analysis keeps every observed value (min_nonzero = 0). The series
# filter of Hlasny et al. (2025, Sect. 2.5), which drops region-species series
# with fewer than 6 non-zero values, is kept as a sensitivity check (min_nonzero = 6).

transform_versions <- tibble::tribble(
  ~version,     ~comparable_across_countries, ~note,
  "log_damage", FALSE, "Pooled reference only. Units are fixed per species, but levels depend on region size and on NUTS level (national vs NUTS1-3), so levels are not comparable across countries.",
  "z_within",   TRUE,  "Standardised within each region-species series; removes region size and scale.",
  "detrended",  TRUE,  "Residuals of a linear time trend per series, in log units; removes shared long-term trends.",
  "diff",       TRUE,  "Year-to-year change in log_damage; removes level and most trend.",
  "anomaly",    TRUE,  "z_within minus the same species' mean z_within in that year in all other regions; removes Europe-wide year effects."
)

detrend_series <- function(y, t) {
  if (length(y) < 3 || stats::sd(y) == 0) return(rep(NA_real_, length(y)))
  as.numeric(stats::resid(stats::lm(y ~ t)))
}

make_transforms <- function(cells, min_nonzero = 0) {
  obs <- cells |>
    dplyr::filter(status %in% c("observed_positive", "observed_zero")) |>
    dplyr::group_by(country, nuts_id, species) |>
    dplyr::filter(sum(damage > 0) >= min_nonzero) |>
    dplyr::ungroup()

  obs |>
    dplyr::arrange(country, nuts_id, species, year) |>
    dplyr::group_by(country, nuts_id, species) |>
    dplyr::mutate(
      log_damage = log1p(damage),
      n_obs = dplyr::n(),
      z_within = if (dplyr::first(n_obs) >= 3 && stats::sd(log_damage) > 0)
        (log_damage - mean(log_damage)) / stats::sd(log_damage) else NA_real_,
      detrended = detrend_series(log_damage, year),
      diff = dplyr::if_else(year - dplyr::lag(year) == 1,
                            log_damage - dplyr::lag(log_damage), NA_real_)
    ) |>
    dplyr::group_by(species, year) |>
    dplyr::mutate(
      k = sum(!is.na(z_within)),
      other_mean = dplyr::if_else(k > 1, (sum(z_within, na.rm = TRUE) - z_within) / (k - 1), NA_real_),
      anomaly = z_within - other_mean
    ) |>
    dplyr::ungroup() |>
    dplyr::select(country, nuts_id, year, species, unit, damage, status,
                  log_damage, z_within, detrended, diff, anomaly)
}


# -----------------------------------------------------------------------------
# correlate.R
# -----------------------------------------------------------------------------
# Steps 4-6: pairwise correlations, temporal vs spatial, transferability.
#
# All correlations use pairwise-complete region-years: only rows where BOTH
# species have a value in the version being analysed.
# NOTE: series are autocorrelated and region-years are not independent, so
# p-values (and FDR-adjusted p-values) are optimistic. Interpret mainly by
# effect size, n and consistency across countries.

min_n_interpret <- 10   # pairs / countries with fewer co-observed region-years are flagged

species_pairs <- function(species) {
  cmb <- utils::combn(sort(unique(species)), 2)
  tibble::tibble(species_a = cmb[1, ], species_b = cmb[2, ])
}

# Wide table: one row per region-year, one column per species
make_wide <- function(tr, var) {
  tr |>
    dplyr::select(country, nuts_id, year, species, value = dplyr::all_of(var)) |>
    tidyr::pivot_wider(names_from = species, values_from = value)
}

get_col <- function(W, sp) if (sp %in% names(W)) W[[sp]] else rep(NA_real_, nrow(W))

# Pearson + Spearman for two vectors (pairwise complete)
pair_stats <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  out <- list(n = sum(ok), r_pearson = NA_real_, p_pearson = NA_real_,
              r_spearman = NA_real_, p_spearman = NA_real_)
  if (out$n >= 4) {
    x <- x[ok]; y <- y[ok]
    if (stats::sd(x) > 0 && stats::sd(y) > 0) {
      pt <- stats::cor.test(x, y, method = "pearson")
      st <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
      out$r_pearson <- unname(pt$estimate);  out$p_pearson <- pt$p.value
      out$r_spearman <- unname(st$estimate); out$p_spearman <- st$p.value
    }
  }
  out
}

# Step 4: all pairs x versions, with BH-FDR across all pairs within version x method
pairwise_correlations <- function(tr, species,
                                  versions = c("z_within", "detrended", "diff", "anomaly", "log_damage")) {
  pairs <- species_pairs(species)
  # damage > 0 indicator on the same rows (all versions share the row set/order)
  P <- make_wide(dplyr::mutate(tr, pos = as.numeric(damage > 0)), "pos")
  res <- purrr::map(versions, function(v) {
    W <- make_wide(tr, v)
    stopifnot(identical(W$nuts_id, P$nuts_id), identical(W$year, P$year))
    reg <- paste(W$country, W$nuts_id)
    st <- purrr::map2(pairs$species_a, pairs$species_b, function(a, b) {
      x <- get_col(W, a); y <- get_col(W, b)
      ok <- is.finite(x) & is.finite(y)
      c(pair_stats(x, y),
        # co-observed region-years with damage > 0 in both species; low values
        # mean the correlation is driven mainly by shared zeros
        n_both_positive = sum(ok & get_col(P, a) == 1 & get_col(P, b) == 1, na.rm = TRUE),
        n_regions = dplyr::n_distinct(reg[ok]),
        n_countries = dplyr::n_distinct(W$country[ok]),
        countries = paste(sort(unique(W$country[ok])), collapse = ","))
    })
    dplyr::bind_cols(pairs, version = v, dplyr::bind_rows(st))
  })
  dplyr::bind_rows(res) |>
    dplyr::group_by(version) |>
    dplyr::mutate(
      fdr_pearson  = stats::p.adjust(p_pearson, method = "BH"),
      fdr_spearman = stats::p.adjust(p_spearman, method = "BH")
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      low_n = n < min_n_interpret,
      reference_only = version == "log_damage"
    )
}

# Step 5: spatial correlation = correlation of region means of log_damage
# across regions, using only the years in which both species were observed.
spatial_correlations <- function(tr, species, min_regions = 4) {
  pairs <- species_pairs(species)
  W <- make_wide(tr, "log_damage")
  reg <- paste(W$country, W$nuts_id)
  st <- purrr::map2(pairs$species_a, pairs$species_b, function(a, b) {
    x <- get_col(W, a); y <- get_col(W, b)
    ok <- is.finite(x) & is.finite(y)
    out <- list(n_regions_spatial = 0L, r_spatial_pearson = NA_real_, p_spatial_pearson = NA_real_,
                r_spatial_spearman = NA_real_)
    if (!any(ok)) return(out)
    m <- tibble::tibble(reg = reg[ok], x = x[ok], y = y[ok]) |>
      dplyr::group_by(reg) |>
      dplyr::summarise(mx = mean(x), my = mean(y), .groups = "drop")
    out$n_regions_spatial <- nrow(m)
    if (nrow(m) >= min_regions) {
      s <- pair_stats(m$mx, m$my)
      out$r_spatial_pearson <- s$r_pearson; out$p_spatial_pearson <- s$p_pearson
      out$r_spatial_spearman <- s$r_spearman
    }
    out
  })
  dplyr::bind_cols(pairs, dplyr::bind_rows(st))
}

# Combine temporal (pooled within-series z_within / detrended) and spatial.
# Linked = |r| >= 0.3; "insufficient" when temporal n < 10 or < 5 regions.
temporal_vs_spatial <- function(pw, sp, link_threshold = 0.3, min_regions_spatial = 5) {
  temporal <- pw |>
    dplyr::filter(version %in% c("z_within", "detrended")) |>
    dplyr::select(species_a, species_b, version, n, r_pearson) |>
    tidyr::pivot_wider(names_from = version, values_from = c(n, r_pearson)) |>
    dplyr::rename(n_temporal = n_z_within, r_temporal_z = r_pearson_z_within,
                  r_temporal_detrended = r_pearson_detrended) |>
    dplyr::select(-n_detrended)

  temporal |>
    dplyr::left_join(sp, by = c("species_a", "species_b")) |>
    dplyr::mutate(
      temporal_link = abs(r_temporal_z) >= link_threshold,
      spatial_link  = abs(r_spatial_pearson) >= link_threshold,
      linked_by = dplyr::case_when(
        is.na(r_temporal_z) | n_temporal < min_n_interpret ~ "insufficient data",
        is.na(r_spatial_pearson) | n_regions_spatial < min_regions_spatial ~
          dplyr::if_else(temporal_link, "timing (place untestable)", "neither (place untestable)"),
        temporal_link & spatial_link ~ "both",
        temporal_link ~ "timing",
        spatial_link ~ "place",
        TRUE ~ "neither"
      )
    )
}

# Step 6a: per-country correlations (countries with n >= 10 for the pair)
per_country_correlations <- function(tr, species, versions = c("z_within", "detrended", "anomaly")) {
  pairs <- species_pairs(species)
  purrr::map_dfr(versions, function(v) {
    W <- make_wide(tr, v)
    idx <- split(seq_len(nrow(W)), W$country)
    purrr::map2_dfr(pairs$species_a, pairs$species_b, function(a, b) {
      x <- get_col(W, a); y <- get_col(W, b)
      ok <- is.finite(x) & is.finite(y)
      if (sum(ok) < min_n_interpret) return(NULL)
      purrr::imap_dfr(idx, function(i, ctry) {
        if (sum(ok[i]) < min_n_interpret) return(NULL)
        s <- pair_stats(x[i], y[i])
        tibble::tibble(species_a = a, species_b = b, version = v, country = ctry,
                       n = s$n, r_pearson = s$r_pearson, p_pearson = s$p_pearson,
                       r_spearman = s$r_spearman)
      })
    })
  })
}

# Step 6b: leave-one-country-out pooled Pearson r
loco_correlations <- function(tr, species, version = "z_within") {
  pairs <- species_pairs(species)
  W <- make_wide(tr, version)
  purrr::map2_dfr(pairs$species_a, pairs$species_b, function(a, b) {
    x <- get_col(W, a); y <- get_col(W, b)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < min_n_interpret) return(NULL)
    ctrs <- sort(unique(W$country[ok]))
    purrr::map_dfr(ctrs, function(ctry) {
      keep <- W$country != ctry
      s <- pair_stats(x[keep], y[keep])
      tibble::tibble(species_a = a, species_b = b, version = version, dropped_country = ctry,
                     n_without = s$n, r_without = s$r_pearson)
    })
  })
}

# Step 6c: per-pair transferability summary and flag.
#   consistent   : >= 2 countries (n >= 10 each), all with the pooled sign,
#                  SD of country r <= 0.2, and no leave-one-country-out sign flip
#   inconsistent : >= 2 countries but any of the above fails
#   insufficient data : pooled n < 10 or < 2 countries with n >= 10
transferability_summary <- function(pw, pc, loco, version = "z_within", max_sd = 0.2) {
  pooled <- pw |>
    dplyr::filter(version == !!version) |>
    dplyr::select(species_a, species_b, n_pooled = n, r_pooled = r_pearson,
                  fdr_pooled = fdr_pearson, countries_pooled = countries)

  by_country <- pc |>
    dplyr::filter(version == !!version, !is.na(r_pearson)) |>
    dplyr::left_join(pooled, by = c("species_a", "species_b")) |>
    dplyr::group_by(species_a, species_b) |>
    dplyr::summarise(
      n_countries = dplyr::n(),
      countries_n10 = paste(country, collapse = ","),
      r_mean = mean(r_pearson), r_sd = stats::sd(r_pearson),
      r_min = min(r_pearson), r_max = max(r_pearson),
      sign_consistency = mean(sign(r_pearson) == sign(dplyr::first(r_pooled))),
      .groups = "drop"
    )

  loco_sum <- loco |>
    dplyr::filter(version == !!version) |>
    dplyr::left_join(pooled, by = c("species_a", "species_b")) |>
    dplyr::group_by(species_a, species_b) |>
    dplyr::summarise(
      loco_max_abs_change = suppressWarnings(max(abs(r_without - r_pooled), na.rm = TRUE)),
      loco_most_influential = dropped_country[which.max(abs(r_without - r_pooled))][1],
      loco_sign_flip = any(sign(r_without) != sign(r_pooled), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(loco_max_abs_change = dplyr::if_else(is.finite(loco_max_abs_change),
                                                       loco_max_abs_change, NA_real_))

  pooled |>
    dplyr::left_join(by_country, by = c("species_a", "species_b")) |>
    dplyr::left_join(loco_sum, by = c("species_a", "species_b")) |>
    dplyr::mutate(
      version = version,
      n_countries = dplyr::coalesce(n_countries, 0L),
      transferability = dplyr::case_when(
        is.na(r_pooled) | n_pooled < min_n_interpret | n_countries < 2 ~ "insufficient data",
        sign_consistency == 1 & r_sd <= max_sd & !dplyr::coalesce(loco_sign_flip, FALSE) ~ "consistent",
        TRUE ~ "inconsistent"
      ),
      strength = cut(abs(r_pooled), c(-Inf, 0.1, 0.3, 0.5, Inf),
                     labels = c("negligible", "weak", "moderate", "strong"))
    )
}


# -----------------------------------------------------------------------------
# metrics.R
# -----------------------------------------------------------------------------
# Cross-species correlation metrics, version 3 (aligned with the PhD aims:
# how much does each species tell us about every other species?).
#
# For every pair of species (all 1,176 pairs), using only region-years where
# both are observed (all recorded values; zeros are real observations):
#   same_year   : r of z_within (standardised within each region-species series)
#   beyond_year : r of anomaly (each species' Europe-wide yearly mean removed),
#                 i.e. the link that is not just shared bad years
#   lag_ab      : does A's year t predict B's year t+1 BEYOND B's own year t?
#                 Partial correlation of A(t) with B(t+1), both adjusted for
#                 B(t) (anomaly scale; Granger-type). Plain lagged correlations
#                 are misleading because outbreaks last several years, so any
#                 linked pair looks linked across years in both directions.
#   lag_ba      : the same with A and B swapped
#   co_occur    : r of damage > 0 indicators (phi), i.e. do the two tend to
#                 occur together?
#
# Uncertainty: 95% intervals from a region block bootstrap (whole regions are
# resampled, 999 times), which respects autocorrelation within a region's
# series. Intervals are NA when fewer than 5 regions contribute (too few
# blocks to resample reliably).

min_n_metric <- 20      # minimum co-observed region-years for a metric
boot_reps <- 999

# Pearson r with a region block bootstrap interval, using per-region sums
boot_r <- function(x, y, g, B = boot_reps, seed = 1) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]; g <- g[ok]
  n <- length(x)
  out <- c(r = NA_real_, lo = NA_real_, hi = NA_real_, n = n, n_regions = dplyr::n_distinct(g))
  if (n < min_n_metric || stats::sd(x) == 0 || stats::sd(y) == 0) return(out)
  out["r"] <- stats::cor(x, y)
  G <- out[["n_regions"]]
  if (G < 5) return(out)
  st <- rowsum(cbind(1, x, y, x * x, y * y, x * y), g)       # G x 6 sufficient stats
  set.seed(seed)
  w <- stats::rmultinom(B, G, rep(1 / G, G))                  # G x B region counts
  S <- t(w) %*% st                                            # B x 6
  num <- S[, 1] * S[, 6] - S[, 2] * S[, 3]
  den <- sqrt((S[, 1] * S[, 4] - S[, 2]^2) * (S[, 1] * S[, 5] - S[, 3]^2))
  rb <- num / den
  q <- stats::quantile(rb[is.finite(rb)], c(0.025, 0.975), names = FALSE)
  out["lo"] <- q[1]; out["hi"] <- q[2]
  out
}

# Partial correlation of x with y adjusting both for z (residuals of linear
# regressions on z, fitted on rows where x, y and z are all observed).
# The adjustment is estimated once, not inside each bootstrap draw.
boot_r_partial <- function(x, y, z, g, ...) {
  ok <- is.finite(x) & is.finite(y) & is.finite(z)
  if (sum(ok) < min_n_metric || stats::sd(z[ok]) == 0)
    return(c(r = NA_real_, lo = NA_real_, hi = NA_real_, n = sum(ok), n_regions = dplyr::n_distinct(g[ok])))
  rx <- ry <- rep(NA_real_, length(x))
  rx[ok] <- stats::resid(stats::lm(x[ok] ~ z[ok]))
  ry[ok] <- stats::resid(stats::lm(y[ok] ~ z[ok]))
  boot_r(rx, ry, g, ...)
}

# Wide table aligned to a fixed set of region-year rows
wide_on <- function(d, var, keys, species) {
  W <- matrix(NA_real_, length(keys), length(species), dimnames = list(NULL, species))
  k <- match(paste(d$nuts_id, d$year), keys)
  s <- match(d$species, species)
  ok <- !is.na(k) & !is.na(s)
  W[cbind(k[ok], s[ok])] <- d[[var]][ok]
  W
}

correlation_metrics <- function(tr, species, ts) {
  rows <- dplyr::distinct(tr, country, nuts_id, year) |> dplyr::arrange(nuts_id, year)
  keys <- paste(rows$nuts_id, rows$year)
  Z  <- wide_on(tr, "z_within", keys, species)
  A  <- wide_on(tr, "anomaly", keys, species)
  A1 <- wide_on(dplyr::mutate(tr, year = year - 1L), "anomaly", keys, species)  # value of year t+1
  Pz <- wide_on(dplyr::mutate(tr, pos = as.numeric(damage > 0)), "pos", keys, species)
  Pf <- wide_on(dplyr::mutate(tr, pos = as.numeric(damage > 0)), "pos", keys, species)
  g <- rows$nuts_id

  pairs <- species_pairs(species)
  res <- purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
    a <- pairs$species_a[i]; b <- pairs$species_b[i]
    sy <- boot_r(Z[, a], Z[, b], g, seed = i)
    by <- boot_r(A[, a], A[, b], g, seed = i + 1e5)
    lab <- boot_r_partial(A[, a], A1[, b], A[, b], g, seed = i + 2e5)
    lba <- boot_r_partial(A[, b], A1[, a], A[, a], g, seed = i + 3e5)
    co <- boot_r(Pz[, a], Pz[, b], g, seed = i + 4e5)
    both <- is.finite(Z[, a]) & is.finite(Z[, b])
    tibble::tibble(
      species_a = a, species_b = b,
      n = sy[["n"]], n_regions = sy[["n_regions"]],
      n_countries = dplyr::n_distinct(rows$country[both]),
      n_both_damage = sum(both & Pf[, a] == 1 & Pf[, b] == 1, na.rm = TRUE),
      same_year = sy[["r"]], same_year_lo = sy[["lo"]], same_year_hi = sy[["hi"]],
      beyond_year = by[["r"]], beyond_year_lo = by[["lo"]], beyond_year_hi = by[["hi"]],
      lag_ab = lab[["r"]], lag_ab_lo = lab[["lo"]], lag_ab_hi = lab[["hi"]],
      lag_ba = lba[["r"]], lag_ba_lo = lba[["lo"]], lag_ba_hi = lba[["hi"]],
      co_occur = co[["r"]], co_occur_lo = co[["lo"]], co_occur_hi = co[["hi"]], n_co_occur = co[["n"]]
    )
  })

  excl0 <- function(lo, hi) !is.na(lo) & (lo > 0 | hi < 0)
  res |>
    dplyr::left_join(ts |> dplyr::filter(version == "z_within") |>
                       dplyr::select(species_a, species_b, transferability, countries_n10),
                     by = c("species_a", "species_b")) |>
    dplyr::mutate(
      same_year_sure = excl0(same_year_lo, same_year_hi),
      beyond_year_sure = excl0(beyond_year_lo, beyond_year_hi),
      lag_ab_sure = excl0(lag_ab_lo, lag_ab_hi),
      lag_ba_sure = excl0(lag_ba_lo, lag_ba_hi),
      # one-line verdict per pair
      link = dplyr::case_when(
        is.na(same_year) ~ "not enough data",
        same_year_sure & abs(same_year) >= 0.3 & beyond_year_sure & abs(beyond_year) >= 0.2 ~ "strong, beyond shared bad years",
        same_year_sure & abs(same_year) >= 0.3 ~ "strong, mostly shared bad years",
        same_year_sure ~ "weak but real",
        TRUE ~ "no clear link"
      )
    )
}

# Per species: how much do the other species tell us about it?
pick_best <- function(v, r, ok) if (any(ok)) v[ok][which.max(abs(r[ok]))] else v[NA_integer_][1]

species_information <- function(cm, species_meta) {
  both <- dplyr::bind_rows(
    dplyr::rename(cm, focal = species_a, partner = species_b),
    dplyr::rename(cm, focal = species_b, partner = species_a)
  )
  both |>
    dplyr::group_by(species = focal) |>
    dplyr::summarise(
      partners_tested = sum(!is.na(same_year)),
      strong_links = sum(link == "strong, beyond shared bad years"),
      strong_but_shared_years = sum(link == "strong, mostly shared bad years"),
      weak_links = sum(link == "weak but real"),
      # best partners only among links with a reliable interval (>= 5 regions)
      best_partner = pick_best(partner, same_year, !is.na(same_year_lo)),
      best_r = pick_best(same_year, same_year, !is.na(same_year_lo)),
      best_partner_beyond_years = pick_best(partner, beyond_year, !is.na(beyond_year_lo)),
      best_r_beyond_years = pick_best(beyond_year, beyond_year, !is.na(beyond_year_lo)),
      .groups = "drop"
    ) |>
    dplyr::left_join(dplyr::select(species_meta, species, guild = guild_from_file, host_tree), by = "species") |>
    dplyr::arrange(dplyr::desc(strong_links), dplyr::desc(abs(best_r)))
}

# Lagged pairs in ordered form: "leader in year t -> follower in year t+1"
lag_pairs <- function(cm) {
  dplyr::bind_rows(
    cm |> dplyr::transmute(leader = species_a, follower = species_b, r = lag_ab, lo = lag_ab_lo, hi = lag_ab_hi,
                           sure = lag_ab_sure, same_year, n_regions),
    cm |> dplyr::transmute(leader = species_b, follower = species_a, r = lag_ba, lo = lag_ba_lo, hi = lag_ba_hi,
                           sure = lag_ba_sure, same_year, n_regions)
  ) |>
    dplyr::filter(!is.na(r)) |>
    dplyr::arrange(dplyr::desc(sure), dplyr::desc(abs(r)))
}


# -----------------------------------------------------------------------------
# hierarchy.R
# -----------------------------------------------------------------------------
# Correlation metrics along the hierarchy used in the data file and in
# Hlasny et al. (2025): the file's indicator columns group every insect code
#   level 1 (4 broad groups, as in the paper): guild x host type
#             borers on conifers / borers on broadleaves /
#             defoliators on conifers / defoliators on broadleaves
#   level 2 (host tree): borers on spruce / pine / fir / other conifers (larch),
#             defoliators on spruce / pine; broadleaf groups as in level 1
#   level 3: the insect codes themselves (species, genus, aggregate groups)
#
# Group value in a region-year = sum of the damage of its recorded member codes
# (NA if none recorded). Units are the same within a guild (borers m3,
# defoliators ha). Zeros are real observations, as everywhere else.
# Correlations at levels 1-2 use the same metrics as the species level
# (R/metrics.R): same-year r of z_within and beyond-shared-bad-years r of the
# anomaly, with region block-bootstrap intervals.

hierarchy_table <- function(species_meta) {
  f <- species_meta$flags
  has <- function(x) grepl(x, f, fixed = TRUE)
  level1 <- dplyr::case_when(
    has("Bark borers on conifers") ~ "borers on conifers",
    has("Bark borers on decidous") ~ "borers on broadleaves",
    has("Defoliators on conifers") ~ "defoliators on conifers",
    has("Defoliators on decidous") ~ "defoliators on broadleaves"
  )
  level2 <- dplyr::case_when(
    has("Bark borers on spruce") ~ "borers on spruce",
    has("Bark borers on pine") ~ "borers on pine",
    has("Bark borers on fir") ~ "borers on fir",
    level1 == "borers on conifers" ~ "borers on other conifers",
    has("Defoliators on spruce") ~ "defoliators on spruce",
    has("Defoliators on pine") ~ "defoliators on pine",
    TRUE ~ level1
  )
  tibble::tibble(species = species_meta$species, unit = species_meta$unit,
                 level1 = level1, level2 = level2)
}

# Sum member codes into group cells (cells-like table for make_transforms)
aggregate_cells <- function(cells, hier, level) {
  cells |>
    dplyr::left_join(dplyr::select(hier, species, group = dplyr::all_of(level)), by = "species") |>
    dplyr::filter(status != "not_monitored") |>
    dplyr::group_by(country, nuts_id, year, group, unit) |>
    dplyr::summarise(damage = sum(damage), n_codes = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(status = dplyr::if_else(damage > 0, "observed_positive", "observed_zero")) |>
    dplyr::rename(species = group)
}

# Pairwise metrics for one level (no transferability step at group level)
level_correlations <- function(cells, hier, level) {
  gc <- aggregate_cells(cells, hier, level)
  tr <- make_transforms(gc, min_nonzero = 0)
  groups <- sort(unique(gc$species))
  ts0 <- tibble::tibble(version = character(), species_a = character(), species_b = character(),
                        transferability = character(), countries_n10 = integer())
  correlation_metrics(tr, groups, ts0) |>
    dplyr::select(-transferability, -countries_n10) |>
    dplyr::mutate(level = level, .before = 1)
}

# Paper-style check: Europe-wide yearly totals (sum over all regions recorded
# that year), log(1 + x), Pearson r over the 23 years
hier_europe_correlations <- function(cells, hier, level) {
  w <- aggregate_cells(cells, hier, level) |>
    dplyr::group_by(species, year) |>
    dplyr::summarise(total = log1p(sum(damage)), .groups = "drop") |>
    tidyr::pivot_wider(names_from = species, values_from = total)
  g <- setdiff(names(w), "year")
  tidyr::expand_grid(species_a = g, species_b = g) |>
    dplyr::filter(species_a < species_b) |>
    dplyr::mutate(level = level,
                  r_europe_total = purrr::map2_dbl(species_a, species_b, function(a, b) {
                    ok <- !is.na(w[[a]]) & !is.na(w[[b]])
                    if (sum(ok) >= 10) stats::cor(w[[a]][ok], w[[b]][ok]) else NA_real_
                  }))
}

# Do insects in the same group move together more than insects in different
# groups? Species pairs are classed by the lowest level they share; the mean r
# per class is compared with 999 random relabellings of the codes (labels
# shuffled among codes, keeping group sizes).
hierarchy_pair_test <- function(cm, hier, var = "same_year", n_perm = 999, seed = 20261013) {
  d <- cm |> dplyr::filter(!is.na(.data[[var]])) |>
    dplyr::select(species_a, species_b, r = dplyr::all_of(var))
  cls <- function(h) {
    l1 <- stats::setNames(h$level1, h$species); l2 <- stats::setNames(h$level2, h$species)
    dplyr::case_when(
      l2[d$species_a] == l2[d$species_b] ~ "same host-tree group (level 2)",
      l1[d$species_a] == l1[d$species_b] ~ "same broad group only (level 1)",
      TRUE ~ "different broad groups"
    )
  }
  obs <- c(tapply(d$r, cls(hier), mean))
  set.seed(seed)
  perm <- replicate(n_perm, {
    h <- hier; i <- sample(nrow(h)); h$level1 <- h$level1[i]; h$level2 <- h$level2[i]
    m <- c(tapply(d$r, factor(cls(h), names(obs)), mean)); unname(m[names(obs)])
  })
  tibble::tibble(metric = var, pair_class = names(obs), n_pairs = as.integer(table(cls(hier))[names(obs)]),
                 mean_r = as.numeric(obs),
                 mean_r_random = rowMeans(perm, na.rm = TRUE),
                 p_value = (rowSums(perm >= obs, na.rm = TRUE) + 1) / (n_perm + 1))
}


# -----------------------------------------------------------------------------
# cm_basic.R
# -----------------------------------------------------------------------------
# Correlation metrics, back to basics (Oct 2026).
#
# Purpose: test the core premise of the thesis, "damage from different agents
# tends to fluctuate together, so the existing records carry information about
# the missing ones", insect code vs insect code (all 49 codes, 1,176 pairs).
#
# Data: value1 (value2 = the same damage in another unit, not used); 0 =
# monitored, no damage; empty / not reported = unknown (never treated as 0).
# Codes as reported, except the CZ spruce bark-borer group, counted as ips_typ
# as in Hlasny et al. (2025) (decision Oct 2026).
# Only region-years where both codes are recorded. Each region-code series is
# standardised against its own mean (z_within) so regions of any size compare.
# Pearson r with 95% ranges from resampling whole regions (999 times).
#
# Five questions per pair:
#   Q1 same_year   : do they have bad years together?
#   Q2 beyond_year : still linked after removing Europe-wide bad years?
#   Q3 lag_ab/ba   : does one's bad year come before the other's (t -> t+1),
#                    beyond the follower's own persistence?
#   Q4 transfer    : does the link hold in each country separately?
#   Q5 structure   : do linked codes share a broad group / host tree?
# Q1-Q3: correlation_metrics() (R/metrics.R); Q4: per_country_correlations(),
# loco_correlations(), transferability_summary() (R/correlate.R);
# Q5: hierarchy_pair_test() (R/hierarchy.R).


# One clean table, one row per pair
cm_basic_table <- function(cm, hier) {
  h <- dplyr::select(hier, species, level1, level2)
  cm |>
    dplyr::left_join(dplyr::rename_with(h, ~ paste0(.x, "_a")), by = c("species_a" = "species_a")) |>
    dplyr::left_join(dplyr::rename_with(h, ~ paste0(.x, "_b")), by = c("species_b" = "species_b")) |>
    dplyr::mutate(
      shared_group = dplyr::case_when(
        level2_a == level2_b ~ "same host-tree group",
        level1_a == level1_b ~ "same broad group",
        TRUE ~ "different broad groups"),
      lead = dplyr::case_when(
        lag_ab_sure & !lag_ba_sure ~ paste(species_a, "leads", species_b),
        lag_ba_sure & !lag_ab_sure ~ paste(species_b, "leads", species_a),
        lag_ab_sure & lag_ba_sure ~ "both directions",
        TRUE ~ "none clear")
    ) |>
    dplyr::transmute(
      species_a, species_b, group_a = level1_a, group_b = level1_b, shared_group,
      n_region_years = n, n_regions, n_countries,
      `Q1_same_year_r` = same_year, Q1_lo = same_year_lo, Q1_hi = same_year_hi,
      `Q2_beyond_shared_years_r` = beyond_year, Q2_lo = beyond_year_lo, Q2_hi = beyond_year_hi,
      Q3_lead = lead, Q3_r_a_then_b = lag_ab, Q3_r_b_then_a = lag_ba,
      Q4_holds_across_countries = transferability, Q4_countries = countries_n10,
      link)
}

# Order of codes for the matrix: by broad group, host-tree group, then name
cm_basic_order <- function(hier) {
  hier |>
    dplyr::arrange(factor(level1, c("borers on conifers", "borers on broadleaves",
                                    "defoliators on conifers", "defoliators on broadleaves")),
                   level2, species) |>
    dplyr::pull(species)
}


# One plain verdict per pair, answering "can we lean on this link to fill gaps?"
#   strong  = clear same-year link (95% range excludes 0) with |r| >= 0.3
#   usable     : strong, and same sign / similar size in every country checked (>= 2)
#   promising  : strong, but recorded together in < 2 countries, so untested
#   unreliable : strong, but the link differs between countries
#   weak       : clear but |r| < 0.3
#   no link / not enough data
verdict_levels <- c("usable", "promising", "unreliable", "weak", "no link", "not enough data")

cm_verdict <- function(t) {
  strong <- c("strong, beyond shared bad years", "strong, mostly shared bad years")
  t |>
    dplyr::mutate(verdict = dplyr::case_when(
      link == "not enough data" ~ "not enough data",
      link == "no clear link" ~ "no link",
      link == "weak but real" ~ "weak",
      link %in% strong & Q4_holds_across_countries == "consistent" ~ "usable",
      link %in% strong & Q4_holds_across_countries == "insufficient data" ~ "promising",
      link %in% strong ~ "unreliable"),
      verdict = factor(verdict, verdict_levels),
      # TRUE when the link is mostly years that were bad all over Europe
      mostly_europe_wide_years = link == "strong, mostly shared bad years") |>
    dplyr::select(species_a, species_b, group_a, group_b, verdict,
                  r = Q1_same_year_r, r_lo = Q1_lo, r_hi = Q1_hi,
                  r_without_europe_wide_years = Q2_beyond_shared_years_r, mostly_europe_wide_years,
                  countries = Q4_countries, lead = Q3_lead, n_region_years, n_regions, n_countries)
}


# -----------------------------------------------------------------------------
# cm_spearman.R
# -----------------------------------------------------------------------------
# Spearman (rank) version of the correlation metrics (Oct 2026).
#
# Same pipeline as the Pearson metrics, but each region-insect series is first
# turned into ranks (1 = that region's lowest year, ties averaged) and then
# standardised. A Pearson r on these within-region ranks is a within-region
# Spearman correlation: it asks whether two insects' years are ORDERED the
# same way, ignoring how extreme the outbreak years are. This makes the
# metric robust to a few huge outbreak years dominating a series.
# The Europe-wide-year removal (anomaly) is rebuilt on the rank scale exactly
# as in make_transforms(). Ranks are deterministic; the bootstrap inside
# correlation_metrics() sets its own seed.

rank_transforms <- function(tr) {
  tr |>
    dplyr::group_by(country, nuts_id, species) |>
    dplyr::mutate(
      rk = rank(log_damage, ties.method = "average"),
      z_within = if (dplyr::n() >= 3 && stats::sd(rk) > 0) (rk - mean(rk)) / stats::sd(rk) else NA_real_
    ) |>
    dplyr::group_by(species, year) |>
    dplyr::mutate(
      k = sum(!is.na(z_within)),
      other_mean = dplyr::if_else(k > 1, (sum(z_within, na.rm = TRUE) - z_within) / (k - 1), NA_real_),
      anomaly = z_within - other_mean
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-rk, -k, -other_mean)
}

# Pearson vs Spearman verdicts side by side, one row per pair
compare_rank_verdicts <- function(vp, vs) {
  dplyr::inner_join(
    dplyr::select(vp, species_a, species_b, verdict_pearson = verdict, r_pearson = r),
    dplyr::select(vs, species_a, species_b, verdict_spearman = verdict, r_spearman = r),
    by = c("species_a", "species_b"))
}


# -----------------------------------------------------------------------------
# cm_predict.R
# -----------------------------------------------------------------------------
# Predictive correlation metric (Oct 2026).
#
# Question: if a country never reported insect B, do its records of insect A
# help us guess B's ups and downs there? This is exactly what gap-filling asks.
#
# Test (leave one country out): for every ordered pair A -> B and every country
# c where both are recorded together, learn the link from the OTHER countries
# and predict B in c from A in the same region-years. Values are z_within
# (log(1 + damage), each region series against its own mean), so the test is
# about timing (which years are high), not about levels.
#
#   skill = 1 - error(guess using A) / error(guess "normal year")
#     > 0 : A helps;  0 : no help;  < 0 : A misleads (the link does not transfer)
#   skill_beyond_year = the same, but the baseline already knows B's
#     Europe-wide yearly mean (from the other countries), so it asks whether A
#     adds information beyond years that were bad all over Europe.
#
# predictive_by_country(): one row per ordered pair x held-out country.
# predictive_summary(): errors summed over all held-out countries (pooled
# skill) and the share of held-out countries where A helped. No randomness,
# so no seed is needed.

predictive_by_country <- function(tr, species, min_test = 10, min_train = 20) {
  w <- tr |>
    dplyr::filter(!is.na(z_within)) |>
    dplyr::select(country, nuts_id, year, species, z_within) |>
    tidyr::pivot_wider(names_from = species, values_from = z_within)
  pairs <- tidyr::expand_grid(predictor = species, target = species) |>
    dplyr::filter(predictor != target, predictor %in% names(w), target %in% names(w))

  purrr::pmap_dfr(pairs, function(predictor, target) {
    d <- tibble::tibble(country = w$country, year = w$year, a = w[[predictor]], b = w[[target]]) |>
      dplyr::filter(!is.na(a), !is.na(b))
    cs <- d |> dplyr::count(country) |> dplyr::filter(n >= min_test) |> dplyr::pull(country)
    if (length(cs) < 2) return(NULL)
    purrr::map_dfr(cs, function(c) {
      trn <- dplyr::filter(d, country != c); tst <- dplyr::filter(d, country == c)
      if (nrow(trn) < min_train || stats::sd(trn$a) == 0) return(NULL)
      # Europe-wide yearly mean of B, from the other countries only
      ym <- trn |> dplyr::group_by(year) |> dplyr::summarise(ym = mean(b), .groups = "drop")
      trn <- dplyr::left_join(trn, ym, by = "year")
      tst <- dplyr::left_join(tst, ym, by = "year") |> dplyr::mutate(ym = dplyr::coalesce(ym, 0))
      f1 <- stats::lm(b ~ a, trn)                 # A only
      f0 <- stats::lm(b ~ ym, trn)                # Europe-wide yearly mean only
      f2 <- stats::lm(b ~ a + ym, trn)            # A on top of the yearly mean
      tibble::tibble(predictor, target, held_out = c, n = nrow(tst),
                     e_normal = sum(tst$b^2),     # "normal year" guess = 0
                     e_a = sum((tst$b - stats::predict(f1, tst))^2),
                     e_year = sum((tst$b - stats::predict(f0, tst))^2),
                     e_year_a = sum((tst$b - stats::predict(f2, tst))^2),
                     skill = 1 - e_a / e_normal)
    })
  })
}

# One row per unordered pair (both directions pooled) with a plain verdict:
#   predicts well     : pooled skill >= 0.05 and A helped in >= 75% of held-out countries
#   predicts a little : pooled skill > 0 and A helped in at least half
#   misleads          : pooled skill < -0.05 (the link learnt elsewhere points the wrong way)
#   no help           : the rest;  cannot test : recorded together in < 2 countries
predictive_summary <- function(pc, verdict) {
  s <- pc |>
    dplyr::mutate(species_a = pmin(predictor, target), species_b = pmax(predictor, target)) |>
    dplyr::group_by(species_a, species_b) |>
    dplyr::summarise(n_countries_tested = dplyr::n_distinct(held_out),
                     skill = 1 - sum(e_a) / sum(e_normal),
                     skill_beyond_year = 1 - sum(e_year_a) / sum(e_year),
                     helped_share = mean(e_a < e_normal),
                     countries_tested = paste(sort(unique(held_out)), collapse = ","),
                     .groups = "drop")
  verdict |>
    dplyr::select(species_a, species_b, group_a, group_b, verdict, r) |>
    dplyr::left_join(s, by = c("species_a", "species_b")) |>
    dplyr::mutate(predictive = dplyr::case_when(
      is.na(skill) | n_countries_tested < 2 ~ "cannot test",
      skill >= 0.05 & helped_share >= 0.75 ~ "predicts well",
      skill > 0 & helped_share >= 0.5 ~ "predicts a little",
      skill < -0.05 ~ "misleads",
      TRUE ~ "no help"),
      predictive = factor(predictive, c("predicts well", "predicts a little", "no help", "misleads", "cannot test")))
}


# -----------------------------------------------------------------------------
# partner_eq.R
# -----------------------------------------------------------------------------
# The pairwise equation from the original notes, fitted for the usable pairs:
#   log(B + 1) = a + b * log(A + 1)
# on all region-years where both insects are recorded (value1, zeros kept,
# CZ spruce group counted as ips_typ). This is the equation the partner
# method uses for gap-filling (partner_predict(), R/gapfill_compare.R).
# Fitted in both directions, since predicting B from A is not the same line
# as predicting A from B. Ordinary least squares; no randomness.

partner_equations <- function(tr, verdict) {
  w <- tr |>
    dplyr::select(country, nuts_id, year, species, log_damage) |>
    tidyr::pivot_wider(names_from = species, values_from = log_damage)
  u <- dplyr::filter(verdict, verdict == "usable")
  fit_one <- function(x, y) {
    f <- stats::lm(y ~ x); s <- summary(f)
    c(a = unname(stats::coef(f)[1]), b = unname(stats::coef(f)[2]),
      b_se = unname(s$coefficients[2, 2]), r2 = s$r.squared)
  }
  purrr::pmap_dfr(u[, c("species_a", "species_b", "r")], function(species_a, species_b, r) {
    ok <- !is.na(w[[species_a]]) & !is.na(w[[species_b]])
    xa <- w[[species_a]][ok]; xb <- w[[species_b]][ok]
    ab <- fit_one(xa, xb)   # B from A
    ba <- fit_one(xb, xa)   # A from B
    tibble::tibble(species_a, species_b, n_region_years = sum(ok),
                   n_countries = dplyr::n_distinct(w$country[ok]),
                   r_within_region = r,
                   a_B_from_A = ab[["a"]], b_B_from_A = ab[["b"]], b_se_B_from_A = ab[["b_se"]],
                   a_A_from_B = ba[["a"]], b_A_from_B = ba[["b"]], b_se_A_from_B = ba[["b_se"]],
                   r2 = ab[["r2"]])
  }) |>
    dplyr::arrange(dplyr::desc(r_within_region))
}


# -----------------------------------------------------------------------------
# ranking.R
# -----------------------------------------------------------------------------
# Species - NUTS occurrence ranking (suggested by T. Hlasny).
#
# "Occurs" in a region = at least one year with damage > 0.
# "Monitored" in a region = at least one year with a reported value (0 or > 0).
# Damage totals are only comparable within a unit (borers m3, defoliators ha),
# so shares and within-region ranks are computed per unit.
# Region counts mix NUTS levels: 11 countries report one national series, which
# counts as one region (column n_national_units).

# PL uses 3-letter district codes (BIA, GDA, ...) aggregated to NUTS2 level
# (Hlasny et al. 2025, Sect. 2.2), so country is needed as well as the code.
nuts_level <- function(nuts_id, country) {
  dplyr::case_when(
    country == "PL"        ~ "NUTS2",
    grepl(";", nuts_id)    ~ "NUTS1 (merged)",
    nchar(nuts_id) == 2    ~ "national",
    nchar(nuts_id) == 3    ~ "NUTS1",
    nchar(nuts_id) == 4    ~ "NUTS2",
    nchar(nuts_id) == 5    ~ "NUTS3",
    TRUE ~ NA_character_
  )
}

# Per region-species summary (observed cells only)
region_species_summary <- function(cells, min_nonzero = 6) {
  cells |>
    dplyr::filter(status != "not_monitored") |>
    dplyr::group_by(country, nuts_id, species, unit) |>
    dplyr::summarise(
      years_monitored   = dplyr::n(),
      years_with_damage = sum(damage > 0),
      first_year = min(year), last_year = max(year),
      total_damage = sum(damage),
      max_damage   = max(damage),
      peak_year    = if (max(damage) > 0) year[which.max(damage)] else NA_integer_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      nuts_level = nuts_level(nuts_id, country),
      occurs = years_with_damage > 0,
      usable_series = years_with_damage >= min_nonzero   # Hlasny et al. (2025) filter
    )
}

# Table 1: species ranked by number of NUTS units where they occur
species_occurrence_ranking <- function(rs, species_meta) {
  rs |>
    dplyr::group_by(species, unit) |>
    dplyr::summarise(
      n_countries_monitored = dplyr::n_distinct(country),
      n_countries_occurring = dplyr::n_distinct(country[occurs]),
      countries_occurring   = paste(sort(unique(country[occurs])), collapse = ","),
      n_regions_monitored   = dplyr::n(),
      n_regions_occurring   = sum(occurs),
      n_national_units      = sum(occurs & nuts_level == "national"),
      n_usable_series       = sum(usable_series),
      region_years_with_damage = sum(years_with_damage),
      total_damage          = sum(total_damage),
      .groups = "drop"
    ) |>
    dplyr::group_by(unit) |>
    dplyr::mutate(pct_of_unit_total = 100 * total_damage / sum(total_damage)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pct_regions_occurring = 100 * n_regions_occurring / n_regions_monitored,
      rank_by_regions = rank(-n_regions_occurring, ties.method = "min"),
      rank_by_damage_within_unit = NA_integer_
    ) |>
    dplyr::group_by(unit) |>
    dplyr::mutate(rank_by_damage_within_unit = as.integer(rank(-total_damage, ties.method = "min"))) |>
    dplyr::ungroup() |>
    dplyr::left_join(dplyr::select(species_meta, species, guild = guild_from_file,
                                   host_tree, is_aggregate), by = "species") |>
    dplyr::arrange(rank_by_regions, dplyr::desc(total_damage)) |>
    dplyr::relocate(rank_by_regions, species, guild, host_tree, unit)
}

# Table 2: within each region, species ranked by total damage (per unit)
region_species_ranking <- function(rs) {
  rs |>
    dplyr::group_by(country, nuts_id, unit) |>
    dplyr::mutate(
      rank_in_region = as.integer(rank(-total_damage, ties.method = "min")),
      pct_of_region_unit_total = if (sum(total_damage) > 0)
        100 * total_damage / sum(total_damage) else NA_real_
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(country, nuts_id, unit, rank_in_region) |>
    dplyr::select(country, nuts_id, nuts_level, unit, rank_in_region, species,
                  total_damage, pct_of_region_unit_total, years_with_damage, years_monitored,
                  first_year, last_year, peak_year, usable_series)
}

# Table 3: region x species matrix of years with damage (blank = not monitored)
occurrence_matrix <- function(rs, sp_rank) {
  rs |>
    dplyr::mutate(species = factor(species, sp_rank$species)) |>
    dplyr::arrange(species) |>
    dplyr::select(country, nuts_id, nuts_level, species, years_with_damage) |>
    tidyr::pivot_wider(names_from = species, values_from = years_with_damage) |>
    dplyr::arrange(country, nuts_id)
}

write_ranking_xlsx <- function(sp_rank, reg_rank, occ, path) {
  readme <- tibble::tibble(
    sheet = c("species_ranking", "region_ranking", "occurrence_matrix", "", "Definitions"),
    description = c(
      "Species ranked by the number of NUTS units where they occur (>= 1 year with damage > 0). Damage ranks and shares are within unit (m3 for borers, ha for defoliators).",
      "Within each NUTS unit, species ranked by total damage 2000-2022, separately per unit.",
      "Number of years with damage > 0 for each NUTS unit x species. Blank = not monitored. 0 = monitored, never any damage.",
      "",
      "Occurs = at least one year with damage > 0. usable_series = at least 6 years with damage > 0 (filter of Hlasny et al. 2025). NUTS levels differ by country: 11 countries report one national series. CZ spruce bark-borer group counted as Ips typographus; CZ sub-regions summed to NUTS2."
    )
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(list(README = readme, species_ranking = sp_rank,
                           region_ranking = reg_rank, occurrence_matrix = occ), path)
  path
}

# ---- Species-NUTS occurrence ranking, version 2 (Oct 2026) --------------------
# Built directly from the raw long table, codes as reported except the CZ
# spruce bark-borer group, counted as ips_typ as in Hlasny et al. (2025).
#   occurs in a NUTS unit = damage (value1) > 0 in at least one year 2000-2022
# Code type:
#   aggregate group : codes combining several insects ("&") and Def_dec (all
#                     defoliators on broadleaves); content can differ between
#                     countries, so not comparable as one insect
#   single insect   : species codes and genus-level codes (_sp / _spp), which
#                     countries report as one insect
code_type <- function(species) {
  dplyr::if_else(grepl("&", species) | species == "Def_dec", "aggregate group", "single insect")
}

occurrence_ranking_v2 <- function(long, species_meta, recode = group_to_species) {
  long |>
    dplyr::left_join(dplyr::select(recode, country, species = from, to), by = c("country", "species")) |>
    dplyr::mutate(species = dplyr::coalesce(to, species)) |>
    dplyr::filter(!is.na(damage)) |>
    dplyr::group_by(species, country, nuts_id) |>
    dplyr::summarise(occurs = any(damage > 0), .groups = "drop") |>
    dplyr::group_by(species) |>
    dplyr::summarise(n_units_with_damage = sum(occurs),
                     n_units_reported = dplyr::n(),
                     n_countries_with_damage = dplyr::n_distinct(country[occurs]),
                     .groups = "drop") |>
    dplyr::left_join(dplyr::select(species_meta, species, guild = guild_from_file, host_tree, unit),
                     by = "species") |>
    dplyr::mutate(code_type = code_type(species)) |>
    dplyr::arrange(dplyr::desc(n_units_with_damage), species) |>
    dplyr::mutate(rank = rank(-n_units_with_damage, ties.method = "min")) |>
    dplyr::relocate(rank, species, code_type, guild, host_tree, unit)
}


# -----------------------------------------------------------------------------
# hosts.R
# -----------------------------------------------------------------------------
# Host-tree abundance per region (Appendix "NUTS_Forests" of Hlasny et al. 2025,
# received Oct 2026) and its use in gap-filling.
#
# The appendix gives forest area (ha) and growing stock (m3) for Norway spruce,
# Scots pine, oaks and fir, plus totals for all conifers and all broadleaves,
# per NUTS region (CZ, DE, PL, AT, FI) or per country (other countries).
#
# Decisions (to confirm with the data compilers):
#  - Area (ha) is the host measure. Volumes are not used: for BG and LT the
#    species volumes are about 1000x too small relative to area (probably
#    reported in 1000 m3).
#  - "-" is read as "host tree absent" (area 0). This can be switched off with
#    dash_means_absent = FALSE, in which case "-" means "no data".
#  - Definitions of area differ: FI counts only stands where the species
#    dominates (>75%), SE areas are derived from volume, SI species areas look
#    like "area where present". Use areas as approximate host amounts.
#  - Code fixes: CR -> HR, SL -> SI; DEB + DEC summed to match "DEB; DEC".
#    DE4 holds Brandenburg + Berlin (DE3 has no own row); CZ01, DE5, DE6 are missing.

host_cols <- c("spruce", "pine", "oak", "fir", "conifers_all", "broadleaves_all")

read_host_appendix <- function(path) {
  sheets <- setdiff(readxl::excel_sheets(path), "DataOrigin_Quality")
  purrr::map_dfr(sheets, function(sh) {
    x <- readxl::read_excel(path, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal")
    x <- x[-(1:2), 1:14]
    names(x) <- c("region_name", "code", paste0(rep(host_cols, each = 2), c("_ha", "_m3")))
    x |>
      dplyr::filter(!is.na(code), !is.na(spruce_ha), !grepl("^(Source|Notes)", region_name)) |>
      dplyr::mutate(sheet = sh, code = trimws(code), region_name = trimws(gsub("\\*", "", region_name)))
  })
}

host_area_long <- function(appx, dash_means_absent = TRUE) {
  ha <- appx |>
    dplyr::select(sheet, region_name, code, dplyr::ends_with("_ha")) |>
    tidyr::pivot_longer(dplyr::ends_with("_ha"), names_to = "host_group", values_to = "raw") |>
    dplyr::mutate(
      host_group = sub("_ha$", "", host_group),
      dash = trimws(raw) %in% c("-", "–"),
      area_ha = suppressWarnings(as.numeric(raw)),
      area_ha = dplyr::if_else(dash & dash_means_absent, 0, area_ha),
      code = dplyr::recode(code, CR = "HR", SL = "SI", DEB = "DEB; DEC", DEC = "DEB; DEC")
    )
  # sum Rheinland-Pfalz (DEB) and Saarland (DEC) into the merged region
  ha |>
    dplyr::group_by(code, host_group) |>
    dplyr::summarise(area_ha = if (all(is.na(area_ha))) NA_real_ else sum(area_ha, na.rm = TRUE),
                     dash = all(dash), sheet = dplyr::first(sheet), .groups = "drop") |>
    dplyr::rename(nuts_id = code)
}

# Which host group of the appendix each insect's host tree corresponds to.
# Larch and minor broadleaves are not in the appendix: the conifer/broadleaf
# totals are used for scaling, but presence cannot be judged ("not specific").
host_group_of <- function(host_tree) {
  dplyr::case_when(
    host_tree == "Picea abies" ~ "spruce",
    host_tree == "Pinus sylvestris" ~ "pine",
    host_tree == "Quercus spp." ~ "oak",
    host_tree == "Abies alba" ~ "fir",
    host_tree == "Larix decidua" ~ "conifers_all",
    TRUE ~ "broadleaves_all"
  )
}

# Region x insect host information:
#   host_status: "present", "absent" (area 0, no recorded damage on that host),
#                "not specific" (larch / minor broadleaves), "no host data"
#   host_area_ha: area used to scale damage (specific host, else group total)
# Recorded damage on the same host tree in the region overrides "absent".
host_status_table <- function(cells, species_meta, host_long) {
  sp <- species_meta |>
    dplyr::transmute(species, host_tree, host_group = host_group_of(host_tree),
                     specific = host_group %in% c("spruce", "pine", "oak", "fir"))
  evidence <- cells |>
    dplyr::left_join(dplyr::select(species_meta, species, host_tree), by = "species") |>
    dplyr::group_by(nuts_id, host_tree) |>
    dplyr::summarise(damage_recorded_on_host = any(status == "observed_positive"), .groups = "drop")
  regions <- dplyr::distinct(cells, country, nuts_id)
  tidyr::crossing(regions, sp) |>
    dplyr::left_join(host_long |> dplyr::select(nuts_id, host_group, area_ha), by = c("nuts_id", "host_group")) |>
    dplyr::left_join(evidence, by = c("nuts_id", "host_tree")) |>
    dplyr::mutate(
      damage_recorded_on_host = dplyr::coalesce(damage_recorded_on_host, FALSE),
      host_status = dplyr::case_when(
        !nuts_id %in% host_long$nuts_id ~ "no host data",
        !specific ~ "not specific",
        damage_recorded_on_host ~ "present",
        is.na(area_ha) ~ "no host data",
        area_ha > 0 ~ "present",
        TRUE ~ "absent"
      ),
      host_area_ha = area_ha
    ) |>
    dplyr::select(country, nuts_id, species, host_tree, host_group, host_status, host_area_ha, damage_recorded_on_host)
}

# Host-scaled matrix: log(1 + 1000 * damage / host area) = damage per 1000 ha
# of host forest. Rows of regions without a usable host area are left empty.
host_scaled_L <- function(L, hs) {
  A <- host_area_matrix(L, hs)
  Ys <- log1p(1000 * expm1(L$Y) / A)
  Ys[!is.finite(Ys)] <- NA
  L2 <- L; L2$Y <- Ys; L2$area <- A
  # covariate for "any damage?": standardised log host area, 0 where unknown
  X <- log(A); X[!is.finite(X)] <- NA
  X <- (X - mean(X, na.rm = TRUE)) / stats::sd(X, na.rm = TRUE); X[is.na(X)] <- 0
  L2$X <- X
  L2
}

host_area_matrix <- function(L, hs) {
  key <- paste(hs$nuts_id, hs$species)
  area <- stats::setNames(hs$host_area_ha, key)
  A <- matrix(NA_real_, nrow(L$Y), ncol(L$Y))
  for (s in seq_along(L$species)) A[, s] <- area[paste(L$rows$nuts_id, L$species[s])]
  A[!is.na(A) & A <= 0] <- NA   # absent host: damage is structurally zero, not scaled
  A
}

# Mark "impossible" cells: not monitored and host absent
apply_host_status <- function(cells, hs) {
  cells |>
    dplyr::left_join(dplyr::select(hs, nuts_id, species, host_status), by = c("nuts_id", "species")) |>
    dplyr::mutate(
      host_absent = host_status == "absent",
      status = dplyr::if_else(status == "not_monitored" & host_absent, "impossible", status)
    )
}

# ---- Host-aware two-part model ----------------------------------------------
# Same two parts as R/hurdle.R, with host information added:
#   - absent host tree  -> damage is 0 (no prediction needed)
#   - Part 1 "any damage?" also sees log host area (one slope per insect)
#   - Part 2 "how much?" models damage per 1000 ha of host forest, then
#     multiplies back by the region's host area. Where host area is unknown,
#     the plain Part 2 prediction (R/hurdle.R) is used.
host_absent_matrix <- function(L, hs) {
  st <- stats::setNames(hs$host_status, paste(hs$nuts_id, hs$species))
  Z <- matrix(FALSE, nrow(L$Y), ncol(L$Y))
  for (s in seq_along(L$species)) Z[, s] <- st[paste(L$rows$nuts_id, L$species[s])] %in% "absent"
  Z
}

# Natural-scale prediction of the host-aware model.
host_combine <- function(p1, a_scaled, sm_scaled, a_plain, sm_plain, area, absent) {
  amount <- ifelse(!is.na(area) & !is.na(a_scaled),
                   pmax(expm1(a_scaled) * sm_scaled, 0) * area / 1000,
                   pmax(expm1(a_plain) * sm_plain, 0))
  ifelse(absent, 0, p1 * amount)
}

# Cross-validation on the same hidden blocks as h_cv (R/hurdle.R), with the
# chosen sizes of each part. Returns h_cv's hidden cells with host columns added.
host_cv <- function(L, Lh, absent, blocks, h_cv, best1, best2) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ytr <- L$Y; Ytr[H] <- NA
    B <- hurdle_parts(Ytr)$B
    Ys <- Lh$Y; Ys[H] <- NA
    Y2s <- hurdle_parts(Ys)$Y2
    f1 <- logit_fit(B, best1$K, best1$lambda, X = Lh$X)
    f2 <- lfm_fit(Y2s, K = best2$K, lambda = best2$lambda)
    idx <- which(H, arr.ind = TRUE)
    out[[length(out) + 1]] <- tibble::tibble(
      fold = f, row = idx[, 1], col = idx[, 2],
      p1_host = logit_predict(f1)[idx], p2_scaled = lfm_predict(f2)[idx],
      sm_scaled = smearing(f2, Y2s), area = Lh$area[idx], absent = absent[idx])
  }
  dplyr::bind_rows(out) |> dplyr::inner_join(h_cv, by = c("fold", "row", "col"))
}

# Natural-scale comparison of: one-part, two-part (no hosts), host-aware two-part.
host_compare <- function(hc, best1, best2, hs) {
  d <- hc |>
    dplyr::mutate(
      p1 = .data[[paste0("p1|", best1)]], a = .data[[paste0("p2|", best2)]], sm = .data[[paste0("sm|", best2)]],
      real = expm1(observed),
      one = pmax(expm1(one_part), 0),
      two = p1 * pmax(expm1(a) * sm, 0),
      host = host_combine(p1_host, p2_scaled, sm_scaled, a, sm, area, absent)
    ) |>
    dplyr::filter(!is.na(one), !is.na(two), !is.na(host)) |>
    dplyr::left_join(dplyr::select(hs, nuts_id, species, host_status), by = c("nuts_id", "species"))
  tot <- d |>
    dplyr::group_by(fold, country, species, year) |>
    dplyr::summarise(dplyr::across(c(real, one, two, host), sum), .groups = "drop") |>
    dplyr::filter(real > 0)
  lr <- function(x) abs(log((x + 1) / (tot$real + 1)))
  models <- c("One-part factor model", "Two-part model", "Two-part model + host trees")
  list(
    cells = d,
    summary = tibble::tibble(
      model = models,
      zeros_called_zero = c(mean(d$one[d$real == 0] < 1), mean(d$p1[d$real == 0] < 0.5),
                            mean(d$absent[d$real == 0] | d$p1_host[d$real == 0] < 0.5)),
      auc_any = c(auc(d$one, as.numeric(d$real > 0)), auc(d$p1, as.numeric(d$real > 0)),
                  auc(ifelse(d$absent, 0, d$p1_host), as.numeric(d$real > 0))),
      median_abs_log_ratio_totals = c(stats::median(lr(tot$one)), stats::median(lr(tot$two)), stats::median(lr(tot$host))),
      share_totals_within_2x = c(mean(lr(tot$one) <= log(2)), mean(lr(tot$two) <= log(2)), mean(lr(tot$host) <= log(2))),
      n_cells = nrow(d), n_blocks = nrow(tot)
    ),
    by_species = d |>
      dplyr::group_by(species) |>
      dplyr::summarise(
        n_cells = dplyr::n(),
        auc_host = auc(ifelse(absent, 0, p1_host), as.numeric(real > 0)),
        rmse_host = sqrt(mean((log1p(host) - observed)^2)),
        rmse_two = sqrt(mean((log1p(two) - observed)^2)),
        rmse_base = sqrt(mean((base_sy - observed)^2, na.rm = TRUE)),
        .groups = "drop"
      )
  )
}

# Final host-aware fit on all recorded data
host_final <- function(L, Lh, h_final, best1, best2) {
  f1 <- logit_fit(hurdle_parts(L$Y)$B, best1$K, best1$lambda, X = Lh$X)
  Y2s <- hurdle_parts(Lh$Y)$Y2
  f2 <- lfm_fit(Y2s, K = best2$K, lambda = best2$lambda)
  P2s <- lfm_predict(f2); ok <- !is.na(Y2s) & !is.na(P2s)
  list(P1 = logit_predict(f1), P2s = P2s, smear_s = mean(exp(Y2s[ok] - P2s[ok])),
       sigma_s = stats::sd(Y2s[ok] - P2s[ok]), plain = h_final, area = Lh$area)
}

# Confidence per insect, same rule as hurdle_confidence() but for this model
host_confidence <- function(by_species, species) {
  tibble::tibble(species = species) |>
    dplyr::left_join(by_species, by = "species") |>
    dplyr::mutate(confidence = dplyr::case_when(
      is.na(n_cells) ~ "untested",
      rmse_host < rmse_base & !is.na(auc_host) & auc_host >= 0.7 ~ "works",
      TRUE ~ "weak"
    ))
}

# Harmonised table with host information (replaces hurdle_fill output)
host_fill <- function(L, fin, absent, conf, cells, hs, unit_lookup) {
  n <- nrow(L$Y); p <- ncol(L$Y); z <- stats::qnorm(0.9)
  mon_cs <- cells |> dplyr::group_by(country, species) |>
    dplyr::summarise(ever = any(status != "not_monitored"), .groups = "drop")
  ix <- cbind(rep(seq_len(n), p), rep(seq_len(p), each = n))
  tibble::tibble(row = ix[, 1], col = ix[, 2]) |>
    dplyr::mutate(
      country = L$rows$country[row], nuts_id = L$rows$nuts_id[row], year = L$rows$year[row],
      species = L$species[col], y = L$Y[ix], absent = absent[ix],
      p_any = fin$P1[ix], area = fin$area[ix], as = fin$P2s[ix], ap = fin$plain$P2[ix]
    ) |>
    dplyr::left_join(mon_cs, by = c("country", "species")) |>
    dplyr::left_join(dplyr::select(conf, species, confidence), by = "species") |>
    dplyr::left_join(dplyr::select(hs, nuts_id, species, host_status, host_area_ha), by = c("nuts_id", "species")) |>
    dplyr::left_join(unit_lookup, by = "species") |>
    dplyr::mutate(
      scaled = !is.na(area) & !is.na(as),
      value_type = dplyr::case_when(
        !is.na(y) & y > 0 ~ "observed damage",
        !is.na(y) ~ "observed zero",
        absent ~ "zero: host tree absent",
        is.na(p_any) | (is.na(as) & is.na(ap)) ~ "not fillable (nothing recorded in this region-year)",
        TRUE ~ "filled"
      ),
      fl = value_type == "filled",
      gap_type = dplyr::case_when(!fl ~ NA_character_,
                                  ever ~ "country monitors this insect, missed this year/region",
                                  TRUE ~ "country never monitored this insect"),
      amount_if_damaged = dplyr::case_when(
        fl & scaled ~ pmax(expm1(as) * fin$smear_s, 0) * area / 1000,
        fl ~ pmax(expm1(ap) * fin$plain$smear, 0)),
      amount_lo80 = dplyr::case_when(
        fl & scaled ~ pmax(expm1(as - z * fin$sigma_s), 0) * area / 1000,
        fl ~ pmax(expm1(ap - z * fin$plain$sigma2), 0)),
      amount_hi80 = dplyr::case_when(
        fl & scaled ~ expm1(as + z * fin$sigma_s) * area / 1000,
        fl ~ expm1(ap + z * fin$plain$sigma2)),
      p_any = dplyr::if_else(fl, p_any, NA_real_),
      damage = dplyr::case_when(
        value_type %in% c("observed damage", "observed zero") ~ expm1(y),
        value_type == "zero: host tree absent" ~ 0,
        fl ~ p_any * amount_if_damaged),
      amount_basis = dplyr::case_when(fl & scaled ~ "per host area", fl ~ "no host area (plain model)"),
      confidence = dplyr::if_else(fl, confidence, NA_character_)
    ) |>
    dplyr::select(country, nuts_id, year, species, unit, value_type, damage, p_any,
                  amount_if_damaged, amount_lo80, amount_hi80, gap_type, confidence,
                  host_status, host_area_ha, amount_basis)
}


# -----------------------------------------------------------------------------
# lfm.R
# -----------------------------------------------------------------------------
# Latent factor model (prototype) for harmonising / gap-filling EU-ForDAM.
#
# Data matrix Y: rows = region-years (all NUTS units x 2000-2022),
# columns = species; values = log(damage + 1) for observed cells (zeros are real
# observations: monitored, no damage), NA where not monitored.
#
# Model (low-rank matrix factorisation with biases):
#   y[i, s] = mu[s] + c[i] + sum_k U[i, k] * V[s, k] + error
#   mu[s] : species level             c[i] : overall damage intensity of region-year i
#   U[i,] : region-year scores on K hidden factors
#   V[s,] : how strongly species s responds to each factor
# Fitted by alternating ridge regressions (ALS) on observed cells only.
# K = 0 gives the purely additive model (region-year intensity + species level).
#
# A gap cell (i, s) is predicted from region-year i's factor scores, estimated
# from the OTHER species recorded there, and species s's loadings, estimated
# from regions where s IS recorded.

build_lfm_matrix <- function(cells, tr) {
  rows <- cells |> dplyr::distinct(country, nuts_id, year) |> dplyr::arrange(country, nuts_id, year)
  sp <- sort(unique(tr$species))
  Y <- matrix(NA_real_, nrow(rows), length(sp), dimnames = list(NULL, sp))
  key <- paste(rows$nuts_id, rows$year)
  idx_r <- match(paste(tr$nuts_id, tr$year), key)
  idx_c <- match(tr$species, sp)
  Y[cbind(idx_r, idx_c)] <- tr$log_damage
  list(Y = Y, rows = rows, species = sp)
}

# Fit the model on the observed (non-NA) cells of Y.
# Optional soft hierarchy ("guild hint"): `group` gives each species' group;
# with tau > 0 each species' level and loadings are pulled towards its group's
# average (penalty tau * ||v_s - group mean||^2) instead of only towards 0.
# Species with much data follow their own pattern; species with little data
# lean on their group. tau = 0 gives exactly the plain model.
# tau sets the pull on the level; tau_links (default = tau) the pull on the
# loadings, so the two parts of the hint can be tested separately.
lfm_fit <- function(Y, K, lambda, lambda_c = 1, n_iter = 200, tol = 1e-6, seed = 20261009,
                    group = NULL, tau = 0, tau_links = tau) {
  set.seed(seed)
  n <- nrow(Y); p <- ncol(Y); M <- !is.na(Y)
  mu <- colMeans(Y, na.rm = TRUE); mu[is.na(mu)] <- 0
  cc <- rep(0, n)
  U <- matrix(0, n, K)
  V <- matrix(stats::rnorm(p * K, sd = 0.1), p, K)
  # rows sharing the same set of observed species are solved together
  pat <- apply(M, 1, function(m) paste(which(m), collapse = ","))
  groups <- split(seq_len(n), pat)
  empty_rows <- which(rowSums(M) == 0)
  pen_row <- diag(c(lambda_c, rep(lambda, K)), K + 1)
  pen_col <- diag(c(1e-8, rep(lambda, K)), K + 1)
  hier <- !is.null(group) && (tau > 0 || tau_links > 0)
  if (hier) {
    tvec <- c(tau, rep(tau_links, K))
    pen_col <- pen_col + diag(tvec, K + 1)
    gi <- split(seq_len(p), group)
  }
  prev <- Inf
  for (it in seq_len(n_iter)) {
    for (g in groups) {
      cols <- M[g[1], ]
      if (!any(cols)) next
      X <- cbind(1, V[cols, , drop = FALSE])
      R <- t(Y[g, cols, drop = FALSE]) - mu[cols]
      B <- solve(crossprod(X) + pen_row, crossprod(X, R))
      cc[g] <- B[1, ]
      if (K > 0) U[g, ] <- t(B[-1, , drop = FALSE])
    }
    if (hier) {   # group means of level and loadings (current values)
      Th <- cbind(mu, V)
      for (gg in gi) Th[gg, ] <- matrix(colMeans(Th[gg, , drop = FALSE]), length(gg), K + 1, byrow = TRUE)
    }
    for (s in seq_len(p)) {
      rr <- M[, s]
      if (!any(rr)) next
      X <- cbind(1, U[rr, , drop = FALSE])
      rhs <- crossprod(X, Y[rr, s] - cc[rr])
      if (hier) rhs <- rhs + tvec * Th[s, ]
      b <- solve(crossprod(X) + pen_col, rhs)
      mu[s] <- b[1]
      if (K > 0) V[s, ] <- b[-1]
    }
    fit <- outer(cc, mu, "+") + U %*% t(V)
    loss <- mean((Y[M] - fit[M])^2)
    if (abs(prev - loss) < tol) break
    prev <- loss
  }
  list(mu = mu, c = cc, U = U, V = V, K = K, lambda = lambda, iter = it,
       train_rmse = sqrt(loss), empty_rows = empty_rows)
}

lfm_predict <- function(fit) {
  P <- outer(fit$c, fit$mu, "+") + fit$U %*% t(fit$V)
  P[fit$empty_rows, ] <- NA  # no information about these region-years
  P
}

# ---- Cross-validation: hide whole country x species blocks -----------------
# Eligible block: the country records the species (usable series) and at least
# one other species, so the region-years still carry information, and the
# species is recorded in at least one other country, so there is something
# to learn its behaviour from (species recorded in one country cannot be tested).
# Blocks are spread round-robin over folds within each country, so a country
# never loses all its species in one fold.
make_cv_blocks <- function(L, n_folds = 10, seed = 20261009) {
  set.seed(seed)
  obs <- which(!is.na(L$Y), arr.ind = TRUE)
  blocks <- tibble::tibble(country = L$rows$country[obs[, 1]], species = L$species[obs[, 2]]) |>
    dplyr::distinct()
  n_sp <- blocks |> dplyr::count(country, name = "n_species_country")
  n_ct <- blocks |> dplyr::count(species, name = "n_countries_species")
  blocks |>
    dplyr::left_join(n_sp, by = "country") |>
    dplyr::left_join(n_ct, by = "species") |>
    dplyr::filter(n_species_country >= 2, n_countries_species >= 2) |>
    dplyr::group_by(country) |>
    dplyr::mutate(fold = ((sample.int(n_folds, 1) + sample(dplyr::n())) %% n_folds) + 1) |>
    dplyr::ungroup()
}

held_mask <- function(L, blocks_f) {
  H <- matrix(FALSE, nrow(L$Y), ncol(L$Y))
  for (j in seq_len(nrow(blocks_f))) {
    r <- L$rows$country == blocks_f$country[j]
    s <- match(blocks_f$species[j], L$species)
    H[r, s] <- !is.na(L$Y[r, s])
  }
  H
}

# Baselines, using training data only
baseline_species_mean <- function(Ytr) matrix(colMeans(Ytr, na.rm = TRUE), nrow(Ytr), ncol(Ytr), byrow = TRUE)

baseline_species_year <- function(Ytr, year) {
  P <- baseline_species_mean(Ytr)
  for (y in unique(year)) {
    r <- year == y
    m <- colMeans(Ytr[r, , drop = FALSE], na.rm = TRUE)
    P[r, ] <- matrix(ifelse(is.na(m), P[which(r)[1], ], m), sum(r), ncol(Ytr), byrow = TRUE)
  }
  P
}

# Best single helper: for each hidden block (country c, species s), choose the
# species recorded in c whose training correlation with s (other countries,
# >= 30 shared region-years) is highest; predict by linear regression.
# Falls back to the species-year baseline where the helper is missing.
baseline_best_helper <- function(Ytr, rows, blocks_f, species, fallback) {
  P <- fallback
  for (j in seq_len(nrow(blocks_f))) {
    in_c <- rows$country == blocks_f$country[j]
    s <- match(blocks_f$species[j], species)
    cand <- setdiff(which(colSums(!is.na(Ytr[in_c, , drop = FALSE])) > 0), s)
    best <- NULL; best_r <- -Inf
    for (h in cand) {
      ok <- !in_c & !is.na(Ytr[, s]) & !is.na(Ytr[, h])
      if (sum(ok) < 30 || stats::sd(Ytr[ok, h]) == 0 || stats::sd(Ytr[ok, s]) == 0) next
      r <- stats::cor(Ytr[ok, s], Ytr[ok, h])
      if (r > best_r) { best_r <- r; best <- list(h = h, fit = stats::lm(Ytr[ok, s] ~ Ytr[ok, h])) }
    }
    if (is.null(best)) next
    tgt <- in_c & !is.na(Ytr[, best$h])
    P[tgt, s] <- stats::coef(best$fit)[1] + stats::coef(best$fit)[2] * Ytr[tgt, best$h]
  }
  P
}

# Run CV for all model configurations and baselines. Returns one row per
# held-out cell and model.
lfm_cv <- function(L, blocks, grid) {
  folds <- sort(unique(blocks$fold))
  out <- list()
  for (f in folds) {
    bf <- dplyr::filter(blocks, fold == f)
    H <- held_mask(L, bf)
    Ytr <- L$Y; Ytr[H] <- NA
    idx <- which(H, arr.ind = TRUE)
    cell <- tibble::tibble(fold = f, row = idx[, 1], col = idx[, 2],
                           country = L$rows$country[idx[, 1]], nuts_id = L$rows$nuts_id[idx[, 1]],
                           year = L$rows$year[idx[, 1]], species = L$species[idx[, 2]],
                           observed = L$Y[idx])
    sy <- baseline_species_year(Ytr, L$rows$year)
    preds <- list(
      "Baseline: species average" = baseline_species_mean(Ytr),
      "Baseline: species x year average" = sy,
      "Baseline: best single helper" = baseline_best_helper(Ytr, L$rows, bf, L$species, sy)
    )
    for (g in seq_len(nrow(grid))) {
      fit <- lfm_fit(Ytr, K = grid$K[g], lambda = grid$lambda[g])
      preds[[grid$model[g]]] <- lfm_predict(fit)
    }
    for (m in names(preds)) out[[length(out) + 1]] <- dplyr::mutate(cell, model = m, predicted = preds[[m]][idx])
  }
  dplyr::bind_rows(out)
}

# Scores: RMSE on log(damage + 1); "dynamics r" = mean correlation between
# predicted and observed over time within each held-out region-species series
# (>= 6 years); skill = 1 - MSE / MSE of the species x year baseline.
lfm_scores <- function(cv, by = character()) {
  # compare models on the same cells: those every model could predict
  common <- cv |> dplyr::group_by(fold, row, col) |>
    dplyr::summarise(ok = all(!is.na(predicted)), .groups = "drop") |> dplyr::filter(ok)
  cv <- dplyr::semi_join(cv, common, by = c("fold", "row", "col"))
  ser <- cv |>
    dplyr::filter(!is.na(predicted)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model", by))), nuts_id, species) |>
    dplyr::summarise(r = if (dplyr::n() >= 6 && stats::sd(predicted) > 0 && stats::sd(observed) > 0)
                       stats::cor(predicted, observed) else NA_real_, .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model", by)))) |>
    dplyr::summarise(dynamics_r = mean(r, na.rm = TRUE), n_series = sum(!is.na(r)), .groups = "drop")
  base <- cv |>
    dplyr::filter(model == "Baseline: species x year average", !is.na(predicted)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(mse_base = mean((predicted - observed)^2), .groups = "drop")
  sc <- cv |>
    dplyr::filter(!is.na(predicted)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model", by)))) |>
    dplyr::summarise(n_cells = dplyr::n(), rmse = sqrt(mean((predicted - observed)^2)),
                     mse = mean((predicted - observed)^2), bias = mean(predicted - observed),
                     .groups = "drop") |>
    dplyr::left_join(ser, by = c("model", by))
  if (length(by)) sc <- dplyr::left_join(sc, base, by = by) else sc$mse_base <- base$mse_base
  sc |> dplyr::mutate(skill_vs_year_baseline = 1 - mse / mse_base) |> dplyr::select(-mse, -mse_base)
}

# Final fit on all data with the chosen configuration; factors rotated with
# varimax for interpretation (rotation does not change predictions).
lfm_final <- function(L, K, lambda) {
  fit <- lfm_fit(L$Y, K = K, lambda = lambda)
  if (K >= 2) {
    vm <- stats::varimax(fit$V, normalize = FALSE)
    fit$V <- fit$V %*% vm$rotmat
    fit$U <- fit$U %*% vm$rotmat
  }
  # orient each factor so that its largest loading is positive
  if (K >= 1) for (k in seq_len(K)) {
    sg <- sign(fit$V[which.max(abs(fit$V[, k])), k])
    fit$V[, k] <- fit$V[, k] * sg; fit$U[, k] <- fit$U[, k] * sg
  }
  fit
}

lfm_loadings <- function(fit, L, species_meta) {
  tibble::as_tibble(stats::setNames(as.data.frame(fit$V), paste0("F", seq_len(fit$K)))) |>
    dplyr::mutate(species = L$species, species_level = fit$mu) |>
    dplyr::left_join(dplyr::select(species_meta, species, guild = guild_from_file, host_tree), by = "species") |>
    dplyr::relocate(species, guild, host_tree)
}

lfm_scores_by_year <- function(fit, L) {
  tibble::as_tibble(stats::setNames(as.data.frame(fit$U), paste0("F", seq_len(fit$K)))) |>
    dplyr::bind_cols(L$rows, intensity = fit$c) |>
    dplyr::filter(!seq_len(nrow(L$rows)) %in% fit$empty_rows)
}

# Gap-filled table: every not-monitored cell in a region-year that records at
# least one other species. PROTOTYPE: host presence is not yet checked, so a
# filled value may refer to a species whose host tree is absent.
lfm_fill <- function(fit, L) {
  P <- lfm_predict(fit)
  gap <- which(is.na(L$Y) & !is.na(P), arr.ind = TRUE)
  tibble::tibble(country = L$rows$country[gap[, 1]], nuts_id = L$rows$nuts_id[gap[, 1]],
                 year = L$rows$year[gap[, 1]], species = L$species[gap[, 2]],
                 log_damage_pred = P[gap], damage_pred = pmax(expm1(P[gap]), 0))
}

# Pick example hidden series for plotting: for a spread of species, the series
# with the median dynamics r (typical case, not cherry-picked).
lfm_example_pick <- function(cv, best_model, n = 5) {
  cv |>
    dplyr::filter(model == best_model, !is.na(predicted)) |>
    dplyr::group_by(country, nuts_id, species) |>
    dplyr::filter(dplyr::n() >= 15) |>
    dplyr::summarise(r = stats::cor(predicted, observed), .groups = "drop") |>
    dplyr::filter(species %in% c("pha_cya", "ips_typ", "tom_spp", "lym_mon", "pit_spp", "geo_db")) |>
    dplyr::group_by(species) |>
    dplyr::arrange(r, .by_group = TRUE) |>
    dplyr::slice(ceiling(dplyr::n() / 2)) |>
    dplyr::ungroup() |>
    dplyr::slice_head(n = n) |>
    dplyr::transmute(nuts_id, species, title = paste0(species, ", ", nuts_id, " (", country, ")"))
}



# -----------------------------------------------------------------------------
# hurdle.R
# -----------------------------------------------------------------------------
# Two-part (hurdle) latent factor model for gap-filling.
#
# 64% of recorded values are zeros (0 = monitored, no damage), so each gap is
# split into two questions:
#   Part 1  "Any damage?"   P(damage > 0) from a logistic factor model fitted to
#                           all recorded values (1 = damage, 0 = recorded zero).
#   Part 2  "How much?"     log(damage + 1) given damage > 0, from the linear
#                           factor model (R/lfm.R) fitted to positive values only.
#   Combined: expected damage = P(any) x expected amount if damaged.
#
# Both parts share the structure of R/lfm.R: insect level + region-year
# intensity + region-year scores x insect loadings on K hidden factors.

sigmoid <- function(x) 1 / (1 + exp(-x))

# Logistic low-rank model fitted by gradient descent (Adam) on recorded cells.
#   logit P[i, s] = a[s] + b[i] + U[i, ] . V[s, ]
# Ridge penalties: lambda on U and V, lambda_b on the row intensity b.
# Optional covariate matrix X (same shape as B, e.g. log host area) enters with
# one slope per insect: + c[s] * X[i, s] (ridge penalty 1 on c).
# Optional soft hierarchy as in lfm_fit(): with `group` and tau > 0, each
# species' level a[s] and loadings V[s, ] are pulled towards its group mean.
logit_fit <- function(B, K, lambda, lambda_b = 1, n_iter = 600, lr = 0.05, seed = 20261012, X = NULL,
                      group = NULL, tau = 0, tau_links = tau) {
  set.seed(seed)
  M <- !is.na(B); Bz <- B; Bz[!M] <- 0
  n <- nrow(B); p <- ncol(B)
  rate <- pmin(pmax(colMeans(B, na.rm = TRUE), 0.01), 0.99); rate[is.na(rate)] <- 0.5
  a <- stats::qlogis(rate); b <- rep(0, n)
  U <- matrix(stats::rnorm(n * K, sd = 0.01), n, K)
  V <- matrix(stats::rnorm(p * K, sd = 0.01), p, K)
  # Adam state
  pars <- list(a = a, b = b, U = U, V = V, c = rep(0, p))
  if (is.null(X)) X <- matrix(0, n, p)
  hier <- !is.null(group) && (tau > 0 || tau_links > 0)
  gmean <- function(x) { x <- as.matrix(x); for (g in unique(group)) { i <- group == g
    x[i, ] <- matrix(colMeans(x[i, , drop = FALSE]), sum(i), ncol(x), byrow = TRUE) }; x }
  m <- lapply(pars, function(x) x * 0); v <- m
  b1 <- 0.9; b2 <- 0.999; eps <- 1e-8
  for (t in seq_len(n_iter)) {
    eta <- outer(pars$b, pars$a, "+") + X * rep(pars$c, each = n) + if (K > 0) pars$U %*% t(pars$V) else 0
    G <- (sigmoid(eta) - Bz) * M                     # d loss / d eta on recorded cells
    grads <- list(
      a = colSums(G),
      b = rowSums(G) + 2 * lambda_b * pars$b,
      U = if (K > 0) G %*% pars$V + 2 * lambda * pars$U else pars$U,
      V = if (K > 0) t(G) %*% pars$U + 2 * lambda * pars$V else pars$V,
      c = colSums(G * X) + 2 * pars$c
    )
    if (hier) {
      grads$a <- grads$a + 2 * tau * (pars$a - as.numeric(gmean(pars$a)))
      if (K > 0) grads$V <- grads$V + 2 * tau_links * (pars$V - gmean(pars$V))
    }
    for (nm in names(pars)) {
      m[[nm]] <- b1 * m[[nm]] + (1 - b1) * grads[[nm]]
      v[[nm]] <- b2 * v[[nm]] + (1 - b2) * grads[[nm]]^2
      pars[[nm]] <- pars[[nm]] - lr * (m[[nm]] / (1 - b1^t)) / (sqrt(v[[nm]] / (1 - b2^t)) + eps)
    }
  }
  pars$K <- K; pars$X <- X; pars$empty_rows <- which(rowSums(M) == 0)
  pars
}

logit_predict <- function(fit) {
  n <- length(fit$b)
  P <- sigmoid(outer(fit$b, fit$a, "+") + fit$X * rep(fit$c, each = n) + if (fit$K > 0) fit$U %*% t(fit$V) else 0)
  P[fit$empty_rows, ] <- NA
  P
}

# Split the log(damage + 1) matrix into the two parts
hurdle_parts <- function(Y) {
  B <- ifelse(is.na(Y), NA_real_, as.numeric(Y > 0))
  Y2 <- Y; Y2[!is.na(Y2) & Y2 <= 0] <- NA
  list(B = B, Y2 = Y2)
}

# Smearing factor (Duan) to turn a mean of log(x + 1) into a mean of x:
# mean of exp(residuals) of part 2 on its training cells.
smearing <- function(fit2, Y2) {
  P <- lfm_predict(fit2); ok <- !is.na(Y2) & !is.na(P)
  mean(exp(Y2[ok] - P[ok]))
}

# Cross-validation on the same hidden country x insect blocks as R/lfm.R.
# Returns one row per hidden cell with predictions from every candidate.
hurdle_cv <- function(L, blocks, grid1, grid2, one_part) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    bf <- dplyr::filter(blocks, fold == f)
    H <- held_mask(L, bf)
    Ytr <- L$Y; Ytr[H] <- NA
    hp <- hurdle_parts(Ytr)
    idx <- which(H, arr.ind = TRUE)
    base <- tibble::tibble(fold = f, row = idx[, 1], col = idx[, 2],
                           country = L$rows$country[idx[, 1]], nuts_id = L$rows$nuts_id[idx[, 1]],
                           year = L$rows$year[idx[, 1]], species = L$species[idx[, 2]],
                           observed = L$Y[idx])
    # baselines for "any damage?": insect rate, insect x year rate (training data)
    rate_s <- matrix(colMeans(hp$B, na.rm = TRUE), nrow(Ytr), ncol(Ytr), byrow = TRUE)
    rate_sy <- baseline_species_year(hp$B, L$rows$year)
    p1 <- list(`P1 baseline: insect rate` = rate_s, `P1 baseline: insect x year rate` = rate_sy)
    for (g in seq_len(nrow(grid1))) p1[[grid1$model[g]]] <- logit_predict(logit_fit(hp$B, grid1$K[g], grid1$lambda[g]))
    p2 <- list(`P2 baseline: insect x year mean` = baseline_species_year(hp$Y2, L$rows$year))
    sm <- list(`P2 baseline: insect x year mean` = 1)
    for (g in seq_len(nrow(grid2))) {
      fit2 <- lfm_fit(hp$Y2, K = grid2$K[g], lambda = grid2$lambda[g])
      p2[[grid2$model[g]]] <- lfm_predict(fit2)
      sm[[grid2$model[g]]] <- smearing(fit2, hp$Y2)
    }
    one <- lfm_predict(lfm_fit(Ytr, K = one_part$K, lambda = one_part$lambda))
    base_sy <- baseline_species_year(Ytr, L$rows$year)   # one-part insect x year baseline
    rows <- base |> dplyr::mutate(one_part = one[idx], base_sy = base_sy[idx])
    for (nm in names(p1)) rows[[paste0("p1|", nm)]] <- p1[[nm]][idx]
    for (nm in names(p2)) { rows[[paste0("p2|", nm)]] <- p2[[nm]][idx]; rows[[paste0("sm|", nm)]] <- sm[[nm]] }
    out[[length(out) + 1]] <- rows
  }
  dplyr::bind_rows(out)
}

# Scores for each part, and choice of the best configuration of each
auc <- function(p, y) {
  ok <- !is.na(p) & !is.na(y); p <- p[ok]; y <- y[ok]
  if (length(unique(y)) < 2) return(NA_real_)
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

hurdle_part_scores <- function(hcv) {
  y1 <- as.numeric(hcv$observed > 0)
  s1 <- purrr::map_dfr(grep("^p1\\|", names(hcv), value = TRUE), function(cl) {
    p <- pmin(pmax(hcv[[cl]], 1e-4), 1 - 1e-4); ok <- !is.na(p)
    tibble::tibble(part = "1: any damage?", model = sub("^p1\\|", "", cl),
                   auc = auc(p, y1), brier = mean((p[ok] - y1[ok])^2),
                   log_loss = -mean(y1[ok] * log(p[ok]) + (1 - y1[ok]) * log(1 - p[ok])))
  })
  pos <- hcv$observed > 0
  s2 <- purrr::map_dfr(grep("^p2\\|", names(hcv), value = TRUE), function(cl) {
    e <- hcv[[cl]][pos] - hcv$observed[pos]; ok <- !is.na(e)
    tibble::tibble(part = "2: how much, if damaged?", model = sub("^p2\\|", "", cl),
                   rmse_log = sqrt(mean(e[ok]^2)))
  })
  dplyr::bind_rows(s1, s2)
}

# Combined comparison, two-part vs one-part, on the same hidden cells:
#  - RMSE of log(damage + 1): two-part uses P(any) x predicted log amount,
#    which is the expected value of log(damage + 1)
#  - zero calls: share of truly zero cells predicted as "probably no damage"
#  - block totals: |log ratio| of predicted vs real total damage per hidden
#    country x insect x year (what Europe-wide totals depend on)
hurdle_compare <- function(hcv, best1, best2) {
  p <- hcv[[paste0("p1|", best1)]]; a <- hcv[[paste0("p2|", best2)]]; sm <- hcv[[paste0("sm|", best2)]]
  d <- hcv |>
    dplyr::mutate(
      p_any = p, amount_log = a,
      two_part_log = p * a,
      two_part_damage = p * pmax(expm1(a) * sm, 0),
      one_part_damage = pmax(expm1(one_part), 0),
      real_damage = expm1(observed)
    ) |>
    dplyr::filter(!is.na(p_any), !is.na(amount_log), !is.na(one_part), !is.na(base_sy))
  tot <- d |>
    dplyr::group_by(fold, country, species, year) |>
    dplyr::summarise(real = sum(real_damage), two = sum(two_part_damage), one = sum(one_part_damage), .groups = "drop") |>
    dplyr::filter(real > 0)
  list(
    cells = d,
    summary = tibble::tibble(
      model = c("One-part factor model", "Two-part factor model"),
      rmse_log = c(sqrt(mean((d$one_part - d$observed)^2)), sqrt(mean((d$two_part_log - d$observed)^2))),
      zeros_called_zero = c(mean(d$one_part[d$observed == 0] < log1p(1)),
                            mean(d$p_any[d$observed == 0] < 0.5)),
      median_abs_log_ratio_totals = c(stats::median(abs(log((tot$one + 1) / (tot$real + 1)))),
                                      stats::median(abs(log((tot$two + 1) / (tot$real + 1))))),
      share_totals_within_2x = c(mean(abs(log((tot$one + 1) / (tot$real + 1))) <= log(2)),
                                 mean(abs(log((tot$two + 1) / (tot$real + 1))) <= log(2)))
    ),
    by_species = d |>
      dplyr::group_by(species) |>
      dplyr::summarise(
        n_cells = dplyr::n(),
        auc_any = auc(p_any, as.numeric(observed > 0)),
        rmse_two = sqrt(mean((two_part_log - observed)^2)),
        rmse_one = sqrt(mean((one_part - observed)^2)),
        rmse_base = sqrt(mean((base_sy - observed)^2, na.rm = TRUE)),
        .groups = "drop"
      )
  )
}
# ---- Final fit and gap-filled dataset ---------------------------------------

hurdle_final <- function(L, K1, lambda1, K2, lambda2) {
  hp <- hurdle_parts(L$Y)
  f1 <- logit_fit(hp$B, K1, lambda1)
  f2 <- lfm_fit(hp$Y2, K = K2, lambda = lambda2)
  P2 <- lfm_predict(f2); ok <- !is.na(hp$Y2) & !is.na(P2)
  list(P1 = logit_predict(f1), P2 = P2, smear = mean(exp(hp$Y2[ok] - P2[ok])),
       sigma2 = stats::sd(hp$Y2[ok] - P2[ok]), f1 = f1, f2 = f2)
}

# Confidence per insect from the cross-validation:
#   "works"    : tested, and the two-part model beats the insect x year baseline
#                and separates damage / no damage reasonably (AUC >= 0.7)
#   "weak"     : tested, but not better than the baseline or AUC < 0.7
#   "untested" : insect recorded in only one country, so it could not be tested
hurdle_confidence <- function(by_species, species) {
  tibble::tibble(species = species) |>
    dplyr::left_join(by_species, by = "species") |>
    dplyr::mutate(confidence = dplyr::case_when(
      is.na(n_cells) ~ "untested",
      rmse_two < rmse_base & !is.na(auc_any) & auc_any >= 0.7 ~ "works",
      TRUE ~ "weak"
    ))
}

# Harmonised table: every region-year-insect cell, observed or filled.
# Filled values are expected damage = P(any) x mean amount if damaged, with an
# 80% range for the amount if damaged. Host presence is not yet known, so a
# filled value may refer to an insect whose host tree is absent.
hurdle_fill <- function(L, fin, conf, cells, unit_lookup) {
  n <- nrow(L$Y); p <- ncol(L$Y)
  grid <- tibble::tibble(row = rep(seq_len(n), p), col = rep(seq_len(p), each = n))
  z <- stats::qnorm(0.9)
  mon_cs <- cells |> dplyr::group_by(country, species) |>
    dplyr::summarise(ever = any(status != "not_monitored"), .groups = "drop")
  grid |>
    dplyr::mutate(
      country = L$rows$country[row], nuts_id = L$rows$nuts_id[row], year = L$rows$year[row],
      species = L$species[col],
      y = L$Y[cbind(row, col)], p_any = fin$P1[cbind(row, col)], a = fin$P2[cbind(row, col)]
    ) |>
    dplyr::left_join(mon_cs, by = c("country", "species")) |>
    dplyr::left_join(dplyr::select(conf, species, confidence), by = "species") |>
    dplyr::left_join(unit_lookup, by = "species") |>
    dplyr::mutate(
      value_type = dplyr::case_when(
        !is.na(y) & y > 0 ~ "observed damage",
        !is.na(y) ~ "observed zero",
        is.na(p_any) | is.na(a) ~ "not fillable (nothing recorded in this region-year)",
        TRUE ~ "filled"
      ),
      gap_type = dplyr::case_when(
        value_type != "filled" ~ NA_character_,
        ever ~ "country monitors this insect, missed this year/region",
        TRUE ~ "country never monitored this insect"
      ),
      amount_if_damaged = dplyr::if_else(value_type == "filled", pmax(expm1(a) * fin$smear, 0), NA_real_),
      amount_lo80 = dplyr::if_else(value_type == "filled", pmax(expm1(a - z * fin$sigma2), 0), NA_real_),
      amount_hi80 = dplyr::if_else(value_type == "filled", expm1(a + z * fin$sigma2), NA_real_),
      p_any = dplyr::if_else(value_type == "filled", p_any, NA_real_),
      damage = dplyr::case_when(
        value_type %in% c("observed damage", "observed zero") ~ expm1(y),
        value_type == "filled" ~ p_any * amount_if_damaged,
        TRUE ~ NA_real_
      ),
      confidence = dplyr::if_else(value_type == "filled", confidence, NA_character_),
      host_presence = dplyr::if_else(value_type == "filled", "unknown (needs host-tree data)", NA_character_)
    ) |>
    dplyr::select(country, nuts_id, year, species, unit, value_type, damage, p_any,
                  amount_if_damaged, amount_lo80, amount_hi80, gap_type, confidence, host_presence)
}

# Europe-wide totals per insect and year: observed, plus filled (by confidence)
hurdle_totals <- function(filled) {
  filled |>
    dplyr::group_by(species, unit, year) |>
    dplyr::summarise(
      observed = sum(damage[value_type %in% c("observed damage", "observed zero")]),
      filled_works = sum(damage[value_type == "filled" & confidence == "works"]),
      filled_weak = sum(damage[value_type == "filled" & confidence == "weak"]),
      filled_untested = sum(damage[value_type == "filled" & confidence == "untested"]),
      cells_not_fillable = sum(value_type == "not fillable (nothing recorded in this region-year)"),
      .groups = "drop"
    ) |>
    dplyr::mutate(total_with_reliable_fill = observed + filled_works,
                  share_filled_reliable = filled_works / pmax(total_with_reliable_fill, 1e-9))
}


# -----------------------------------------------------------------------------
# gapfill_compare.R
# -----------------------------------------------------------------------------
# Gap-filling: three methods tested head to head (Oct 2026).
#
# Gaps = insects a country did not report (unknown, never treated as 0).
# Test: hide whole country x insect blocks that WERE reported (same blocks as
# R/lfm.R), predict them with each method from the remaining data only, and
# compare with the real values.
#
#   1. simple average  : the insect's mean log(1 + damage) in that year, over
#                        all regions that reported it (the yardstick)
#   2. partner method  : the insect's partners = other insects whose damage
#                        correlates with it (r >= 0.3) in the training data,
#                        co-recorded in >= 20 region-years and >= 2 countries.
#                        Each partner recorded in the hidden region-year gives
#                        a prediction by linear regression; predictions are
#                        averaged with weights r^2. No partner recorded there:
#                        falls back to the simple average.
#   3. factor model    : the two-part latent factor model (R/hurdle.R, R/lfm.R),
#                        P(any damage) x amount if damaged, with the insect's
#                        level pulled towards its broad group (strength 50),
#                        the best version found earlier.
# Each method gives two predictions per hidden cell:
#   *_log : on the log(1 + damage) scale, used for the per-cell error and for
#           telling damage from no damage (for the factor model:
#           P(any) x log amount, its expected log value)
#   plain : expected damage in the original unit, summed for block totals.

partner_predict <- function(Ytr, H, rows, base, min_r = 0.3, min_n = 20, min_countries = 2) {
  P <- base
  used <- matrix(FALSE, nrow(Ytr), ncol(Ytr))
  for (s in which(colSums(H) > 0)) {
    hid <- which(H[, s])
    num <- rep(0, length(hid)); den <- rep(0, length(hid))
    for (p in setdiff(seq_len(ncol(Ytr)), s)) {
      ok <- !is.na(Ytr[, s]) & !is.na(Ytr[, p])
      if (sum(ok) < min_n || dplyr::n_distinct(rows$country[ok]) < min_countries) next
      if (stats::sd(Ytr[ok, s]) == 0 || stats::sd(Ytr[ok, p]) == 0) next
      r <- stats::cor(Ytr[ok, s], Ytr[ok, p])
      if (r < min_r) next
      b <- stats::coef(stats::lm(Ytr[ok, s] ~ Ytr[ok, p]))
      xp <- Ytr[hid, p]
      has <- !is.na(xp)
      num[has] <- num[has] + r^2 * (b[1] + b[2] * xp[has])
      den[has] <- den[has] + r^2
    }
    got <- den > 0
    P[hid[got], s] <- pmax(num[got] / den[got], 0)
    used[hid[got], s] <- TRUE
  }
  list(P = P, used = used)
}

#   4. factor model + forest area (optional, when Lh / absent are given):
#                        the same model, plus the NUTS forest table: damage per
#                        1000 ha of host forest in the amount part, log host
#                        area in the any-damage part, host tree absent -> 0
#                        (R/hosts.R). Regions without area use version 3.
#   5. factor model + forest area (amount only): presence from version 3,
#                        amount from version 4.
gapfill_compare_cv <- function(L, blocks, group, K1, lambda1, K2, lambda2, tau = 50,
                               Lh = NULL, absent = NULL) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ytr <- L$Y; Ytr[H] <- NA
    idx <- which(H, arr.ind = TRUE)
    # 1. simple average
    base <- baseline_species_year(Ytr, L$rows$year)
    # 2. partner method
    pm <- partner_predict(Ytr, H, L$rows, base)
    # 3. factor model with level hint
    hp <- hurdle_parts(Ytr)
    f1 <- logit_fit(hp$B, K1, lambda1, group = group, tau = tau, tau_links = 0)
    f2 <- lfm_fit(hp$Y2, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
    p_any <- logit_predict(f1)[idx]; a <- lfm_predict(f2)[idx]
    fm <- p_any * pmax(expm1(a) * smearing(f2, hp$Y2), 0)
    if (!is.null(Lh)) {
      Ys <- Lh$Y; Ys[H] <- NA
      Y2s <- hurdle_parts(Ys)$Y2
      g1 <- logit_fit(hp$B, K1, lambda1, X = Lh$X, group = group, tau = tau, tau_links = 0)
      g2 <- lfm_fit(Y2s, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
      p_h <- logit_predict(g1)[idx]; a_s <- lfm_predict(g2)[idx]
      area <- Lh$area[idx]; ab <- absent[idx]
      fh <- host_combine(p_h, a_s, smearing(g2, Y2s), a, smearing(f2, hp$Y2), area, ab)
      # log-scale prediction: amount on the original scale when area is known
      a_nat <- ifelse(!is.na(area) & !is.na(a_s), log1p(pmax(expm1(a_s) * area / 1000, 0)), a)
      fh_log <- ifelse(ab, 0, p_h * a_nat)
      # 5. forest area for the amount only: presence from version 3
      fa <- host_combine(p_any, a_s, smearing(g2, Y2s), a, smearing(f2, hp$Y2), area, ab)
      fa_log <- ifelse(ab, 0, p_any * a_nat)
    } else { fh <- fh_log <- fa <- fa_log <- NA_real_ }
    out[[length(out) + 1]] <- tibble::tibble(
      fold = f, row = idx[, 1], col = idx[, 2],
      country = L$rows$country[idx[, 1]], nuts_id = L$rows$nuts_id[idx[, 1]],
      year = L$rows$year[idx[, 1]], species = L$species[idx[, 2]],
      observed = L$Y[idx],
      `simple average` = pmax(expm1(base[idx]), 0),
      `partner method` = pmax(expm1(pm$P[idx]), 0),
      `factor model` = fm,
      `factor model + forest area` = fh,
      `factor model + forest area (amount only)` = fa,
      `simple average_log` = base[idx], `partner method_log` = pm$P[idx], `factor model_log` = p_any * a,
      `factor model + forest area_log` = fh_log,
      `factor model + forest area (amount only)_log` = fa_log,
      partner_used = pm$used[idx])
  }
  dplyr::bind_rows(out)
}

gapfill_compare_scores <- function(cvr) {
  methods <- c("simple average", "partner method", "factor model", "factor model + forest area",
               "factor model + forest area (amount only)", "hybrid (factor level + partner ups and downs)",
               "blend: factor model + 25% notes equation")
  methods <- methods[methods %in% names(cvr) & vapply(methods, function(m) any(!is.na(cvr[[m]])), TRUE)]
  lg <- paste0(methods, "_log")
  d0 <- cvr |> dplyr::filter(dplyr::if_all(dplyr::all_of(c(methods, lg)), ~ !is.na(.x)))
  d <- d0 |>
    tidyr::pivot_longer(dplyr::all_of(methods), names_to = "method", values_to = "pred") |>
    dplyr::select(-dplyr::all_of(lg)) |>
    dplyr::left_join(d0 |> tidyr::pivot_longer(dplyr::all_of(lg), names_to = "method", values_to = "pred_log") |>
                       dplyr::mutate(method = sub("_log$", "", method)) |>
                       dplyr::select(fold, row, col, method, pred_log),
                     by = c("fold", "row", "col", "method")) |>
    dplyr::mutate(method = factor(method, methods), real = expm1(observed),
                  err = pred_log - observed)
  tot <- d |> dplyr::group_by(method, fold, country, species, year) |>
    dplyr::summarise(real = sum(real), pred = sum(pred), .groups = "drop") |>
    dplyr::filter(real > 0) |>
    dplyr::mutate(lr = abs(log((pred + 1) / (real + 1))))
  overall <- d |> dplyr::group_by(method) |>
    dplyr::summarise(n_cells = dplyr::n(),
                     error_log = sqrt(mean(err^2)),
                     damage_vs_none_auc = auc(pred_log, as.numeric(observed > 0)),
                     .groups = "drop") |>
    dplyr::left_join(tot |> dplyr::group_by(method) |>
                       dplyr::summarise(totals_within_2x = mean(lr <= log(2)),
                                        typical_total_error_x = exp(stats::median(lr))), by = "method")
  by_species <- d |> dplyr::group_by(species, method) |>
    dplyr::summarise(n_cells = dplyr::n(), error_log = sqrt(mean(err^2)),
                     auc = auc(pred_log, as.numeric(observed > 0)), .groups = "drop") |>
    dplyr::group_by(species) |>
    dplyr::mutate(best = method[which.min(error_log)],
                  gain_vs_simple = 1 - error_log / error_log[method == "simple average"]) |>
    dplyr::ungroup()
  partner_cov <- cvr |> dplyr::summarise(share_cells_with_partner = mean(partner_used))
  list(overall = overall, by_species = by_species, partner_coverage = partner_cov)
}

# ---- Final gap-filled dataset (version 1) ---------------------------------------
# Every region x year x insect cell:
#   reported damage / reported zero : kept exactly as reported
#   zero: host tree absent          : not reported, and the insect's host tree
#                                     does not grow in the region (forest table)
#   filled                          : not reported (unknown) -> estimated
#   not fillable                    : nothing at all recorded in that region-year
# Method: factor model + forest area for the amount (version 5 of the test);
# partner method for the insects where it tested best, where a partner is
# recorded in that region-year (otherwise the factor model).
# Confidence of a filled value, from the hide-and-predict test of its method:
#   reliable : beats the simple average and separates damage from no damage
#              (AUC >= 0.7) for this insect
#   weak     : tested, but not both of the above
#   untested : insect reported by one country only, so it cannot be tested

gapfill_final <- function(L, hs, group, K1, lambda1, K2, lambda2, tau = 50, partner_species = character()) {
  Lh <- host_scaled_L(L, hs); absent <- host_absent_matrix(L, hs)
  hp <- hurdle_parts(L$Y); Y2s <- hurdle_parts(Lh$Y)$Y2
  f1 <- logit_fit(hp$B, K1, lambda1, group = group, tau = tau, tau_links = 0)
  f2 <- lfm_fit(hp$Y2, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
  g2 <- lfm_fit(Y2s, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
  P1 <- logit_predict(f1); P2 <- lfm_predict(f2); P2s <- lfm_predict(g2)
  sm <- smearing(f2, hp$Y2); sms <- smearing(g2, Y2s)
  # 80% range of the amount if damaged, from the spread of the amount model's errors
  ok <- !is.na(hp$Y2) & !is.na(P2); sd2 <- stats::sd(hp$Y2[ok] - P2[ok])
  oks <- !is.na(Y2s) & !is.na(P2s); sds <- stats::sd(Y2s[oks] - P2s[oks])
  scaled <- !is.na(Lh$area) & !is.na(P2s)
  amount_log <- ifelse(scaled, log1p(pmax(expm1(P2s) * sms, 0) * Lh$area / 1000), log1p(pmax(expm1(P2) * sm, 0)))
  spread <- ifelse(scaled, sds, sd2)
  E <- host_combine(P1, P2s, sms, P2, sm, Lh$area, absent)
  # partner method on all data for the chosen insects
  H <- is.na(L$Y); H[, !L$species %in% partner_species] <- FALSE
  pm <- partner_predict(L$Y, H, L$rows, baseline_species_year(L$Y, L$rows$year))
  n <- nrow(L$Y); p <- ncol(L$Y); ix <- cbind(rep(seq_len(n), p), rep(seq_len(p), each = n))
  tibble::tibble(
    country = L$rows$country[ix[, 1]], nuts_id = L$rows$nuts_id[ix[, 1]], year = L$rows$year[ix[, 1]],
    species = L$species[ix[, 2]], y = L$Y[ix], absent = absent[ix], empty = is.na(P1[ix]),
    p_any = P1[ix], amount_log = amount_log[ix], spread = spread[ix], factor_fill = E[ix],
    partner_used = pm$used[ix], partner_fill = pmax(expm1(pm$P[ix]), 0), host_area_ha = Lh$area[ix]
  )
}

gapfill_dataset <- function(raw_fill, gf_species, all_species, unit_lookup) {
  z <- stats::qnorm(0.9)
  conf <- gf_species |>
    dplyr::filter(as.character(method) == as.character(best)) |>
    dplyr::transmute(species, method_used = as.character(best),
                     confidence = dplyr::if_else(gain_vs_simple > 0 & !is.na(auc) & auc >= 0.7, "reliable", "weak"))
  conf <- dplyr::bind_rows(conf, tibble::tibble(species = setdiff(all_species, conf$species),
                                                method_used = "factor model + forest area (amount only)",
                                                confidence = "untested"))
  raw_fill |>
    dplyr::left_join(conf, by = "species") |>
    dplyr::left_join(unit_lookup, by = "species") |>
    dplyr::mutate(
      use_partner = method_used == "partner method" & partner_used,
      cand = dplyr::if_else(use_partner, partner_fill, factor_fill),
      value_type = dplyr::case_when(
        !is.na(y) & y > 0 ~ "reported damage",
        !is.na(y) ~ "reported zero",
        absent ~ "zero: host tree absent",
        (empty & !use_partner) | is.na(cand) ~ "not fillable (nothing recorded in this region-year)",
        TRUE ~ "filled"),
      fl = value_type == "filled",
      fill_method = dplyr::case_when(!fl ~ NA_character_, use_partner ~ "partner method",
                                     TRUE ~ "factor model + forest area"),
      damage = dplyr::case_when(
        value_type %in% c("reported damage", "reported zero") ~ expm1(y),
        value_type == "zero: host tree absent" ~ 0,
        fl ~ cand),
      p_any = dplyr::if_else(fl & !use_partner, p_any, NA_real_),
      amount_if_damaged = dplyr::if_else(fl & !use_partner, expm1(amount_log), NA_real_),
      amount_lo80 = dplyr::if_else(fl & !use_partner, pmax(expm1(amount_log - z * spread), 0), NA_real_),
      amount_hi80 = dplyr::if_else(fl & !use_partner, expm1(amount_log + z * spread), NA_real_),
      confidence = dplyr::if_else(fl, confidence, NA_character_)) |>
    dplyr::select(country, nuts_id, year, species, unit, value_type, damage, fill_method, confidence,
                  p_any, amount_if_damaged, amount_lo80, amount_hi80, host_area_ha)
}

# Europe-wide totals per insect and year: reported, and filled by confidence
gapfill_totals <- function(gd) {
  gd |>
    dplyr::group_by(species, unit, year) |>
    dplyr::summarise(
      reported = sum(damage[value_type %in% c("reported damage", "reported zero")]),
      filled_reliable = sum(damage[value_type == "filled" & confidence == "reliable"]),
      filled_weak = sum(damage[value_type == "filled" & confidence == "weak"]),
      filled_untested = sum(damage[value_type == "filled" & confidence == "untested"]),
      cells_not_fillable = sum(grepl("^not fillable", value_type)),
      .groups = "drop") |>
    dplyr::mutate(total_reported_plus_reliable = reported + filled_reliable)
}


# -----------------------------------------------------------------------------
# gapfill_v3.R
# -----------------------------------------------------------------------------
# Gap-filling, aligned with the correlation metrics (Oct 2026).
#
# The earlier gap-filling (v1) used the insect codes as reported; the
# correlation metrics count the CZ spruce bark-borer group as I. typographus
# (as in Hlasny et al. 2025). The v3 targets rebuild the factor-model inputs
# from the same data as the correlation metrics (cb_cells / cb_tr), so both
# parts of the thesis rest on one dataset. Model settings (K, ridge penalties,
# group hint) are the ones chosen earlier; they are not re-tuned here.
#
# model_vs_correlation(): does the fitted factor model reproduce the
# correlation metrics? For every pair, the same within-region Pearson r is
# computed on the model's fitted values (expected log damage = P(any) x
# log amount) over the same region-years where both insects are recorded,
# and compared with the r measured on the data.

model_vs_correlation <- function(raw, verdict) {
  d <- raw |>
    dplyr::filter(!is.na(y)) |>
    dplyr::mutate(fit = p_any * amount_log) |>
    dplyr::select(country, nuts_id, year, species, fit)
  w <- tidyr::pivot_wider(d, names_from = species, values_from = fit)
  reg <- paste(w$country, w$nuts_id)
  zfun <- function(v) {   # standardise within each region series
    stats::ave(v, reg, FUN = function(x) {
      ok <- !is.na(x)
      if (sum(ok) < 3 || stats::sd(x[ok]) == 0) return(rep(NA_real_, length(x)))
      (x - mean(x[ok])) / stats::sd(x[ok])
    })
  }
  sp <- intersect(unique(c(verdict$species_a, verdict$species_b)), names(w))
  Z <- vapply(sp, function(s) zfun(w[[s]]), numeric(nrow(w)))
  r_model <- purrr::map2_dbl(verdict$species_a, verdict$species_b, function(a, b) {
    if (!a %in% sp || !b %in% sp) return(NA_real_)
    x <- Z[, a]; y <- Z[, b]; ok <- !is.na(x) & !is.na(y)
    if (sum(ok) < 20) NA_real_ else stats::cor(x[ok], y[ok])
  })
  verdict |>
    dplyr::select(species_a, species_b, verdict, r_data = r) |>
    dplyr::mutate(r_model = r_model)
}


# ---- Gap-filled dataset v2: factor model only, with honest ranges ------------
#
# 1. The final method is tested exactly as it is used: for each of the 10
#    folds, hide the country x insect blocks, refit the whole final model
#    (gapfill_final) on the rest, and keep the predictions for the hidden cells.
# 2. Ranges come from that test (empirical, not from model assumptions): for
#    every hidden country x insect x year total we know the real value, so the
#    spread of log(real / predicted) says how far off a filled total typically
#    is. The 10% and 90% points of that spread give an 80% range for any
#    filled total. Errors within a country x insect block are strongly shared
#    (a whole country's level can be off), so ranges are built for totals,
#    not for single cells added up as if independent.
#    Calibration is checked by learning the spread on half the folds and
#    counting how often the other half's real totals fall inside.
# 3. Fill rules:
#      reported damage / reported zero : kept
#      zero: host tree absent          : host tree not in the region (forest table)
#      not filled: aggregate code      : aggregate reporting codes (e.g. CambXyl_con&MD,
#                                        Def_dec) are country-specific reporting
#                                        categories, not insects; filling them in
#                                        other countries would double-count damage
#                                        already reported under species codes
#      not fillable                    : nothing recorded in that region-year
#      filled                          : everything else, with a confidence:
#        reliable : insect tested and the model beats the simple average for it
#                   and separates damage from no damage (AUC >= 0.7)
#        weak     : tested, but not both
#        untested : insect recorded in one country only (cannot be tested)
#    Europe-wide totals use reported + reliable + weak fills (weak flagged);
#    untested fills are reported separately and not added to the headline.

final_method_cv <- function(L, blocks, hs, group, K1, lambda1, K2, lambda2, tau = 50) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ltr <- L; Ltr$Y[H] <- NA
    raw <- gapfill_final(Ltr, hs, group, K1, lambda1, K2, lambda2, tau = tau)
    idx <- which(H, arr.ind = TRUE)
    k <- (idx[, 2] - 1) * nrow(L$Y) + idx[, 1]       # raw is laid out column by column
    out[[length(out) + 1]] <- tibble::tibble(
      fold = f, country = raw$country[k], nuts_id = raw$nuts_id[k], year = raw$year[k],
      species = raw$species[k], observed = L$Y[idx], pred = raw$factor_fill[k],
      pred_log = ifelse(raw$absent[k], 0, raw$p_any[k] * raw$amount_log[k]),   # log-scale prediction
      p_any = raw$p_any[k], absent = raw$absent[k])
  }
  dplyr::bind_rows(out)
}

# Hidden totals per fold x country x insect x year, with log(real / predicted)
cv_block_totals <- function(fc) {
  fc |>
    dplyr::filter(!is.na(pred)) |>
    dplyr::group_by(fold, country, species, year) |>
    dplyr::summarise(real = sum(expm1(observed)), pred = sum(pred), .groups = "drop") |>
    dplyr::mutate(lr = log((real + 1) / (pred + 1)))
}

# Per-insect test scores of the final method (for the confidence labels)
final_method_scores <- function(fc, gf_species_simple) {
  fc |>
    dplyr::filter(!is.na(pred)) |>
    dplyr::group_by(species) |>
    dplyr::summarise(n_cells = dplyr::n(),
                     error_log = sqrt(mean((pred_log - observed)^2)),
                     auc = auc(pred_log, as.numeric(observed > 0)), .groups = "drop") |>
    dplyr::left_join(gf_species_simple, by = "species") |>
    dplyr::mutate(confidence = dplyr::if_else(error_log < error_log_simple & !is.na(auc) & auc >= 0.7,
                                              "reliable", "weak"))
}

# Error spread of totals: per insect when it has >= min_n hidden totals,
# otherwise the pooled spread of all insects
range_table <- function(bt, min_n = 30) {
  pooled <- stats::quantile(bt$lr, c(0.1, 0.5, 0.9), names = FALSE)
  bt |>
    dplyr::group_by(species) |>
    dplyr::summarise(n_totals = dplyr::n(),
                     q10 = stats::quantile(lr, 0.1, names = FALSE),
                     q50 = stats::quantile(lr, 0.5, names = FALSE),
                     q90 = stats::quantile(lr, 0.9, names = FALSE), .groups = "drop") |>
    dplyr::mutate(source = dplyr::if_else(n_totals >= min_n, "this insect", "all insects"),
                  q10 = dplyr::if_else(n_totals >= min_n, q10, pooled[1]),
                  q50 = dplyr::if_else(n_totals >= min_n, q50, pooled[2]),
                  q90 = dplyr::if_else(n_totals >= min_n, q90, pooled[3]))
}

# Calibration check: learn the spread on odd folds, test on even folds, and back
range_calibration <- function(bt, min_n = 30) {
  one <- function(train, test) {
    rt <- range_table(train, min_n)
    pooled <- stats::quantile(train$lr, c(0.1, 0.9), names = FALSE)
    test |>
      dplyr::left_join(dplyr::select(rt, species, q10, q90), by = "species") |>
      dplyr::mutate(q10 = dplyr::coalesce(q10, pooled[1]), q90 = dplyr::coalesce(q90, pooled[2]),
                    inside = lr >= q10 & lr <= q90)
  }
  odd <- bt$fold %% 2 == 1
  dplyr::bind_rows(one(bt[odd, ], bt[!odd, ]), one(bt[!odd, ], bt[odd, ])) |>
    dplyr::summarise(n_totals = dplyr::n(), share_inside_80pct_range = mean(inside),
                     share_below = mean(lr < q10), share_above = mean(lr > q90))
}

gapfill_dataset_v2 <- function(raw, conf, unit_lookup, all_species) {
  conf_all <- dplyr::bind_rows(
    dplyr::select(conf, species, confidence),
    tibble::tibble(species = setdiff(all_species, conf$species), confidence = "untested"))
  raw |>
    dplyr::left_join(conf_all, by = "species") |>
    dplyr::left_join(unit_lookup, by = "species") |>
    dplyr::mutate(
      value_type = dplyr::case_when(
        !is.na(y) & y > 0 ~ "reported damage",
        !is.na(y) ~ "reported zero",
        absent ~ "zero: host tree absent",
        code_type(species) == "aggregate group" ~ "not filled: aggregate reporting code",
        empty | is.na(factor_fill) ~ "not fillable (nothing recorded in this region-year)",
        TRUE ~ "filled"),
      damage = dplyr::case_when(
        value_type %in% c("reported damage", "reported zero") ~ expm1(y),
        value_type == "zero: host tree absent" ~ 0,
        value_type == "filled" ~ factor_fill),
      p_any = dplyr::if_else(value_type == "filled", p_any, NA_real_),
      confidence = dplyr::if_else(value_type == "filled", confidence, NA_character_)) |>
    dplyr::select(country, nuts_id, year, species, unit, value_type, damage, confidence, p_any, host_area_ha)
}

# Country x insect x year filled totals with 80% ranges, and Europe-wide
# insect x year totals: reported + filled (reliable and weak), with an 80%
# range from 2000 draws: each country's filled total is multiplied by a test
# error log(real / predicted) resampled from that insect's hidden totals
# (all insects pooled when it has < min_n), countries drawn independently.
gapfill_totals_v2 <- function(gd, rt, bt, n_draw = 2000, min_n = 30, seed = 20261015) {
  set.seed(seed)
  ct <- gd |>
    dplyr::group_by(country, species, unit, year) |>
    dplyr::summarise(
      reported = sum(damage[value_type %in% c("reported damage", "reported zero")]),
      filled_tested = sum(damage[value_type == "filled" & confidence %in% c("reliable", "weak")]),
      filled_untested = sum(damage[value_type == "filled" & confidence == "untested"]),
      .groups = "drop") |>
    dplyr::left_join(dplyr::select(rt, species, q10, q90), by = "species") |>
    dplyr::mutate(filled_lo80 = pmax((filled_tested + 1) * exp(q10) - 1, 0),
                  filled_hi80 = (filled_tested + 1) * exp(q90) - 1)
  pool_all <- bt$lr
  pools <- split(bt$lr, bt$species)
  pool_of <- function(s) if (!is.null(pools[[s]]) && length(pools[[s]]) >= min_n) pools[[s]] else pool_all
  eu <- ct |>
    dplyr::group_by(species, unit, year) |>
    dplyr::group_modify(function(d, key) {
      f <- d$filled_tested[d$filled_tested > 0]
      draws <- if (length(f) > 0) {
        e <- matrix(sample(pool_of(key$species), n_draw * length(f), replace = TRUE), n_draw)
        rowSums(pmax(exp(e) * matrix(f + 1, n_draw, length(f), byrow = TRUE) - 1, 0))
      } else rep(0, n_draw)
      tibble::tibble(reported = sum(d$reported), filled = sum(d$filled_tested),
                     filled_untested = sum(d$filled_untested),
                     total = sum(d$reported) + sum(d$filled_tested),
                     total_lo80 = sum(d$reported) + stats::quantile(draws, 0.1, names = FALSE),
                     total_mid = sum(d$reported) + stats::quantile(draws, 0.5, names = FALSE),
                     total_hi80 = sum(d$reported) + stats::quantile(draws, 0.9, names = FALSE))
    }) |>
    dplyr::ungroup()
  list(country = ct, europe = eu)
}


# -----------------------------------------------------------------------------
# lfm_m12.R
# -----------------------------------------------------------------------------
# Factor model, steps M1 and M2 (Oct 2026). New functions alongside the
# existing ones (R/lfm.R, R/hurdle.R), so earlier results stay reproducible.
#
# M1 - region + year structure for the region-year intensity.
#   Before (M0): every region-year i has its own intensity c_i, pulled
#   towards 0 (ridge lambda_c = 1).
#   Now: c_i is pulled towards a_region + b_year,
#       penalty  lambda_rc * (c_i - a_r(i) - b_t(i))^2,
#   where a_r (region level: size, reporting style) and b_t (Europe-wide year)
#   are themselves estimated from all region-years (mild ridge, k0 = 1).
#   Region-years with few insects recorded then borrow their intensity from
#   their region's usual level and that year's Europe-wide level, instead of
#   from a handful of records. Same in the any-damage part (logit_fit_rs).
#
# M2 - more hidden factors, with each insect's sensitivities (loadings)
#   pulled towards its broad group's (tau_links > 0), so extra factors can
#   pick up insect-specific links without overfitting.
#
# Seeds fixed as in the originals.

region_year_prior <- function(cc, region, year, keep, k0 = 1, sweeps = 5) {
  a <- stats::setNames(rep(0, length(unique(region))), unique(region))
  b <- stats::setNames(rep(0, length(unique(year))), unique(year))
  r <- as.character(region); t <- as.character(year)
  for (s in seq_len(sweeps)) {
    ra <- tapply((cc - b[t])[keep], r[keep], sum); na <- tapply(rep(1, sum(keep)), r[keep], sum)
    a[names(ra)] <- ra / (na + k0)
    rb <- tapply((cc - a[r])[keep], t[keep], sum); nb <- tapply(rep(1, sum(keep)), t[keep], sum)
    b[names(rb)] <- rb / (nb + k0)
  }
  list(prior = unname(a[r] + b[t]), a = a, b = b)
}

lfm_fit_rs <- function(Y, K, lambda, region, year, lambda_rc = 5, n_iter = 200, tol = 1e-6,
                       seed = 20261009, group = NULL, tau = 0, tau_links = tau) {
  set.seed(seed)
  n <- nrow(Y); p <- ncol(Y); M <- !is.na(Y)
  mu <- colMeans(Y, na.rm = TRUE); mu[is.na(mu)] <- 0
  cc <- rep(0, n); pr <- rep(0, n)
  U <- matrix(0, n, K)
  V <- matrix(stats::rnorm(p * K, sd = 0.1), p, K)
  pat <- apply(M, 1, function(m) paste(which(m), collapse = ","))
  groups <- split(seq_len(n), pat)
  empty_rows <- which(rowSums(M) == 0); keep <- rowSums(M) > 0
  pen_row <- diag(c(lambda_rc, rep(lambda, K)), K + 1)
  pen_col <- diag(c(1e-8, rep(lambda, K)), K + 1)
  hier <- !is.null(group) && (tau > 0 || tau_links > 0)
  if (hier) {
    tvec <- c(tau, rep(tau_links, K))
    pen_col <- pen_col + diag(tvec, K + 1)
    gi <- split(seq_len(p), group)
  }
  prev <- Inf
  for (it in seq_len(n_iter)) {
    for (g in groups) {
      cols <- M[g[1], ]
      if (!any(cols)) next
      X <- cbind(1, V[cols, , drop = FALSE])
      R <- t(Y[g, cols, drop = FALSE]) - mu[cols]
      rhs <- crossprod(X, R); rhs[1, ] <- rhs[1, ] + lambda_rc * pr[g]   # pull towards region + year
      B <- solve(crossprod(X) + pen_row, rhs)
      cc[g] <- B[1, ]
      if (K > 0) U[g, ] <- t(B[-1, , drop = FALSE])
    }
    ry <- region_year_prior(cc, region, year, keep); pr <- ry$prior
    if (hier) {
      Th <- cbind(mu, V)
      for (gg in gi) Th[gg, ] <- matrix(colMeans(Th[gg, , drop = FALSE]), length(gg), K + 1, byrow = TRUE)
    }
    for (s in seq_len(p)) {
      rr <- M[, s]
      if (!any(rr)) next
      X <- cbind(1, U[rr, , drop = FALSE])
      rhs <- crossprod(X, Y[rr, s] - cc[rr])
      if (hier) rhs <- rhs + tvec * Th[s, ]
      b <- solve(crossprod(X) + pen_col, rhs)
      mu[s] <- b[1]
      if (K > 0) V[s, ] <- b[-1]
    }
    fit <- outer(cc, mu, "+") + U %*% t(V)
    loss <- mean((Y[M] - fit[M])^2)
    if (abs(prev - loss) < tol) break
    prev <- loss
  }
  list(mu = mu, c = cc, U = U, V = V, K = K, lambda = lambda, iter = it, a_region = ry$a, b_year = ry$b,
       train_rmse = sqrt(loss), empty_rows = empty_rows)
}

logit_fit_rs <- function(B, K, lambda, region, year, lambda_rb = 5, n_iter = 600, lr = 0.05,
                         seed = 20261012, group = NULL, tau = 0, tau_links = tau, update_every = 25) {
  set.seed(seed)
  M <- !is.na(B); Bz <- B; Bz[!M] <- 0
  n <- nrow(B); p <- ncol(B); keep <- rowSums(M) > 0
  rate <- pmin(pmax(colMeans(B, na.rm = TRUE), 0.01), 0.99); rate[is.na(rate)] <- 0.5
  pars <- list(a = stats::qlogis(rate), b = rep(0, n),
               U = matrix(stats::rnorm(n * K, sd = 0.01), n, K),
               V = matrix(stats::rnorm(p * K, sd = 0.01), p, K))
  pr <- rep(0, n)
  hier <- !is.null(group) && (tau > 0 || tau_links > 0)
  gmean <- function(x) { x <- as.matrix(x); for (g in unique(group)) { i <- group == g
    x[i, ] <- matrix(colMeans(x[i, , drop = FALSE]), sum(i), ncol(x), byrow = TRUE) }; x }
  m <- lapply(pars, function(x) x * 0); v <- m
  b1 <- 0.9; b2 <- 0.999; eps <- 1e-8
  for (t in seq_len(n_iter)) {
    if (t %% update_every == 0) pr <- region_year_prior(pars$b, region, year, keep)$prior
    eta <- outer(pars$b, pars$a, "+") + if (K > 0) pars$U %*% t(pars$V) else 0
    G <- (sigmoid(eta) - Bz) * M
    grads <- list(
      a = colSums(G),
      b = rowSums(G) + 2 * lambda_rb * (pars$b - pr),
      U = if (K > 0) G %*% pars$V + 2 * lambda * pars$U else pars$U,
      V = if (K > 0) t(G) %*% pars$U + 2 * lambda * pars$V else pars$V)
    if (hier) {
      grads$a <- grads$a + 2 * tau * (pars$a - as.numeric(gmean(pars$a)))
      if (K > 0) grads$V <- grads$V + 2 * tau_links * (pars$V - gmean(pars$V))
    }
    for (nm in names(pars)) {
      m[[nm]] <- b1 * m[[nm]] + (1 - b1) * grads[[nm]]
      v[[nm]] <- b2 * v[[nm]] + (1 - b2) * grads[[nm]]^2
      pars[[nm]] <- pars[[nm]] - lr * (m[[nm]] / (1 - b1^t)) / (sqrt(v[[nm]] / (1 - b2^t)) + eps)
    }
  }
  pars$c <- rep(0, p); pars$X <- matrix(0, n, p)
  pars$K <- K; pars$empty_rows <- which(rowSums(M) == 0)
  pars
}

# The final gap-filling model (as gapfill_final, factor model only) with the
# M1/M2 options. spec: list(K1, lambda1, K2, lambda2, tau, tau_links, lambda_rc)
# lambda_rc = 0 and tau_links = 0 reproduce the M0 structure (intensity
# pulled to 0 instead of region + year).
gapfill_final_rs <- function(L, hs, group, spec) {
  Lh <- host_scaled_L(L, hs); absent <- host_absent_matrix(L, hs)
  hp <- hurdle_parts(L$Y); Y2s <- hurdle_parts(Lh$Y)$Y2
  reg <- paste(L$rows$country, L$rows$nuts_id); yr <- L$rows$year
  lrc <- if (spec$lambda_rc > 0) spec$lambda_rc else 1
  if (spec$lambda_rc > 0) {
    f1 <- logit_fit_rs(hp$B, spec$K1, spec$lambda1, reg, yr, lambda_rb = spec$lambda_rc,
                       group = group, tau = spec$tau, tau_links = spec$tau_links)
    f2 <- lfm_fit_rs(hp$Y2, spec$K2, spec$lambda2, reg, yr, lambda_rc = spec$lambda_rc,
                     group = group, tau = spec$tau, tau_links = spec$tau_links)
    g2 <- lfm_fit_rs(Y2s, spec$K2, spec$lambda2, reg, yr, lambda_rc = spec$lambda_rc,
                     group = group, tau = spec$tau, tau_links = spec$tau_links)
  } else {
    f1 <- logit_fit(hp$B, spec$K1, spec$lambda1, group = group, tau = spec$tau, tau_links = spec$tau_links)
    f2 <- lfm_fit(hp$Y2, K = spec$K2, lambda = spec$lambda2, group = group, tau = spec$tau, tau_links = spec$tau_links)
    g2 <- lfm_fit(Y2s, K = spec$K2, lambda = spec$lambda2, group = group, tau = spec$tau, tau_links = spec$tau_links)
  }
  P1 <- logit_predict(f1); P2 <- lfm_predict(f2); P2s <- lfm_predict(g2)
  sm <- smearing(f2, hp$Y2); sms <- smearing(g2, Y2s)
  scaled <- !is.na(Lh$area) & !is.na(P2s)
  amount_log <- ifelse(scaled, log1p(pmax(expm1(P2s) * sms, 0) * Lh$area / 1000), log1p(pmax(expm1(P2) * sm, 0)))
  E <- host_combine(P1, P2s, sms, P2, sm, Lh$area, absent)
  list(P1 = P1, amount_log = amount_log, E = E, absent = absent, V2 = g2$V, V1 = f1$V)
}

# Hide-and-predict test of one model spec (same blocks as before)
variant_cv <- function(L, blocks, hs, group, spec) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ltr <- L; Ltr$Y[H] <- NA
    r <- gapfill_final_rs(Ltr, hs, group, spec)
    idx <- which(H, arr.ind = TRUE)
    out[[length(out) + 1]] <- tibble::tibble(
      fold = f, country = L$rows$country[idx[, 1]], species = L$species[idx[, 2]], year = L$rows$year[idx[, 1]],
      observed = L$Y[idx], pred = r$E[idx],
      pred_log = ifelse(r$absent[idx], 0, r$P1[idx] * r$amount_log[idx]))
  }
  dplyr::bind_rows(out)
}

variant_scores <- function(fc, name) {
  d <- dplyr::filter(fc, !is.na(pred))
  bt <- d |> dplyr::group_by(fold, country, species, year) |>
    dplyr::summarise(real = sum(expm1(observed)), pred = sum(pred), .groups = "drop") |>
    dplyr::filter(real > 0) |> dplyr::mutate(lr = abs(log((pred + 1) / (real + 1))))
  tibble::tibble(model = name, n_cells = nrow(d),
                 error_log = sqrt(mean((d$pred_log - d$observed)^2)),
                 damage_vs_none_auc = auc(d$pred_log, as.numeric(d$observed > 0)),
                 totals_within_2x = mean(bt$lr <= log(2)),
                 typical_total_error_x = exp(stats::median(bt$lr)))
}

# How many of the usable pairs does the model's amount part link (loading
# cosine >= 0.3), and how well do its links track the measured r?
variant_links <- function(L, hs, group, spec, verdict, name) {
  r <- gapfill_final_rs(L, hs, group, spec)
  V <- r$V2; rownames(V) <- L$species
  nv <- sqrt(rowSums(V^2)); C <- (V %*% t(V)) / (nv %o% nv)
  rm <- mapply(function(a, b) if (a %in% rownames(C) && b %in% rownames(C)) C[a, b] else NA,
               verdict$species_a, verdict$species_b)
  u <- verdict$verdict == "usable"
  tibble::tibble(model = name, usable_linked = sum(rm[u] >= 0.3, na.rm = TRUE), usable_n = sum(u),
                 agreement_with_r = stats::cor(verdict$r, rm, use = "pair"))
}

# Full-data fit of a spec, laid out like gapfill_final()'s output (one row per
# region-year x insect, column by column), so the v2 dataset functions apply.
gapfill_raw_rs <- function(L, hs, group, spec) {
  r <- gapfill_final_rs(L, hs, group, spec)
  Lh <- host_scaled_L(L, hs)
  n <- nrow(L$Y); p <- ncol(L$Y); ix <- cbind(rep(seq_len(n), p), rep(seq_len(p), each = n))
  tibble::tibble(
    country = L$rows$country[ix[, 1]], nuts_id = L$rows$nuts_id[ix[, 1]], year = L$rows$year[ix[, 1]],
    species = L$species[ix[, 2]], y = L$Y[ix], absent = r$absent[ix], empty = is.na(r$P1[ix]),
    p_any = r$P1[ix], amount_log = r$amount_log[ix], factor_fill = r$E[ix], host_area_ha = Lh$area[ix])
}


# -----------------------------------------------------------------------------
# range_rule.R
# -----------------------------------------------------------------------------
# Draft "could this insect occur here?" rule (Oct 2026), test round.
#
# The factor model fills an insect wherever its host tree grows, which puts
# e.g. the pine processionary moth (Mediterranean) into central European pine
# forests. Until an expert country x insect range table is available, a
# data-only draft rule decides where an insect may be filled:
#   in range  : the country reports damage (> 0) from the insect, or
#               shares a land border with a country that does;
#   otherwise : "zero: outside range (draft rule)", like host tree absent.
# Countries that report the insect only as zeros (monitored, never damaged)
# count as in range only through a neighbour. The table is written out so it
# can be corrected by hand (Tomas / Roman) and read back in later.

country_neighbours <- list(
  AT = c("CH", "CZ", "DE", "HU", "IT", "SI", "SK"), BG = c("RO", "RS"), CH = c("AT", "DE", "IT"),
  CZ = c("AT", "DE", "PL", "SK"), DE = c("AT", "CH", "CZ", "PL"), FI = "SE",
  HR = c("HU", "RS", "SI"), HU = c("AT", "HR", "RO", "RS", "SI", "SK"), IT = c("AT", "CH", "SI"),
  LT = "PL", PL = c("CZ", "DE", "LT", "SK"), RO = c("BG", "HU", "RS"),
  RS = c("BG", "HR", "HU", "RO"), SE = "FI", SI = c("AT", "HR", "HU", "IT"), SK = c("AT", "CZ", "HU", "PL"))

range_draft <- function(L) {
  countries <- sort(unique(L$rows$country))
  dmg <- tibble::tibble(country = rep(L$rows$country, ncol(L$Y)),
                        species = rep(L$species, each = nrow(L$Y)), y = as.vector(L$Y)) |>
    dplyr::group_by(country, species) |>
    dplyr::summarise(damage_reported = any(y > 0, na.rm = TRUE), .groups = "drop")
  tidyr::expand_grid(country = countries, species = L$species) |>
    dplyr::left_join(dmg, by = c("country", "species")) |>
    dplyr::mutate(damage_reported = dplyr::coalesce(damage_reported, FALSE)) |>
    dplyr::group_by(species) |>
    dplyr::mutate(neighbour_reports = purrr::map_lgl(country, function(c)
      any(country_neighbours[[c]] %in% country[damage_reported])),
      in_range = damage_reported | neighbour_reports,
      reason = dplyr::case_when(damage_reported ~ "reports damage",
                                neighbour_reports ~ "neighbour reports damage",
                                TRUE ~ "no evidence nearby")) |>
    dplyr::ungroup()
}

# Wide version for hand correction: 1 = in range, 0 = outside
range_draft_wide <- function(rd) {
  rd |> dplyr::transmute(species, country, in_range = as.integer(in_range)) |>
    tidyr::pivot_wider(names_from = country, values_from = in_range) |>
    dplyr::arrange(species)
}

# Apply to the v2 dataset: filled cells outside the range become 0
apply_range <- function(gd, rd) {
  gd |>
    dplyr::left_join(dplyr::select(rd, country, species, in_range), by = c("country", "species")) |>
    dplyr::mutate(
      outside = value_type == "filled" & !dplyr::coalesce(in_range, TRUE),
      value_type = dplyr::if_else(outside, "zero: outside range (draft rule)", value_type),
      damage = dplyr::if_else(outside, 0, damage),
      confidence = dplyr::if_else(outside, NA_character_, confidence),
      p_any = dplyr::if_else(outside, NA_real_, p_any)) |>
    dplyr::select(-in_range, -outside)
}

# Filled damage per insect (all years), before and after the draft range rule
range_effect <- function(tot_before, tot_after) {
  f <- function(t) t$europe |> dplyr::group_by(species, unit) |>
    dplyr::summarise(filled = sum(total_mid - reported), reported = sum(reported), .groups = "drop")
  dplyr::full_join(dplyr::rename(f(tot_before), filled_before = filled),
                   dplyr::select(f(tot_after), species, filled_after = filled), by = "species") |>
    dplyr::mutate(filled_vs_reported_before = filled_before / pmax(reported, 1),
                  filled_vs_reported_after = filled_after / pmax(reported, 1)) |>
    dplyr::arrange(dplyr::desc(filled_before))
}

# Manual corrections to the draft range table: for each listed insect, only
# the given countries are in range (an expert assumption, labelled as such).
# Scenario (Oct 2026): tha_pit taken as Thaumetopoea pityocampa, the
# Mediterranean pine processionary moth; among the 16 EU-ForDAM countries it
# occurs in BG, CH (south), HR, IT and SI. Germany's own reports are kept as
# reported; the rule only decides where gaps may be filled.
range_overrides_pityocampa <- tibble::tibble(species = "tha_pit", allowed = list(c("BG", "CH", "HR", "IT", "SI")))

apply_range_overrides <- function(rd, ov) {
  for (k in seq_len(nrow(ov))) {
    i <- rd$species == ov$species[k]
    rd$in_range[i] <- rd$country[i] %in% ov$allowed[[k]]
    rd$reason[i] <- dplyr::if_else(rd$in_range[i], "expert assumption: in range", "expert assumption: outside range")
  }
  rd
}


# -----------------------------------------------------------------------------
# figures.R
# -----------------------------------------------------------------------------
# Figures. Colours: diverging blue <-> grey <-> red for correlations,
# single-hue blue ramp for magnitudes, fixed categorical slots for guild/host.

pal <- list(
  surface = "#fcfcfb", ink = "#0b0b0b", ink2 = "#52514e", na = "#e6e5e1",
  div_low = "#1c5cab", div_mid = "#f0efec", div_high = "#c23b3a",
  seq = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"),
  guild = c(borer = "#2a78d6", defoliator = "#eb6834", other = "#1baf7a",
            `bark borer` = "#2a78d6", `wood borer` = "#4a3aa7"),
  host = c(conifer = "#008300", broadleaf = "#eda100", both = "#e87ba4")
)

theme_eu <- function(base = 9) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = pal$surface, colour = NA),
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(colour = pal$ink),
      axis.text = ggplot2::element_text(colour = pal$ink2),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(colour = pal$ink2, hjust = 0)
    )
}

save_fig <- function(p, path, w, h) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, p, width = w, height = h, dpi = 200, bg = pal$surface)
  path
}

plot_coverage <- function(cov_cs) {
  sp_order <- cov_cs |> dplyr::group_by(species) |>
    dplyr::summarise(k = sum(pct_observed > 0)) |> dplyr::arrange(dplyr::desc(k), species) |>
    dplyr::pull(species)
  ct_order <- cov_cs |> dplyr::group_by(country) |>
    dplyr::summarise(k = sum(pct_observed > 0)) |> dplyr::arrange(k) |> dplyr::pull(country)
  d <- cov_cs |>
    dplyr::mutate(species = factor(species, sp_order), country = factor(country, ct_order),
                  pct_observed = dplyr::na_if(pct_observed, 0))
  ggplot2::ggplot(d, ggplot2::aes(species, country, fill = pct_observed)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.5) +
    ggplot2::scale_fill_gradientn(colours = pal$seq, limits = c(0, 100), na.value = pal$na,
                                  name = "% region-years\nobserved") +
    ggplot2::labs(x = NULL, y = NULL, title = "Monitoring coverage: country x species",
                  caption = "Grey = not monitored. Observed = positive or zero value reported, 2000-2022.") +
    theme_eu() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
}

# Heatmap of a species x species matrix in clustering order, with guild/host strips
plot_matrix <- function(M, order, lookup, fill_name, title, caption, type = c("r", "n")) {
  type <- match.arg(type)
  d <- as.data.frame(as.table(M[order, order]), stringsAsFactors = FALSE) |>
    stats::setNames(c("a", "b", "value")) |>
    dplyr::mutate(a = factor(a, order), b = factor(b, rev(order)))
  if (type == "n") d$value[d$value == 0] <- NA

  ann <- lookup |> dplyr::filter(species %in% order) |>
    dplyr::mutate(a = factor(species, order))
  n <- length(order)

  p <- ggplot2::ggplot(d, ggplot2::aes(a, b)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = pal$surface, linewidth = 0.3)
  p <- if (type == "r") {
    # mark missing cells explicitly: the NA grey is too close to the r = 0 midpoint
    p + ggplot2::geom_point(data = dplyr::filter(d, is.na(value), as.character(a) != as.character(b)),
                            shape = 4, size = 1, colour = pal$ink2) +
      ggplot2::scale_fill_gradient2(low = pal$div_low, mid = pal$div_mid, high = pal$div_high,
                                      midpoint = 0, limits = c(-1, 1), na.value = pal$na, name = fill_name)
  } else {
    p + ggplot2::scale_fill_gradientn(colours = pal$seq, trans = "log10", na.value = pal$na, name = fill_name)
  }
  p +
    ggnewscale_guild(ann, n) +
    ggplot2::coord_equal(clip = "off", ylim = c(0.5, n + 2.6), expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = title, caption = caption) +
    theme_eu(8) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
}

# Guild and host strips above the matrix drawn as coloured points with a legend
# encoded by shape, so identity never relies on colour alone.
ggnewscale_guild <- function(ann, n) {
  list(
    ggplot2::geom_point(data = ann, ggplot2::aes(a, n + 1.2, colour = guild, shape = guild),
                        size = 2.2, inherit.aes = FALSE),
    ggplot2::geom_point(data = ann, ggplot2::aes(a, n + 2.1, colour = host, shape = host),
                        size = 2.2, inherit.aes = FALSE),
    ggplot2::scale_colour_manual(values = c(pal$guild, pal$host), na.value = pal$ink2,
                                 name = "Guild (row 1) / host (row 2)"),
    ggplot2::scale_shape_manual(values = c(borer = 15, defoliator = 17, other = 18,
                                           `bark borer` = 15, `wood borer` = 3,
                                           conifer = 16, broadleaf = 1, both = 10),
                                na.value = 4, name = "Guild (row 1) / host (row 2)"),
    ggplot2::annotate("text", x = 0.3, y = c(n + 1.2, n + 2.1), label = c("guild", "host"),
                      hjust = 1, size = 2.5, colour = pal$ink2)
  )
}

# Country-level r for the strongest pairs (with >= 2 countries)
plot_top_pairs <- function(ts, pc, k = 15, version = "z_within") {
  top <- ts |>
    dplyr::filter(n_countries >= 2, !is.na(r_pooled)) |>
    dplyr::arrange(dplyr::desc(abs(r_pooled))) |>
    dplyr::slice_head(n = k) |>
    dplyr::mutate(pair = paste(species_a, "x", species_b))
  lev <- rev(top$pair)
  d <- pc |> dplyr::filter(version == !!version) |>
    dplyr::inner_join(dplyr::select(top, species_a, species_b, pair, transferability),
                      by = c("species_a", "species_b")) |>
    dplyr::mutate(pair = factor(pair, lev)) |>
    # alternate labels above/below the dot so neighbouring countries stay legible
    dplyr::group_by(pair) |> dplyr::arrange(r_pearson, .by_group = TRUE) |>
    dplyr::mutate(lab_v = dplyr::if_else(dplyr::row_number() %% 2 == 1, -1.1, 2.1)) |>
    dplyr::ungroup()
  top <- top |> dplyr::mutate(pair = factor(pair, lev))
  ggplot2::ggplot(d, ggplot2::aes(r_pearson, pair)) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(size = n), colour = pal$seq[3], alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = country, vjust = lab_v), size = 2.3, colour = pal$ink2) +
    ggplot2::geom_point(data = top, ggplot2::aes(r_pooled, pair), shape = 124, size = 6,
                        colour = pal$ink, inherit.aes = FALSE) +
    ggplot2::geom_text(data = top, ggplot2::aes(1.02, pair, label = transferability),
                       hjust = 0, size = 2.5, colour = pal$ink2, inherit.aes = FALSE) +
    ggplot2::scale_x_continuous(limits = c(-1, 1.45), breaks = seq(-1, 1, 0.5)) +
    ggplot2::scale_size_area(max_size = 5, name = "n region-years") +
    ggplot2::labs(x = paste0("Pearson r (", version, ")"), y = NULL,
                  title = "Strongest pairs: correlation within each country",
                  caption = "Dots = countries (n >= 10); black bar = pooled r; right = transferability flag.") +
    theme_eu() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# Species ranking: regions where the species occurs (dark) out of regions where
# it is monitored (light), one panel per guild.
plot_species_ranking <- function(sp_rank) {
  d <- sp_rank |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other"),
                  species = factor(species, rev(species)),
                  lab = paste0(n_regions_occurring, " (", n_countries_occurring, " ctry)"))
  ggplot2::ggplot(d, ggplot2::aes(y = species)) +
    ggplot2::geom_col(ggplot2::aes(x = n_regions_monitored), fill = "#e6e5e1", width = 0.75) +
    ggplot2::geom_col(ggplot2::aes(x = n_regions_occurring, fill = guild), width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(x = n_regions_monitored, label = lab), hjust = -0.1,
                       size = 2.4, colour = pal$ink2) +
    ggplot2::scale_fill_manual(values = pal$guild, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::facet_grid(guild ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "Number of NUTS units", y = NULL,
                  title = "Where does each insect occur?",
                  subtitle = "Coloured bar = units with damage in at least one year; grey bar = units where monitored",
                  caption = "Label: units with damage (number of countries). National-level countries count as one unit.") +
    theme_eu(9) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# Region x species map: years with damage. Blank = not monitored,
# grey = monitored but never damaged, blue = years with damage.
plot_occurrence_map <- function(rs, sp_rank) {
  d <- rs |>
    dplyr::mutate(
      species = factor(species, sp_rank$species),
      region = paste0(nuts_id, " "),
      yrs = dplyr::na_if(years_with_damage, 0L)
    )
  reg_order <- d |> dplyr::distinct(country, region) |> dplyr::arrange(dplyr::desc(country), dplyr::desc(region))
  d$region <- factor(d$region, reg_order$region)
  ggplot2::ggplot(d, ggplot2::aes(species, region, fill = yrs)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.3) +
    ggplot2::scale_fill_gradientn(colours = pal$seq, limits = c(1, 23), na.value = "#cfcec9",
                                  name = "Years with\ndamage") +
    ggplot2::facet_grid(country ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL, title = "Insect occurrence by NUTS unit, 2000-2022",
                  caption = paste("Blank = not monitored. Grey = monitored, no damage recorded. Insects ordered by number of units where they occur.",
                                  "PL rows are district codes aggregated to NUTS2.")) +
    theme_eu(7) +
    ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5),
                   strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold"),
                   panel.spacing.y = ggplot2::unit(1.5, "pt"))
}

# Rank-occupancy curve: species (y, ranked) against number of NUTS units where
# they occur (x), joined by a line from the rarest to the most widespread.
plot_occupancy_curve <- function(sp_rank) {
  d <- sp_rank |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other")) |>
    dplyr::arrange(n_regions_occurring, dplyr::desc(species)) |>
    dplyr::mutate(species = factor(species, species), y = as.integer(species))
  n_units <- max(sp_rank$n_regions_monitored)
  ggplot2::ggplot(d, ggplot2::aes(n_regions_occurring, y)) +
    ggplot2::geom_path(colour = pal$ink2, linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(colour = guild, shape = guild), size = 2.6) +
    ggplot2::geom_text(ggplot2::aes(label = n_regions_occurring), hjust = 0, nudge_x = 1.3,
                       size = 2.4, colour = pal$ink2) +
    ggplot2::scale_y_continuous(breaks = d$y, labels = as.character(d$species),
                                expand = ggplot2::expansion(add = 0.8)) +
    ggplot2::scale_x_continuous(limits = c(0, n_units), breaks = seq(0, 80, 10)) +
    ggplot2::scale_colour_manual(values = pal$guild, name = "Guild") +
    ggplot2::scale_shape_manual(values = c(borer = 16, defoliator = 17, other = 15), name = "Guild") +
    ggplot2::labs(x = "Number of NUTS units where the species occurs (damage > 0 in at least one year)",
                  y = NULL, title = "Species occupancy across NUTS units",
                  subtitle = "Few widespread species, many species found in few units",
                  caption = paste0("Out of ", n_units, " NUTS units in the dataset. National-level countries count as one unit; PL district codes aggregated to NUTS2.")) +
    theme_eu(9) +
    ggplot2::theme(legend.position = c(0.82, 0.15),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3),
                   panel.grid.major.y = ggplot2::element_line(colour = "#f3f2ee", linewidth = 0.2))
}

# Two rank-occupancy curves side by side: NUTS units where each species is
# monitored, and NUTS units where it occurs. Each panel is ranked on its own
# measure, so species order differs between panels.
plot_occupancy_curves_monitored <- function(sp_rank) {
  # One graph: insects ranked by the number of NUTS units with damage (filled
  # dot); the hollow dot on the same row is the number of units that reported
  # the insect at all (0 or > 0). The gap between them = checked but never found.
  n_units <- max(sp_rank$n_regions_monitored)
  d <- sp_rank |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other")) |>
    dplyr::arrange(n_regions_occurring, n_regions_monitored, dplyr::desc(species)) |>
    dplyr::mutate(species = factor(species, species),
                  lab = paste0(n_regions_occurring, " / ", n_regions_monitored))
  pts <- dplyr::bind_rows(
    d |> dplyr::transmute(species, guild, n = n_regions_monitored, what = "reported (0 or > 0)"),
    d |> dplyr::transmute(species, guild, n = n_regions_occurring, what = "damage > 0 in at least one year")
  ) |>
    dplyr::mutate(what = factor(what, c("damage > 0 in at least one year", "reported (0 or > 0)")))
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = d, ggplot2::aes(x = n_regions_occurring, xend = n_regions_monitored,
                                                 y = species, yend = species, colour = guild),
                          linewidth = 1.6, alpha = 0.25) +
    ggplot2::geom_point(data = pts, ggplot2::aes(n, species, colour = guild, shape = what), size = 2.4, stroke = 0.9) +
    ggplot2::geom_text(data = d, ggplot2::aes(x = n_units + 2, y = species, label = lab),
                       hjust = 0, size = 2.4, colour = pal$ink2) +
    ggplot2::annotate("text", x = n_units + 2, y = nrow(d) + 1.2, label = "damage / reported",
                      hjust = 0, size = 2.4, fontface = "bold", colour = pal$ink2) +
    ggplot2::scale_shape_manual(values = c(`damage > 0 in at least one year` = 16, `reported (0 or > 0)` = 1),
                                name = "NUTS units where the insect was") +
    ggplot2::scale_colour_manual(values = c(pal$guild, other = pal$ink2), name = "Guild") +
    ggplot2::scale_x_continuous(limits = c(0, n_units + 12), breaks = seq(0, 80, 10)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Number of NUTS units", y = NULL,
                  title = "Species-NUTS occurrence ranking",
                  subtitle = "Ranked by units with damage (filled dot). Hollow dot = units that reported the insect; the gap = reported but never damaged",
                  caption = paste0("Out of ", n_units, " NUTS units, 2000-2022. 0 = monitored, no damage; no value = not monitored. ",
                                   "National-level countries count as one unit; PL district codes aggregated to NUTS2.")) +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical", legend.box.just = "left",
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3),
                   panel.grid.major.y = ggplot2::element_line(colour = "#f3f2ee", linewidth = 0.2))
}

# Explainer: what a correlation looks like. For each example pair and country,
# the yearly z-score of both species (mean over the country's regions).
# Lines that rise and fall together = high r.
plot_correlation_explainer <- function(tr, pc, examples) {
  d <- purrr::pmap_dfr(examples, function(a, b, country, label) {
    tr |>
      dplyr::filter(country == !!country, species %in% c(a, b), !is.na(z_within)) |>
      dplyr::group_by(species, year) |>
      dplyr::summarise(z = mean(z_within), .groups = "drop") |>
      dplyr::mutate(role = dplyr::if_else(species == a, "I. typographus", "Partner species"),
                    country = country, label = label, pair_b = b)
  })
  rr <- examples |>
    dplyr::left_join(dplyr::filter(pc, version == "z_within") |>
                       dplyr::select(a = species_a, b = species_b, country, r_pearson),
                     by = c("a", "b", "country")) |>
    dplyr::mutate(panel = paste0(country, ":  r = ", formatC(r_pearson, format = "f", digits = 2)))
  d <- d |> dplyr::left_join(dplyr::select(rr, label, country, panel), by = c("label", "country"))
  d$label <- factor(d$label, unique(examples$label))
  ggplot2::ggplot(d, ggplot2::aes(year, z, colour = role)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#d9d8d4", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = c(`I. typographus` = pal$guild[["borer"]],
                                            `Partner species` = pal$guild[["defoliator"]]), name = NULL) +
    ggplot2::facet_wrap(label ~ panel, ncol = 3, labeller = ggplot2::labeller(.multi_line = FALSE)) +
    ggplot2::labs(x = NULL, y = "Damage, standardised (0 = that region's average)",
                  title = "What a correlation looks like",
                  subtitle = "Lines that rise and fall together = high r. Top row: the link holds in every country. Bottom row: strong in one country, absent in others.",
                  caption = "z_within averaged over each country's regions. r = correlation across region-years in that country.") +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   panel.grid.major.y = ggplot2::element_line(colour = "#f3f2ee", linewidth = 0.2))
}

# ---- Latent factor model figures -------------------------------------------

# Model comparison: skill vs the species x year baseline and dynamics r
plot_lfm_comparison <- function(score, grid, best_model, n_blocks) {
  d <- score |>
    dplyr::left_join(grid, by = "model") |>
    dplyr::filter(is.na(K) | lambda == 50 | (K == 0)) |>
    dplyr::mutate(
      label = dplyr::case_when(
        is.na(K) ~ sub("Baseline: ", "", model),
        K == 0 ~ "Additive (region-year intensity, no factors)",
        TRUE ~ paste0(K, " hidden factor", ifelse(K > 1, "s", ""))),
      type = dplyr::case_when(is.na(K) ~ "Baseline", model == best_model ~ "Chosen model", TRUE ~ "Factor model"),
      label = factor(label, rev(unique(label[order(type != "Baseline", K)])))
    ) |>
    tidyr::pivot_longer(c(skill_vs_year_baseline, dynamics_r), names_to = "metric") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(metric = dplyr::recode(metric,
      skill_vs_year_baseline = "Error reduction vs 'species x year' baseline",
      dynamics_r = "Gets the ups and downs right (r)"))
  ggplot2::ggplot(d, ggplot2::aes(value, label, colour = type)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#d9d8d4") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = value, yend = label), linewidth = 0.6, alpha = 0.5) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(value, format = "f", digits = 2)),
                       hjust = -0.4, size = 2.6, colour = pal$ink2) +
    ggplot2::facet_wrap(~ metric, scales = "free_x") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.25))) +
    ggplot2::scale_colour_manual(values = c(Baseline = "#9c9b97", `Factor model` = pal$seq[3],
                                            `Chosen model` = pal$guild[["defoliator"]]), name = NULL) +
    ggplot2::labs(x = NULL, y = NULL, title = "Does the factor model fill gaps better than simple rules?",
                  subtitle = paste0("Whole country x species blocks hidden and predicted (10-fold cross-validation, ", n_blocks, " blocks)"),
                  caption = "Error = mean squared error on log(damage + 1). r = correlation between predicted and real yearly series.") +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# Per species: how well can the chosen model fill this species' gaps?
plot_lfm_species <- function(score_sp, best_model, species_meta) {
  d <- score_sp |>
    dplyr::filter(model == best_model) |>
    dplyr::left_join(dplyr::select(species_meta, species, guild = guild_from_file), by = "species") |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other")) |>
    dplyr::arrange(skill_vs_year_baseline) |>
    dplyr::mutate(species = factor(species, species))
  ggplot2::ggplot(d, ggplot2::aes(skill_vs_year_baseline, species, fill = guild)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("r = ", formatC(dynamics_r, format = "f", digits = 2)),
                                    x = pmax(skill_vs_year_baseline, 0)), hjust = -0.15, size = 2.4,
                       colour = pal$ink2) +
    ggplot2::scale_fill_manual(values = pal$guild, name = "Guild") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.2))) +
    ggplot2::labs(x = "Error reduction vs 'species x year' baseline (right = model helps)", y = NULL,
                  title = "Which insects can the factor model fill?",
                  subtitle = "Chosen model, hidden country x species blocks. Label: r for the ups and downs.",
                  caption = "Only species recorded in at least 2 countries can be tested.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", legend.justification = "left")
}

# Examples: real vs predicted for hidden series
plot_lfm_examples <- function(cv, best_model, examples) {
  d <- cv |>
    dplyr::filter(model %in% c(best_model, "Baseline: species x year average")) |>
    dplyr::inner_join(examples, by = c("nuts_id", "species")) |>
    dplyr::mutate(model = dplyr::if_else(model == best_model, "Factor model prediction", "Species x year baseline"))
  obs <- dplyr::distinct(d, title, year, observed)
  ggplot2::ggplot(d, ggplot2::aes(year)) +
    ggplot2::geom_line(data = obs, ggplot2::aes(y = observed, colour = "Real (hidden from the model)"), linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = predicted, colour = model, linetype = model), linewidth = 0.7) +
    ggplot2::facet_wrap(~ title, ncol = 3, scales = "free_y") +
    ggplot2::scale_colour_manual(values = c(`Real (hidden from the model)` = pal$ink,
                                            `Factor model prediction` = pal$guild[["defoliator"]],
                                            `Species x year baseline` = "#9c9b97"), name = NULL) +
    ggplot2::scale_linetype_manual(values = c(`Factor model prediction` = "solid",
                                              `Species x year baseline` = "22"), guide = "none") +
    ggplot2::labs(x = NULL, y = "log(damage + 1)", title = "Hidden series: real vs predicted",
                  subtitle = "The model never saw these values; it predicted them from other species in the same region and the species elsewhere.") +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   panel.grid.major.y = ggplot2::element_line(colour = "#f3f2ee", linewidth = 0.2))
}

# What the model learned: species loadings on the hidden factors, and the
# Europe-wide yearly pattern of overall intensity and of each factor
plot_lfm_factors <- function(load, year_scores) {
  K <- sum(grepl("^F[0-9]+$", names(load)))
  ld <- load |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other")) |>
    tidyr::pivot_longer(dplyr::matches("^F[0-9]+$"), names_to = "factor", values_to = "loading") |>
    dplyr::group_by(species) |> dplyr::mutate(ord = loading[factor == "F2"]) |> dplyr::ungroup() |>
    dplyr::arrange(guild, ord) |>
    dplyr::mutate(species = factor(species, unique(species)),
                  factor = paste("Factor", sub("F", "", factor)))
  p1 <- ggplot2::ggplot(ld, ggplot2::aes(loading, species, fill = guild)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::facet_grid(guild ~ factor, scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_manual(values = pal$guild, guide = "none") +
    ggplot2::labs(x = "Loading (how strongly the species follows the factor)", y = NULL,
                  title = "What the hidden factors represent") +
    theme_eu(8) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  ys <- year_scores |>
    dplyr::group_by(year) |>
    dplyr::summarise(dplyr::across(c(intensity, dplyr::matches("^F[0-9]+$")), mean), .groups = "drop") |>
    tidyr::pivot_longer(-year) |>
    dplyr::mutate(name = dplyr::recode(name, intensity = "Overall damage intensity",
                                       !!!stats::setNames(paste("Factor", seq_len(K)), paste0("F", seq_len(K)))))
  p2 <- ggplot2::ggplot(ys, ggplot2::aes(year, value)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#d9d8d4") +
    ggplot2::geom_line(colour = pal$seq[4], linewidth = 0.9) +
    ggplot2::facet_wrap(~ name, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "Mean score across regions",
                  title = "Europe-wide yearly pattern",
                  caption = "Factor scores averaged over all region-years with data.") +
    theme_eu(8) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
  list(loadings = p1, years = p2)
}

# ---- Correlation metrics v3 figures ------------------------------------------

# Strongest links with 95% bootstrap intervals: same-year r and r beyond
# shared bad years, for pairs with a reliable interval (>= 5 regions)
plot_cm_top <- function(cm, k = 25) {
  top <- cm |>
    dplyr::filter(!is.na(same_year_lo)) |>
    dplyr::arrange(dplyr::desc(abs(same_year))) |>
    dplyr::slice_head(n = k) |>
    dplyr::mutate(pair = paste(species_a, "&", species_b))
  d <- dplyr::bind_rows(
    top |> dplyr::transmute(pair, what = "Same year", r = same_year, lo = same_year_lo, hi = same_year_hi),
    top |> dplyr::transmute(pair, what = "Beyond shared bad years", r = beyond_year, lo = beyond_year_lo, hi = beyond_year_hi)
  ) |>
    dplyr::mutate(pair = factor(pair, rev(top$pair)), what = factor(what, c("Same year", "Beyond shared bad years")))
  ggplot2::ggplot(d, ggplot2::aes(r, pair, colour = what)) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.6,
                            position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::scale_colour_manual(values = c(`Same year` = pal$seq[4], `Beyond shared bad years` = pal$guild[["defoliator"]]),
                                 name = NULL) +
    # axis covers every dot and range, so negative links are not cut off
    ggplot2::scale_x_continuous(limits = c(min(-0.2, floor(min(d$lo, d$r, na.rm = TRUE) * 5) / 5), 1),
                                breaks = seq(-1, 1, 0.2)) +
    ggplot2::labs(x = "Correlation r (dot) with 95% range (line). Negative = one up when the other is down", y = NULL,
                  title = "Strongest links between insect species",
                  subtitle = "Blue: do they have bad years together? Orange: still linked after removing Europe-wide bad years?",
                  caption = "Ranges from resampling whole regions 999 times; only pairs observed together in at least 5 regions.") +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# How much do the other species tell us about each species?
plot_cm_species <- function(cms) {
  d <- cms |>
    dplyr::filter(strong_links + strong_but_shared_years + weak_links > 0) |>
    dplyr::transmute(species, guild = dplyr::coalesce(guild, "other"),
                     `Strong, beyond shared bad years` = strong_links,
                     `Strong, mostly shared bad years` = strong_but_shared_years,
                     `Weak but real` = weak_links) |>
    tidyr::pivot_longer(-c(species, guild), names_to = "type", values_to = "n") |>
    dplyr::mutate(type = factor(type, rev(c("Strong, beyond shared bad years", "Strong, mostly shared bad years", "Weak but real"))))
  ord <- d |> dplyr::group_by(species) |>
    dplyr::summarise(s = sum(n * c(3, 2, 1)[as.integer(factor(type, levels(d$type)[3:1]))]), t = sum(n)) |>
    dplyr::arrange(t, s)
  d$species <- factor(d$species, ord$species)
  ggplot2::ggplot(d, ggplot2::aes(n, species, fill = type)) +
    ggplot2::geom_col(width = 0.7, colour = pal$surface, linewidth = 0.3) +
    ggplot2::facet_grid(guild ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_manual(values = c(`Strong, beyond shared bad years` = pal$seq[5],
                                          `Strong, mostly shared bad years` = pal$seq[3],
                                          `Weak but real` = pal$seq[1]), name = NULL,
                               guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::labs(x = "Number of other species clearly linked to it", y = NULL,
                  title = "How much do other insects tell us about each insect?",
                  subtitle = "Links whose 95% range excludes zero. Strong = |r| >= 0.3; 'beyond' = still |r| >= 0.2 after removing Europe-wide bad years.",
                  caption = "Species with no clear link, or recorded too rarely together with others to test, are not shown.") +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# Lead effects: does one species' bad year predict another's next year,
# beyond that species' own previous year?
plot_cm_lags <- function(lags, k = 20) {
  d <- lags |>
    dplyr::filter(sure) |>
    dplyr::arrange(dplyr::desc(abs(r))) |>
    dplyr::slice_head(n = k) |>
    dplyr::mutate(pair = paste(leader, "then", follower), dir = ifelse(r > 0, "more damage next year", "less damage next year"))
  d$pair <- factor(d$pair, rev(d$pair))
  ggplot2::ggplot(d, ggplot2::aes(r, pair, colour = dir)) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.6) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = c(`more damage next year` = pal$div_high, `less damage next year` = pal$div_low),
                                 name = "A bad year for the first species means...") +
    ggplot2::labs(x = "Partial correlation (dot) with 95% range (line)", y = NULL,
                  title = "Does one insect's bad year come before another's?",
                  subtitle = "First species in year t vs second species in year t+1, after accounting for the second species' own year t",
                  caption = paste("Strongest links whose 95% range excludes zero; >= 5 regions. About 1 in 20 tested links",
                                  "would pass by chance, so treat single links as hypotheses.")) +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# ---- Two-part model figures ----------------------------------------------------

status_cols <- c(`observed` = "#184f95", `filled: works` = "#008300", `filled: weak` = "#eda100",
                 `filled: untested` = "#9c9b97", `zero: host absent` = "#8a5a44", `not fillable` = "#e6e5e1")

plot_hurdle_compare <- function(summary) {
  d <- summary |>
    dplyr::transmute(model,
      `Real zeros called 'probably no damage'` = zeros_called_zero,
      `Block totals within 2x of the truth` = share_totals_within_2x) |>
    tidyr::pivot_longer(-model, names_to = "metric") |>
    dplyr::mutate(model = factor(model, rev(summary$model)))
  ggplot2::ggplot(d, ggplot2::aes(value, model, fill = model)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(100 * value), "%")), hjust = -0.2, size = 3.2, colour = pal$ink) +
    ggplot2::facet_wrap(~ metric, ncol = 1) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(100 * x, "%"), limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_fill_manual(values = c(`One-part factor model` = "#9c9b97", `Two-part factor model` = pal$guild[["defoliator"]],
                                          `Two-part model` = pal$guild[["defoliator"]], `Two-part model + host trees` = "#008300"), guide = "none") +
    ggplot2::labs(x = NULL, y = NULL, title = "Gap-filling models compared",
                  subtitle = "Hidden country x insect blocks, predicted and compared with the real values",
                  caption = "Block total = predicted vs real total damage of one hidden country x insect x year.") +
    theme_eu(10) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# Harmonised dataset at a glance: country x insect, coloured by the dominant
# status of its region-year cells
plot_harmonised_status <- function(filled, sp_order) {
  d <- filled |>
    dplyr::mutate(status = dplyr::case_when(
      value_type %in% c("observed damage", "observed zero") ~ "observed",
      value_type == "filled" ~ paste0("filled: ", confidence),
      value_type == "zero: host tree absent" ~ "zero: host absent",
      TRUE ~ "not fillable")) |>
    dplyr::count(country, species, status) |>
    dplyr::group_by(country, species) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(species = factor(species, sp_order),
                  status = factor(status, names(status_cols)),
                  country = factor(country, rev(sort(unique(country)))))
  ggplot2::ggplot(d, ggplot2::aes(species, country, fill = status)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = status_cols, name = NULL, drop = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = "Harmonised dataset: what is observed and what is filled",
                  subtitle = "Each tile shows the most common status of that country x insect's region-years",
                  caption = paste("Filled confidence from the hidden-block test: works = beats the baseline and separates damage from no damage;",
                                  "\nweak = does not; untested = insect recorded in one country only. Host absent = host tree not in the region's forest inventory.")) +
    theme_eu(8) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
                   legend.position = "top", legend.justification = "left")
}

# How much of each insect's Europe-wide total would come from filled values
plot_totals_composition <- function(totals, k = 20) {
  d <- totals |>
    dplyr::group_by(species, unit) |>
    dplyr::summarise(observed = sum(observed), `filled: works` = sum(filled_works),
                     `filled: weak` = sum(filled_weak), `filled: untested` = sum(filled_untested), .groups = "drop") |>
    dplyr::slice_max(observed, n = k) |>
    dplyr::mutate(lab = paste0(species, "  (observed ", formatC(observed / 1e6, format = "f", digits = 1), " M ", unit, ")"))
  ord <- d |> dplyr::arrange(observed) |> dplyr::pull(lab)
  d <- d |>
    tidyr::pivot_longer(c(observed, `filled: works`, `filled: weak`, `filled: untested`), names_to = "source") |>
    dplyr::group_by(lab) |> dplyr::mutate(share = value / sum(value)) |> dplyr::ungroup() |>
    dplyr::mutate(lab = factor(lab, ord), source = factor(source, rev(c("observed", "filled: works", "filled: weak", "filled: untested"))))
  ggplot2::ggplot(d, ggplot2::aes(share, lab, fill = source)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0.5, colour = pal$surface, linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = status_cols[c("observed", "filled: works", "filled: weak", "filled: untested")], name = NULL,
                               guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(100 * x, "%"), expand = c(0, 0)) +
    ggplot2::labs(x = "Share of Europe-wide total 2000-2022", y = NULL,
                  title = "How much of each insect's Europe-wide total would be filled in?",
                  subtitle = "20 insects with the largest observed damage. Large orange/grey shares = totals not yet trustworthy.",
                  caption = "Cells where the host tree is absent are set to zero. Larch and minor broadleaf hosts cannot be checked (not in the host table).") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", legend.justification = "left")
}

# ---- Agent-group harmonisation -------------------------------------------------

group_cols <- c(`observed` = "#184f95", `back-cast` = "#5b8fd6", `filled: works` = "#008300",
                `filled: weak` = "#eda100", `filled: untested` = "#9c9b97",
                `zero: host absent` = "#8a5a44", `zero: never reported` = "#c9c3b4", `not fillable` = "#e6e5e1")

plot_group_status <- function(gh) {
  d <- gh |>
    dplyr::mutate(status = dplyr::case_when(
      value_type %in% c("observed damage", "observed zero") ~ "observed",
      grepl("^back-cast", value_type) ~ "back-cast",
      grepl("^filled", value_type) ~ paste0("filled: ", confidence),
      value_type == "zero: host tree absent" ~ "zero: host absent",
      grepl("^zero: never reported", value_type) ~ "zero: never reported",
      TRUE ~ "not fillable")) |>
    dplyr::count(country, group, status) |>
    dplyr::group_by(country, group) |> dplyr::slice_max(n, n = 1, with_ties = FALSE) |> dplyr::ungroup() |>
    dplyr::mutate(status = factor(status, names(group_cols)), country = factor(country, rev(sort(unique(country)))))
  ggplot2::ggplot(d, ggplot2::aes(group, country, fill = status)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = group_cols, name = NULL, drop = TRUE) +
    ggplot2::labs(x = NULL, y = NULL, title = "Harmonised by agent group: observed, back-cast or zero",
                  subtitle = "Each tile = most common status of that country x group's region-years, 2000-2022",
                  caption = "Back-cast = years before a country's series starts. Never reported = the country never reports this group; assumed no damage (0).") +
    theme_eu(9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "top", legend.justification = "left")
}

plot_group_totals <- function(gt) {
  d <- gt |>
    dplyr::group_by(group, unit) |>
    dplyr::summarise(Observed = sum(observed), `Back-cast early years` = sum(back_cast),
                     `Model guess if never-reported meant 'unknown'` = sum(model_guess_never_reported), .groups = "drop") |>
    dplyr::mutate(lab = paste0(group, " (", unit, ")")) |>
    tidyr::pivot_longer(c(Observed, `Back-cast early years`, `Model guess if never-reported meant 'unknown'`), names_to = "source") |>
    dplyr::mutate(source = factor(source, rev(c("Observed", "Back-cast early years", "Model guess if never-reported meant 'unknown'"))))
  ord <- d |> dplyr::filter(source == "Observed") |> dplyr::arrange(value) |> dplyr::pull(lab)
  ggplot2::ggplot(d, ggplot2::aes(value / 1e6, factor(lab, ord), fill = source)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~ unit, scales = "free", ncol = 1) +
    ggplot2::scale_fill_manual(values = c(Observed = "#184f95", `Back-cast early years` = "#5b8fd6",
                                          `Model guess if never-reported meant 'unknown'` = "#eda100"),
                               name = NULL, guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::labs(x = "Million (m3 or ha), 2000-2022", y = NULL, title = "Europe-wide damage by agent group",
                  subtitle = "Harmonised total = observed + back-cast (never-reported = 0). Orange = sensitivity: what the model would add instead.",
                  caption = "Grouping provisional (guild x host tree). Defoliator areas may overlap between insects.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", legend.justification = "left",
                                 strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# ---- gllvm (latent factor model, agent groups) --------------------------------

plot_gllvm_cor <- function(Rp, Rt) {
  tidy <- function(R, lab) {
    if (is.null(R)) return(NULL)
    tibble::as_tibble(R, rownames = "a") |> tidyr::pivot_longer(-a, names_to = "b", values_to = "r") |>
      dplyr::mutate(model = lab)
  }
  d <- dplyr::bind_rows(tidy(Rp, "Presence: damage in the same places and years"),
                        tidy(Rt, "Timing: bad years at the same time"))
  ord <- sort(unique(d$a))
  d <- d |> dplyr::mutate(a = factor(a, ord), b = factor(b, rev(ord)),
                          lab = dplyr::if_else(a == b, "", formatC(r, format = "f", digits = 1)))
  ggplot2::ggplot(d, ggplot2::aes(a, b, fill = r)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = lab), size = 2.6, colour = pal$ink) +
    ggplot2::facet_wrap(~ model) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", limits = c(-1, 1), name = "correlation") +
    ggplot2::labs(x = NULL, y = NULL, title = "Which insect groups go together? (gllvm latent factor model)",
                  subtitle = "Correlation left after removing shared region and Europe-wide year effects",
                  caption = "Red = go up together, blue = one up when the other is down. Recorded data only; groups reported by >= 3 countries.") +
    theme_eu(9) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1),
                                 strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

plot_gllvm_years <- function(ys, ld) {
  top <- ld |> dplyr::group_by(factor) |>
    dplyr::summarise(lab = paste0("+ ", paste(group[order(-loading)][1:2], collapse = ", "),
                                  "\n- ", paste(group[order(loading)][1:2], collapse = ", ")), .groups = "drop")
  d <- ys |> dplyr::left_join(top, by = "factor") |> dplyr::mutate(facet = paste0(factor, "\n", lab))
  ggplot2::ggplot(d, ggplot2::aes(year, score)) +
    ggplot2::geom_hline(yintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_line(colour = "#184f95", linewidth = 0.8) + ggplot2::geom_point(colour = "#184f95", size = 1.4) +
    ggplot2::facet_wrap(~ facet, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "mean factor score", title = "Timing factors over time",
                  subtitle = "High = years when the '+' groups had worse damage than usual relative to the '-' groups",
                  caption = "Average over regions of each region-year's factor score (timing model).") +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = 8))
}

plot_gllvm_vp <- function(vp) {
  ggplot2::ggplot(vp, ggplot2::aes(share, group, fill = component)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~ model) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(100 * x, "%"), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = c(`region (shared by all groups)` = "#9c9b97",
                                          `year (Europe-wide, shared by all groups)` = "#eda100",
                                          `latent factor 1` = "#184f95", `latent factor 2` = "#5b8fd6",
                                          `latent factor 3` = "#a9c6ec"), name = NULL) +
    ggplot2::labs(x = "share of explained variation", y = NULL, title = "What drives each group's variation?",
                  caption = "Timing model has no region effect: each series is measured around its own average.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", legend.justification = "left",
                                 strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# Species-NUTS occurrence ranking (version 2): one dot per code = number of NUTS
# units with damage; colour = guild, shape = code type.
plot_occurrence_ranking_v2 <- function(occ2) {
  n_units <- 77
  d <- occ2 |>
    dplyr::arrange(n_units_with_damage, dplyr::desc(species)) |>
    dplyr::mutate(species = factor(species, species),
                  code_type = factor(code_type, c("single insect", "aggregate group")))
  ggplot2::ggplot(d, ggplot2::aes(n_units_with_damage, species)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = n_units_with_damage, yend = species, colour = guild),
                          linewidth = 0.5, alpha = 0.35) +
    ggplot2::geom_point(ggplot2::aes(colour = guild, shape = code_type), size = 2.6, stroke = 1) +
    ggplot2::geom_text(ggplot2::aes(label = n_units_with_damage), hjust = 0, nudge_x = 1.2, size = 2.5, colour = pal$ink2) +
    ggplot2::scale_colour_manual(values = pal$guild, name = "Guild") +
    ggplot2::scale_shape_manual(values = c(`single insect` = 16, `aggregate group` = 15),
                                labels = c(`single insect` = "single insect (species or genus)",
                                           `aggregate group` = "aggregate group (several insects lumped together)"),
                                name = "Code type", drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, n_units), breaks = seq(0, 70, 10), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = "Number of NUTS units with damage (at least one year with damage > 0, 2000-2022)", y = NULL,
                  title = "Species-NUTS occurrence ranking",
                  subtitle = "EU-ForDAM insect codes as reported, ranked by the number of NUTS units where they caused damage",
                  caption = paste("77 NUTS units in total; 11 countries report one national unit; PL district codes are NUTS2.",
                                  "\nAggregate groups combine several insects, and their content can differ between countries.",
                                  "\nAs in the 2025 EU-ForDAM paper, the CZ spruce bark-borer group (CambXyl_con&BB_con&SM) is counted as ips_typ.")) +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical", legend.box.just = "left",
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# ---- Correlations along the hierarchy ------------------------------------------

# Heatmap of same-year r between groups at one level (lower triangle); the
# number is r, bold when its 95% range excludes 0.
plot_hier_heatmap <- function(hier_cm, hier, level, title) {
  d <- dplyr::filter(hier_cm, level == !!level)
  ord <- hier |> dplyr::distinct(level1, g = .data[[level]]) |>
    dplyr::arrange(factor(level1, c("borers on conifers", "borers on broadleaves",
                                    "defoliators on conifers", "defoliators on broadleaves")), g) |>
    dplyr::pull(g) |> unique()
  both <- dplyr::bind_rows(d, dplyr::rename(d, species_a = species_b, species_b = species_a)) |>
    dplyr::mutate(ia = match(species_a, ord), ib = match(species_b, ord)) |>
    dplyr::filter(ia > ib) |>
    dplyr::mutate(a = factor(species_a, rev(ord)), b = factor(species_b, ord),
                  lab = formatC(same_year, format = "f", digits = 2),
                  face = dplyr::if_else(same_year_sure, "bold", "plain"))
  ggplot2::ggplot(both, ggplot2::aes(b, a, fill = same_year)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = lab, fontface = face), size = 3, colour = pal$ink) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", limits = c(-0.6, 0.6),
                                  oob = scales::squish, name = "r (same year)") +
    ggplot2::labs(x = NULL, y = NULL, title = title,
                  subtitle = "Do the groups have bad years together? Pooled over all regions where both are recorded",
                  caption = "Bold = 95% range (whole regions resampled 999 times) excludes zero.\nGroup value = sum of its recorded insect codes per region-year.") +
    theme_eu(9) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
                                 panel.grid = ggplot2::element_blank())
}

# Mean correlation of insect pairs by how close they are in the hierarchy,
# against random group labels
plot_hier_test <- function(hier_test) {
  d <- hier_test |>
    dplyr::mutate(metric = dplyr::recode(metric, same_year = "Bad years together (same year)",
                                         beyond_year = "Beyond Europe-wide bad years"),
                  pair_class = factor(pair_class, c("same host-tree group (level 2)",
                                                    "same broad group only (level 1)", "different broad groups")),
                  lab = paste0("n = ", n_pairs, ", p = ", formatC(p_value, format = "f", digits = 3)))
  ggplot2::ggplot(d, ggplot2::aes(mean_r, pair_class)) +
    ggplot2::geom_col(fill = "#184f95", width = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = mean_r_random), shape = 124, size = 7, colour = pal$guild[["defoliator"]]) +
    ggplot2::geom_text(ggplot2::aes(label = lab), x = 0.005, hjust = 0, colour = "white", size = 2.8) +
    ggplot2::facet_wrap(~ metric, ncol = 1) +
    ggplot2::labs(x = "Mean correlation of insect pairs", y = NULL,
                  title = "Are insects in the same group more correlated?",
                  subtitle = "Insect pairs grouped by the closest level they share. Orange mark = average with random group labels",
                  caption = "p = share of 999 random relabellings with a mean at least as high as observed.") +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# ---- Option 3 (guild hint) vs plain two-part model ------------------------------

plot_hint_strength <- function(overall, chosen = 50) {
  d <- overall |>
    dplyr::transmute(tau, `Presence: AUC (0.5 = guessing, 1 = perfect)` = auc_any,
                     `Real zeros called 'no damage'` = zeros_called_zero,
                     `Hidden totals within 2x` = totals_within_2x,
                     `Share of tested species passing the reliability rule` = species_works / species_tested) |>
    tidyr::pivot_longer(-tau) |>
    dplyr::mutate(x = factor(tau, sort(unique(tau))))
  ggplot2::ggplot(d, ggplot2::aes(x, value, group = name)) +
    ggplot2::geom_line(colour = "#184f95", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(colour = tau == chosen | tau == 0), size = 2.6) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = pal$guild[["defoliator"]], `FALSE` = "#184f95"), guide = "none") +
    ggplot2::facet_wrap(~ name, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = "Strength of the guild hint (0 = plain model we had before)", y = NULL,
                  title = "Does the guild hint improve gap-filling?",
                  subtitle = paste0("Same hidden country x insect blocks for every version. Orange = plain model (0) and chosen strength (", chosen, ")"),
                  caption = "Hint = each insect's level and links pulled towards the average of its broad group (borers/defoliators on conifers/broadleaves).") +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

plot_hint_species <- function(by_species, hier, chosen = 50) {
  d <- by_species |> dplyr::filter(tau %in% c(0, chosen)) |>
    dplyr::select(tau, species, auc_any) |>
    tidyr::pivot_wider(names_from = tau, values_from = auc_any, names_prefix = "t") |>
    dplyr::rename(plain = t0, hint = !!paste0("t", chosen)) |>
    dplyr::filter(!is.na(plain), !is.na(hint)) |>
    dplyr::left_join(dplyr::select(hier, species, level1), by = "species") |>
    dplyr::arrange(hint) |> dplyr::mutate(species = factor(species, species))
  ggplot2::ggplot(d) +
    ggplot2::geom_vline(xintercept = c(0.5, 0.7), colour = pal$ink2, linewidth = 0.3, linetype = c("solid", "dashed")) +
    ggplot2::geom_segment(ggplot2::aes(x = plain, xend = hint, y = species, yend = species), colour = "#c9c3b4", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(plain, species), shape = 1, size = 2.4, colour = pal$ink2) +
    ggplot2::geom_point(ggplot2::aes(hint, species, colour = level1), size = 2.6) +
    ggplot2::scale_colour_manual(values = c(`borers on conifers` = "#184f95", `borers on broadleaves` = "#5b8fd6",
                                            `defoliators on conifers` = "#eb6834", `defoliators on broadleaves` = "#f2a33a"),
                                 name = "Broad group") +
    ggplot2::labs(x = "Presence prediction for hidden blocks (AUC; 0.5 = guessing, dashed = 0.7)", y = NULL,
                  title = "Per insect: plain model (hollow) vs guild hint (filled)",
                  subtitle = paste0("Insects recorded in at least 2 countries, so they can be tested. Hint strength ", chosen),
                  caption = "AUC = how well the model ranks hidden region-years with damage above those without.") +
    theme_eu(8.5) + ggplot2::theme(legend.position = "bottom")
}

# ---- Correlation metrics, back to basics ---------------------------------------

# All codes vs all codes: same-year r (Q1). Coloured only where the 95% range
# excludes zero; grey dot = tested, no clear link; blank = never recorded together.
plot_cm_basic_matrix <- function(cm, hier, ord) {
  both <- dplyr::bind_rows(cm, dplyr::rename(cm, species_a = species_b, species_b = species_a)) |>
    dplyr::mutate(a = factor(species_a, rev(ord)), b = factor(species_b, ord),
                  show = dplyr::if_else(same_year_sure, same_year, NA_real_))
  cuts <- hier |> dplyr::mutate(i = match(species, ord)) |> dplyr::group_by(level1) |>
    dplyr::summarise(last = max(i)) |> dplyr::filter(last < length(ord)) |> dplyr::pull(last)
  n <- length(ord)
  ggplot2::ggplot(both, ggplot2::aes(b, a)) +
    ggplot2::geom_tile(ggplot2::aes(fill = show), colour = "white", linewidth = 0.2) +
    ggplot2::geom_point(data = dplyr::filter(both, !is.na(same_year), !same_year_sure), size = 0.4, colour = "#9c9b97") +
    ggplot2::geom_vline(xintercept = cuts + 0.5, colour = pal$ink, linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = n - cuts + 0.5, colour = pal$ink, linewidth = 0.4) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", limits = c(-0.8, 0.8),
                                  oob = scales::squish, na.value = "transparent", name = "r (same year)") +
    ggplot2::labs(x = NULL, y = NULL, title = "Which insects have bad years together? (all codes vs all codes)",
                  subtitle = "Coloured = clear link (95% range excludes 0). Grey dot = tested, no clear link. Blank = never recorded together",
                  caption = paste("Lines separate the 4 broad groups: borers on conifers | borers on broadleaves | defoliators on conifers | defoliators on broadleaves.",
                                  "\nCZ spruce bark-borer group counted as ips_typ (as in the paper); region series standardised against their own mean; ranges from resampling whole regions 999 times.")) +
    theme_eu(7.5) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
                   panel.grid = ggplot2::element_blank(), legend.position = "right")
}

# ---- Gap-filling: three methods head to head ----------------------------------

gf_cols <- c(`simple average` = "#9c9b97", `partner method` = "#5b8fd6", `factor model` = "#184f95",
             `factor model + forest area` = "#eda100", `factor model + forest area (amount only)` = "#008300")

plot_gapfill_overall <- function(overall) {
  d <- overall |>
    dplyr::transmute(method,
      `Error per cell (log scale, lower = better)` = error_log,
      `Telling damage from no damage (AUC, 0.5 = guessing)` = damage_vs_none_auc,
      `Hidden totals within 2x of the truth` = totals_within_2x,
      `Typical error of a hidden total (x off, lower = better)` = typical_total_error_x) |>
    tidyr::pivot_longer(-method) |>
    dplyr::mutate(lab = dplyr::if_else(grepl("within", name), paste0(round(100 * value), "%"),
                                       formatC(value, format = "f", digits = 2)))
  ggplot2::ggplot(d, ggplot2::aes(value, method, fill = method)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = lab), hjust = -0.15, size = 3.2, colour = pal$ink) +
    ggplot2::facet_wrap(~ name, scales = "free_x", ncol = 2) +
    ggplot2::scale_fill_manual(values = gf_cols, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::labs(x = NULL, y = NULL, title = "Gap-filling: which method fills hidden values best?",
                  subtitle = "Real reported values hidden (whole country x insect blocks) and predicted from the rest",
                  caption = "Simple average = insect's yearly mean. Partner method = linked insects in the same region-year.\nFactor model = two-part latent factor model with group level hint. Forest area = NUTS host-forest table (ha).") +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

plot_gapfill_species <- function(by_species) {
  ord <- by_species |> dplyr::filter(method == "factor model") |>
    dplyr::arrange(dplyr::desc(gain_vs_simple)) |> dplyr::pull(species)
  d <- by_species |> dplyr::mutate(species = factor(species, rev(ord)))
  ggplot2::ggplot(d, ggplot2::aes(error_log, species)) +
    ggplot2::geom_line(ggplot2::aes(group = species), colour = "#e6e5e1", linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(colour = method), size = 2.5) +
    ggplot2::geom_point(data = dplyr::filter(d, method == best), shape = 1, size = 4.5, colour = pal$ink) +
    ggplot2::scale_colour_manual(values = gf_cols, name = NULL) +
    ggplot2::labs(x = "Error per hidden cell (log scale, lower = better)", y = NULL,
                  title = "Best gap-filling method per insect", subtitle = "Ring = best method for that insect",
                  caption = "Insects recorded in at least 2 countries (others cannot be tested).") +
    theme_eu(8.5) + ggplot2::theme(legend.position = "top", legend.justification = "left")
}

# ---- Gap-filled dataset v1 --------------------------------------------------------

gf_status_cols <- c(reported = "#184f95", `filled: reliable` = "#008300", `filled: weak` = "#eda100",
                    `filled: untested` = "#9c9b97", `zero: host absent` = "#8a5a44", `not fillable` = "#e6e5e1")

plot_gapfill_status <- function(gd, sp_order) {
  d <- gd |>
    dplyr::mutate(status = dplyr::case_when(
      value_type %in% c("reported damage", "reported zero") ~ "reported",
      value_type == "filled" ~ paste0("filled: ", confidence),
      value_type == "zero: host tree absent" ~ "zero: host absent",
      TRUE ~ "not fillable")) |>
    dplyr::count(country, species, status) |>
    dplyr::group_by(country, species) |> dplyr::slice_max(n, n = 1, with_ties = FALSE) |> dplyr::ungroup() |>
    dplyr::mutate(species = factor(species, sp_order), status = factor(status, names(gf_status_cols)),
                  country = factor(country, rev(sort(unique(country)))))
  ggplot2::ggplot(d, ggplot2::aes(species, country, fill = status)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = gf_status_cols, name = NULL, drop = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = "Gap-filled dataset: reported vs filled, country x insect",
                  subtitle = "Each tile = most common status of that country x insect's region-years, 2000-2022",
                  caption = paste("Reliable = beats the simple average and separates damage from no damage in the hide-and-predict test;",
                                  "\nweak = does not; untested = insect reported by one country only. Not reported = unknown, so it is filled, never set to 0.")) +
    theme_eu(8) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
                   legend.position = "top", legend.justification = "left")
}

plot_gapfill_totals <- function(tot, k = 20) {
  d <- tot |>
    dplyr::group_by(species, unit) |>
    dplyr::summarise(reported = sum(reported), `filled: reliable` = sum(filled_reliable),
                     `filled: weak` = sum(filled_weak), `filled: untested` = sum(filled_untested), .groups = "drop") |>
    dplyr::slice_max(reported, n = k) |>
    dplyr::mutate(lab = paste0(species, "  (reported ", formatC(reported / 1e6, format = "f", digits = 1), " M ", unit, ")"))
  ord <- d |> dplyr::arrange(reported) |> dplyr::pull(lab)
  d <- d |> tidyr::pivot_longer(c(reported, `filled: reliable`, `filled: weak`, `filled: untested`), names_to = "source") |>
    dplyr::group_by(lab) |> dplyr::mutate(share = value / sum(value)) |> dplyr::ungroup() |>
    dplyr::mutate(lab = factor(lab, ord),
                  source = factor(source, rev(c("reported", "filled: reliable", "filled: weak", "filled: untested"))))
  ggplot2::ggplot(d, ggplot2::aes(share, lab, fill = source)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = gf_status_cols[c("reported", "filled: reliable", "filled: weak", "filled: untested")],
                               name = NULL, guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(100 * x, "%"), expand = c(0, 0)) +
    ggplot2::labs(x = "Share of Europe-wide total 2000-2022", y = NULL,
                  title = "How much of each insect's Europe-wide total is reported vs filled?",
                  subtitle = "20 insects with the largest reported damage",
                  caption = "Filled amounts are typically several times off for a single region (hide-and-predict test); use with the confidence labels.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", legend.justification = "left")
}

plot_gapfill_v2 <- function(sc) {
  d <- sc |>
    dplyr::transmute(method,
      `Error per cell (log scale, lower = better)` = error_log,
      `Telling damage from no damage (AUC)` = damage_vs_none_auc,
      `Hidden totals within 2x of the truth` = totals_within_2x,
      `Typical error of a hidden total (x off, lower = better)` = typical_total_error_x) |>
    tidyr::pivot_longer(-method) |>
    dplyr::mutate(lab = dplyr::if_else(grepl("within", name), paste0(round(100 * value), "%"), formatC(value, format = "f", digits = 2)),
                  base = method == "factor model (v1)", method = factor(method, rev(levels(sc$method))))
  ggplot2::ggplot(d, ggplot2::aes(value, method, fill = base)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = lab), hjust = -0.15, size = 3, colour = pal$ink) +
    ggplot2::facet_wrap(~ name, scales = "free_x", ncol = 2) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#184f95", `FALSE` = "#9c9b97"), guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.3))) +
    ggplot2::labs(x = NULL, y = NULL, title = "Does memory, neighbours or a country reporting effect improve gap-filling?",
                  subtitle = "Second stage on top of the factor model (blue); same hidden blocks; second stage fitted on the other folds",
                  caption = "Memory = region's other insects last year. Neighbours = same insect in nearby regions of other countries.\nReporting effect = country's level for the insect group. Tweedie = distribution for zero-heavy data.") +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# ---- Correlation metrics: one verdict per pair --------------------------------

verdict_cols <- c(usable = "#008300", promising = "#86b6ef", unreliable = "#eda100",
                  weak = "#c9c8c3", `no link` = "#e6e5e1", `not enough data` = "#f5f4f1")

plot_cm_verdict <- function(v) {
  d <- dplyr::count(v, verdict, .drop = FALSE)
  ggplot2::ggplot(d, ggplot2::aes(n, forcats::fct_rev(verdict), fill = verdict)) +
    ggplot2::geom_col(width = 0.65, colour = "#9c9b97", linewidth = 0.2) +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.2, size = 3.4) +
    ggplot2::scale_fill_manual(values = verdict_cols, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = "insect pairs", y = NULL, title = "Can one insect's records tell us about another's?",
                  subtitle = paste0("All ", nrow(v), " pairs of insect codes in EU-ForDAM, one verdict each"),
                  caption = paste("usable = strong link (r >= 0.3, sure) that holds in every country checked | promising = strong, seen in one country only",
                                  "\nunreliable = strong but differs between countries | weak = sure but r < 0.3 | not enough data = rarely recorded together")) +
    theme_eu(9)
}

# The pairs we can lean on: same-year r with 95% range, and r after removing
# Europe-wide bad years (open circle)
plot_cm_usable <- function(v) {
  d <- dplyr::filter(v, verdict == "usable") |>
    dplyr::mutate(pair = paste(species_a, "-", species_b), pair = forcats::fct_reorder(pair, r))
  ggplot2::ggplot(d, ggplot2::aes(y = pair)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#9c9b97") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = r_lo, xmax = r_hi), height = 0, colour = "#86b6ef") +
    ggplot2::geom_point(ggplot2::aes(x = r), colour = "#008300", size = 2.2) +
    ggplot2::geom_point(ggplot2::aes(x = r_without_europe_wide_years), shape = 21, colour = pal$ink, size = 2) +
    ggplot2::geom_text(ggplot2::aes(x = 0.95, label = countries), hjust = 1, size = 2.6, colour = pal$ink2) +
    ggplot2::scale_x_continuous(limits = c(min(0, d$r_lo) - 0.05, 0.95)) +
    ggplot2::labs(x = "r (same year, within region)", y = NULL,
                  title = "The usable links: insects that move together in every country checked",
                  subtitle = "Green dot = r, bar = 95% range; open circle = r after removing years bad all over Europe; right = countries",
                  caption = "Open circle close to the green dot = the link is regional, not just shared European bad years.") +
    theme_eu(8.5)
}

# Heatmap of all pairs, same verdicts as the bar chart: every measurable pair
# coloured by r; faded = not sure (95% range includes 0); black outline =
# usable (strong + holds in every country checked); blank = not enough data
plot_cm_heatmap <- function(v, hier, ord) {
  both <- dplyr::bind_rows(v, dplyr::rename(v, species_a = species_b, species_b = species_a)) |>
    dplyr::mutate(a = factor(species_a, rev(ord)), b = factor(species_b, ord),
                  sure = factor(dplyr::if_else(verdict == "no link", "not sure (faded)", "sure link"),
                                c("sure link", "not sure (faded)")),
                  show = dplyr::if_else(verdict == "not enough data", NA_real_, r))
  cuts <- hier |> dplyr::mutate(i = match(species, ord)) |> dplyr::group_by(level1) |>
    dplyr::summarise(last = max(i)) |> dplyr::filter(last < length(ord)) |> dplyr::pull(last) |> sort()
  n <- length(ord)
  ggplot2::ggplot(dplyr::filter(both, !is.na(show)), ggplot2::aes(b, a)) +
    ggplot2::geom_tile(ggplot2::aes(fill = show, alpha = sure), colour = "white", linewidth = 0.2) +
    ggplot2::geom_tile(data = dplyr::filter(both, verdict == "usable"), fill = NA, colour = pal$ink, linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = cuts + 0.5, colour = pal$ink, linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = n - cuts + 0.5, colour = pal$ink, linewidth = 0.4) +
    ggplot2::annotate("text", x = c(0, cuts) + diff(c(0, cuts, n)) / 2 + 0.5, y = n + 1.6, size = 2.8, fontface = "bold",
                      label = c("borers on\nconifers", "borers on\nbroadleaves", "defoliators\non conifers", "defoliators on\nbroadleaves"),
                      lineheight = 0.9) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", limits = c(-0.8, 0.8),
                                  oob = scales::squish, name = "r (same year,\nwithin region)") +
    ggplot2::scale_alpha_manual(values = c(`sure link` = 1, `not sure (faded)` = 0.3), name = NULL) +
    ggplot2::scale_x_discrete(drop = FALSE) + ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::coord_cartesian(ylim = c(1, n + 2), clip = "off") +
    ggplot2::labs(x = NULL, y = NULL, title = "Which insects have bad years together?",
                  subtitle = paste0("Red = bad years together, blue = opposite. Strong colour = sure link, faded = could be chance. ",
                                    "Black box = usable (strong and holds in every country checked, ", sum(v$verdict == "usable"), " pairs).",
                                    "\nBlank = recorded together too rarely to measure."),
                  caption = "CZ spruce bark-borer group counted as ips_typ (as in the 2025 EU-ForDAM paper); log(1 + damage), each region's series against its own mean; 95% ranges from resampling whole regions 999 times.") +
    theme_eu(7.5) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "right")
}

# Predictive test: same-year r (x) against out-of-country predictive skill (y),
# one dot per pair that can be tested in >= 2 countries, coloured by verdict
plot_cm_predictive <- function(ps) {
  d <- dplyr::filter(ps, predictive != "cannot test") |>
    dplyr::mutate(verdict = factor(as.character(verdict), verdict_levels),
                  skill = pmax(skill, -0.6))   # very poor pairs drawn at the bottom edge
  lab <- d |> dplyr::filter(predictive == "predicts well") |> dplyr::slice_max(skill, n = 6) |>
    dplyr::mutate(pair = paste(species_a, "-", species_b))
  cols <- c(verdict_cols[c("usable", "promising", "unreliable")], weak = "#9c9b97", `no link` = "#c9c8c3")
  ggplot2::ggplot(d, ggplot2::aes(r, skill)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.05, ymax = Inf, fill = "#e9f3ea") +
    ggplot2::geom_hline(yintercept = 0, colour = pal$ink2) +
    ggplot2::geom_point(data = dplyr::filter(d, !verdict %in% c("usable", "unreliable")), ggplot2::aes(colour = verdict), size = 1.6) +
    ggplot2::geom_point(data = dplyr::filter(d, verdict %in% c("usable", "unreliable")), ggplot2::aes(colour = verdict), size = 2.6) +
    ggrepel::geom_text_repel(data = lab, ggplot2::aes(label = pair), size = 2.6, colour = pal$ink,
                             nudge_x = -0.25, direction = "y", segment.colour = "#9c9b97", seed = 20261014) +
    ggplot2::scale_colour_manual(values = cols, name = "correlation verdict", drop = TRUE) +
    ggplot2::coord_cartesian(ylim = c(-0.6, 0.4), xlim = c(-0.5, 0.75)) +
    ggplot2::labs(x = "same-year r (correlation metrics)",
                  y = "predictive skill in a country left out\n(0 = no better than 'normal year')",
                  title = "Does a link learnt in some countries predict another country?",
                  subtitle = paste0("Each dot = one insect pair recorded together in at least 2 countries (", nrow(d),
                                    " pairs). Green band = predicts well (skill >= 0.05)."),
                  caption = "Skill = 1 - error using insect A / error guessing a normal year, for insect B in a country left out; both directions pooled. Below 0 = the link misleads.\nPairs below -0.6 are drawn at the bottom edge.") +
    theme_eu(9) + ggplot2::theme(legend.position = "right")
}

# ---- Table of the usable pairs as an image ---------------------------------------

# Readable names for the insect codes in the usable pairs (from the code
# abbreviations; aggregates named by the host tree given in the database)
insect_names <- c(
  `CambXyl_con&BB_con&BO` = "Conifer borer group (pine)", `CambXyl_con&MD` = "Conifer borer group (larch)",
  agr_big = "Agrilus biguttatus", bup_pin = "Bupalus piniaria", den_pin = "Dendrolimus pini",
  dip_sp = "Diprion spp.", ips_acu = "Ips acuminatus", ips_dup = "Ips duplicatus",
  ips_sex = "Ips sexdentatus", ips_typ = "Ips typographus", lym_mon = "Lymantria monacha",
  pan_fla = "Panolis flammea", pha_cya = "Phaenops cyanea", pis_pin = "Pissodes pini",
  pit_cha = "Pityogenes chalcographus", pit_spp = "Pityokteines spp.", pri_abi = "Pristiphora abietina", pol_pol = "Polygraphus poligraphus", tha_pit = "Thaumetopoea pinivora",
  sco_int = "Scolytus intricatus", sco_rat = "Scolytus ratzeburgi", tom_spp = "Tomicus spp.")

host_short <- c(`Pinus sylvestris` = "pine", `Picea abies` = "spruce", `Abies alba` = "fir",
                `Larix decidua` = "larch", `Quercus spp.` = "oak", `Betula spp.` = "birch")

usable_table <- function(v, species_meta) {
  h <- setNames(host_short[species_meta$host_tree], species_meta$species)
  nm <- function(x) dplyr::coalesce(unname(insect_names[x]), x)
  v |>
    dplyr::filter(verdict == "usable") |>
    dplyr::mutate(
      group = dplyr::case_when(
        r < 0 ~ "4. Opposite link",
        grepl("defoliators", group_a) & grepl("defoliators", group_b) ~ "3. Pine and spruce defoliators",
        group_a == "borers on conifers" & group_b == "borers on conifers" ~ "1. Conifer bark beetles and borers",
        TRUE ~ "2. Conifer borers with oak and birch borers"),
      Pair = paste(nm(species_a), "x", nm(species_b)),
      Hosts = paste(h[species_a], "x", h[species_b]),
      r_txt = formatC(r, format = "f", digits = 2),
      rw_txt = paste0(formatC(r_without_europe_wide_years, format = "f", digits = 2),
                      ifelse(mostly_europe_wide_years, " *", "")),
      Countries = gsub(",", ", ", countries)) |>
    dplyr::arrange(group, dplyr::desc(abs(r)))
}

plot_usable_table <- function(ut) {
  fills <- c(`1. Conifer bark beetles and borers` = "#e3eef9", `2. Conifer borers with oak and birch borers` = "#fdf0e3",
             `3. Pine and spruce defoliators` = "#e6f2e6", `4. Opposite link` = "#ececf6")
  rows <- list(); fill <- character(); bold <- logical(); i <- 0
  for (g in unique(ut$group)) {
    rows[[length(rows) + 1]] <- data.frame(`#` = "", Pair = g, Hosts = "", r = "", rw = "", Countries = "", check.names = FALSE)
    fill <- c(fill, "#d8d6cf"); bold <- c(bold, TRUE)
    d <- dplyr::filter(ut, group == g)
    rows[[length(rows) + 1]] <- data.frame(`#` = as.character(i + seq_len(nrow(d))), Pair = d$Pair, Hosts = d$Hosts,
                                           r = d$r_txt, rw = d$rw_txt, Countries = d$Countries, check.names = FALSE)
    fill <- c(fill, rep(fills[[g]], nrow(d))); bold <- c(bold, rep(FALSE, nrow(d))); i <- i + nrow(d)
  }
  tab <- do.call(rbind, rows)
  names(tab) <- c("#", "Pair", "Host trees", "r", "r without\nEurope-wide years", "Countries where it holds")
  th <- gridExtra::ttheme_minimal(
    base_size = 10,
    core = list(bg_params = list(fill = fill, col = "white"),
                fg_params = list(hjust = 0, x = 0.04, fontface = ifelse(bold, "bold", "plain"))),
    colhead = list(bg_params = list(fill = "#16302B"), fg_params = list(col = "white", fontface = "bold", hjust = 0, x = 0.04)))
  g <- gridExtra::tableGrob(tab, rows = NULL, theme = th)
  title <- grid::textGrob("The 30 usable insect pairs: strong (r >= 0.3, sure) and consistent in every country checked",
                          x = 0.02, hjust = 0, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  note <- grid::textGrob(paste("r = same-year correlation within regions (log damage, each region against its own mean). * = link mostly reflects years bad all over Europe",
                               "(r drops below 0.2 once they are removed).\nCZ spruce bark-borer group counted as I. typographus. Latin names from code abbreviations; group (pine)/(larch) = aggregate codes CambXyl_con&BB_con&BO / CambXyl_con&MD."),
                         x = 0.02, hjust = 0, gp = grid::gpar(fontsize = 8.5, col = "#52514e"))
  gridExtra::arrangeGrob(title, g, note, ncol = 1, heights = grid::unit(c(0.45, 1, 0.5), c("in", "null", "in")))
}

save_grob <- function(g, path, w, h) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(path, width = w, height = h, units = "in", res = 200, bg = pal$surface, type = "cairo")
  grid::grid.newpage(); grid::grid.draw(g); grDevices::dev.off()
  path
}

# ---- The notes' equation log(B+1) = a + b log(A+1) --------------------------------

plot_partner_eq_table <- function(pe) {
  nm <- function(x) dplyr::coalesce(unname(insect_names[x]), x)
  f2 <- function(x) formatC(x, format = "f", digits = 2)
  tab <- data.frame(
    `#` = seq_len(nrow(pe)),
    `A` = nm(pe$species_a), `B` = nm(pe$species_b),
    `a` = f2(pe$a_B_from_A), `b` = paste0(f2(pe$b_B_from_A), " ± ", f2(pe$b_se_B_from_A)),
    `a ` = f2(pe$a_A_from_B), `b ` = paste0(f2(pe$b_A_from_B), " ± ", f2(pe$b_se_A_from_B)),
    `R2` = f2(pe$r2), `n` = pe$n_region_years, check.names = FALSE)
  tab$b <- gsub("±", "+/-", tab$b); tab$`b ` <- gsub("±", "+/-", tab$`b `)
  names(tab) <- c("#", "Insect A", "Insect B", "a\n(B from A)", "b\n(B from A)", "a\n(A from B)", "b\n(A from B)",
                  "R²", "region-\nyears")
  names(tab)[8] <- "R-sq"
  fill <- rep(c("#f4f3ee", "#fcfcfb"), length.out = nrow(tab))
  th <- gridExtra::ttheme_minimal(
    base_size = 9.5,
    core = list(bg_params = list(fill = fill, col = "white"), fg_params = list(hjust = 0, x = 0.05)),
    colhead = list(bg_params = list(fill = "#16302B"), fg_params = list(col = "white", fontface = "bold", hjust = 0, x = 0.05)))
  g <- gridExtra::tableGrob(tab, rows = NULL, theme = th)
  title <- grid::textGrob("log(B + 1) = a + b x log(A + 1), fitted for the 30 usable pairs",
                          x = 0.02, hjust = 0, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  note <- grid::textGrob(paste(
    "Fitted by least squares on all region-years where both insects are recorded (value1; borers m3, defoliators ha; zeros kept). b +/- standard error.",
    "\nb = how strongly B follows A: b = 0.5 means B rises about 0.5% for each 1% rise in A (for large values). a = log(B + 1) expected when A = 0.",
    "\nR-sq is the same in both directions. Pooled over regions of very different size, so R-sq here is lower than the within-region correlation (r) suggests."),
    x = 0.02, hjust = 0, gp = grid::gpar(fontsize = 8.5, col = "#52514e"))
  gridExtra::arrangeGrob(title, g, note, ncol = 1, heights = grid::unit(c(0.45, 1, 0.7), c("in", "null", "in")))
}

# Scatter of one pair: left = the notes' equation on raw log values (all regions
# pooled), right = the same after comparing each region with its own mean (what
# the correlation metrics use). Points coloured by country.
plot_partner_eq_scatter <- function(tr, a, b) {
  nm <- function(x) dplyr::coalesce(unname(insect_names[x]), x)
  d <- tr |>
    dplyr::filter(species %in% c(a, b)) |>
    dplyr::select(country, nuts_id, year, species, log_damage) |>
    tidyr::pivot_wider(names_from = species, values_from = log_damage) |>
    dplyr::filter(!is.na(.data[[a]]), !is.na(.data[[b]])) |>
    dplyr::rename(x = !!a, y = !!b)
  f <- stats::lm(y ~ x, d)
  # right panel: z_within (each region series standardised; series with no
  # variation, e.g. all zeros, drop out), exactly as in the correlation metrics
  dw <- tr |>
    dplyr::filter(species %in% c(a, b)) |>
    dplyr::select(country, nuts_id, year, species, z_within) |>
    tidyr::pivot_wider(names_from = species, values_from = z_within) |>
    dplyr::filter(!is.na(.data[[a]]), !is.na(.data[[b]])) |>
    dplyr::rename(x = !!a, y = !!b)
  fw <- stats::lm(y ~ x, dw)
  both <- dplyr::bind_rows(
    dplyr::mutate(d, panel = "1. Equation from the notes: all regions pooled"),
    dplyr::mutate(dw, panel = "2. Each region against its own normal (correlation metrics)"))
  lab <- tibble::tibble(
    panel = unique(both$panel),
    txt = c(sprintf("log(B+1) = %.2f + %.2f log(A+1)\nR-sq = %.2f, n = %d", stats::coef(f)[1], stats::coef(f)[2], summary(f)$r.squared, nrow(d)),
            sprintf("r = %.2f, n = %d", stats::cor(dw$x, dw$y), nrow(dw))))
  lines <- dplyr::bind_rows(
    tibble::tibble(panel = lab$panel[1], x = range(d$x), y = stats::coef(f)[1] + stats::coef(f)[2] * range(d$x)),
    tibble::tibble(panel = lab$panel[2], x = range(dw$x), y = stats::coef(fw)[1] + stats::coef(fw)[2] * range(dw$x)))
  cc <- c(AT = "#2a78d6", CZ = "#eb6834", DE = "#1baf7a", HR = "#4a3aa7", SI = "#eda100",
          PL = "#e87ba4", LT = "#008300", SK = "#c23b3a", BG = "#52514e", IT = "#86b6ef", RO = "#9c9b97")
  ggplot2::ggplot(both, ggplot2::aes(x, y)) +
    ggplot2::geom_point(ggplot2::aes(colour = country), alpha = 0.55, size = 1.4) +
    ggplot2::geom_line(data = lines, linewidth = 1, colour = pal$ink) +
    ggplot2::geom_label(data = lab, ggplot2::aes(x = -Inf, y = Inf, label = txt), hjust = -0.05, vjust = 1.1,
                        size = 3, label.size = 0, fill = "#ffffffcc", inherit.aes = FALSE) +
    ggplot2::facet_wrap(~ panel, scales = "free") +
    ggplot2::scale_colour_manual(values = cc, name = "country") +
    ggplot2::labs(x = paste0("A: ", nm(a), "  (log(damage + 1))"), y = paste0("B: ", nm(b), "  (log(damage + 1))"),
                  title = paste0("Predicting ", nm(b), " from ", nm(a)),
                  subtitle = "Each dot = one region-year where both are recorded. Line = least-squares fit.",
                  caption = paste("Left: the equation as used by the partner method; the fit also has to bridge differences in region size and reporting level, so points spread more.",
                                  "\nRight: each region's series standardised against its own mean and spread (series with no variation drop out), as in the correlation metrics (axes in standard units): only shared ups and downs remain.")) +
    theme_eu(9) + ggplot2::theme(legend.position = "right", strip.text = ggplot2::element_text(face = "bold"))
}

# Gap-filled dataset v2: Europe-wide yearly totals, reported only vs reported +
# filled (tested insects), with the 80% range of the filled part
plot_gf2_europe <- function(eu, n = 8) {
  nm <- function(x) dplyr::coalesce(unname(insect_names[x]), x)
  top <- eu |> dplyr::group_by(species) |> dplyr::summarise(f = sum(total_mid - reported)) |>
    dplyr::slice_max(f, n = n) |> dplyr::pull(species)
  d <- dplyr::filter(eu, species %in% top) |>
    dplyr::mutate(lab = paste0(nm(species), " (", unit, ")"))
  ggplot2::ggplot(d, ggplot2::aes(year)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = total_lo80, ymax = total_hi80), fill = "#86b6ef", alpha = 0.45) +
    ggplot2::geom_line(ggplot2::aes(y = total_mid, colour = "reported + filled"), linewidth = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = reported, colour = "reported only"), linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = c(`reported only` = pal$ink, `reported + filled` = "#1c5cab"), name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    ggplot2::facet_wrap(~ lab, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = NULL, y = "Europe-wide damage per year",
                  title = "Europe-wide damage: reported vs reported + gap-filled",
                  subtitle = paste0("The ", n, " insects with the most filled damage. Band = 80% range of the filled part, from the hide-and-predict test"),
                  caption = "Filled = factor model; insects tested in >= 2 countries only (untested fills and aggregate reporting codes excluded).\nLine = middle, band = 80% range: each country's filled total x test errors resampled from hidden totals of that insect.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold"))
}

# ---- Correlation report tables as images ------------------------------------------

plot_simple_table <- function(df, title, note = NULL, base_size = 10.5) {
  fill <- rep(c("#f4f3ee", "#fcfcfb"), length.out = nrow(df))
  th <- gridExtra::ttheme_minimal(
    base_size = base_size,
    core = list(bg_params = list(fill = fill, col = "white"), fg_params = list(hjust = 0, x = 0.04)),
    colhead = list(bg_params = list(fill = "#16302B"), fg_params = list(col = "white", fontface = "bold", hjust = 0, x = 0.04)))
  g <- gridExtra::tableGrob(df, rows = NULL, theme = th)
  ttl <- grid::textGrob(title, x = 0.02, hjust = 0, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  parts <- list(ttl, g); h <- c(0.45, 1)
  if (!is.null(note)) {
    parts[[3]] <- grid::textGrob(note, x = 0.02, hjust = 0, gp = grid::gpar(fontsize = 8.5, col = "#52514e")); h <- c(h, 0.45)
  }
  gridExtra::arrangeGrob(grobs = parts, ncol = 1, heights = grid::unit(h, c("in", "null", "in")[seq_along(h)]))
}

cm_table_questions <- function() {
  data.frame(
    Question = c("1. Does insect A have bad years when insect B does,\n    in the same region?",
                 "2. Does that link hold in every country?",
                 "3. Is it regional, or just years bad all over Europe?\n    Does one come first?",
                 "4. Do linked insects share feeding type or host tree?"),
    Metric = c("same-year r, within region", "r per country",
               "r after removing Europe-wide years;\nA this year vs B next year",
               "share of strong links within vs\nbetween groups"),
    Objective = c("the premise", "1, gap-filling (links learnt in one\ncountry fill gaps in another)",
                  "3, drivers and anticipation", "2, damage across agents\nand host trees"),
    check.names = FALSE)
}

cm_table_verdicts <- function(v) {
  v |> dplyr::count(verdict, name = "pairs", .drop = FALSE) |>
    dplyr::mutate(share = paste0(round(100 * pairs / sum(pairs)), "%"),
                  meaning = c("strong, and holds in every country checked: lean on it",
                              "strong, but only seen in one country: cannot check yet",
                              "strong, but differs between countries: do not transfer",
                              "sure but small (r < 0.3)", "no detectable link",
                              "recorded together too rarely to measure"),
                  verdict = as.character(verdict)) |>
    as.data.frame()
}

cm_table_groups <- function(v) {
  strong <- c("usable", "promising", "unreliable")
  v |> dplyr::filter(verdict != "not enough data") |>
    dplyr::mutate(pair = dplyr::if_else(group_a == group_b, "same broad group", "different broad groups")) |>
    dplyr::group_by(pair) |>
    dplyr::summarise(`measurable pairs` = dplyr::n(), `strong links` = sum(verdict %in% strong),
                     `share strong` = paste0(round(100 * mean(verdict %in% strong)), "%"),
                     usable = sum(verdict == "usable"), .groups = "drop") |>
    as.data.frame()
}


# =============================================================================
#  RUN
# =============================================================================
results <- run_all()
