# targets pipeline: cross-species correlation analysis of EU-ForDAM insect damage.
# Run with targets::tar_make(); render the report with quarto render reports/report.qmd
# No gap-filling, imputation or forecasting happens in this pipeline.

library(targets)

tar_option_set(packages = c("dplyr", "tidyr", "readr", "readxl", "stringr", "tibble",
                            "purrr", "ggplot2", "writexl"))
tar_source("R")

tab <- function(x, name) write_csv_out(x, file.path("outputs/tables", name))

list(
  # ---- Step 1: inspection --------------------------------------------------
  tar_target(raw_file, "data/raw/EUForDam_REW3.xlsx", format = "file"),
  tar_target(raw, read_raw(raw_file)),
  tar_target(long, to_long(raw)),
  tar_target(counts, summarise_counts(long)),
  tar_target(coverage_raw, country_species_coverage(long)),
  tar_target(sp_lookup, species_lookup_raw(long)),
  tar_target(long_csv, write_csv_out(long, "data/processed/eufordam_long_raw.csv"), format = "file"),
  tar_target(counts_csv, tab(counts, "step1_counts.csv"), format = "file"),
  tar_target(coverage_raw_csv, tab(coverage_raw, "step1_country_species_coverage.csv"), format = "file"),
  tar_target(sp_lookup_csv, tab(sp_lookup, "step1_species_in_file.csv"), format = "file"),

  # ---- Prepare: CZ sub-rows summed, value1 only, CZ spruce group -> ips_typ ----
  tar_target(species_meta, species_meta_from_raw(raw)),
  tar_target(dmg, prepare_damage(long, species_meta)),
  tar_target(dmg_csv, write_csv_out(dmg, "data/processed/eufordam_damage_nuts.csv"), format = "file"),

  # ---- Step 2: missingness classes -----------------------------------------
  tar_target(cells, classify_cells(dmg)),
  # ---- Host trees (appendix of Hlasny et al. 2025): absent host = impossible --
  tar_target(host_file, "data/raw/Appendix_NUTS_Forests_12.xlsx", format = "file"),
  tar_target(host_appx, read_host_appendix(host_file)),
  tar_target(host_long, host_area_long(host_appx, dash_means_absent = TRUE)),
  tar_target(host_status, host_status_table(cells, species_meta, host_long)),
  tar_target(cells_host, apply_host_status(cells, host_status)),
  tar_target(host_status_csv, tab(host_status, "host_status_region_insect.csv"), format = "file"),
  tar_target(host_area_csv, tab(host_long, "host_area_by_region.csv"), format = "file"),
  tar_target(cells_csv, write_csv_out(cells, "data/processed/cells_classified.csv"), format = "file"),
  tar_target(cov_species, coverage_summary(cells_host, "species")),
  tar_target(cov_country, coverage_summary(cells_host, "country")),
  tar_target(cov_cs, coverage_country_species(cells_host)),
  tar_target(cov_species_csv, tab(cov_species, "coverage_by_species.csv"), format = "file"),
  tar_target(cov_country_csv, tab(cov_country, "coverage_by_country.csv"), format = "file"),
  tar_target(cov_cs_csv, tab(cov_cs, "coverage_country_species.csv"), format = "file"),
  tar_target(fig_coverage, save_fig(plot_coverage(cov_cs), "outputs/figures/coverage_country_species.png", 11, 5),
             format = "file"),

  # ---- Species - NUTS occurrence ranking -------------------------------------
  tar_target(rs_summary, region_species_summary(cells)),
  tar_target(sp_rank, species_occurrence_ranking(rs_summary, species_meta)),
  tar_target(reg_rank, region_species_ranking(rs_summary)),
  tar_target(occ_matrix, occurrence_matrix(rs_summary, sp_rank)),
  tar_target(sp_rank_csv, tab(sp_rank, "species_occurrence_ranking.csv"), format = "file"),
  tar_target(reg_rank_csv, tab(reg_rank, "region_species_ranking.csv"), format = "file"),
  tar_target(occ_matrix_csv, tab(occ_matrix, "occurrence_matrix_region_species.csv"), format = "file"),
  tar_target(fig_sp_rank, save_fig(plot_species_ranking(sp_rank),
                                    "outputs/figures/species_occurrence_ranking.png", 8, 9.5), format = "file"),
  tar_target(fig_occupancy, save_fig(plot_occupancy_curve(sp_rank),
                                     "outputs/figures/species_occupancy_curve.png", 7.5, 9.5), format = "file"),
  tar_target(fig_occupancy_both, save_fig(plot_occupancy_curves_monitored(sp_rank),
                                          "outputs/figures/species_occupancy_monitored_vs_occurring.png", 9, 10.5),
             format = "file"),
  tar_target(fig_occ_map, save_fig(plot_occurrence_map(rs_summary, sp_rank),
                                   "outputs/figures/occurrence_map_region_species.png", 11, 14), format = "file"),
  tar_target(occ_rank2, occurrence_ranking_v2(long, species_meta)),
  tar_target(occ_rank2_csv, tab(occ_rank2, "species_nuts_occurrence_ranking_v2.csv"), format = "file"),
  tar_target(ranking_xlsx, write_ranking_xlsx(sp_rank, reg_rank, occ_matrix,
                                              "outputs/tables/species_nuts_occurrence_ranking.xlsx"),
             format = "file"),

  # ---- Step 3: transformed versions ----------------------------------------
  # Main: all observed values (0 = monitored, no damage). Sensitivity: the
  # >= 6 non-zero series filter of Hlasny et al. (2025).
  tar_target(tr, make_transforms(cells, min_nonzero = 0)),
  tar_target(tr_paper6, make_transforms(cells, min_nonzero = 6)),
  tar_target(tr_csv, write_csv_out(tr, "data/processed/transformed_observed.csv"), format = "file"),
  tar_target(versions_csv, tab(transform_versions, "transform_versions.csv"), format = "file"),
  tar_target(all_species, sort(unique(cells$species))),

  # ---- Step 4: pairwise correlations ---------------------------------------
  tar_target(pw, pairwise_correlations(tr, all_species)),
  tar_target(pw_paper6, pairwise_correlations(tr_paper6, all_species, versions = c("z_within", "anomaly"))),
  tar_target(pw_csv, tab(pw, "pairwise_correlations.csv"), format = "file"),
  tar_target(pw_paper6_csv, tab(pw_paper6, "pairwise_correlations_sensitivity_paper_filter6.csv"), format = "file"),

  # ---- Comparison with Hlasny et al. (2025) Table 6 -------------------------
  tar_target(paper_t6, europe_total_correlations(dmg)),
  tar_target(paper_t6_csv, tab(paper_t6, "paper_table6_replication.csv"), format = "file"),

  # ---- Step 5: temporal vs spatial -----------------------------------------
  tar_target(sp_cor, spatial_correlations(tr, all_species)),
  tar_target(tvs, temporal_vs_spatial(pw, sp_cor)),
  tar_target(tvs_csv, tab(tvs, "temporal_vs_spatial.csv"), format = "file"),

  # ---- Step 6: transferability ---------------------------------------------
  tar_target(pc, per_country_correlations(tr, all_species)),
  tar_target(loco, dplyr::bind_rows(lapply(c("z_within", "detrended", "anomaly"), function(v)
    loco_correlations(tr, all_species, v)))),
  tar_target(ts, dplyr::bind_rows(lapply(c("z_within", "detrended", "anomaly"), function(v)
    transferability_summary(pw, pc, loco, v)))),
  tar_target(pc_csv, tab(pc, "per_country_correlations.csv"), format = "file"),
  tar_target(loco_csv, tab(loco, "leave_one_country_out.csv"), format = "file"),
  tar_target(ts_csv, tab(ts, "transferability_summary.csv"), format = "file"),
  tar_target(fig_explainer, save_fig(plot_correlation_explainer(tr, pc, tibble::tribble(
    ~a,        ~b,        ~country, ~label,
    "ips_typ", "pit_spp", "AT", "Pityokteines (consistent)",
    "ips_typ", "pit_spp", "CZ", "Pityokteines (consistent)",
    "ips_typ", "pit_spp", "HR", "Pityokteines (consistent)",
    "ips_typ", "pit_cha", "DE", "P. chalcographus (inconsistent)",
    "ips_typ", "pit_cha", "LT", "P. chalcographus (inconsistent)",
    "ips_typ", "pit_cha", "SK", "P. chalcographus (inconsistent)")),
    "outputs/figures/correlation_explainer.png", 11, 6.5), format = "file"),
  tar_target(fig_top_pairs,
             save_fig(plot_top_pairs(dplyr::filter(ts, version == "z_within"), pc),
                      "outputs/figures/top_pairs_by_country.png", 9, 7), format = "file"),

  # ---- Correlation metrics v3 (all pairs, bootstrap CIs, lags, co-occurrence) ----
  tar_target(cm, correlation_metrics(tr, all_species, ts)),
  tar_target(cm_species, species_information(cm, species_meta)),
  tar_target(cm_lags, lag_pairs(cm)),
  tar_target(cm_csv, tab(cm, "correlation_metrics_all_pairs.csv"), format = "file"),
  tar_target(fig_cm_top, save_fig(plot_cm_top(cm), "outputs/figures/cm_top_links.png", 9, 8.5), format = "file"),
  tar_target(fig_cm_species, save_fig(plot_cm_species(cm_species), "outputs/figures/cm_links_per_species.png", 8.5, 9.5),
             format = "file"),
  tar_target(fig_cm_lags, save_fig(plot_cm_lags(cm_lags), "outputs/figures/cm_lead_effects.png", 9, 7), format = "file"),
  tar_target(cm_species_csv, tab(cm_species, "correlation_metrics_by_species.csv"), format = "file"),
  tar_target(cm_lags_csv, tab(cm_lags, "correlation_metrics_lagged.csv"), format = "file"),

  # ---- Correlation metrics, back to basics (paper's CZ rule, Q1-Q5) ---------
  # Hierarchy of the 2025 paper (Table 1; R/paper_hier.R). Aggregate reporting
  # codes count only at group level: not ranked, correlated or filled as insects
  tar_target(ph_file, "data/lookup/paper_hierarchy_2025.csv", format = "file"),
  tar_target(ph_hier, paper_hier_table(read_paper_hierarchy(ph_file), species_meta)),
  tar_target(cb_dmg, prepare_damage(long, species_meta, group_to_species_paper)),   # CZ + DE spruce group -> ips_typ, as in the paper
  tar_target(cb_cells, classify_cells(cb_dmg)),
  tar_target(cb_tr, make_transforms(cb_cells, min_nonzero = 0)),     # all codes (the factor model also learns from aggregates)
  tar_target(cb_species, intersect(sort(unique(cb_cells$species)), paper_species_codes(ph_hier))),   # single insects only
  tar_target(ph_hier_sp, dplyr::filter(ph_hier, species %in% cb_species)),

  # ---- Ranking and Table 2 check on the paper's hierarchy -------------------
  tar_target(p_rank_species, paper_species_ranking(cb_dmg, ph_hier)),
  tar_target(p_rank_groups, paper_group_ranking(cb_dmg, ph_hier)),
  tar_target(p_aggregates, paper_aggregate_table(cb_dmg, ph_hier)),
  tar_target(p_table2, paper_table2_check(cb_dmg, ph_hier)),
  tar_target(p_rank_species_csv, tab(p_rank_species, "RANK_species_level3.csv"), format = "file"),
  tar_target(p_rank_groups_csv, tab(p_rank_groups, "RANK_groups_level1_level2.csv"), format = "file"),
  tar_target(p_aggregates_csv, tab(p_aggregates, "RANK_aggregate_codes.csv"), format = "file"),
  tar_target(p_table2_csv, tab(p_table2, "CHECK_totals_vs_paper_table2.csv"), format = "file"),
  tar_target(fig_rank_l1, save_fig(plot_rank_level1(p_rank_groups), "outputs/figures/RANK_1_level1_guild_x_host_type.png", 9, 3.6),
             format = "file"),
  tar_target(fig_rank_l2, save_fig(plot_rank_level2(p_rank_groups), "outputs/figures/RANK_2_level2_by_host_tree.png", 9, 4),
             format = "file"),
  tar_target(fig_rank_l3, save_fig(plot_rank_level3(p_rank_species), "outputs/figures/RANK_3_level3_individual_species.png", 9, 8),
             format = "file"),
  tar_target(fig_p_rank, save_grob(plot_paper_ranking(p_rank_groups, p_rank_species),
                                   "outputs/figures/species_nuts_occurrence_ranking.png", 9, 13), format = "file"),

  # ---- Group-level correlations (levels 1-2, aggregates included) ------------
  tar_target(p_group_cm, dplyr::bind_rows(
    dplyr::left_join(paper_group_correlations(cb_cells, ph_hier, "level1"),
                     paper_group_europe_correlations(cb_cells, ph_hier, "level1"), by = c("level", "species_a", "species_b")),
    dplyr::left_join(paper_group_correlations(cb_cells, ph_hier, "level2_paper"),
                     paper_group_europe_correlations(cb_cells, ph_hier, "level2_paper"), by = c("level", "species_a", "species_b")))),
  tar_target(p_group_cm_csv, tab(p_group_cm, "CM_groups_level1_level2.csv"), format = "file"),
  tar_target(fig_p_group_l1, save_fig(plot_paper_group_heatmap(p_group_cm, "level1"), "outputs/figures/CM_groups_level1.png", 8, 6),
             format = "file"),
  tar_target(fig_p_group_l2, save_fig(plot_paper_group_heatmap(p_group_cm, "level2"), "outputs/figures/CM_groups_level2.png", 9, 7),
             format = "file"),
  tar_target(cb_pw, pairwise_correlations(cb_tr, cb_species, versions = "z_within")),
  tar_target(cb_pc, per_country_correlations(cb_tr, cb_species, versions = "z_within")),
  tar_target(cb_loco, loco_correlations(cb_tr, cb_species, "z_within")),
  tar_target(cb_ts, transferability_summary(cb_pw, cb_pc, cb_loco, "z_within")),
  tar_target(cb_cm, correlation_metrics(cb_tr, cb_species, cb_ts)),
  tar_target(cb_table, cm_basic_table(cb_cm, ph_hier)),
  tar_target(cb_info, species_information(cb_cm, species_meta)),
  tar_target(cb_lags, lag_pairs(cb_cm)),
  tar_target(cb_q5, dplyr::bind_rows(hierarchy_pair_test(cb_cm, ph_hier_sp, "same_year"),
                                     hierarchy_pair_test(cb_cm, ph_hier_sp, "beyond_year"))),
  tar_target(cb_table_csv, tab(cb_table, "CM_basic_all_pairs.csv"), format = "file"),
  tar_target(cb_verdict, cm_verdict(cb_table)),
  tar_target(cb_pred_country, predictive_by_country(cb_tr, cb_species)),
  tar_target(cb_pred, predictive_summary(cb_pred_country, cb_verdict)),
  tar_target(cb_pred_csv, tab(cb_pred, "CM_predictive_pairs.csv"), format = "file"),
  tar_target(cb_pred_country_csv, tab(cb_pred_country, "CM_predictive_by_country.csv"), format = "file"),
  tar_target(fig_cb_pred, save_fig(plot_cm_predictive(cb_pred), "outputs/figures/CM_predictive.png", 9.5, 6.5), format = "file"),
  # Spearman (within-region rank) version of the same pipeline
  tar_target(sp_tr, rank_transforms(cb_tr)),
  tar_target(sp_pw, pairwise_correlations(sp_tr, cb_species, versions = "z_within")),
  tar_target(sp_pc, per_country_correlations(sp_tr, cb_species, versions = "z_within")),
  tar_target(sp_loco, loco_correlations(sp_tr, cb_species, "z_within")),
  tar_target(sp_ts, transferability_summary(sp_pw, sp_pc, sp_loco, "z_within")),
  tar_target(sp_cm, correlation_metrics(sp_tr, cb_species, sp_ts)),
  tar_target(sp_verdict, cm_verdict(cm_basic_table(sp_cm, ph_hier))),
  tar_target(sp_compare, compare_rank_verdicts(cb_verdict, sp_verdict)),
  tar_target(sp_compare_csv, tab(sp_compare, "CM_pearson_vs_spearman.csv"), format = "file"),
  tar_target(cb_usable, usable_table(cb_verdict, species_meta)),
  tar_target(fig_cb_usable_table, save_grob(plot_usable_table(cb_usable), "outputs/figures/CM_usable_pairs_table.png", 10.5, 10.4),
             format = "file"),
  tar_target(cb_partner_eq, partner_equations(cb_tr, cb_verdict)),
  tar_target(cb_partner_eq_csv, tab(cb_partner_eq, "CM_usable_pairs_equation.csv"), format = "file"),
  tar_target(fig_cb_partner_eq_table, save_grob(plot_partner_eq_table(cb_partner_eq), "outputs/figures/CM_usable_pairs_equation_table.png", 11, 9.4),
             format = "file"),
  tar_target(fig_cb_partner_eq_scatter, save_fig(plot_partner_eq_scatter(cb_tr, "ips_typ", "pit_spp"),
                                                 "outputs/figures/CM_equation_example_ips_typ_pit_spp.png", 11, 5.6), format = "file"),
  tar_target(fig_cb_tab_questions, save_grob(plot_simple_table(cm_table_questions(), "Four questions per insect pair, and the objective each one serves"),
                                             "outputs/figures/CM_table_questions.png", 10, 3.4), format = "file"),
  tar_target(fig_cb_tab_verdicts, save_grob(plot_simple_table(cm_table_verdicts(cb_verdict), paste0("One verdict per insect pair (", format(nrow(cb_verdict), big.mark = ","), " pairs)"),
                                                              "strong = sure link (95% range excludes 0) with r >= 0.3; checked = recorded together in >= 2 countries"),
                                            "outputs/figures/CM_table_verdicts.png", 8.5, 3.6), format = "file"),
  tar_target(fig_cb_tab_groups, save_grob(plot_simple_table(cm_table_groups(cb_verdict), "Feeding type matters: strong links within vs between broad groups",
                                                            paste0("Level 1 groups of the 2025 paper: borers / defoliators on gymnosperms / angiosperms. Permutation test (999 relabellings): p = ",
                                                                   formatC(cb_q5$p_value[cb_q5$metric == "same_year" & grepl("level 1", cb_q5$pair_class)], format = "f", digits = 3), ".")),
                                          "outputs/figures/CM_table_groups.png", 8.5, 2.2), format = "file"),
  tar_target(cb_verdict_csv, tab(cb_verdict, "CM_pairs_verdict.csv"), format = "file"),
  tar_target(fig_cb_heatmap, save_fig(plot_cm_heatmap(cb_verdict, ph_hier_sp, cm_basic_order(ph_hier_sp)),
                                      "outputs/figures/CM_heatmap.png", 11, 11), format = "file"),
  tar_target(fig_cb_verdict, save_fig(plot_cm_verdict(cb_verdict), "outputs/figures/CM_verdict.png", 8, 4.2), format = "file"),
  tar_target(fig_cb_usable, save_fig(plot_cm_usable(cb_verdict), "outputs/figures/CM_usable_pairs.png", 8.5, 7), format = "file"),
  tar_target(cb_info_csv, tab(cb_info, "CM_basic_per_insect.csv"), format = "file"),
  tar_target(cb_lags_csv, tab(cb_lags, "CM_basic_Q3_lead_effects.csv"), format = "file"),
  tar_target(cb_q5_csv, tab(cb_q5, "CM_basic_Q5_structure_test.csv"), format = "file"),
  tar_target(cb_pc_csv, tab(cb_pc, "CM_basic_Q4_per_country.csv"), format = "file"),
  tar_target(fig_cb_matrix, save_fig(plot_cm_basic_matrix(cb_cm, ph_hier_sp, cm_basic_order(ph_hier_sp)),
                                     "outputs/figures/CM_basic_1_matrix.png", 11, 10.5), format = "file"),
  tar_target(fig_cb_info, save_fig(plot_cm_species(cb_info), "outputs/figures/CM_basic_2_links_per_insect.png", 8.5, 9.5),
             format = "file"),
  tar_target(fig_cb_transfer, save_fig(plot_top_pairs(cb_ts, cb_pc), "outputs/figures/CM_basic_3_across_countries.png", 9, 7),
             format = "file"),

  # ---- Helper finder --------------------------------------------------------
  tar_target(hf, helper_finder(cells, ts, pw, species_meta)),
  tar_target(hf_best, helper_best(hf, k = 3)),
  tar_target(hf_csv, tab(hf, "helper_finder_all_pairs.csv"), format = "file"),
  tar_target(hf_best_csv, tab(hf_best, "helper_finder_best3.csv"), format = "file"),
  tar_target(hf_xlsx, {
    p <- "outputs/tables/helper_finder.xlsx"
    writexl::write_xlsx(list(
      README = tibble::tibble(item = c("best_helpers", "all_pairs", "grade A", "grade B", "grade C",
                                       "r", "r_anomaly", "gap_coverage"),
        meaning = c("Up to 3 best helpers per target insect (A before B, then strongest link).",
                    "Every target-helper combination with all metrics.",
                    "Recommended: link holds in >= 2 countries (same sign, similar strength), |r| >= 0.3, and it survives removing Europe-wide bad years (|r_anomaly| >= 0.2, same sign).",
                    "Possible: consistent but weaker (0.2-0.3), consistent but mostly shared bad years, or |r| >= 0.3 tested in one country only. All grades need >= 2 regions and >= 10 years where both had damage.",
                    "Not suitable: weak, inconsistent between countries, or helper never recorded where target is missing.",
                    "Correlation of yearly ups and downs within regions (z_within).",
                    "Same after removing Europe-wide bad years; shows the link beyond shared drought years.",
                    "% of the target's missing region-years in which the helper is recorded.")),
      best_helpers = hf_best, all_pairs = hf), p)
    p
  }, format = "file"),
  tar_target(fig_hf, save_fig(plot_helper_finder(hf_best, sp_rank),
                              "outputs/figures/helper_finder.png", 10, 10), format = "file"),

  # ---- Latent factor model (prototype) --------------------------------------
  tar_target(lfm_L, build_lfm_matrix(cells, tr)),
  tar_target(lfm_blocks, make_cv_blocks(lfm_L, n_folds = 10)),
  tar_target(lfm_grid, tidyr::expand_grid(K = c(0, 1, 2, 3, 4, 6), lambda = c(5, 20, 50, 100)) |>
               dplyr::filter(!(K == 0 & lambda != 20)) |>
               dplyr::mutate(model = paste0("Factor model: K=", K, ", lambda=", lambda))),
  tar_target(lfm_cv_res, lfm_cv(lfm_L, lfm_blocks, lfm_grid)),
  tar_target(lfm_score, lfm_scores(lfm_cv_res)),
  # Parsimony rule: the simplest factor model (smallest K >= 1, then largest
  # lambda) whose CV RMSE is within 0.5% of the best factor model.
  tar_target(lfm_best, {
    sc <- dplyr::filter(lfm_score, grepl("^Factor", model)) |> dplyr::left_join(lfm_grid, by = "model")
    sc |> dplyr::filter(K >= 1, rmse <= min(rmse) * 1.005) |>
      dplyr::arrange(K, dplyr::desc(lambda)) |> dplyr::slice(1)
  }),
  tar_target(lfm_score_species, lfm_scores(dplyr::filter(lfm_cv_res, model %in% c(
    lfm_best$model, "Baseline: species x year average", "Baseline: best single helper")), by = "species")),
  tar_target(lfm_fit_final, lfm_final(lfm_L, lfm_best$K, lfm_best$lambda)),
  tar_target(lfm_load, lfm_loadings(lfm_fit_final, lfm_L, species_meta)),
  tar_target(lfm_year_scores, lfm_scores_by_year(lfm_fit_final, lfm_L)),
  tar_target(lfm_filled, lfm_fill(lfm_fit_final, lfm_L)),
  tar_target(fig_lfm_comp, save_fig(plot_lfm_comparison(lfm_score, lfm_grid, lfm_best$model, nrow(lfm_blocks)),
                                    "outputs/figures/lfm_model_comparison.png", 11, 5.5), format = "file"),
  tar_target(fig_lfm_sp, save_fig(plot_lfm_species(lfm_score_species, lfm_best$model, species_meta),
                                  "outputs/figures/lfm_skill_by_species.png", 8, 8), format = "file"),
  tar_target(fig_lfm_ex, save_fig(plot_lfm_examples(lfm_cv_res, lfm_best$model,
                                    lfm_example_pick(lfm_cv_res, lfm_best$model, n = 6)),
    "outputs/figures/lfm_examples.png", 11, 6.5), format = "file"),
  tar_target(fig_lfm_fac, {
    pp <- plot_lfm_factors(lfm_load, lfm_year_scores)
    c(save_fig(pp$loadings, "outputs/figures/lfm_factor_loadings.png", 8, 10),
      save_fig(pp$years, "outputs/figures/lfm_factor_years.png", 6, 6))
  }, format = "file"),
  tar_target(lfm_score_csv, tab(lfm_score, "lfm_cv_scores_by_model.csv"), format = "file"),
  tar_target(lfm_score_sp_csv, tab(lfm_score_species, "lfm_cv_scores_by_species.csv"), format = "file"),
  tar_target(lfm_load_csv, tab(lfm_load, "lfm_factor_loadings.csv"), format = "file"),
  tar_target(lfm_filled_csv, write_csv_out(lfm_filled, "data/processed/lfm_gap_filled_PROTOTYPE.csv"), format = "file"),

  # ---- Two-part (hurdle) factor model and gap-filled dataset -----------------
  tar_target(h_grid1, tidyr::expand_grid(K = c(0, 2, 4), lambda = c(1, 10)) |>
               dplyr::mutate(model = paste0("any-damage model: K=", K, ", lambda=", lambda))),
  tar_target(h_grid2, tidyr::expand_grid(K = c(0, 2, 4), lambda = c(20, 50, 100)) |>
               dplyr::mutate(model = paste0("amount model: K=", K, ", lambda=", lambda))),
  tar_target(h_cv, hurdle_cv(lfm_L, lfm_blocks, h_grid1, h_grid2, list(K = lfm_best$K, lambda = lfm_best$lambda))),
  tar_target(h_part_scores, hurdle_part_scores(h_cv)),
  tar_target(h_best1, h_part_scores |> dplyr::filter(part == "1: any damage?", grepl("^any-damage", model)) |>
               dplyr::arrange(log_loss) |> dplyr::slice(1) |> dplyr::left_join(h_grid1, by = "model")),
  tar_target(h_best2, h_part_scores |> dplyr::filter(part == "2: how much, if damaged?", grepl("^amount", model)) |>
               dplyr::arrange(rmse_log) |> dplyr::slice(1) |> dplyr::left_join(h_grid2, by = "model")),
  tar_target(h_cmp, hurdle_compare(h_cv, h_best1$model, h_best2$model)),
  tar_target(h_final, hurdle_final(lfm_L, h_best1$K, h_best1$lambda, h_best2$K, h_best2$lambda)),
  tar_target(h_conf, hurdle_confidence(h_cmp$by_species, lfm_L$species)),
  # host-aware version: absent host -> 0, host area in both parts
  tar_target(hh_L, host_scaled_L(lfm_L, host_status)),
  tar_target(hh_absent, host_absent_matrix(lfm_L, host_status)),
  tar_target(hh_cv, host_cv(lfm_L, hh_L, hh_absent, lfm_blocks, h_cv, h_best1, h_best2)),
  tar_target(hh_cmp, host_compare(hh_cv, h_best1$model, h_best2$model, host_status)),
  tar_target(hh_final, host_final(lfm_L, hh_L, h_final, h_best1, h_best2)),
  tar_target(hh_conf, host_confidence(hh_cmp$by_species, lfm_L$species)),
  tar_target(h_filled, host_fill(lfm_L, hh_final, hh_absent, hh_conf, cells, host_status,
                                 dplyr::select(species_meta, species, unit))),
  tar_target(h_totals, hurdle_totals(h_filled)),
  tar_target(fig_h_cmp, save_fig(plot_hurdle_compare(hh_cmp$summary), "outputs/figures/hurdle_vs_one_part.png", 8, 4.5), format = "file"),
  tar_target(fig_h_status, save_fig(plot_harmonised_status(h_filled, sp_rank$species),
                                    "outputs/figures/harmonised_status_map.png", 12, 5.5), format = "file"),
  tar_target(fig_h_totals, save_fig(plot_totals_composition(h_totals), "outputs/figures/europe_totals_composition.png", 10, 7),
             format = "file"),
  tar_target(h_part_scores_csv, tab(h_part_scores, "hurdle_cv_part_scores.csv"), format = "file"),
  tar_target(h_cmp_csv, tab(h_cmp$summary, "hurdle_vs_one_part.csv"), format = "file"),
  tar_target(h_conf_csv, tab(hh_conf, "hurdle_confidence_by_species.csv"), format = "file"),
  tar_target(hh_cmp_csv, tab(hh_cmp$summary, "gapfill_models_compared_with_hosts.csv"), format = "file"),
  tar_target(h_totals_csv, tab(h_totals, "europe_totals_observed_plus_filled_PRELIMINARY.csv"), format = "file"),
  tar_target(h_filled_csv, write_csv_out(h_filled, "data/processed/eufordam_harmonised_PRELIMINARY.csv"), format = "file"),

  # ---- Harmonisation by gap type, at insect and agent-group level -------------
  # Gap types: recorded / host absent / never reported by this country /
  # before the series starts / missing year inside the series.
  tar_target(groups, agent_groups(species_meta, guild_lookup)),
  tar_target(groups_csv, tab(groups, "agent_groups_PROVISIONAL.csv"), format = "file"),
  tar_target(g_raw, group_cells(cells, groups) |> dplyr::rename(species = group)),
  tar_target(g_meta, group_meta(groups)),
  tar_target(g_host, host_status_table(g_raw, g_meta, host_long)),
  tar_target(g_cells, apply_host_status(g_raw, g_host)),
  tar_target(gap_tab, dplyr::bind_rows(gap_summary(cells_host, "insect"), gap_summary(g_cells, "agent group"))),
  tar_target(gap_tab_csv, tab(gap_tab, "gap_types_insect_vs_group.csv"), format = "file"),
  tar_target(g_gt, gap_types(g_cells)),
  tar_target(g_L, build_lfm_matrix(g_cells, group_tr(dplyr::rename(g_cells, group = species)))),
  tar_target(g_blocks, make_cv_blocks(g_L, n_folds = 10)),
  tar_target(g_grid1, tidyr::expand_grid(K = c(0, 1, 2), lambda = c(1, 10)) |>
               dplyr::mutate(model = paste0("any-damage model: K=", K, ", lambda=", lambda))),
  tar_target(g_grid2, tidyr::expand_grid(K = c(0, 1, 2), lambda = c(5, 20, 50)) |>
               dplyr::mutate(model = paste0("amount model: K=", K, ", lambda=", lambda))),
  tar_target(g_cv, hurdle_cv(g_L, g_blocks, g_grid1, g_grid2, list(K = 1, lambda = 20))),
  tar_target(g_part_scores, hurdle_part_scores(g_cv)),
  tar_target(g_best1, g_part_scores |> dplyr::filter(grepl("^any-damage", model)) |>
               dplyr::arrange(log_loss) |> dplyr::slice(1) |> dplyr::left_join(g_grid1, by = "model")),
  tar_target(g_best2, g_part_scores |> dplyr::filter(grepl("^amount", model)) |>
               dplyr::arrange(rmse_log) |> dplyr::slice(1) |> dplyr::left_join(g_grid2, by = "model")),
  tar_target(g_hL, host_scaled_L(g_L, g_host)),
  tar_target(g_absent, host_absent_matrix(g_L, g_host)),
  tar_target(g_hcv, host_cv(g_L, g_hL, g_absent, g_blocks, g_cv, g_best1, g_best2)),
  tar_target(g_cmp, host_compare(g_hcv, g_best1$model, g_best2$model, g_host)),
  tar_target(g_hfinal0, hurdle_final(g_L, g_best1$K, g_best1$lambda, g_best2$K, g_best2$lambda)),
  tar_target(g_final, host_final(g_L, g_hL, g_hfinal0, g_best1, g_best2)),
  tar_target(g_conf, host_confidence(g_cmp$by_species, g_L$species)),
  tar_target(g_fill0, host_fill(g_L, g_final, g_absent, g_conf, g_cells, g_host,
                                dplyr::rename(dplyr::select(g_meta, species, unit), species = species))),
  # back-casting the years before a series starts (AT, PL, FI start late)
  tar_target(bc_cv_insect, backcast_cv(lfm_L)),
  tar_target(bc_cv_group, backcast_cv(g_L)),
  tar_target(bc_scores, dplyr::bind_rows(
    dplyr::mutate(backcast_scores(bc_cv_insect), level = "insect"),
    dplyr::mutate(backcast_scores(bc_cv_group), level = "agent group"))),
  tar_target(bc_scores_csv, tab(bc_scores, "backcast_test_scores.csv"), format = "file"),
  tar_target(g_bc, backcast_fill(g_L, g_gt)),
  tar_target(g_harm, group_harmonised(g_fill0, g_gt, g_bc, dplyr::filter(bc_scores, level == "agent group"))),
  tar_target(g_totals, group_totals(g_harm)),
  tar_target(fig_g_status, save_fig(plot_group_status(g_harm), "outputs/figures/group_status_map.png", 9, 6.5), format = "file"),
  tar_target(fig_g_totals, save_fig(plot_group_totals(g_totals), "outputs/figures/group_totals.png", 10, 7), format = "file"),
  tar_target(g_cmp_csv, tab(g_cmp$summary, "group_gapfill_models_compared.csv"), format = "file"),
  tar_target(g_conf_csv, tab(g_conf, "group_confidence.csv"), format = "file"),
  tar_target(g_totals_csv, tab(g_totals, "europe_totals_by_agent_group_PRELIMINARY.csv"), format = "file"),
  tar_target(g_harm_csv, write_csv_out(g_harm, "data/processed/eufordam_harmonised_agent_groups_PRELIMINARY.csv"), format = "file"),

  # ---- Correlations along the hierarchy (4 broad groups / host-tree groups) --
  tar_target(hier, hierarchy_table(species_meta)),
  tar_target(hier_csv, tab(hier, "hierarchy_levels.csv"), format = "file"),
  tar_target(hier_cm, dplyr::bind_rows(level_correlations(cells, hier, "level1"),
                                       level_correlations(cells, hier, "level2"))),
  tar_target(hier_europe, dplyr::bind_rows(hier_europe_correlations(cells, hier, "level1"),
                                           hier_europe_correlations(cells, hier, "level2"))),
  tar_target(hier_test, dplyr::bind_rows(hierarchy_pair_test(cm, hier, "same_year"),
                                         hierarchy_pair_test(cm, hier, "beyond_year"))),
  tar_target(hier_cm_csv, tab(dplyr::left_join(hier_cm, hier_europe, by = c("level", "species_a", "species_b")),
                              "correlation_metrics_hierarchy_levels.csv"), format = "file"),
  tar_target(hier_test_csv, tab(hier_test, "correlation_hierarchy_test.csv"), format = "file"),
  tar_target(fig_hier_l1, save_fig(plot_hier_heatmap(hier_cm, hier, "level1", "Correlation between the 4 broad groups (as in Hlasny et al. 2025)"),
                                   "outputs/figures/hierarchy_level1_correlations.png", 7, 5.5), format = "file"),
  tar_target(fig_hier_l2, save_fig(plot_hier_heatmap(hier_cm, hier, "level2", "Correlation between host-tree groups"),
                                   "outputs/figures/hierarchy_level2_correlations.png", 9, 7.5), format = "file"),
  tar_target(fig_hier_test, save_fig(plot_hier_test(hier_test), "outputs/figures/hierarchy_pair_test.png", 8, 5.5), format = "file"),

  # ---- Option 3: two-part model with a guild hint vs plain ---------------------
  tar_target(hm_group, hier$level1[match(lfm_L$species, hier$species)]),
  # versions: strength of the hint on level (tau) and links (tau_links);
  # "level only" / "links only" switch one part off (ablation)
  tar_target(hm_settings, dplyr::bind_rows(
    tibble::tibble(tau = c(0, 5, 20, 50, 100, 300, 1000)) |>
      dplyr::mutate(tau_links = tau, label = paste0("t", tau)),
    tibble::tibble(label = c("level only (50)", "links only (50)"), tau = c(50, 0), tau_links = c(0, 50)))),
  tar_target(hm_cv, hier_model_cv(lfm_L, lfm_blocks, hm_group, h_best1$K, h_best1$lambda,
                                  h_best2$K, h_best2$lambda, settings = hm_settings)),
  tar_target(hm_nested, hier_nested_select(hm_cv, paste0("t", c(0, 5, 20, 50, 100, 300, 1000)))),
  tar_target(hm_scores, {
    sc <- hier_model_scores(dplyr::bind_rows(dplyr::select(hm_cv, -tau_links), dplyr::select(hm_nested, -chosen_part1, -chosen_part2, -tau_links)))
    lab <- c(t0 = "plain (no hint)", nested = "hint, strength chosen without the test fold (strict)",
             `level only (50)` = "hint on level only (50)", `links only (50)` = "hint on links only (50)")
    sc$overall <- sc$overall |> dplyr::mutate(tau = suppressWarnings(as.numeric(sub("^t", "", version))),
      model = dplyr::coalesce(lab[version], paste0("guild hint, strength ", tau)), .before = 1)
    sc$by_species <- sc$by_species |> dplyr::mutate(tau = suppressWarnings(as.numeric(sub("^t", "", version))))
    sc
  }),
  tar_target(hm_nested_choice, dplyr::distinct(hm_nested, fold, chosen_part1, chosen_part2)),
  tar_target(hm_nested_choice_csv, tab(hm_nested_choice, "guild_hint_nested_choice_by_fold.csv"), format = "file"),
  tar_target(hm_overall_csv, tab(hm_scores$overall, "guild_hint_vs_plain_overall.csv"), format = "file"),
  tar_target(hm_species_csv, tab(hm_scores$by_species, "guild_hint_vs_plain_by_species.csv"), format = "file"),
  tar_target(fig_hm_strength, save_fig(plot_hint_strength(dplyr::filter(hm_scores$overall, !is.na(tau)), 50), "outputs/figures/guild_hint_strength.png", 9, 6.5), format = "file"),
  tar_target(fig_hm_species, save_fig(plot_hint_species(dplyr::filter(hm_scores$by_species, !is.na(tau)), hier, 50), "outputs/figures/guild_hint_by_species.png", 8, 8.5), format = "file"),

  # ---- Gap-filling: simple average vs partner method vs factor model ---------
  tar_target(gf_cv, gapfill_compare_cv(lfm_L, lfm_blocks, hm_group, h_best1$K, h_best1$lambda,
                                       h_best2$K, h_best2$lambda, tau = 50,
                                       Lh = host_scaled_L(lfm_L, host_status),
                                       absent = host_absent_matrix(lfm_L, host_status))),
  tar_target(gf_scores, gapfill_compare_scores(gf_cv)),
  # Hybrid: the notes' equation with the factor model's level (R/gapfill_hybrid.R)
  tar_target(gf_hyb, hybrid_from_cv(lfm_L, lfm_blocks, gf_cv)),
  tar_target(gf_cv_hyb, gf_cv |> dplyr::left_join(gf_hyb, by = c("fold", "row", "col")) |>
               dplyr::left_join(blend_from_cv(gf_cv), by = c("fold", "row", "col"))),
  tar_target(gf_scores_hyb, gapfill_compare_scores(gf_cv_hyb)),
  tar_target(gf_scores_hyb_partner, gapfill_compare_scores(dplyr::filter(gf_cv_hyb, hybrid_used))),
  tar_target(gf_hyb_csv, tab(dplyr::bind_rows(all = gf_scores_hyb$overall, cells_with_partner = gf_scores_hyb_partner$overall, .id = "cells"),
                             "GF_hybrid_vs_methods.csv"), format = "file"),
  tar_target(gf_overall_csv, tab(gf_scores$overall, "GF_methods_overall.csv"), format = "file"),
  tar_target(gf_species_csv, tab(gf_scores$by_species, "GF_methods_by_insect.csv"), format = "file"),
  tar_target(fig_gf_overall, save_fig(plot_gapfill_overall(gf_scores$overall), "outputs/figures/GF_1_methods_overall.png", 11, 7), format = "file"),
  tar_target(fig_gf_species, save_fig(plot_gapfill_species(gf_scores$by_species), "outputs/figures/GF_2_methods_by_insect.png", 9, 9.5), format = "file"),

  # ---- Gap-filling v3: same data as the correlation metrics (R/gapfill_v3.R) ----
  tar_target(gf3_L, build_lfm_matrix(cb_cells, cb_tr)),
  tar_target(gf3_blocks, make_cv_blocks(gf3_L, n_folds = 10)),
  tar_target(gf3_group, ph_hier$level1[match(gf3_L$species, ph_hier$species)]),
  tar_target(gf3_cv, gapfill_compare_cv(gf3_L, gf3_blocks, gf3_group, h_best1$K, h_best1$lambda,
                                        h_best2$K, h_best2$lambda, tau = 50,
                                        Lh = host_scaled_L(gf3_L, host_status),
                                        absent = host_absent_matrix(gf3_L, host_status))),
  tar_target(gf3_scores, gapfill_compare_scores(dplyr::filter(gf3_cv, species %in% cb_species))),   # scored on single insects only
  tar_target(gf3_raw, gapfill_final(gf3_L, host_status, gf3_group, h_best1$K, h_best1$lambda,
                                    h_best2$K, h_best2$lambda, tau = 50)),
  tar_target(gf3_model_cor, model_vs_correlation(gf3_raw, cb_verdict)),
  tar_target(gf3_model_cor_csv, tab(gf3_model_cor, "GF3_model_vs_correlation_metrics.csv"), format = "file"),

  # ---- Gap-filled dataset v2: factor model only, ranges from the test ---------
  tar_target(gf3_fcv, final_method_cv(gf3_L, gf3_blocks, host_status, gf3_group, h_best1$K, h_best1$lambda,
                                      h_best2$K, h_best2$lambda, tau = 50)),
  tar_target(gf3_bt, cv_block_totals(dplyr::filter(gf3_fcv, species %in% cb_species))),
  tar_target(gf3_simple, gf3_scores$by_species |> dplyr::filter(method == "simple average") |>
               dplyr::select(species, error_log_simple = error_log)),
  tar_target(gf3_conf, final_method_scores(dplyr::filter(gf3_fcv, species %in% cb_species), gf3_simple)),
  tar_target(gf3_range, range_table(gf3_bt)),
  tar_target(gf3_calib, range_calibration(gf3_bt)),
  tar_target(gf3_data, gapfill_dataset_v2(gf3_raw, gf3_conf, dplyr::select(species_meta, species, unit), gf3_L$species)),
  tar_target(gf3_tot, gapfill_totals_v2(gf3_data, gf3_range, gf3_bt)),
  tar_target(gf3_data_csv, write_csv_out(gf3_data, "data/processed/eufordam_gapfilled_v2.csv"), format = "file"),
  tar_target(gf3_eu_csv, tab(gf3_tot$europe, "GF2_europe_totals_with_ranges.csv"), format = "file"),
  tar_target(gf3_country_csv, tab(gf3_tot$country, "GF2_country_totals_with_ranges.csv"), format = "file"),
  tar_target(gf3_conf_csv, tab(gf3_conf, "GF2_confidence_by_insect.csv"), format = "file"),
  tar_target(gf3_range_csv, tab(dplyr::bind_cols(gf3_range), "GF2_total_error_ranges_by_insect.csv"), format = "file"),
  tar_target(gf3_calib_csv, tab(gf3_calib, "GF2_range_calibration.csv"), format = "file"),
  tar_target(fig_gf3_europe, save_fig(plot_gf2_europe(gf3_tot$europe), "outputs/figures/GF2_europe_totals_with_ranges.png", 10, 10),
             format = "file"),
  tar_target(gf3_summary_csv, tab(dplyr::count(gf3_data, value_type, confidence), "GF2_dataset_summary.csv"), format = "file"),

  # ---- Draft range rule, test round (R/range_rule.R) ---------------------------
  tar_target(rng_draft, range_draft(gf3_L)),
  tar_target(rng_draft_csv, tab(rng_draft, "GF2_range_draft_long.csv"), format = "file"),
  tar_target(rng_draft_xlsx, {
    path <- "outputs/tables/GF2_range_draft_for_correction.xlsx"
    writexl::write_xlsx(list(`in range (1 = yes)` = range_draft_wide(rng_draft), reasons = rng_draft), path); path
  }, format = "file"),
  tar_target(gf3r_data, apply_range(gf3_data, rng_draft)),
  tar_target(gf3r_tot, gapfill_totals_v2(gf3r_data, gf3_range, gf3_bt)),
  tar_target(gf3r_effect, range_effect(gf3_tot, gf3r_tot)),
  tar_target(gf3r_effect_csv, tab(gf3r_effect, "GF2_range_rule_effect.csv"), format = "file"),
  tar_target(gf3r_eu_csv, tab(gf3r_tot$europe, "GF2_europe_totals_with_ranges_rangerule.csv"), format = "file"),
  tar_target(gf3r_summary_csv, tab(dplyr::count(gf3r_data, value_type, confidence), "GF2_dataset_summary_rangerule.csv"), format = "file"),
  tar_target(gf3r_data_csv, write_csv_out(gf3r_data, "data/processed/eufordam_gapfilled_v2_rangerule.csv"), format = "file"),
  tar_target(fig_gf3r_europe, save_fig(plot_gf2_europe(gf3r_tot$europe), "outputs/figures/GF2_europe_totals_with_ranges_rangerule.png", 10, 10),
             format = "file"),

  # ---- Factor model steps M1 / M2 (R/lfm_m12.R) ---------------------------------
  tar_target(m_specs, {
    base <- list(K1 = h_best1$K, lambda1 = h_best1$lambda, K2 = h_best2$K, lambda2 = h_best2$lambda, tau = 50)
    list(
      `M0 current` = c(base, list(tau_links = 0, lambda_rc = 0)),
      `M1 region+year (pull 5)` = c(base, list(tau_links = 0, lambda_rc = 5)),
      `M1 region+year (pull 20)` = c(base, list(tau_links = 0, lambda_rc = 20)),
      `M2 K=3 + group links` = modifyList(c(base, list(tau_links = 50, lambda_rc = 5)), list(K1 = 3, K2 = 3)),
      `M2 K=4 + group links` = modifyList(c(base, list(tau_links = 50, lambda_rc = 5)), list(K1 = 4, K2 = 4)),
      `M2 K=4, no group links` = modifyList(c(base, list(tau_links = 0, lambda_rc = 5)), list(K1 = 4, K2 = 4)))
  }),
  tar_target(m_name, names(m_specs)),
  tar_target(m_cv, variant_cv(gf3_L, gf3_blocks, host_status, gf3_group, m_specs[[m_name]]),
             pattern = map(m_name), iteration = "list"),
  tar_target(m_scores, purrr::map2_dfr(m_cv, m_name, function(cv, nm) variant_scores(dplyr::filter(cv, species %in% cb_species), nm))),
  tar_target(m_links, purrr::map_dfr(m_name, function(nm) variant_links(gf3_L, host_status, gf3_group, m_specs[[nm]], cb_verdict, nm))),
  tar_target(m_scores_csv, tab(dplyr::left_join(m_scores, m_links, by = "model"), "GF_M0_M1_M2_comparison.csv"), format = "file"),

  # ---- Gap-filled dataset v3: model M2 (3 factors + group links) + draft range rule
  #      (tha_pit = T. pityocampa as in the paper: southern range only)
  tar_target(v3_spec_name, "M2 K=3 + group links"),
  tar_target(v3_spec, m_specs[[v3_spec_name]]),
  tar_target(v3_cv, dplyr::filter(m_cv[[which(m_name == v3_spec_name)]], species %in% cb_species)),   # hide-and-predict test of M2, single insects
  tar_target(v3_bt, cv_block_totals(v3_cv)),
  tar_target(v3_conf, final_method_scores(v3_cv, gf3_simple)),
  tar_target(v3_range, range_table(v3_bt)),
  tar_target(v3_calib, range_calibration(v3_bt)),
  tar_target(v3_raw, gapfill_raw_rs(gf3_L, host_status, gf3_group, v3_spec)),
  tar_target(rng_pityo, apply_range_overrides(rng_draft, range_overrides_pityocampa)),
  tar_target(v3_data, apply_range(gapfill_dataset_v2(v3_raw, v3_conf, dplyr::select(species_meta, species, unit), gf3_L$species),
                                  rng_pityo)),
  tar_target(v3_tot, gapfill_totals_v2(v3_data, v3_range, v3_bt)),
  tar_target(v3_data_csv, write_csv_out(v3_data, "data/processed/eufordam_gapfilled_v3.csv"), format = "file"),
  tar_target(v3_eu_csv, tab(v3_tot$europe, "GF3_europe_totals_with_ranges.csv"), format = "file"),
  tar_target(v3_country_csv, tab(v3_tot$country, "GF3_country_totals_with_ranges.csv"), format = "file"),
  tar_target(v3_conf_csv, tab(v3_conf, "GF3_confidence_by_insect.csv"), format = "file"),
  tar_target(v3_calib_csv, tab(v3_calib, "GF3_range_calibration.csv"), format = "file"),
  tar_target(v3_summary_csv, tab(dplyr::count(v3_data, value_type, confidence), "GF3_dataset_summary.csv"), format = "file"),
  tar_target(v3_effect_csv, tab(range_effect(gf3_tot, v3_tot), "GF3_vs_v2_filled_by_insect.csv"), format = "file"),
  tar_target(fig_v3_europe, save_fig(plot_gf2_europe(dplyr::filter(v3_tot$europe, species %in% cb_species)),
                                     "outputs/figures/GF3_europe_totals_with_ranges.png", 10, 10), format = "file"),
  # Totals per group of the paper (levels 1-2): reported (aggregates included) + filled
  tar_target(v3_gtot, gapfill_group_totals(v3_data, v3_bt, ph_hier)),
  tar_target(v3_gtot_csv, tab(v3_gtot, "GF3_europe_totals_by_paper_group.csv"), format = "file"),
  tar_target(v3_gtot_sum_csv, tab(v3_gtot |> dplyr::group_by(level, group, unit) |>
                                    dplyr::summarise(dplyr::across(c(reported, filled), sum), .groups = "drop") |>
                                    dplyr::mutate(total = reported + filled, filled_vs_reported = filled / reported),
                                  "GF3_cumulative_totals_by_paper_group.csv"), format = "file"),
  tar_target(fig_v3_gtot, save_fig(plot_paper_group_totals(v3_gtot), "outputs/figures/GF3_europe_totals_by_paper_group.png", 12, 9),
             format = "file"),

  # ---- Gap-filled dataset v1 ---------------------------------------------------
  tar_target(gf_partner_species, gf_scores$by_species |> dplyr::filter(as.character(best) == "partner method") |>
               dplyr::distinct(species) |> dplyr::pull(species)),
  tar_target(gf_raw, gapfill_final(lfm_L, host_status, hm_group, h_best1$K, h_best1$lambda, h_best2$K, h_best2$lambda,
                                   tau = 50, partner_species = gf_partner_species)),
  tar_target(gf_data, gapfill_dataset(gf_raw, gf_scores$by_species, lfm_L$species,
                                      dplyr::select(species_meta, species, unit))),
  tar_target(gf_totals, gapfill_totals(gf_data)),
  tar_target(gf_data_csv, write_csv_out(gf_data, "data/processed/eufordam_gapfilled_v1.csv"), format = "file"),
  tar_target(gf_totals_csv, tab(gf_totals, "GF_europe_totals_reported_plus_filled.csv"), format = "file"),
  tar_target(gf_summary_csv, tab(dplyr::count(gf_data, value_type, fill_method, confidence), "GF_dataset_summary.csv"), format = "file"),
  tar_target(fig_gf_status, save_fig(plot_gapfill_status(gf_data, occ_rank2$species), "outputs/figures/GF_3_dataset_status.png", 12, 5.5),
             format = "file"),
  tar_target(fig_gf_totals, save_fig(plot_gapfill_totals(gf_totals), "outputs/figures/GF_4_totals_reported_vs_filled.png", 10, 7),
             format = "file"),

  # ---- Gap-filling v2: memory, neighbours, reporting effect, Tweedie --------
  tar_target(nuts_points, c("data/external/nutspt_0.json", "data/external/nutspt_1.json",
                            "data/external/nutspt_2.json", "data/external/nutspt_3.json"), format = "file"),
  tar_target(region_pos, { nuts_points; region_positions(dplyr::distinct(lfm_L$rows, country, nuts_id)) }),
  tar_target(region_pos_csv, tab(region_pos, "region_positions.csv"), format = "file"),
  tar_target(v2_cv, gapfill_v2_cv(lfm_L, lfm_blocks, hm_group, h_best1$K, h_best1$lambda, h_best2$K, h_best2$lambda,
                                  host_status, region_pos)),
  tar_target(v2_pred, gapfill_v2_predict(v2_cv)),
  tar_target(v2_scores, gapfill_v2_scores(v2_pred)),
  tar_target(v2_scores_csv, tab(v2_scores, "GF_v2_extras_scores.csv"), format = "file"),
  tar_target(fig_v2, save_fig(plot_gapfill_v2(v2_scores), "outputs/figures/GF_5_v2_extras.png", 11, 7.5), format = "file"),

  # ---- gllvm latent factor model on agent groups (objective 3, exploratory) --
  tar_target(gl_data, gllvm_data(g_cells)),
  tar_target(gl_pres_fits, gllvm_fit_set(gl_data, "presence")),
  tar_target(gl_time_fits, gllvm_fit_set(gl_data, "timing")),
  tar_target(gl_ic, dplyr::bind_rows(gllvm_ic(gl_pres_fits, "presence"), gllvm_ic(gl_time_fits, "timing"))),
  tar_target(gl_pres, gllvm_best(gl_pres_fits, dplyr::filter(gl_ic, model == "presence"))),
  tar_target(gl_time, gllvm_best(gl_time_fits, dplyr::filter(gl_ic, model == "timing"))),
  tar_target(gl_cor_pres, gllvm_rescor(gl_pres)),
  tar_target(gl_cor_time, gllvm_rescor(gl_time)),
  tar_target(gl_years, gllvm_year_scores(gl_time, attr(gl_time_fits, "years"))),
  tar_target(gl_load, gllvm_loadings(gl_time)),
  tar_target(gl_vp, dplyr::bind_rows(dplyr::mutate(gllvm_vp(gl_pres), model = "presence"),
                                     dplyr::mutate(gllvm_vp(gl_time), model = "timing"))),
  tar_target(gl_check, gllvm_vs_pairwise(gl_cor_time, gl_data)),
  tar_target(gl_ic_csv, tab(gl_ic, "gllvm_model_choice.csv"), format = "file"),
  tar_target(gl_load_csv, tab(gl_load, "gllvm_timing_loadings.csv"), format = "file"),
  tar_target(gl_vp_csv, tab(gl_vp, "gllvm_variance_partitioning.csv"), format = "file"),
  tar_target(gl_check_csv, tab(gl_check, "gllvm_vs_pairwise_correlation.csv"), format = "file"),
  tar_target(fig_gl_cor, save_fig(plot_gllvm_cor(gl_cor_pres, gl_cor_time), "outputs/figures/gllvm_group_correlations.png", 12, 6.5), format = "file"),
  tar_target(fig_gl_years, save_fig(plot_gllvm_years(gl_years, gl_load), "outputs/figures/gllvm_timing_factors.png", 8, 7), format = "file"),
  tar_target(fig_gl_vp, save_fig(plot_gllvm_vp(gl_vp), "outputs/figures/gllvm_variance_partitioning.png", 10, 5), format = "file"),

  # ---- Step 7: guild structure ---------------------------------------------
  tar_target(guild_template_csv,
             write_csv_out(guild_template(species_meta), "data/lookup/species_guild_TEMPLATE.csv"),
             format = "file"),
  tar_target(guild_lookup, load_guild_lookup("data/lookup/species_guild.csv", species_meta) |>
               dplyr::mutate(guild_host = dplyr::if_else(is.na(guild) | is.na(host), NA_character_,
                                                         paste(guild, host, sep = " / "))),
             cue = tar_cue(mode = "always")),
  tar_target(clust, cluster_species(pw, "z_within")),
  tar_target(perm, dplyr::bind_rows(lapply(c("z_within", "detrended", "anomaly"), function(v)
    dplyr::bind_rows(lapply(c("guild", "host", "guild_host"), function(g)
      group_permutation_test(pw, guild_lookup, g, version = v)))))),
  tar_target(perm_csv, tab(perm, "guild_permutation_test.csv"), format = "file"),
  tar_target(fig_cor_heatmap, save_fig(
    plot_matrix(clust$matrix, clust$order, guild_lookup, "Pearson r",
                "Cross-species correlation (z_within, pooled), clustered",
                paste("Average-linkage clustering on 1 - r (missing r set to 0 for clustering only). x = no interpretable r (n < 10 or no variation).",
                      "Guild/host:", unique(guild_lookup$lookup_source)), type = "r"),
    "outputs/figures/correlation_heatmap_clustered.png", 10, 10.5), format = "file"),
  tar_target(fig_n_heatmap, save_fig(
    plot_matrix(cor_matrix(pw, "z_within", "n")[clust$order, clust$order], clust$order, guild_lookup,
                "n co-observed\nregion-years", "Co-observed region-years per pair (same order)",
                "Grey = never co-observed.", type = "n"),
    "outputs/figures/n_coobserved_heatmap.png", 10, 10.5), format = "file")
)
