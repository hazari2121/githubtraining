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
  tar_target(cells_csv, write_csv_out(cells, "data/processed/cells_classified.csv"), format = "file"),
  tar_target(cov_species, coverage_summary(cells, "species")),
  tar_target(cov_country, coverage_summary(cells, "country")),
  tar_target(cov_cs, coverage_country_species(cells)),
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
                                          "outputs/figures/species_occupancy_monitored_vs_occurring.png", 12, 9.5),
             format = "file"),
  tar_target(fig_occ_map, save_fig(plot_occurrence_map(rs_summary, sp_rank),
                                   "outputs/figures/occurrence_map_region_species.png", 11, 14), format = "file"),
  tar_target(ranking_xlsx, write_ranking_xlsx(sp_rank, reg_rank, occ_matrix,
                                              "outputs/tables/species_nuts_occurrence_ranking.xlsx"),
             format = "file"),

  # ---- Step 3: transformed versions ----------------------------------------
  # Main: series with >= 6 non-zero values (Hlasny et al. 2025). Sensitivity: no filter.
  tar_target(tr, make_transforms(cells, min_nonzero = 6)),
  tar_target(tr_nofilter, make_transforms(cells, min_nonzero = 0)),
  tar_target(tr_csv, write_csv_out(tr, "data/processed/transformed_observed.csv"), format = "file"),
  tar_target(versions_csv, tab(transform_versions, "transform_versions.csv"), format = "file"),
  tar_target(all_species, sort(unique(cells$species))),

  # ---- Step 4: pairwise correlations ---------------------------------------
  tar_target(pw, pairwise_correlations(tr, all_species)),
  tar_target(pw_nofilter, pairwise_correlations(tr_nofilter, all_species, versions = c("z_within", "anomaly"))),
  tar_target(pw_csv, tab(pw, "pairwise_correlations.csv"), format = "file"),
  tar_target(pw_nofilter_csv, tab(pw_nofilter, "pairwise_correlations_sensitivity_no_series_filter.csv"), format = "file"),

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
  tar_target(cm, correlation_metrics(tr, tr_nofilter, all_species, ts)),
  tar_target(cm_species, species_information(cm, species_meta)),
  tar_target(cm_lags, lag_pairs(cm)),
  tar_target(cm_csv, tab(cm, "correlation_metrics_all_pairs.csv"), format = "file"),
  tar_target(fig_cm_top, save_fig(plot_cm_top(cm), "outputs/figures/cm_top_links.png", 9, 8.5), format = "file"),
  tar_target(fig_cm_species, save_fig(plot_cm_species(cm_species), "outputs/figures/cm_links_per_species.png", 8.5, 9.5),
             format = "file"),
  tar_target(fig_cm_lags, save_fig(plot_cm_lags(cm_lags), "outputs/figures/cm_lead_effects.png", 9, 7), format = "file"),
  tar_target(cm_species_csv, tab(cm_species, "correlation_metrics_by_species.csv"), format = "file"),
  tar_target(cm_lags_csv, tab(cm_lags, "correlation_metrics_lagged.csv"), format = "file"),

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
  tar_target(fig_lfm_comp, save_fig(plot_lfm_comparison(lfm_score, lfm_grid, lfm_best$model),
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
  # Sensitivity: suspicious zero runs treated as missing; same CV design
  tar_target(tr_zr, flag_zero_runs(tr)),
  tar_target(zero_runs_csv, tab(dplyr::filter(tr_zr, suspicious_zero_run) |>
                                  dplyr::count(country, nuts_id, species, name = "zero_years"),
                                "suspicious_zero_runs.csv"), format = "file"),
  tar_target(lfm_L_nz, build_lfm_matrix(cells, dplyr::filter(tr_zr, !suspicious_zero_run))),
  tar_target(lfm_cv_nz, lfm_cv(lfm_L_nz, make_cv_blocks(lfm_L_nz, n_folds = 10),
                               dplyr::filter(lfm_grid, model %in% c(lfm_best$model, "Factor model: K=0, lambda=20")))),
  tar_target(lfm_score_nz, lfm_scores(lfm_cv_nz)),
  tar_target(lfm_score_nz_csv, tab(lfm_score_nz, "lfm_cv_scores_sensitivity_zero_runs_missing.csv"), format = "file"),
  tar_target(lfm_score_csv, tab(lfm_score, "lfm_cv_scores_by_model.csv"), format = "file"),
  tar_target(lfm_score_sp_csv, tab(lfm_score_species, "lfm_cv_scores_by_species.csv"), format = "file"),
  tar_target(lfm_load_csv, tab(lfm_load, "lfm_factor_loadings.csv"), format = "file"),
  tar_target(lfm_filled_csv, write_csv_out(lfm_filled, "data/processed/lfm_gap_filled_PROTOTYPE.csv"), format = "file"),

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
