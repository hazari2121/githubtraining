# targets pipeline: cross-species correlation analysis of EU-ForDAM insect damage.
# Run with targets::tar_make(); render the report with quarto render reports/report.qmd
# No gap-filling, imputation or forecasting happens in this pipeline.

library(targets)

tar_option_set(packages = c("dplyr", "tidyr", "readr", "readxl", "stringr", "tibble",
                            "purrr", "ggplot2"))
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

  # ---- Prepare: CZ sub-rows summed, value1 only ------------------------------
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

  # ---- Step 3: transformed versions ----------------------------------------
  tar_target(tr, make_transforms(cells)),
  tar_target(tr_sens, make_transforms(cells, drop_suspect_zeros = TRUE)),
  tar_target(tr_csv, write_csv_out(tr, "data/processed/transformed_observed.csv"), format = "file"),
  tar_target(versions_csv, tab(transform_versions, "transform_versions.csv"), format = "file"),
  tar_target(all_species, sort(unique(cells$species))),

  # ---- Step 4: pairwise correlations ---------------------------------------
  tar_target(pw, pairwise_correlations(tr, all_species)),
  tar_target(pw_sens, pairwise_correlations(tr_sens, all_species, versions = c("z_within", "detrended"))),
  tar_target(pw_csv, tab(pw, "pairwise_correlations.csv"), format = "file"),
  tar_target(pw_sens_csv, tab(pw_sens, "pairwise_correlations_sensitivity_no_suspect_zeros.csv"), format = "file"),

  # ---- Step 5: temporal vs spatial -----------------------------------------
  tar_target(sp_cor, spatial_correlations(tr, all_species)),
  tar_target(tvs, temporal_vs_spatial(pw, sp_cor)),
  tar_target(tvs_csv, tab(tvs, "temporal_vs_spatial.csv"), format = "file"),

  # ---- Step 6: transferability ---------------------------------------------
  tar_target(pc, per_country_correlations(tr, all_species)),
  tar_target(loco, dplyr::bind_rows(loco_correlations(tr, all_species, "z_within"),
                                    loco_correlations(tr, all_species, "detrended"))),
  tar_target(ts, dplyr::bind_rows(transferability_summary(pw, pc, loco, "z_within"),
                                  transferability_summary(pw, pc, loco, "detrended"))),
  tar_target(pc_csv, tab(pc, "per_country_correlations.csv"), format = "file"),
  tar_target(loco_csv, tab(loco, "leave_one_country_out.csv"), format = "file"),
  tar_target(ts_csv, tab(ts, "transferability_summary.csv"), format = "file"),
  tar_target(fig_top_pairs,
             save_fig(plot_top_pairs(dplyr::filter(ts, version == "z_within"), pc),
                      "outputs/figures/top_pairs_by_country.png", 9, 7), format = "file"),

  # ---- Step 7: guild structure ---------------------------------------------
  tar_target(guild_template_csv,
             write_csv_out(guild_template(species_meta), "data/lookup/species_guild_TEMPLATE.csv"),
             format = "file"),
  tar_target(guild_lookup, load_guild_lookup("data/lookup/species_guild.csv", species_meta) |>
               dplyr::mutate(guild_host = dplyr::if_else(is.na(guild) | is.na(host), NA_character_,
                                                         paste(guild, host, sep = " / "))),
             cue = tar_cue(mode = "always")),
  tar_target(clust, cluster_species(pw, "z_within")),
  tar_target(perm, dplyr::bind_rows(lapply(c("z_within", "detrended"), function(v)
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
