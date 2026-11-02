# Pest 2 from pest 1: the PROLES methods note run on EU-ForDAM data
# (functions in R/pest2_model.R). Run from the project root after tar_make():
#   Rscript scripts/pest2_run.R ips_typ pit_spp fir
# Arguments: pest 1 code, pest 2 code, pest 2 host column of the forest appendix.
# Needs data/external/region_temperature.csv (nuts_id, temp).
# Outputs in outputs/pest2/<pest1>_to_<pest2>/; fitted models in _targets/pest2/ (not committed).

suppressMessages({ library(targets); library(dplyr); library(brms) })
source("R/pest2_model.R")
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 3) a <- c("ips_typ", "pit_spp", "fir")
pest1 <- a[1]; pest2 <- a[2]; host_col <- a[3]
loro_on <- !"--no-loro" %in% a

out <- file.path("outputs/pest2", paste0(pest1, "_to_", pest2)); dir.create(out, recursive = TRUE, showWarnings = FALSE)
mdir <- "_targets/pest2"; dir.create(mdir, recursive = TRUE, showWarnings = FALSE)
ph <- tar_read(ph_hier); nm <- setNames(ph$name, ph$species)
unit <- tar_read(cb_dmg) |> filter(species == pest2) |> pull(unit) |> first()

# 1-2. Data table and modelling table
dat <- pest2_table(tar_read(cb_dmg), pest1, pest2, host_col, tar_read(region_pos), tar_read(hs_stock), pest2_read_temperature())
mod_dat <- pest2_prep(dat)
train <- filter(mod_dat, known, !is.na(ld2)) |> droplevels()
newreg <- filter(mod_dat, !known) |> droplevels()
write.csv(dat, file.path(out, "input_table.csv"), row.names = FALSE)
cat(sprintf("%s -> %s: %d complete regions (%d rows, %.0f%% zeros), %d prediction regions\n",
            pest1, pest2, n_distinct(train$region), nrow(train), 100 * mean(train$d2 == 0), n_distinct(newreg$region)))

# 3. The model
fit <- pest2_fit(train, file = file.path(mdir, paste0(pest1, "_", pest2, "_fit")))
base <- pest2_fit_base(train, file = file.path(mdir, paste0(pest1, "_", pest2, "_base")))
sink(file.path(out, "model_summary.txt")); print(summary(fit))
cat("\nDivergent transitions:", sum(subset(nuts_params(fit), Parameter == "divergent__")$Value), "\n")
cat("Max R-hat:", round(max(rhat(fit), na.rm = TRUE), 3), "\n"); sink()
ggplot2::ggsave(file.path(out, "pp_check.png"), pp_check(fit, ndraws = 50) + theme_proles(10), width = 7, height = 4.5, dpi = 200, bg = proles[["briza"]])

# 4. Leave one region out: hierarchical model against the two baselines
if (loro_on) {
  lo <- bind_rows(lapply(levels(train$region), function(r) { cat("LORO", r, "\n"); pest2_loro_one(r, train, fit, base) }))
  write.csv(lo, file.path(out, "loro_predictions.csv"), row.names = FALSE)
  write.csv(pest2_scores(lo), file.path(out, "loro_scores.csv"), row.names = FALSE)
  write.csv(pest2_scores(lo, by_region = TRUE), file.path(out, "loro_scores_by_region.csv"), row.names = FALSE)
  print(pest2_scores(lo))
  ggplot2::ggsave(file.path(out, "loro_series.png"),
                  plot_pest2_loro(lo, paste("Leave one region out:", nm[[pest2]], "predicted from", nm[[pest1]])),
                  width = 13, height = 2 + 1.7 * ceiling(n_distinct(lo$region) / 6), dpi = 200, bg = proles[["briza"]])
}

# 5. Predict the prediction regions
pred_new <- pest2_predict_new(fit, newreg)
write.csv(pred_new, file.path(out, "predictions_new_regions.csv"), row.names = FALSE)

# 6. Plots
ggplot2::ggsave(file.path(out, "predictions_new_regions.png"),
                plot_pest2_predictions(pred_new, paste("Predicted", nm[[pest2]], "damage in regions without its records"), unit),
                width = 13, height = 2 + 1.7 * ceiling(n_distinct(pred_new$region) / 6), dpi = 200, bg = proles[["briza"]])
ggplot2::ggsave(file.path(out, "slope_deviation_map.png"), plot_pest2_slope_map(fit, train),
                width = 7, height = 6, dpi = 200, bg = proles[["briza"]])
