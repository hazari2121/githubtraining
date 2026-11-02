# Pest 2 from pest 1, Component 6 of the PROLES note: the hurdle-Gamma version
# for damage with exact zeros (R/pest2_model.R). Run after scripts/pest2_run.R
# for the same pair (its baselines are reused for the comparison):
#   Rscript scripts/pest2_run_hurdle.R ips_typ pit_spp fir

suppressMessages({ library(targets); library(dplyr); library(brms) })
source("R/pest2_model.R"); source("R/climate.R")
a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 3) a <- c("ips_typ", "pit_spp", "fir")
pest1 <- a[1]; pest2 <- a[2]; host_col <- a[3]
loro_on <- !"--no-loro" %in% a
pair <- paste0(pest1, "_to_", pest2)
out <- file.path("outputs/pest2", pair, "hurdle"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
ddir <- file.path("data/processed/pest2", pair); dir.create(ddir, recursive = TRUE, showWarnings = FALSE)
mdir <- "_targets/pest2"; dir.create(mdir, recursive = TRUE, showWarnings = FALSE)
ph <- tar_read(ph_hier); nm <- setNames(ph$name, ph$species)
unit <- tar_read(cb_dmg) |> filter(species == pest2) |> pull(unit) |> first()

dat <- pest2_table(tar_read(cb_dmg), pest1, pest2, host_col, positions_it_fvg(tar_read(region_pos)), tar_read(hs_stock), pest2_read_climate())
mod_dat <- pest2_prep(dat)
train <- filter(mod_dat, known, !is.na(d2)) |> droplevels()
newreg <- filter(mod_dat, !known) |> droplevels()

fit <- pest2_fit_hurdle(train, file = file.path(mdir, paste0(pest1, "_", pest2, "_hurdle")))
sink(file.path(out, "model_summary.txt")); print(summary(fit))
cat("\nDivergent transitions:", sum(subset(nuts_params(fit), Parameter == "divergent__")$Value), "\n")
cat("Max R-hat:", round(max(rhat(fit), na.rm = TRUE), 3), "\n"); sink()
# pp_check on log(damage + 1), the same scale as the Gaussian model's check
yrep <- log1p(posterior_predict(fit, ndraws = 50))
ggplot2::ggsave(file.path(out, "pp_check.png"), bayesplot::ppc_dens_overlay(log1p(train$d2), yrep) + theme_proles(10) +
                  ggplot2::labs(x = "log(damage + 1)", title = "Posterior predictive check, hurdle-Gamma"),
                width = 7, height = 4.5, dpi = 200, bg = proles[["briza"]])
cat(sprintf("zero years: observed %.2f, model %.2f\n", mean(train$d2 == 0), mean(yrep == 0)))

pred_new <- pest2_predict_new(fit, newreg, log_scale = FALSE)
write.csv(pred_new, file.path(ddir, "predictions_new_regions_hurdle.csv"), row.names = FALSE)
ggplot2::ggsave(file.path(out, "predictions_new_regions.png"),
                plot_pest2_predictions(pred_new, paste("Predicted", nm[[pest2]], "damage in regions without its records (hurdle-Gamma)"), unit),
                width = 13, height = 2 + 1.7 * ceiling(n_distinct(pred_new$region[pred_new$host > 0]) / 6), dpi = 200, bg = proles[["briza"]])

if (loro_on) {
  lo <- bind_rows(lapply(levels(train$region), function(r) { cat("LORO hurdle", r, "\n"); pest2_loro_hurdle_one(r, train, fit) }))
  write.csv(lo, file.path(ddir, "loro_predictions_hurdle.csv"), row.names = FALSE)
  prev <- c(file.path(ddir, "loro_predictions.csv"), file.path("outputs/pest2", pair, "loro_predictions.csv"))
  prev <- prev[file.exists(prev)][1]
  all <- if (!is.na(prev)) bind_rows(read.csv(prev), lo) else lo
  write.csv(pest2_scores(all), file.path(out, "loro_scores_all_models.csv"), row.names = FALSE)
  write.csv(pest2_scores(all, by_region = TRUE), file.path(out, "loro_scores_by_region_all_models.csv"), row.names = FALSE)
  print(pest2_scores(all))
}
