# Predictability matrix: predictor (I. typographus, borer groups) x target (R/predict_matrix.R)
#   Rscript scripts/predict_matrix.R
suppressMessages({ library(targets); library(dplyr) })
for (f in c("R/helpers.R", "R/figures.R", "R/level_pairs.R", "R/climate.R", "R/predict_all.R", "R/predict_matrix.R")) source(f)
pm <- predict_matrix(tar_read(cb_dmg), tar_read(ph_hier), positions_it_fvg(tar_read(region_pos)), read.csv(region_climate_file))
write.csv(pm, "outputs/predict_all/PREDICT_matrix.csv", row.names = FALSE)
n <- n_distinct(pm$target[!is.na(pm$R2_tested)])
ggplot2::ggsave("outputs/predict_all/predictability_matrix.png", plot_predict_matrix(pm, tar_read(ph_hier), tar_read(species_meta)),
                width = 10, height = 3 + 0.22 * n, dpi = 200, bg = "white")
print(as.data.frame(pm |> filter(!is.na(R2_tested)) |> arrange(desc(R2_tested)) |> select(predictor, target, countries, regions, r, R2_tested, equation) |> head(25)), digits = 2)
cat("cells:", sum(!is.na(pm$R2_tested)), "of", nrow(pm), "\n")
