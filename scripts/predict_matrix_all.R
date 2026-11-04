# Every insect and group predicting every other one (R/predict_matrix.R)
#   Rscript scripts/predict_matrix_all.R
suppressMessages({ library(targets); library(dplyr) })
for (f in c("R/helpers.R", "R/figures.R", "R/level_pairs.R", "R/climate.R", "R/predict_all.R", "R/predict_matrix.R")) source(f)
pm <- predict_matrix_all(tar_read(cb_dmg), tar_read(ph_hier), positions_it_fvg(tar_read(region_pos)), read.csv(region_climate_file))
write.csv(pm, "outputs/predict_all/PREDICT_matrix_all_pairs.csv", row.names = FALSE)
ggplot2::ggsave("outputs/predict_all/predictability_matrix_all_pairs.png", plot_predict_matrix_all(pm, tar_read(ph_hier), tar_read(species_meta)),
                width = 14, height = 13, dpi = 200, bg = "white")
cat("combinations:", nrow(pm), " with enough overlap (>= 2 countries, 30 region-years):", sum(!is.na(pm$R2_tested)), "\n")
x <- filter(pm, !is.na(R2_tested))
print(table(cut(x$R2_tested, c(-Inf, 0, 0.1, 0.25, 0.5, Inf))))
