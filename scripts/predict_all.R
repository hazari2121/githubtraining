# Predictability of every insect from I. typographus (R/predict_all.R).
# Run from the project root after tar_make():  Rscript scripts/predict_all.R
suppressMessages({ library(targets); library(dplyr) })
for (f in c("R/helpers.R", "R/figures.R", "R/climate.R", "R/predict_all.R")) source(f)
pa <- predict_all(tar_read(cb_dmg), positions_it_fvg(tar_read(region_pos)), read.csv(region_climate_file),
                  tar_read(species_meta), tar_read(ph_hier))
dir.create("outputs/predict_all", showWarnings = FALSE)
write.csv(pa, "outputs/predict_all/PREDICT_from_ips_typographus_all_insects.csv", row.names = FALSE)
n <- sum(!is.na(pa$R2_tested))
ggplot2::ggsave("outputs/predict_all/predictability_from_ips_typographus.png", plot_predict_all(pa),
                width = 12, height = 2.6 + 0.24 * n, dpi = 200, bg = "white")
print(as.data.frame(select(pa, name, countries, regions, region_years, share_zero, R2_fit, R2_tested, r_tested, R2_tested_european_mean, worst_country)), digits = 2)
