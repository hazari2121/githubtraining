# Predictability of every insect from I. typographus with MuMIn::dredge (R/dredge_ips.R)
# Run scripts/model_tables.R first.   Rscript scripts/dredge_ips.R
suppressMessages({ library(dplyr) })
for (f in c("R/helpers.R", "R/figures.R", "R/dredge_ips.R")) source(f)
set.seed(1)
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
ins <- read.csv("outputs/tables/MODEL_insects_with_ips_typographus.csv")
res <- dredge_all(ry, ins)
dir.create("outputs/predict_all", showWarnings = FALSE)
write.csv(res$summary, "outputs/predict_all/DREDGE_ips_typographus_summary.csv", row.names = FALSE)
write.csv(res$importance, "outputs/predict_all/DREDGE_ips_typographus_term_importance.csv", row.names = FALSE)
ggplot2::ggsave("outputs/predict_all/dredge_ips_typographus.png", plot_dredge(res$summary),
                width = 12.5, height = 3 + 0.3 * nrow(res$summary), dpi = 200, bg = "white")
print(as.data.frame(select(res$summary, insect, countries, regions, best_equation, R2_fit, R2_tested_region, R2_tested_country,
                           R2_tested_region_it_alone, R2_tested_region_shuffled_it, q_timing, meaningful)), digits = 2)
cat("\nmeaningful pairs:", sum(res$summary$meaningful), "of", nrow(res$summary), "\n")
