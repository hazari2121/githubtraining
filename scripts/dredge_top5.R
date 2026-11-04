# Top five borers predicted from I. typographus with dredge (AICc and BIC), from
# the input table data/processed/model_region_year.csv (scripts/model_tables.R)
#   Rscript scripts/dredge_top5.R
suppressMessages(library(dplyr))
source("R/dredge_ips.R")
set.seed(1)
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
ins <- read.csv("outputs/tables/MODEL_insects_with_ips_typographus.csv")
sm <- dredge_top_borers(ry, ins)
write.csv(sm, "outputs/predict_all/DREDGE_top5_borers.csv", row.names = FALSE)
options(width = 250)
print(data.frame(insect = substr(sm$insect, 1, 26), rank = sm$rank, countries = sm$countries, regions = sm$regions,
                 equation = sm$best_equation, R2_fit = round(sm$R2_fit, 2), hide_region = round(sm$R2_tested_region, 2),
                 hide_country = round(sm$R2_tested_country, 2), ips_alone = round(sm$R2_tested_region_it_alone, 2),
                 shuffled = round(sm$R2_tested_region_shuffled_it, 2), q = round(sm$q_timing, 3), meaningful = sm$meaningful))
