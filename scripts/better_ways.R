# Mixed model and lasso against the full equation and the European mean (R/equation_ips.R)
#   Rscript scripts/better_ways.R
suppressMessages(library(dplyr))
for (f in c("R/helpers.R", "R/figures.R", "R/equation_ips.R")) source(f)
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
reg <- read.csv("outputs/tables/MODEL_regions.csv")
ins <- read.csv("outputs/tables/MODEL_insects_with_ips_typographus.csv")
targets <- equation_targets("outputs/tables/OVERLAP_target_and_predictor_species.xlsx")
bw <- better_ways(ry, reg, targets, ins)
how <- data.frame(`How to read` = c(
  "Each target insect predicted from I. typographus four ways; score = tested R-squared when each NUTS region (region_*) or each country (country_*) is hidden and predicted from the others.",
  "1 = perfect, 0 = no better than the overall mean, below 0 = worse than the overall mean.",
  "full_equation: Sx ~ IT x VPD x latitude x longitude + host (least squares, all 17 terms).",
  "mixed: Sx ~ IT + host + VPD + latitude + longitude + year effect + region effect (lme4). A hidden region gets the year effects learned from the other regions.",
  "lasso: the terms of the full equation with host, unhelpful ones shrunk to zero (glmnet), strength chosen by hiding regions inside the training data only. lasso_terms_kept: terms kept when fitted on all data.",
  "european_mean: baseline, the mean of the other regions in the same year.",
  "region_mixed_without_it: the mixed model without I. typographus; region_mixed minus this = what I. typographus adds.",
  "mixed_b_it / mixed_t_it, mixed_b_host / mixed_t_host: effect and t value of I. typographus and host in the mixed model (all data).",
  "log(damage + 1); VPD, latitude, longitude, host standardised. Regions without forest data left out."), check.names = FALSE)
writexl::write_xlsx(list(`how to read` = how, results = bw), "outputs/tables/PREDICT_better_ways.xlsx")
ggplot2::ggsave("outputs/predict_all/better_ways.png", plot_better_ways(bw), width = 12.5, height = 3.4 + 0.3 * nrow(bw), dpi = 200, bg = "white")
options(width = 220)
print(as.data.frame(bw |> transmute(target_name = substr(target_name, 1, 24), regions,
  full = round(region_full_equation, 2), mixed = round(region_mixed, 2), mixed_no_it = round(region_mixed_without_it, 2), lasso = round(region_lasso, 2), eu_mean = round(region_european_mean, 2),
  c_full = round(country_full_equation, 2), c_mixed = round(country_mixed, 2), c_lasso = round(country_lasso, 2), c_eu = round(country_european_mean, 2),
  t_it = round(mixed_t_it, 1), t_host = round(mixed_t_host, 1))))
