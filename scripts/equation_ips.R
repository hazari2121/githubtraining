# The equation Sx ~ IT x VPD x latitude x longitude for every target that
# overlaps with I. typographus in at least 3 countries (R/equation_ips.R).
# Run scripts/model_tables.R and scripts/overlap.R first.   Rscript scripts/equation_ips.R
suppressMessages(library(dplyr))
source("R/equation_ips.R")
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
targets <- equation_targets("outputs/tables/OVERLAP_target_and_predictor_species.xlsx")
res <- equation_all(ry, targets)
write_equation_xlsx(res, "outputs/tables/EQUATION_ips_typographus_x_vpd_x_lat_x_lon.xlsx")
options(width = 200)
print(as.data.frame(res$summary |> transmute(target_name = substr(target_name, 1, 26), countries, regions, region_years,
  R2 = round(R2, 2), R2_adj = round(R2_adjusted, 2), p_equation = signif(p_equation, 2),
  tested_region = round(R2_tested_region, 2), tested_country = round(R2_tested_country, 2))))
it <- res$coefficients |> filter(term == "it") |> transmute(target_name = substr(target_name, 1, 26), b_it = round(estimate, 3), p_it = signif(p, 2))
print(as.data.frame(it))
