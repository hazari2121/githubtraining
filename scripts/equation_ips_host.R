# The equation with the host tree: Sx ~ IT x VPD x latitude x longitude + host (R/equation_ips.R)
#   Rscript scripts/equation_ips_host.R
suppressMessages(library(dplyr))
source("R/equation_ips.R")
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
reg <- read.csv("outputs/tables/MODEL_regions.csv")
ins <- read.csv("outputs/tables/MODEL_insects_with_ips_typographus.csv")
targets <- equation_targets("outputs/tables/OVERLAP_target_and_predictor_species.xlsx")
res <- equation_host_all(ry, reg, targets, ins)
write_equation_host_xlsx(res, "outputs/tables/EQUATION_with_host_tree.xlsx")
options(width = 220)
print(as.data.frame(res$summary |> transmute(target_name = substr(target_name, 1, 24), host = gsub("stock_m3_|area_ha_", "", host), regions,
  R2_without = round(R2_without_host, 2), R2_with = round(R2_with_host, 2), b_host = round(b_host, 2), p_host = signif(p_host, 2),
  test_reg_without = round(tested_region_without_host, 2), test_reg_with = round(tested_region_with_host, 2),
  test_ctry_without = round(tested_country_without_host, 2), test_ctry_with = round(tested_country_with_host, 2))))
