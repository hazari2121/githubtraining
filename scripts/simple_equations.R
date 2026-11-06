# Simpler equations against the full equation (R/equation_ips.R)   Rscript scripts/simple_equations.R
suppressMessages(library(dplyr))
source("R/equation_ips.R")
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
reg <- read.csv("outputs/tables/MODEL_regions.csv")
ins <- read.csv("outputs/tables/MODEL_insects_with_ips_typographus.csv")
targets <- equation_targets("outputs/tables/OVERLAP_target_and_predictor_species.xlsx")
se <- simple_equations(ry, reg, targets, ins)
bw <- readxl::read_excel("outputs/tables/PREDICT_better_ways.xlsx", "results") |> select(target, region_mixed, country_mixed)
se <- left_join(se, bw, by = "target") |> arrange(desc(`region_IT + host + VPD + IT x VPD`))
how <- data.frame(`How to read` = c(
  "Each target predicted from I. typographus with simple equations (least squares); score = tested R-squared when each NUTS region (region_*) or country (country_*) is hidden and predicted from the others.",
  "1 = perfect, 0 = no better than the overall mean, below 0 = worse.",
  "IT alone: Sx ~ IT. IT + host + VPD: three terms. IT + host + VPD + IT x VPD: adds 'I. typographus matters more (or less) in dry regions'. host + VPD (no IT): the same without I. typographus.",
  "region_full_equation: Sx ~ IT x VPD x latitude x longitude + host (17 terms). region_mixed / country_mixed: mixed model with year and region effects (PREDICT_better_ways.xlsx).",
  "b_* and p_*: effects in the equation IT + host + VPD + IT x VPD fitted on all data (p-values too small: the same region appears in many years).",
  "log(damage + 1); VPD (ERA5-Land 2000-2022) and host (log host amount, supervisor's host matrix) standardised."), check.names = FALSE)
writexl::write_xlsx(list(`how to read` = how, results = se), "outputs/tables/PREDICT_simple_equations.xlsx")
options(width = 230)
print(as.data.frame(se |> transmute(target_name = substr(target_name, 1, 24), regions, full = round(region_full_equation, 2),
  it_alone = round(`region_IT alone`, 2), it_host_vpd = round(`region_IT + host + VPD`, 2), plus_itxvpd = round(`region_IT + host + VPD + IT x VPD`, 2),
  no_it = round(`region_host + VPD (no IT)`, 2), mixed = round(region_mixed, 2),
  c_it_host_vpd = round(`country_IT + host + VPD`, 2), c_plus = round(`country_IT + host + VPD + IT x VPD`, 2), c_mixed = round(country_mixed, 2),
  b_itxvpd = round(b_it_x_vpd, 2), p_itxvpd = signif(p_it_x_vpd, 2))))
