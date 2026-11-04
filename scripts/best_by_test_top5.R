# Top five borers without dredge: every allowed equation scored by hiding NUTS
# regions, best one kept; compared with the other ways of choosing the equation.
#   Rscript scripts/best_by_test_top5.R
suppressMessages(library(dplyr))
source("R/dredge_ips.R")
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
dr <- read.csv("outputs/predict_all/DREDGE_top5_borers.csv")
fw <- read.csv("outputs/predict_all/PREDICT_top5_forward_summary.csv")
codes <- unique(dr$code)
bt <- bind_rows(lapply(codes, function(s) best_by_test(ry, s)))
write.csv(bt, "outputs/predict_all/BEST_BY_TEST_top5_borers.csv", row.names = FALSE)
pick <- function(r) dr |> filter(rank == r) |> select(code, eq = best_equation, nt = n_terms, reg = R2_tested_region, ctry = R2_tested_country)
cmp <- bind_rows(
  dr |> filter(rank == "AICc") |> transmute(code, way = "full equation (all 15 terms)", n_terms = 15, R2_tested_region = R2_tested_region_full),
  dr |> filter(rank == "AICc") |> transmute(code, way = "I. typographus alone", n_terms = 1, R2_tested_region = R2_tested_region_it_alone),
  fw |> transmute(code = species, way = "step by step (add a term only if it helps)", n_terms = lengths(strsplit(final_equation, "\\+")), R2_tested_region = R2_tested_final),
  pick("AICc") |> transmute(code, way = "dredge, AICc", n_terms = nt, R2_tested_region = reg),
  pick("BIC") |> transmute(code, way = "dredge, BIC", n_terms = nt, R2_tested_region = reg),
  bt |> transmute(code, way = "all 148 equations, best by hiding regions", n_terms, R2_tested_region)) |>
  left_join(distinct(dr, code, insect), by = "code")
write.csv(cmp, "outputs/predict_all/COMPARE_ways_top5_borers.csv", row.names = FALSE)
options(width = 200)
print(as.data.frame(bt |> select(code, best_equation, R2_tested_region, R2_tested_country)), digits = 2)
print(tidyr::pivot_wider(mutate(cmp, R2_tested_region = round(R2_tested_region, 2)), id_cols = way, names_from = insect, values_from = R2_tested_region))
fair <- bind_rows(lapply(codes, function(s) best_by_test_fair(ry, s)))
write.csv(fair, "outputs/predict_all/BEST_BY_TEST_FAIR_top5_borers.csv", row.names = FALSE)
cmp <- bind_rows(cmp, fair |> transmute(code, way = "all 148 equations, chosen without seeing the hidden region (fair)", n_terms = NA, R2_tested_region = R2_tested_region_fair) |>
                   left_join(distinct(dr, code, insect), by = "code"))
write.csv(cmp, "outputs/predict_all/COMPARE_ways_top5_borers.csv", row.names = FALSE)
print(as.data.frame(fair), digits = 2)
print(tidyr::pivot_wider(mutate(cmp, R2_tested_region = round(R2_tested_region, 2)), id_cols = way, names_from = insect, values_from = R2_tested_region))
