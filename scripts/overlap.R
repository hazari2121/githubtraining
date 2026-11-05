# Where target and predictor species overlap (R/overlap.R). Run scripts/model_tables.R first.
#   Rscript scripts/overlap.R
suppressMessages({ library(targets); library(dplyr) })
source("R/overlap.R")
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
pairs <- overlap_pairs(ry, tar_read(species_meta), tar_read(ph_hier))
write_overlap_xlsx(pairs, ry, tar_read(species_meta), tar_read(ph_hier), "outputs/tables/OVERLAP_target_and_predictor_species.xlsx")
cat(n_distinct(pairs$target), "insects;", nrow(pairs), "target x predictor pairs;",
    sum(pairs$meets_3_countries), "pairs overlap in >= 3 countries,", sum(pairs$meets_2_countries), "in >= 2\n")
ips <- filter(pairs, predictor == "ips_typ")
cat("I. typographus as predictor:", sum(ips$meets_3_countries), "targets in >= 3 countries,", sum(ips$meets_2_countries), "in >= 2\n")
best <- pairs |> filter(meets_3_countries) |> count(predictor_name, sort = TRUE) |> head(8); print(best)
