# Tables from the whiteboard, from our data (R/whiteboard_tables.R).
# Run scripts/model_tables.R first.   Rscript scripts/whiteboard_tables.R
suppressMessages({ library(targets); library(dplyr) })
source("R/whiteboard_tables.R")
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
reg <- read.csv("outputs/tables/MODEL_regions.csv")
ins <- read.csv("outputs/tables/MODEL_insects_with_ips_typographus.csv")
meta <- tar_read(species_meta); ph <- tar_read(ph_hier); nm <- setNames(ph$name, ph$species)
codes <- wb_codes(ry, meta)
codes <- codes[order(codes != "ips_typ", meta$guild_from_file[match(codes, meta$species)] != "borer")]
pairs <- wb_pairs(ry, meta, nm)
host_info <- wb_host_blocks(ry, meta, nm, ins, wb_host_of_code())$info
pairs <- wb_add_shared_host(pairs, host_info)
gaps <- wb_gaps(ry, meta, nm)
host <- wb_host_matrix(ins[ins$code %in% codes, ], nm)
grp <- wb_group_r2(tar_read(cb_dmg), ph)
grp_m <- tidyr::pivot_wider(mutate(grp, R2_ups_downs = round(R2_ups_downs, 2)), id_cols = group_a, names_from = group_b, values_from = R2_ups_downs)
train <- wb_training(ry, reg, ins, meta, nm)
how <- data.frame(`How to read` = c(
  "Tables from the whiteboard, built from EU-ForDAM data (value1, 2000-2022). Overlap = same region and same year, both recorded (0 counts as recorded).",
  "pairs: every target x predictor pair. coverage_pct = % of the target's regions where the predictor is also recorded.",
  "  target_host / predictor_host: host trees (as in HOST_tables); shared_host = TRUE when they live on a common tree (shared_host_trees); same_guild = both borers or both defoliators.",
  "  R2_levels: squared correlation of log(damage + 1) over the overlapping region-years (how much damage, region against region).",
  "  R2_ups_downs: the same after standardising each region's series (do bad years of the two coincide?). Empty: fewer than 10 usable region-years.",
  "coverage_matrix, r2_levels_matrix, r2_ups_downs_matrix: the same numbers as grids, target in rows, predictor in columns.",
  "gaps_by_region: for each target, every region where it is NOT recorded, with the predictors that ARE recorded there (most years first).",
  "gaps_count: target x region, number of predictors recorded where the target is missing (empty = the target is recorded there).",
  "host_matrix: 1 = host tree of the insect (supervisor's host matrix where it names the insect, otherwise the host in the data file).",
  "group_r2: R2 of ups and downs between the 2025 paper's groups (borers on fir, spruce, pine; defoliators on spruce, pine).",
  "training: one row per target x region x year where the target is recorded, with the equation's inputs: target and I. typographus damage (empty = not recorded),",
  "  VPD, temperature, precipitation (ERA5-Land 2000-2022), latitude, longitude, host amount (m3 for borers, ha for defoliators), forest area.",
  "Aggregate group codes (e.g. CambXyl_con&MD) are left out: they are not species."), check.names = FALSE)
nonpriv <- list(`how to read` = how[!grepl('^training|^  VPD, temperature', how[[1]]), , drop = FALSE], pairs = pairs,
                coverage_matrix = wb_matrix(pairs, "coverage_pct", codes, 0), r2_levels_matrix = wb_matrix(pairs, "R2_levels", codes),
                r2_ups_downs_matrix = wb_matrix(pairs, "R2_ups_downs", codes), gaps_by_region = gaps,
                gaps_count = wb_gaps_count(gaps, ry, codes), host_matrix = host, group_r2 = grp_m)
writexl::write_xlsx(nonpriv, "outputs/tables/WHITEBOARD_tables.xlsx")
writexl::write_xlsx(c(nonpriv[1], list(training = train), nonpriv[-1]), "data/processed/WHITEBOARD_tables_with_training_data.xlsx")
write.csv(train, "data/processed/WHITEBOARD_training_data.csv", row.names = FALSE)
cat("pairs", nrow(pairs), "| gaps rows", nrow(gaps), "| training rows", nrow(train), "targets", n_distinct(train$target), "\n")
cat("pairs with a shared host:", sum(pairs$shared_host), "of", nrow(pairs), "| with I. typographus:", sum(pairs$shared_host & pairs$predictor == "ips_typ"), "\n")
print(as.data.frame(pairs |> filter(predictor == "ips_typ", countries >= 3) |> arrange(desc(R2_ups_downs)) |>
  transmute(target_name = substr(target_name, 1, 26), shared_host, countries, regions, coverage = round(coverage_pct), R2_levels = round(R2_levels, 2), R2_ups_downs = round(R2_ups_downs, 2)) |> head(10)))
print(as.data.frame(grp_m))
