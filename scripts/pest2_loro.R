# Leave one region out for a fitted pest 2 model (scripts/pest2_run.R and
# scripts/pest2_run_hurdle.R save the fits in _targets/pest2). Resumes after
# an interruption: each held-out region is saved as soon as it is done.
#   Rscript scripts/pest2_loro.R ips_typ pit_spp fir gaussian|hurdle
suppressMessages({ library(targets); library(dplyr); library(brms) })
source("R/pest2_model.R"); source("R/climate.R")
a <- commandArgs(trailingOnly = TRUE)
pest1 <- a[1]; pest2 <- a[2]; host_col <- a[3]; which_model <- a[4]
pair <- paste0(pest1, "_to_", pest2)
ddir <- file.path("data/processed/pest2", pair); mdir <- "_targets/pest2"
out <- file.path("outputs/pest2", pair); dir.create(out, recursive = TRUE, showWarnings = FALSE)
dat <- pest2_table(tar_read(cb_dmg), pest1, pest2, host_col, positions_it_fvg(tar_read(region_pos)), tar_read(hs_stock), pest2_read_climate())
train <- filter(pest2_prep(dat), known, !is.na(d2)) |> droplevels()
fitf <- function(s) readRDS(file.path(mdir, paste0(pest1, "_", pest2, "_", s, ".rds")))
lo <- if (which_model == "gaussian") {
  fit <- fitf("fit"); base <- fitf("base")
  pest2_loro_resume(levels(train$region), function(r) pest2_loro_one(r, train, fit, base), file.path(ddir, "loro"), "gaussian")
} else {
  fit <- fitf("hurdle")
  pest2_loro_resume(levels(train$region), function(r) pest2_loro_hurdle_one(r, train, fit), file.path(ddir, "loro"), "hurdle")
}
# scores over every model whose held-out regions are all done
all <- bind_rows(lapply(list.files(file.path(ddir, "loro"), full.names = TRUE), read.csv))
done <- all |> group_by(model) |> filter(n_distinct(region) == nlevels(train$region)) |> ungroup()
write.csv(pest2_scores(done), file.path(out, "loro_scores.csv"), row.names = FALSE)
write.csv(pest2_scores(done, by_region = TRUE), file.path(out, "loro_scores_by_region.csv"), row.names = FALSE)
print(pest2_scores(done))
