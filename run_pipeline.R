#!/usr/bin/env Rscript
# Usage: Rscript run_pipeline.R path/to/EUForDam.xlsx [output_dir]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript run_pipeline.R <input.xlsx> [output_dir]")
input   <- args[1]
out_dir <- if (length(args) >= 2) args[2] else "output"

source(file.path("R", "eufordam_pipeline.R"))
res <- run_pipeline(input, out_dir)

cat("\nEU-ForDAM pipeline finished. Outputs in:", normalizePath(out_dir), "\n\n")
print(res$qc$overview, n = Inf)
cat("\nEurope-wide totals by group:\n")
print(res$summaries$group_totals, n = Inf)
