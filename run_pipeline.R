#!/usr/bin/env Rscript
# Usage: Rscript run_pipeline.R path/to/EUForDam.xlsx [output_dir] [host_climate.xlsx]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript run_pipeline.R <input.xlsx> [output_dir]")
input   <- args[1]
out_dir <- if (length(args) >= 2) args[2] else "output"

invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))  # UTF-8 text (e.g. Polish names) everywhere
source(file.path("R", "eufordam_pipeline.R"))
source(file.path("R", "host_tables.R"))
source(file.path("R", "host_climate.R"))
source(file.path("R", "datasheet.R"))
source(file.path("R", "pivot_workbook.R"))
source(file.path("R", "query_sheet.R"))
source(file.path("R", "pairs.R"))
source(file.path("R", "web_data.R"))
hc_path <- if (length(args) >= 3) args[3] else NULL
source(file.path("R", "request.R"))
res <- run_pipeline(input, out_dir, host_climate_path = hc_path)

cat("\nEU-ForDAM pipeline finished. Outputs in:", normalizePath(out_dir), "\n\n")
print(res$qc$overview, n = Inf)
cat("\nEurope-wide totals by group:\n")
print(res$summaries$group_totals, n = Inf)

# Answer the requests in requests.xlsx (if present)
if (file.exists("requests.xlsx")) {
  source(file.path("R", "request.R"))
  req <- run_requests("requests.xlsx", file.path(out_dir, "eufordam_model_data.csv"),
                      file.path(out_dir, "requests_output.xlsx"),
                      hc_path = file.path(out_dir, "spatial_units_host_climate.csv"))
  cat("\nRequests answered:", sum(req$index$status == "ok"), "of", nrow(req$index),
      "->", file.path(out_dir, "requests_output.xlsx"), "\n")
}
