#!/usr/bin/env Rscript
# Fill one sheet per request in requests.xlsx.
# Usage: Rscript run_requests.R [requests.xlsx] [output_dir]
# Run run_pipeline.R first: requests are answered from output/eufordam_model_data.csv.
# If requests.xlsx does not exist, a template with example requests is created.

args <- commandArgs(trailingOnly = TRUE)
req_path <- if (length(args) >= 1) args[1] else "requests.xlsx"
out_dir  <- if (length(args) >= 2) args[2] else "output"

source(file.path("R", "eufordam_pipeline.R"))
source(file.path("R", "request.R"))

if (!file.exists(req_path)) {
  write_request_template(req_path)
  cat("Created", req_path, "with example requests. Edit it and run again.\n")
}
md_path <- file.path(out_dir, "eufordam_model_data.csv")
if (!file.exists(md_path)) stop("Run run_pipeline.R first (", md_path, " not found).")

res <- run_requests(req_path, md_path, file.path(out_dir, "requests_output.xlsx"),
                    hc_path = file.path(out_dir, "spatial_units_host_climate.csv"))
print(res$index[, c("sheet", "rows", "status")], n = Inf)
cat("\nResults:", file.path(out_dir, "requests_output.xlsx"), "\n")
