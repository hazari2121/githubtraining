#!/usr/bin/env Rscript
# Examples of tables built with make_table().
# Usage: Rscript examples/example_tables.R path/to/EUForDam.xlsx [output_dir]

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 2) args[2] else "output"

source(file.path("R", "eufordam_pipeline.R"))
source(file.path("R", "host_tables.R"))
source(file.path("R", "make_table.R"))

res <- run_pipeline(args[1], out_dir)
md  <- res$model_data          # one row per damage record
ih  <- res$host$insect_hosts   # one row per insect x host tree

tables <- list(
  # Same idea as the host tables: which insect lives on which tree
  borers_conifers_hosts = make_table(ih, rows = "host", cols = "insect",
                                     filter = guild == "borer" & host_type == "conifers"),
  # Where each insect is recorded
  insect_by_country     = make_table(md, rows = "insect", cols = "country", value = "n_regions"),
  # Did each insect cause damage in each country? (1 damage, 0 only zeros, blank no record)
  insect_damage_country = make_table(md, rows = "insect", cols = "country", value = "any_damage"),
  # Data coverage over time
  insect_by_year        = make_table(md, rows = c("guild", "insect"), cols = "year",
                                     value = "n_regions"),
  # Borer damage (m3) by country and year
  borer_m3_country_year = make_table(md, rows = "country", cols = "year", value = "sum",
                                     filter = guild == "borer"),
  # Defoliator damage (ha) by host tree and year
  defol_ha_host_year    = make_table(md, rows = "host", cols = "year", value = "sum",
                                     filter = guild == "defoliator"),
  # Total damage per insect, one column per unit
  insect_totals         = make_table(md, rows = c("guild", "insect"), cols = "unit",
                                     value = "sum")
)

writexl::write_xlsx(tables, file.path(out_dir, "example_tables.xlsx"))
cat("Wrote", length(tables), "example tables to", file.path(out_dir, "example_tables.xlsx"), "\n")
