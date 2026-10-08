# targets pipeline: cross-species correlation analysis of EU-ForDAM insect damage.
# Run with targets::tar_make(). Only Step 1 (inspection) is implemented so far;
# later steps are added once the open questions on data coding are answered.

library(targets)

tar_option_set(packages = c("dplyr", "tidyr", "readr", "readxl", "stringr", "tibble"))
tar_source("R")

list(
  # Raw workbook: tracked as a file so the pipeline reruns if it ever changes.
  tar_target(raw_file, "data/raw/EUForDam_REW3.xlsx", format = "file"),
  tar_target(raw, read_raw(raw_file)),
  tar_target(long, to_long(raw)),

  tar_target(counts, summarise_counts(long)),
  tar_target(coverage, country_species_coverage(long)),
  tar_target(sp_lookup, species_lookup_raw(long)),

  tar_target(long_csv, write_csv_out(long, "data/processed/eufordam_long_raw.csv"), format = "file"),
  tar_target(counts_csv, write_csv_out(counts, "outputs/tables/step1_counts.csv"), format = "file"),
  tar_target(coverage_csv, write_csv_out(coverage, "outputs/tables/step1_country_species_coverage.csv"), format = "file"),
  tar_target(sp_lookup_csv, write_csv_out(sp_lookup, "outputs/tables/step1_species_in_file.csv"), format = "file")
)
