# make_table(): build any rows x columns table from a tidy EU-ForDAM table.
#
# Base tables (created by run_pipeline()):
#   model_data    one row per damage record (country, nuts, year, insect, guild, host,
#                 host_type, unit, value, ...). Use it for damage amounts and coverage.
#   insect_hosts  one row per insect x host tree, including hosts the supervisor's
#                 matrix adds. Use it for "which insect lives on which tree".
#
# Examples:
#   make_table(insect_hosts, rows = "host", cols = "insect", value = "presence",
#              filter = guild == "borer" & host_type == "conifers")
#   make_table(model_data, rows = "insect", cols = "country", value = "n_records")
#   make_table(model_data, rows = "country", cols = "year", value = "sum",
#              filter = guild == "borer")
#
# value:
#   "presence"    1 if at least one row exists, else 0 (a 0 here means "no record",
#                 not "no damage")
#   "any_damage"  1 if any value > 0, 0 if only zeros recorded, blank if no record
#   "sum"         total damage (only within one unit: filter or split by unit)
#   "mean"        mean damage per record
#   "n_records"   number of records
#   "n_regions"   number of distinct regions (nuts)
#   "n_countries" number of distinct countries
#   "n_years"     number of distinct years
#
# Cells with no record stay blank (NA) for every value except "presence", so missing
# is never turned into zero damage.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

make_table <- function(data, rows, cols = NULL, value = "presence", filter = NULL,
                       include_aggregates = FALSE) {
  value <- match.arg(value, c("presence", "any_damage", "sum", "mean", "n_records",
                              "n_regions", "n_countries", "n_years"))
  f <- substitute(filter)
  if (!is.null(f)) data <- dplyr::filter(data, !!f)
  if ("is_aggregate" %in% names(data) && !include_aggregates)
    data <- dplyr::filter(data, !is_aggregate)

  stat <- value  # "value" is also a data column, so keep the choice under another name
  keys <- c(rows, cols)
  if (stat %in% c("sum", "mean") && "unit" %in% names(data) &&
      !"unit" %in% keys && n_distinct(data$unit) > 1)
    stop("Damage is in more than one unit (m3 and ha). Add a filter such as ",
         "unit == \"m3\", or put \"unit\" in rows or cols.")

  long <- data |>
    group_by(across(all_of(keys))) |>
    summarise(.v = switch(.env$stat,
      presence    = 1,
      any_damage  = as.numeric(any(value > 0)),
      sum         = sum(value),
      mean        = mean(value),
      n_records   = n(),
      n_regions   = n_distinct(nuts),
      n_countries = n_distinct(country),
      n_years     = n_distinct(year)
    ), .groups = "drop")

  if (is.null(cols)) return(rename(long, !!stat := .v))

  fill <- if (stat == "presence") list(.v = 0) else list()
  long |>
    pivot_wider(names_from = all_of(cols), values_from = .v, values_fill = fill,
                names_sort = TRUE) |>
    arrange(across(all_of(rows)))
}
