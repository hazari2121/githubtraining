# Run with: Rscript tests/test_pipeline.R path/to/EUForDam.xlsx
suppressPackageStartupMessages(library(testthat))
source(file.path("R", "eufordam_pipeline.R"))

input <- commandArgs(trailingOnly = TRUE)[1]
raw     <- read_eufordam(input)
cleaned <- clean_eufordam(raw)
d       <- cleaned$data

test_that("zeros are kept as real zero damage", {
  expect_equal(sum(d$value == 0), sum(raw$value1 == 0, na.rm = TRUE))
})

test_that("blank value1 rows are dropped, never filled", {
  expect_false(anyNA(d$value))
  expect_equal(nrow(d) + nrow(cleaned$dropped), nrow(raw))
})

test_that("no rows are invented", {
  expect_lte(nrow(d), nrow(raw))
})

test_that("value_alt carries the opposite unit", {
  alt <- d |> filter(!is.na(value_alt))
  expect_true(all(alt$unit != alt$unit_alt))
})

test_that("region totals reproduce the Excel pivot for DE", {
  piv <- read_excel(input, sheet = "kotg_EUForDam_REW", col_names = FALSE,
                    .name_repair = "minimal")
  grand <- as.numeric(piv[[2]][piv[[1]] %in% "Grand Total"])
  de <- region_totals(cleaned) |> filter(country == "DE")
  expect_equal(sum(de$total), grand, tolerance = 1e-9)
  m3_con <- de |> filter(unit == "m3", tree_type == "con") |> pull(total) |> sum()
  expect_equal(m3_con, 355781107.19764453, tolerance = 1e-9)
})

# Optional second argument: the hand-made host tables workbook to compare against
source(file.path("R", "host_tables.R"))
ht <- build_host_tables(cleaned)
host_ref <- commandArgs(trailingOnly = TRUE)[2]

test_that("host matrices contain only 0/1 and every insect has a host", {
  for (m in ht$matrices) {
    vals <- unlist(m[-1])
    expect_true(all(vals %in% c(0, 1)))
    expect_true(all(colSums(m[-1]) >= 1))
  }
})

test_that("aggregate codes are left out of the insect table", {
  expect_false(any(is_aggregate_code(ht$insects$code)))
})

if (!is.na(host_ref)) test_that("host tables reproduce the hand-made workbook", {
  for (s in c(names(ht$matrices), "insects")) {
    a <- if (s == "insects") ht$insects else ht$matrices[[s]]
    b <- read_excel(host_ref, sheet = s)
    expect_equal(names(a), names(b), info = s)
    expect_equal(as.data.frame(lapply(a, as.character)),
                 as.data.frame(lapply(b, as.character)), info = s)
  }
})
