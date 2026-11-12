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
