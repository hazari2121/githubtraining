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
source(file.path("R", "host_climate.R"))
source(file.path("R", "datasheet.R"))
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

source(file.path("R", "make_table.R"))
md <- build_model_data(cleaned, ht)

test_that("make_table reproduces a host matrix from insect_hosts", {
  t <- make_table(ht$insect_hosts, rows = "host", cols = "insect",
                  filter = guild == "borer" & host_type == "conifers")
  m <- ht$matrices$borers_on_conifers
  # make_table only lists hosts that have insects; compare those rows
  m <- m[m$`host tree` %in% t$host, ]
  for (ins in names(m)[-1])
    expect_equal(t[[ins]][match(m$`host tree`, t$host)], m[[ins]], info = ins)
})

test_that("make_table keeps no-record cells blank, never zero", {
  t <- make_table(md, rows = "insect", cols = "country", value = "sum", filter = unit == "m3")
  n <- make_table(md, rows = "insect", cols = "country", value = "n_records", filter = unit == "m3")
  expect_identical(is.na(as.matrix(t[-1])), is.na(as.matrix(n[-1])))
})

test_that("make_table refuses to add m3 and ha together", {
  expect_error(make_table(md, rows = "country", value = "sum"), "more than one unit")
})

test_that("series filter (>= 6 non-zero years) reproduces most of paper Table 2", {
  cmp <- compare_paper_table2(cleaned)
  expect_true(all(abs(cmp$diff_filtered_pct) < 2))
  expect_gte(sum(abs(cmp$diff_filtered_pct) <= 0.01), 11)
  expect_equal(cmp$total_filtered[cmp$group == "Pityogenes chalcographus"], 28145602, tolerance = 1e-6)
})

# Optional third argument: Data_Klim_Tree_NUTS workbook (host amount + climate)
hc_path <- commandArgs(trailingOnly = TRUE)[3]
if (!is.na(hc_path)) {
  hc <- read_host_climate(hc_path)
  mdh <- add_host_climate(md, hc)

  test_that("every damage unit has host data except Berlin, Bremen, Hamburg", {
    expect_setequal(setdiff(unique(d$nuts), hc$nuts), c("DE3", "DE5", "DE6"))
  })

  test_that("joining host data adds no rows and never invents host amounts", {
    expect_equal(nrow(mdh), nrow(md))
    expect_true(all(is.na(mdh$damage_rel[is.na(mdh$host_amount)])))
    expect_false(any(hc$country == "SL"))
  })

  test_that("Table 2 host share reproduces the spruce groups", {
    sh <- compare_paper_host_share(cleaned, hc)
    for (g in c("Bark borers on conifers", "Bark borers on spruce", "Ips typographus"))
      expect_equal(sh$pct_host_per_year[sh$group == g],
                   sh$paper_pct_host_per_year[sh$group == g], tolerance = 0.05, info = g)
  })
}
