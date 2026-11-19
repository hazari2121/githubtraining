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
source(file.path("R", "pivot_workbook.R"))
source(file.path("R", "query_sheet.R"))
source(file.path("R", "pairs.R"))
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

source(file.path("R", "pivot_workbook.R"))
source(file.path("R", "query_sheet.R"))
source(file.path("R", "pairs.R"))
test_that("pivot data keeps every record and never mixes in invented values", {
  pd <- pivot_data(md |> add_series_info())
  expect_equal(nrow(pd), nrow(md))
  expect_equal(sum(pd$damage), sum(md$value))
})

test_that("every record has a readable insect name and a guild", {
  expect_false(any(md$insect == md$sp_code & md$is_aggregate))
  expect_false(anyNA(md$insect))
  expect_false(anyNA(md$guild))
  expect_equal(unique(md$insect[md$sp_code == "Def_dec"]), "Defoliators on broadleaves (unspecified)")
})

test_that("host matrix sheets match the host tables (same 1/0 pattern)", {
  hm <- host_matrix_tables(ht)
  for (b in names(ht$matrices)) {
    expect_equal(hm$boxes[[b]][-1], ht$matrices[[b]][-1], info = b)
    expect_true(all(grepl("\\(", hm$boxes[[b]]$`host tree`)), info = b)  # Latin (common)
  }
  # every insect lives on at least one tree, and the wide table agrees with the boxes
  trees <- setdiff(names(hm$all), c("insect", "code", "guild", "host_type", "regions",
                                   "countries"))
  expect_true(all(rowSums(hm$all[trees]) >= 1))
  expect_equal(nrow(hm$all), nrow(ht$insects))
  expect_equal(hm$all[["Pinus sylvestris (pine)"]][hm$all$code == "pit_cha"], 1)
  expect_equal(hm$all[["Picea abies (spruce)"]][hm$all$code == "pit_cha"], 1)
})

test_that("guild matrix covers all 18 paper groups and rebuilds Table 1", {
  mds <- add_series_info(md)
  gt <- guild_tables(mds)
  expect_equal(sum(grepl("^L[123] ", names(gt$matrix))), 18)
  expect_equal(nrow(gt$table1), 18)
  expect_gte(sum(gt$table1$countries_match == "yes"), 17)
  # Ips typographus group = ips_typ + the combined spruce bark beetle code
  ips <- gt$matrix$code[gt$matrix[["L3 Ips typographus"]] == 1]
  expect_setequal(ips, c("ips_typ", "CambXyl_con&BB_con&SM"))
})

source(file.path("R", "request.R"))
mdr <- add_series_info(md)

test_that("hierarchy: totals add up the same at every insect, space and time level", {
  base <- mdr |> group_by(unit) |> summarise(t = sum(value))
  for (lv in c("guild", "L1", "L2", "L3", "insect"))
    for (sp in c("europe", "country", "region"))
      for (tm in c("all", "period", "year")) {
        r <- run_request(mdr, list(name = "t", insect_level = lv, space_level = sp,
                                   time_level = tm, measure = "total"))
        got <- r |> group_by(unit) |> summarise(t = sum(total))
        expect_equal(got$t, base$t, tolerance = 1e-9, info = paste(lv, sp, tm))
      }
})

test_that("each record sits in exactly one group per level", {
  h <- add_hierarchy(mdr)
  expect_false(anyNA(h$L1)); expect_false(anyNA(h$L2)); expect_false(anyNA(h$L3))
})

test_that("requests filter, never fill missing with 0, and reject bad options", {
  r <- run_request(mdr, list(name = "t", insect_level = "insect", space_level = "country",
                             time_level = "all", measure = "total", columns = "space",
                             insects = "Ips typographus; Phaenops cyanea"))
  expect_setequal(r$insect, c("Ips typographus", "Phaenops cyanea"))
  expect_true(is.na(r$CH[r$insect == "Phaenops cyanea"]))   # not recorded in CH -> blank
  expect_error(run_request(mdr, list(name = "t", insect_level = "species")), "must be one of")
})

test_that("host drill-down data has the same 1/0 pattern as the host matrix", {
  hd <- host_drill_data(mdr, ht)
  hm <- host_matrix_tables(ht)$all
  for (code in hm$code) for (tree in setdiff(names(hm), c("insect", "code", "guild", "host_type", "regions", "countries")))
    expect_equal(max(hd$present[hd$sp_code == code & hd$host_tree == tree]), hm[[tree]][hm$code == code],
                 info = paste(code, tree))
  expect_true(all(hd$present %in% c(0, 1)))
  # regions: an insect appears only in regions where it is recorded
  rec <- mdr |> distinct(sp_code, nuts)
  got <- hd |> distinct(sp_code, nuts = region_code)
  expect_equal(nrow(dplyr::anti_join(got, rec, by = c("sp_code", "nuts"))), 0)
  expect_equal(nrow(dplyr::anti_join(rec, got, by = c("sp_code", "nuts"))), 0)
})

test_that("query sheet lists every label of every dimension", {
  pd <- pivot_data(mdr)
  ql <- query_lists(pd)
  for (v in query_dims) expect_setequal(na.omit(ql[[v]]), as.character(unique(pd[[v]])))
})

test_that("pairs table: counts are consistent and missing is never zero", {
  p <- build_pairs(mdr, ht)
  expect_equal(nrow(p), nrow(dplyr::distinct(p, code_A, code_B)))
  expect_true(all(p$both_damaged <= p$shared_region_years))
  expect_true(all(p$shared_regions >= 1))
  expect_true(all(is.na(p$corr_within_region) == (p$corr_regions == 0)))
  # shared region-years recomputed directly for one pair
  ry <- mdr |> dplyr::distinct(sp_code, nuts, year)
  a <- ry |> dplyr::filter(sp_code == "ips_typ"); b <- ry |> dplyr::filter(sp_code == "pit_cha")
  n <- nrow(dplyr::inner_join(a, b, by = c("nuts", "year")))
  expect_equal(p$shared_region_years[p$code_A == "ips_typ" & p$code_B == "pit_cha"], n)
  # spruce bark beetles move together
  expect_gt(p$corr_within_region[p$code_A == "ips_typ" & p$code_B == "pit_cha"], 0.5)
})
