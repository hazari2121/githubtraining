# 'query' sheet: choose rows, columns, measure and filters from dropdowns; the table
# recalculates instantly with plain Excel formulas (INDEX / MATCH / SUMIFS / COUNTIFS),
# which work in every Excel version and need no refresh.
#
# Rules, as everywhere in the pipeline:
#   * a cell with no record is blank, never 0
#   * one unit at a time (m3 = borers, ha = defoliators), never added together

query_dims <- c("guild", "L1", "L2", "L3", "insect", "host_type", "host_tree",
                "country", "region_code", "year")
query_measures <- c("total", "records", "average", "any damage", "lives on (1/0)")

.col_letter <- function(i) openxlsx2::int2col(i)

# Sorted labels of every dimension, one column each (for row / column headers)
query_lists <- function(df) {
  vals <- lapply(query_dims, function(v) {
    x <- sort(unique(df[[v]]))
    if (v %in% c("L1", "L2", "L3")) x <- c(setdiff(x, "(no subgroup)"), intersect("(no subgroup)", x))
    as.character(x)
  })
  n <- max(lengths(vals))
  out <- as.data.frame(lapply(vals, function(x) c(x, rep(NA, n - length(x)))),
                       stringsAsFactors = FALSE)
  names(out) <- query_dims
  out
}

add_query_sheet <- function(wb, df, hd, data_sheet = "data", host_sheet = "host data",
                            max_rows = 100, max_cols = 80) {
  lists <- query_lists(df)
  n_data <- nrow(df) + 1
  cols_d <- setNames(seq_along(df), names(df))
  rng <- function(v) {
    l <- .col_letter(cols_d[[v]])
    sprintf("'%s'!$%s$2:$%s$%d", data_sheet, l, l, n_data)
  }
  last_d <- .col_letter(ncol(df))
  all_d  <- sprintf("'%s'!$A$2:$%s$%d", data_sheet, last_d, n_data)
  head_d <- sprintf("'%s'!$A$1:$%s$1", data_sheet, last_d)
  dimrng <- function(cell) sprintf("INDEX(%s,0,MATCH(%s,%s,0))", all_d, cell, head_d)

  n_host <- nrow(hd) + 1
  last_h <- .col_letter(ncol(hd))
  all_h  <- sprintf("'%s'!$A$2:$%s$%d", host_sheet, last_h, n_host)
  head_h <- sprintf("'%s'!$A$1:$%s$1", host_sheet, last_h)
  hdimrng <- function(cell) sprintf("INDEX(%s,0,MATCH(%s,%s,0))", all_h, cell, head_h)
  hcol <- function(v) {
    l <- .col_letter(match(v, names(hd)))
    sprintf("'%s'!$%s$2:$%s$%d", host_sheet, l, l, n_host)
  }

  # ---- control panel ------------------------------------------------------
  ctrl <- data.frame(
    label = c("Rows", "Columns", "Measure", "Unit",
              "Filter: guild", "Filter: L1", "Filter: L2", "Filter: L3", "Filter: insect",
              "Filter: host tree", "Filter: country", "Years from", "Years to",
              "In paper dataset only", "Mixed-species codes"),
    value = c("insect", "year", "total", "m3",
              "(all)", "(all)", "(all)", "(all)", "(all)", "(all)", "(all)",
              "2000", "2022", "no", "include"),
    stringsAsFactors = FALSE)
  # control cells: column C, rows 3..17
  cc <- setNames(paste0("$C$", 3:17), c("rows", "cols", "measure", "unit", "guild", "L1", "L2",
                                        "L3", "insect", "host_tree", "country", "from", "to",
                                        "paper", "combined"))

  wb <- wb |>
    wb_add_data(sheet = "query", x = "Data on request: pick from the dropdowns in column C; the table updates instantly.",
                dims = "B1") |>
    wb_add_font(sheet = "query", dims = "B1", bold = TRUE, size = 13) |>
    wb_add_data(sheet = "query", x = ctrl, dims = "B3", col_names = FALSE) |>
    wb_add_fill(sheet = "query", dims = "C3:C17", color = wb_color("FFF2CC")) |>
    wb_add_border(sheet = "query", dims = "B3:C17") |>
    wb_add_data(sheet = "query", dims = "E3", col_names = FALSE, x = data.frame(c(
      "Rows / Columns: guild, L1, L2, L3 (paper groups), insect (every species), host_type, host_tree, country, region_code, year.",
      "Measure: total damage, number of records, average per record, any damage (1 = damage > 0, 0 = only zeros), lives on (1/0) = insect x host matrix (rows/columns must be insect levels and host_type / host_tree).",
      "Unit: m3 = borers (damaged wood), ha = defoliators (defoliated area). One unit at a time; they are never added together.",
      "Blank cell = no record (not recorded). 0 = monitored, no damage.",
      "Filters: (all) = no filter. In paper dataset only: yes = only series with at least 6 years of damage, as analysed in the paper. Mixed-species codes: include / exclude codes covering several insects (e.g. Bark beetles on spruce (unspecified)).",
      "Showing up to 100 rows x 80 columns; region_code has 77 entries."))) |>
    wb_add_font(sheet = "query", dims = "E3:E8", italic = TRUE, color = wb_color("595959"))

  # dropdowns
  lst <- function(v) {
    l <- .col_letter(match(v, names(lists)))
    sprintf("'lists'!$%s$2:$%s$%d", l, l, sum(!is.na(lists[[v]])) + 1)
  }
  dv <- function(wb, cell, values) wb_add_data_validation(wb, sheet = "query", dims = sub("\\$", "", gsub("\\$", "", cell)),
                                                          type = "list", value = values)
  dimlist <- paste0('"', paste(query_dims, collapse = ","), '"')
  wb <- wb |>
    dv(cc[["rows"]], dimlist) |> dv(cc[["cols"]], dimlist) |>
    dv(cc[["measure"]], paste0('"', paste(query_measures, collapse = ","), '"')) |>
    dv(cc[["unit"]], '"m3,ha"') |>
    dv(cc[["paper"]], '"no,yes"') |> dv(cc[["combined"]], '"include,exclude"')
  for (v in c("guild", "L1", "L2", "L3", "insect", "host_tree", "country"))
    wb <- dv(wb, cc[[v]], sprintf("'lists'!$%s$2:$%s$%d", .col_letter(match(v, names(lists)) + ncol(lists) + 1),
                                  .col_letter(match(v, names(lists)) + ncol(lists) + 1),
                                  sum(!is.na(lists[[v]])) + 2))

  # ---- criteria shared by all cells ------------------------------------------
  f <- function(v) sprintf('IF(%s="(all)","*",%s)', cc[[v]], cc[[v]])
  crit <- paste(
    sprintf('%s,%s', rng("unit"), cc[["unit"]]),
    sprintf('%s,%s', rng("guild"), f("guild")),
    sprintf('%s,%s', rng("L1"), f("L1")),
    sprintf('%s,%s', rng("L2"), f("L2")),
    sprintf('%s,%s', rng("L3"), f("L3")),
    sprintf('%s,%s', rng("insect"), f("insect")),
    sprintf('%s,%s', rng("host_tree"), f("host_tree")),
    sprintf('%s,%s', rng("country"), f("country")),
    sprintf('%s,">="&%s', rng("year"), cc[["from"]]),
    sprintf('%s,"<="&%s', rng("year"), cc[["to"]]),
    sprintf('%s,IF(%s="yes","yes","*")', rng("in_paper_dataset"), cc[["paper"]]),
    sprintf('%s,IF(%s="exclude","yes","*")', rng("single_species"), cc[["combined"]]),
    sep = ",")
  hcrit <- paste(
    sprintf('%s,%s', hcol("guild"), f("guild")),
    sprintf('%s,%s', hcol("L1"), f("L1")),
    sprintf('%s,%s', hcol("L2"), f("L2")),
    sprintf('%s,%s', hcol("L3"), f("L3")),
    sprintf('%s,%s', hcol("insect"), f("insect")),
    sprintf('%s,%s', hcol("host_tree"), f("host_tree")),
    sprintf('%s,IF(%s="exclude","yes","*")', hcol("single_species"), cc[["combined"]]),
    sep = ",")

  # ---- table ----------------------------------------------------------------
  r0 <- 20; c0 <- 2                            # header row 20, row labels in column B
  first_col <- .col_letter(c0 + 1); last_col <- .col_letter(c0 + max_cols)
  wb <- wb |>
    wb_add_formula(sheet = "query", dims = paste0("B", r0),
                   x = sprintf('=%s&" \\ "&%s', cc[["rows"]], cc[["cols"]])) |>
    wb_add_font(sheet = "query", dims = paste0("B", r0), bold = TRUE)

  # column headers
  lab_all <- sprintf("'lists'!$A$2:$%s$%d", .col_letter(ncol(lists)), nrow(lists) + 1)
  lab_head <- sprintf("'lists'!$A$1:$%s$1", .col_letter(ncol(lists)))
  label_formula <- function(sel, k)
    sprintf('=IFERROR(IF(INDEX(%s,%d,MATCH(%s,%s,0))="","",INDEX(%s,%d,MATCH(%s,%s,0))),"")',
            lab_all, k, sel, lab_head, lab_all, k, sel, lab_head)
  for (j in seq_len(max_cols))
    wb <- wb_add_formula(wb, sheet = "query", dims = paste0(.col_letter(c0 + j), r0),
                         x = label_formula(cc[["cols"]], j))
  for (i in seq_len(max_rows))
    wb <- wb_add_formula(wb, sheet = "query", dims = paste0("B", r0 + i),
                         x = label_formula(cc[["rows"]], i))

  # cells
  rowrng <- dimrng(cc[["rows"]]); colrng <- dimrng(cc[["cols"]])
  cell_formula <- function(rl, cl) {
    key <- sprintf('%s,%s,%s,%s', rowrng, rl, colrng, cl)
    n   <- sprintf('COUNTIFS(%s,%s)', key, crit)
    s   <- sprintf('SUMIFS(%s,%s,%s)', rng("damage"), key, crit)
    pos <- sprintf('COUNTIFS(%s,%s,%s,">0")', key, crit, rng("damage"))
    hkey <- sprintf('%s,%s,%s,%s', hdimrng(cc[["rows"]]), rl, hdimrng(cc[["cols"]]), cl)
    host <- sprintf('IFERROR(IF(COUNTIFS(%s,%s,%s,1)>0,1,0),"n/a")', hkey, hcrit, hcol("present"))
    sprintf(paste0('=IF(OR(%s="",%s=""),"",IF(%s="lives on (1/0)",%s,IF(%s=0,"",',
                   'CHOOSE(MATCH(%s,{"total","records","average","any damage"},0),%s,%s,%s/%s,IF(%s>0,1,0)))))'),
            rl, cl, cc[["measure"]], host, n, cc[["measure"]], s, n, s, n, pos)
  }
  m <- matrix("", max_rows, max_cols)
  for (i in seq_len(max_rows)) for (j in seq_len(max_cols))
    m[i, j] <- cell_formula(paste0("$B", r0 + i), paste0(.col_letter(c0 + j), "$", r0))
  for (j in seq_len(max_cols))
    wb <- wb_add_formula(wb, sheet = "query", dims = paste0(.col_letter(c0 + j), r0 + 1),
                         x = m[, j])

  wb |>
    wb_add_fill(sheet = "query", dims = sprintf("B%d:%s%d", r0, last_col, r0), color = wb_color("D9E1F2")) |>
    wb_add_font(sheet = "query", dims = sprintf("C%d:%s%d", r0, last_col, r0), bold = TRUE) |>
    wb_add_conditional_formatting(sheet = "query", dims = sprintf("C%d:%s%d", r0 + 1, last_col, r0 + max_rows),
                                  rule = sprintf('AND(%s="lives on (1/0)",C%d=1)', cc[["measure"]], r0 + 1),
                                  type = "expression", style = "is_host_drill") |>
    wb_add_numfmt(sheet = "query", dims = sprintf("C%d:%s%d", r0 + 1, last_col, r0 + max_rows),
                  numfmt = "#,##0.##") |>
    wb_set_col_widths(sheet = "query", cols = c(1, 2, 3, 4, 5), widths = c(2, 40, 22, 2, 14)) |>
    wb_set_col_widths(sheet = "query", cols = (c0 + 1):(c0 + max_cols), widths = 14) |>
    wb_freeze_pane(sheet = "query", first_active_row = r0 + 1, first_active_col = c0 + 1) |>
    # lists sheet: labels per dimension + filter lists with "(all)" first
    wb_add_data(sheet = "lists", x = lists, na = "") |>
    wb_add_data(sheet = "lists", x = as.data.frame(lapply(lists, function(x) c("(all)", x)),
                                                   stringsAsFactors = FALSE) |>
                  setNames(paste0(names(lists), "_filter")),
                start_col = ncol(lists) + 2, na = "")
}
