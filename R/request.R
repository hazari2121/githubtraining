# Data on request: aggregate the EU-ForDAM records along hierarchies.
#
# Hierarchies (coarse -> fine):
#   insect: guild -> L1 (guild x conifers/broadleaves) -> L2 (guild x host genus)
#           -> L3 (selected species, as in the paper) -> insect (every code)
#   space : europe -> country -> region
#   time  : all -> period (5-year blocks) -> year
#   host  : none -> host_type -> host_tree
# L1-L3 follow the paper's Table 1 (reference/paper_table1.csv). Every record is in
# exactly one L1 group and at most one L2 / L3 group; records outside an L2 / L3 group
# are shown as "other ...", so totals add up at every level.
#
# Missing data: only recorded values are aggregated. A total over regions or years
# with no record is a partial total, never filled with 0. Coverage columns
# (n_records, n_regions, n_countries, n_years) are always reported in long layout.
# Units are never added together: unit is always part of the breakdown.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

request_fields <- tibble::tribble(
  ~field, ~allowed, ~default, ~meaning,
  "name", "text (max 31 characters)", "", "Sheet name for the result.",
  "insect_level", "guild, L1, L2, L3, insect", "insect", "Level of the insect hierarchy.",
  "space_level", "europe, country, region", "europe", "Level of the spatial hierarchy.",
  "time_level", "all, period, year", "year", "all = 2000-2022 together; period = 5-year blocks.",
  "host_level", "none, host_type, host_tree", "none", "Extra breakdown by host.",
  "measure", "total, mean_per_year, mean_per_record, max, relative_pct, n_records, n_regions, n_countries, n_series, any_damage",
    "total", "What each cell holds. relative_pct = % of host damaged per year (needs host data).",
  "columns", "none, time, space, insect, host", "none", "Dimension spread across columns (wide table). none = long table with coverage columns.",
  "insects", "names or codes, separated by ;", "", "Keep only these. Matched at the chosen insect_level first, otherwise at any level (guild, L1-L3, insect name, sp_code).",
  "countries", "codes separated by ;", "", "Keep only these countries, e.g. CZ; DE.",
  "years", "e.g. 2010-2022 or 2003; 2018", "", "Keep only these years.",
  "unit", "m3, ha", "", "Keep only this unit (borers m3, defoliators ha).",
  "host_trees", "names separated by ;", "", "Keep only these host trees (Latin or common name).",
  "paper_series_only", "yes, no", "no", "yes = only series with >= 6 non-zero years (paper filter).",
  "include_combined_codes", "yes, no", "yes", "no = drop codes combining several insects (e.g. Def_dec)."
)

add_hierarchy <- function(md, ref_dir = "reference", period_years = 5) {
  t1 <- readr::read_csv(file.path(ref_dir, "paper_table1.csv"), show_col_types = FALSE) |>
    mutate(column = group_col_name(group),
           short = str_remove(paper_label, "\\s*\\(.*\\)$"))
  level_label <- function(level) {
    g <- t1 |> filter(level == !!level)
    out <- rep(NA_character_, nrow(md))
    for (i in seq_len(nrow(g))) out[md[[g$column[i]]] %in% TRUE] <- g$short[i]
    out
  }
  l1 <- level_label(1); l2 <- level_label(2); l3 <- level_label(3)
  start <- 2000 + ((md$year - 2000) %/% period_years) * period_years
  md |>
    mutate(L1 = l1,
           L2 = coalesce(l2, paste("other", L1)),
           L3 = coalesce(l3, if_else(str_starts(L2, "other "), L2, paste("other", L2))),
           period = paste0(start, "-", pmin(start + period_years - 1, 2022)),
           europe = "Europe", all = "2000-2022", none = "all hosts")
}

.split <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(str_trim(x))) return(character())
  str_trim(str_split(x, ";")[[1]])
}

.years <- function(x) {
  v <- .split(x)
  unlist(lapply(v, function(s) if (str_detect(s, "-")) {
    r <- as.integer(str_split(s, "-")[[1]]); seq(r[1], r[2]) } else as.integer(s)))
}

# Host amount used as denominator for relative_pct, per unit of space and level.
.host_denominator <- function(d, level, hc, ref_dir) {
  if (is.null(hc) || level == "guild") return(rep(NA_real_, nrow(d)))
  if (level == "insect") return(d$host_amount)
  ghc <- readr::read_csv(file.path(ref_dir, "group_host_columns.csv"), show_col_types = FALSE)
  t1 <- readr::read_csv(file.path(ref_dir, "paper_table1.csv"), show_col_types = FALSE) |>
    mutate(short = str_remove(paper_label, "\\s*\\(.*\\)$"))
  map <- t1 |> select(group, short) |> left_join(ghc, by = "group") |>
    group_by(short) |> summarise(cols = list(unique(host_column)))
  cols <- map$cols[match(d[[level]], map$short)]
  vapply(seq_len(nrow(d)), function(i) {
    cs <- cols[[i]]; r <- match(d$nuts[i], hc$nuts)
    if (is.null(cs) || is.na(r)) return(NA_real_)
    v <- unlist(hc[r, cs]); if (anyNA(v)) NA_real_ else sum(v)
  }, numeric(1))
}

run_request <- function(md, req, hc = NULL, ref_dir = "reference") {
  get <- function(f) {
    v <- req[[f]]; if (is.null(v) || is.na(v) || !nzchar(str_trim(as.character(v))))
      request_fields$default[request_fields$field == f] else str_trim(as.character(v))
  }
  check <- function(f, v) {
    ok <- str_split(request_fields$allowed[request_fields$field == f], ",\\s*")[[1]]
    if (!v %in% ok) stop("Request '", get("name"), "': ", f, " = '", v, "' must be one of: ",
                         paste(ok, collapse = ", "), call. = FALSE)
    v
  }
  lv  <- check("insect_level", get("insect_level"))
  sp  <- c(europe = "europe", country = "country", region = "nuts")[check("space_level", get("space_level"))]
  tm  <- c(all = "all", period = "period", year = "year")[check("time_level", get("time_level"))]
  hs  <- c(none = "none", host_type = "host_type", host_tree = "host_label")[check("host_level", get("host_level"))]
  ms  <- check("measure", get("measure"))
  cl  <- check("columns", get("columns"))

  d <- add_hierarchy(md, ref_dir)
  # filters
  ins <- .split(get("insects"))
  if (length(ins)) {
    # a name is matched at the requested insect level if it exists there
    # (e.g. "Ips typographus" at insect level = the species code only; at L3 = the
    # paper group incl. the combined spruce bark beetle code), otherwise at any level
    lvl_col <- if (lv == "insect") c("insect", "sp_code") else lv
    keep <- rep(FALSE, nrow(d))
    for (nm in tolower(ins)) {
      here <- Reduce(`|`, lapply(lvl_col, function(v) tolower(d[[v]]) == nm))
      if (!any(here)) here <- Reduce(`|`, lapply(c("guild", "L1", "L2", "L3", "insect", "sp_code"),
                                                 function(v) tolower(d[[v]]) == nm))
      keep <- keep | here
    }
    unmatched <- ins[!tolower(ins) %in% tolower(unlist(d[c("guild", "L1", "L2", "L3", "insect", "sp_code")]))]
    if (length(unmatched)) warning("Request '", get("name"), "': no match for ",
                                   paste(unmatched, collapse = ", "), call. = FALSE)
    d <- d[keep, ]
  }
  if (length(.split(get("countries")))) d <- filter(d, country %in% toupper(.split(get("countries"))))
  if (length(.years(get("years")))) d <- filter(d, year %in% .years(get("years")))
  if (nzchar(get("unit"))) d <- filter(d, unit == get("unit"))
  ht <- tolower(.split(get("host_trees")))
  if (length(ht)) d <- filter(d, tolower(host_tree) %in% ht | tolower(host) %in% ht |
                                 tolower(host_label) %in% ht)
  if (get("paper_series_only") == "yes") d <- filter(d, in_paper_series)
  if (get("include_combined_codes") == "no") d <- filter(d, !is_aggregate)
  if (nrow(d) == 0) return(tibble(note = "No records match this request."))

  if (ms == "relative_pct") {
    if (lv == "guild" || is.null(hc))
      stop("Request '", get("name"), "': relative_pct needs host data and insect_level L1, L2, L3 or insect.",
           call. = FALSE)
    d$.host <- .host_denominator(d, lv, hc, ref_dir)
  }

  if (!".host" %in% names(d)) d$.host <- NA_real_
  keys <- unique(c("unit", lv, sp, tm, hs))
  out <- d |>
    group_by(across(all_of(keys))) |>
    summarise(
      value = switch(ms,
        total = sum(value),
        mean_per_year = sum(value) / n_distinct(year),
        mean_per_record = mean(value),
        max = max(value),
        relative_pct = {
          # only regions with host data; each region-year's host counted once
          ok <- !is.na(.host)
          first <- ok & !duplicated(paste(nuts, year))
          if (!any(ok) || sum(.host[first]) == 0) NA_real_
          else 100 * sum(value[ok]) / sum(.host[first])
        },
        n_records = n(),
        n_regions = n_distinct(nuts),
        n_countries = n_distinct(country),
        n_series = n_distinct(series_id),
        any_damage = as.numeric(any(value > 0))),
      n_records = n(), n_regions = n_distinct(nuts), n_countries = n_distinct(country),
      n_years = n_distinct(year), .groups = "drop") |>
    (\(x) if (ms %in% c("n_records", "n_regions", "n_countries")) select(x, -value)
           else rename(x, !!ms := value))() |>
    rename_with(~ c(nuts = "region", host_label = "host_tree", all = "years",
                    none = "host")[.x], any_of(c("nuts", "host_label", "all", "none")))

  # drop dimensions that carry a single constant label
  for (v in c("europe", "years", "host")) if (v %in% names(out) && n_distinct(out[[v]]) == 1)
    out[[v]] <- NULL

  if (cl == "none") return(out)
  wide_var <- c(time = unname(c(all = "years", period = "period", year = "year")[names(tm)]),
                space = unname(c(europe = "europe", country = "country", nuts = "region")[sp]),
                insect = lv,
                host = unname(c(none = "host", host_type = "host_type", host_label = "host_tree")[hs]))[[cl]]
  if (!wide_var %in% names(out))
    stop("Request '", get("name"), "': columns = ", cl, " but that dimension is not broken down.",
         call. = FALSE)
  out |>
    select(-any_of(setdiff(c("n_records", "n_regions", "n_countries", "n_years"), ms))) |>
    arrange(.data[[wide_var]]) |>
    pivot_wider(names_from = all_of(wide_var), values_from = all_of(ms))
}

# Template with example requests and dropdowns
example_requests <- function() {
  tibble::tribble(
    ~name, ~insect_level, ~space_level, ~time_level, ~host_level, ~measure, ~columns, ~insects, ~countries, ~years, ~unit, ~host_trees, ~paper_series_only, ~include_combined_codes,
    "L1 Europe by year", "L1", "europe", "year", "none", "total", "time", "", "", "", "", "", "no", "yes",
    "L2 by country", "L2", "country", "all", "none", "total", "space", "", "", "", "", "", "no", "yes",
    "L3 species by country, paper", "L3", "country", "all", "none", "total", "space", "", "", "", "", "", "yes", "yes",
    "Ips vs Phaenops by year", "insect", "europe", "year", "none", "total", "insect", "Ips typographus; Phaenops cyanea", "", "", "", "", "no", "no",
    "Spruce vs pine borers, CZ DE", "L2", "country", "period", "none", "total", "time", "Borers on P. abies; Borers on P. sylvestris", "CZ; DE", "", "", "", "no", "yes",
    "Relative damage L2 by country", "L2", "country", "all", "none", "relative_pct", "space", "", "", "", "", "", "yes", "yes",
    "Coverage - regions per insect", "insect", "europe", "year", "none", "n_regions", "time", "", "", "", "", "", "no", "yes",
    "Damage by host tree, 5-year", "guild", "europe", "period", "host_tree", "total", "time", "", "", "", "", "", "no", "yes"
  )
}

write_request_template <- function(path, requests = example_requests()) {
  library(openxlsx2)
  n <- 200
  opts <- setNames(str_split(request_fields$allowed, ",\\s*"), request_fields$field)
  wb <- wb_workbook() |>
    wb_add_worksheet("requests") |>
    wb_add_data_table(x = requests, table_name = "requests", table_style = "TableStyleMedium2") |>
    wb_set_col_widths(cols = seq_len(ncol(requests)),
                      widths = c(32, 12, 12, 11, 11, 14, 10, 40, 14, 14, 7, 18, 10, 12)) |>
    wb_freeze_pane(first_active_row = 2, first_active_col = 2)
  for (f in c("insect_level", "space_level", "time_level", "host_level", "measure", "columns",
              "unit", "paper_series_only", "include_combined_codes")) {
    col <- match(f, names(requests))
    wb <- wb |> wb_add_data_validation(dims = wb_dims(rows = 2:n, cols = col), type = "list",
                                       value = paste0('"', paste(opts[[f]], collapse = ","), '"'))
  }
  wb <- wb |>
    wb_add_worksheet("fields") |>
    wb_add_data_table(x = request_fields, table_name = "fields") |>
    wb_set_col_widths(cols = 1:4, widths = c(22, 60, 10, 90))
  wb_save(wb, path, overwrite = TRUE)
  invisible(path)
}

# Lists of valid names per level, to help writing the 'insects' filter
request_vocabulary <- function(md, ref_dir = "reference") {
  d <- add_hierarchy(md, ref_dir)
  bind_rows(
    tibble(level = "guild", name = sort(unique(d$guild))),
    tibble(level = "L1", name = sort(unique(d$L1))),
    tibble(level = "L2", name = sort(unique(d$L2))),
    tibble(level = "L3", name = sort(unique(d$L3))),
    d |> distinct(level = "insect", name = insect, code = sp_code, guild, L1, L2, L3) |> arrange(guild, name),
    tibble(level = "host_tree", name = sort(unique(d$host_label))),
    tibble(level = "country", name = sort(unique(d$country)))
  )
}

run_requests <- function(requests_path, model_data_path, out_path, hc_path = NULL,
                         ref_dir = "reference") {
  md <- readr::read_csv(model_data_path, show_col_types = FALSE, guess_max = 1e5)
  hc <- if (!is.null(hc_path) && file.exists(hc_path))
    readr::read_csv(hc_path, show_col_types = FALSE) else NULL
  reqs <- readxl::read_excel(requests_path, sheet = "requests", col_types = "text") |>
    filter(!is.na(name), nzchar(name))
  if (anyDuplicated(reqs$name)) stop("Request names must be unique.")

  results <- list(); index <- list()
  for (i in seq_len(nrow(reqs))) {
    r <- as.list(reqs[i, ])
    nm <- substr(str_squish(str_replace_all(r$name, "[\\[\\]:*?/\\\\]", " ")), 1, 31)
    res <- tryCatch(run_request(md, r, hc, ref_dir),
                    error = function(e) tibble(error = conditionMessage(e)))
    results[[nm]] <- res
    index[[i]] <- tibble(sheet = nm, rows = nrow(res),
                         status = if ("error" %in% names(res)) res$error[1] else "ok",
                         request = paste(names(r), unlist(r), sep = " = ", collapse = " | ")[1])
  }
  index <- bind_rows(index)
  writexl::write_xlsx(c(list(index = index, vocabulary = request_vocabulary(md, ref_dir)), results),
                      out_path)
  invisible(list(index = index, results = results))
}
