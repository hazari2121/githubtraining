# Data files for the web explorer (web/ folder next to index.html).
#   records.json  one array per line: [insect, region, year, host, damage, n_records, in_paper]
#                 (insect / region / host are indexes into meta.json)
#   meta.json     insects (with hierarchy, guild, unit, hosts), regions, host trees
#   pairs.json    the pairs table (one object per line)
# Missing combinations are simply absent: the page never turns them into 0.

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

# Host key of a record's host tree: the forest-data column used as its host amount.
# Trees without their own column use all conifers / all broadleaves (as in the pipeline).
host_key_of <- function(host_label) {
  dplyr::case_when(startsWith(host_label, "Picea") ~ "spruce", startsWith(host_label, "Pinus") ~ "pine",
                   startsWith(host_label, "Abies") ~ "fir", startsWith(host_label, "Quercus") ~ "oak",
                   startsWith(host_label, "Larix") ~ "coniferous", TRUE ~ "deciduous")
}
HOST_KEYS <- c("spruce", "pine", "fir", "oak", "coniferous", "deciduous")

write_web_data <- function(model_data, host, pairs, out_dir, host_climate = NULL) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  merged_codes <- character()
  if (exists("apply_counted_as") && file.exists("reference/counted_as.csv")) {
    ca <- readr::read_csv("reference/counted_as.csv", show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    merged_codes <- setNames(ca$note, ca$counted_as)
    model_data <- apply_counted_as(model_data)
  }
  h <- add_hierarchy(model_data)
  l2 <- if_else(startsWith(h$L2, "other "), "(no subgroup)", h$L2)
  l3 <- if_else(h$L3 == h$L2 & startsWith(h$L3, "other "), "(no subgroup)", h$L3)
  h$L2 <- l2; h$L3 <- l3

  ins <- h |>
    distinct(code = sp_code, insect, guild, unit, L1, L2, L3,
             single = !is_aggregate) |>
    group_by(code) |> slice(1) |> ungroup() |>
    arrange(guild, L1, L2, L3, insect)
  hosts_of <- bind_rows(
    host$insect_hosts |> left_join(read_host_references()$hosts |> distinct(host, host_label), by = "host") |>
      distinct(code, host_label),
    model_data |> filter(is_aggregate) |> distinct(code = sp_code, host_label)
  ) |> group_by(code) |> summarise(hosts = list(sort(unique(host_label))), .groups = "drop")
  ins <- ins |> left_join(hosts_of, by = "code")

  regs <- h |> distinct(nuts, country, region) |> group_by(nuts) |> slice(1) |> ungroup() |>
    arrange(country, nuts)
  # readable names where the damage data only has a code (DE, AT, FI, whole countries):
  # from the host file, otherwise from the map lookup
  nm_host <- if (is.null(host_climate)) character() else
    setNames(gsub("\\*", "", gsub(";", " + ", host_climate$unit_name)), host_climate$nuts)
  cw_path <- "reference/map/map_regions.csv"
  nm_map <- if (file.exists(cw_path)) {
    cw <- readr::read_csv(cw_path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    setNames(gsub(";", " + ", cw$gadm_names), cw$nuts)
  } else character()
  regs$region <- ifelse(regs$region == regs$nuts,
                        dplyr::coalesce(unname(nm_host[regs$nuts]), unname(nm_map[regs$nuts]), regs$region),
                        regs$region)
  trees <- read_host_references()$hosts |> distinct(host_label, tree_type, order) |>
    group_by(host_label, tree_type) |> summarise(order = min(order), .groups = "drop") |>
    arrange(order)

  rec <- h |>
    group_by(sp_code, nuts, year, host_label) |>
    summarise(v = sum(value), n = n(), paper = as.integer(any(in_paper_series)), .groups = "drop") |>
    mutate(i = match(sp_code, ins$code) - 1L, r = match(nuts, regs$nuts) - 1L,
           t = match(host_label, trees$host_label) - 1L, v = round(v, 4),
           k = match(host_key_of(host_label), HOST_KEYS) - 1L)

  # host amount per region and key: [ha, m3]; m3 values implausibly low (< 20 m3 / ha,
  # e.g. BG and LT species columns, likely in thousands of m3) are left out (null)
  host_of <- function(nuts) {
    if (is.null(host_climate)) return(NULL)
    h1 <- host_climate[host_climate$nuts == nuts, ]
    if (nrow(h1) == 0) return(NULL)
    setNames(lapply(HOST_KEYS, function(k) {
      ha <- h1[[paste0(k, "_ha")]]; m3 <- h1[[paste0(k, "_m3")]]
      if (!is.na(ha) && !is.na(m3) && ha > 0 && m3 / ha < 20) m3 <- NA
      c(if (is.na(ha)) NA else ha, if (is.na(m3)) NA else m3)
    }), HOST_KEYS)
  }
  excluded <- if (is.null(host_climate)) character() else unlist(lapply(regs$nuts, function(n) {
    h1 <- host_climate[host_climate$nuts == n, ]; if (nrow(h1) == 0) return(NULL)
    k <- HOST_KEYS[vapply(HOST_KEYS, function(k) { ha <- h1[[paste0(k, "_ha")]]; m3 <- h1[[paste0(k, "_m3")]]
      !is.na(ha) && !is.na(m3) && ha > 0 && m3 / ha < 20 }, logical(1))]
    if (length(k)) paste0(n, " (", paste(k, collapse = ", "), " m3)")
  }))

  meta <- list(
    generated = format(Sys.Date()),
    insects = lapply(seq_len(nrow(ins)), function(k) list(
      code = ins$code[k], name = ins$insect[k], guild = ins$guild[k], unit = ins$unit[k],
      L1 = ins$L1[k], L2 = ins$L2[k], L3 = ins$L3[k], single = ins$single[k],
      hosts = I(ins$hosts[[k]]),
      note = if (ins$code[k] %in% names(merged_codes)) unname(merged_codes[ins$code[k]]) else NA)),
    regions = lapply(seq_len(nrow(regs)), function(k) list(
      code = regs$nuts[k], country = regs$country[k], name = regs$region[k],
      host = { hv <- host_of(regs$nuts[k]); if (is.null(hv)) NA else hv })),   # NA -> null
    host_keys = HOST_KEYS,
    host_excluded = I(excluded),
    trees = lapply(seq_len(nrow(trees)), function(k) list(
      name = trees$host_label[k], type = trees$tree_type[k]))
  )
  writeLines(toJSON(meta, auto_unbox = TRUE, na = "null"), file.path(out_dir, "meta.json"))

  lines <- sprintf("[%d,%d,%d,%d,%s,%d,%d,%d]", rec$i, rec$r, rec$year, rec$t,
                   format(rec$v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE),
                   rec$n, rec$paper, rec$k)
  writeLines(c("[", paste0(lines, c(rep(",", length(lines) - 1), "")), "]"),
             file.path(out_dir, "records.json"))

  pj <- pairs |>
    transmute(a = code_A, b = code_B, regions = shared_regions, ry = shared_region_years,
              both = both_damaged, overlap = damage_overlap, corr = corr_within_region,
              corr_n = corr_regions, corr_pos = corr_positive_share,
              lag_ab = lag_A_then_B, lag_ba = lag_B_then_A, hosts = shared_hosts)
  pl <- vapply(seq_len(nrow(pj)), function(k)
    as.character(toJSON(as.list(pj[k, ]), auto_unbox = TRUE, na = "null")), character(1))
  writeLines(c("[", paste0(pl, c(rep(",", length(pl) - 1), "")), "]"),
             file.path(out_dir, "pairs.json"))
  invisible(out_dir)
}
