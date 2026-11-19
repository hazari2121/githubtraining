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

write_web_data <- function(model_data, host, pairs, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
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
  trees <- read_host_references()$hosts |> distinct(host_label, tree_type, order) |>
    group_by(host_label, tree_type) |> summarise(order = min(order), .groups = "drop") |>
    arrange(order)

  rec <- h |>
    group_by(sp_code, nuts, year, host_label) |>
    summarise(v = sum(value), n = n(), paper = as.integer(any(in_paper_series)), .groups = "drop") |>
    mutate(i = match(sp_code, ins$code) - 1L, r = match(nuts, regs$nuts) - 1L,
           t = match(host_label, trees$host_label) - 1L, v = round(v, 4))

  meta <- list(
    generated = format(Sys.Date()),
    insects = lapply(seq_len(nrow(ins)), function(k) list(
      code = ins$code[k], name = ins$insect[k], guild = ins$guild[k], unit = ins$unit[k],
      L1 = ins$L1[k], L2 = ins$L2[k], L3 = ins$L3[k], single = ins$single[k],
      hosts = I(ins$hosts[[k]]))),
    regions = lapply(seq_len(nrow(regs)), function(k) list(
      code = regs$nuts[k], country = regs$country[k], name = regs$region[k])),
    trees = lapply(seq_len(nrow(trees)), function(k) list(
      name = trees$host_label[k], type = trees$tree_type[k]))
  )
  writeLines(toJSON(meta, auto_unbox = TRUE, na = "null"), file.path(out_dir, "meta.json"))

  lines <- sprintf("[%d,%d,%d,%d,%s,%d,%d]", rec$i, rec$r, rec$year, rec$t,
                   format(rec$v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE),
                   rec$n, rec$paper)
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
