# Pairs table: how every pair of insects co-occurs and co-fluctuates.
#
# Unit of comparison = region-year. Damage of an insect in a region-year is the sum of
# its records there (all hosts, repeated rows included: data is data). A region-year
# where an insect has no record is missing (NA) - never 0.
#
# Columns per pair (A, B):
#   shared_regions        regions where both are recorded
#   shared_region_years   region-years where both are recorded
#   both_damaged          region-years with damage > 0 for both
#   damage_overlap        both_damaged / region-years where at least one is damaged
#                         (Jaccard index, among shared region-years)
#   corr_within_region    median over shared regions of the Spearman correlation of
#                         the yearly damage of A and B in that region (regions with at
#                         least `min_years` shared years and variation in both)
#   corr_regions          number of regions used for corr_within_region
#   corr_positive_share   share of those regions with a positive correlation
#   lag_A_then_B          as corr_within_region, A in year t vs B in year t+1
#   lag_B_then_A          B in year t vs A in year t+1
#   shared_hosts          host trees both live on (host tables)
#   same_L1, same_L2      same paper group at L1 / L2

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

.spearman <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3 || sd(x) == 0 || sd(y) == 0) return(NA_real_)
  suppressWarnings(cor(x, y, method = "spearman"))
}

# Codes counted as another insect (reference/counted_as.csv), e.g. the combined spruce
# bark beetle code counted as Ips typographus. Returns model_data with sp_code, insect
# and is_aggregate replaced for those records.
apply_counted_as <- function(model_data, ref = "reference/counted_as.csv") {
  if (!file.exists(ref)) return(model_data)
  ca <- readr::read_csv(ref, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  names_of <- model_data |> distinct(sp_code, insect)
  for (k in seq_len(nrow(ca))) {
    hit <- model_data$sp_code == ca$code[k]
    if (!any(hit)) next
    target_name <- names_of$insect[match(ca$counted_as[k], names_of$sp_code)]
    model_data$sp_code[hit] <- ca$counted_as[k]
    if (!is.na(target_name)) model_data$insect[hit] <- target_name
    model_data$is_aggregate[hit] <- FALSE
  }
  model_data
}

build_pairs <- function(model_data, host, min_years = 6) {
  model_data <- apply_counted_as(model_data)
  h <- if (exists("add_hierarchy")) add_hierarchy(model_data) else
    mutate(model_data, L1 = NA_character_, L2 = NA_character_)
  info <- h |>
    distinct(sp_code, insect, guild, unit, single_species = if_else(is_aggregate, "no", "yes"),
             L1, L2) |>
    group_by(sp_code) |> slice(1) |> ungroup()

  # host trees per insect (species: host tables incl. supervisor additions;
  # mixed codes: recorded host)
  hosts <- bind_rows(
    host$insect_hosts |> distinct(sp_code = code, host),
    model_data |> filter(is_aggregate) |> distinct(sp_code, host)
  ) |> group_by(sp_code) |> summarise(hosts = list(unique(host)), .groups = "drop")

  ry <- model_data |>
    group_by(sp_code, nuts, year) |>
    summarise(v = sum(value), .groups = "drop")
  wide <- ry |> pivot_wider(names_from = sp_code, values_from = v)   # NA = not recorded
  codes <- sort(unique(ry$sp_code))
  nuts <- wide$nuts; years <- wide$year

  # lagged values: value of each code in the same region one year later
  lead1 <- wide |> mutate(year = year - 1L)
  lead1 <- left_join(wide |> select(nuts, year), lead1, by = c("nuts", "year"))

  within_corr <- function(a, b) {
    r <- vapply(split(seq_along(a), nuts), function(i) {
      ok <- !is.na(a[i]) & !is.na(b[i])
      if (sum(ok) < min_years) return(NA_real_)
      .spearman(a[i][ok], b[i][ok])
    }, numeric(1))
    r <- r[!is.na(r)]
    c(median = if (length(r)) median(r) else NA_real_, n = length(r),
      pos = if (length(r)) mean(r > 0) else NA_real_)
  }

  pr <- t(combn(codes, 2))
  out <- lapply(seq_len(nrow(pr)), function(k) {
    a_code <- pr[k, 1]; b_code <- pr[k, 2]
    a <- wide[[a_code]]; b <- wide[[b_code]]
    both <- !is.na(a) & !is.na(b)
    if (!any(both)) return(NULL)
    dmg_a <- both & a > 0; dmg_b <- both & b > 0
    either <- sum(dmg_a | dmg_b)
    cw <- within_corr(a, b)
    l_ab <- within_corr(a, lead1[[b_code]])
    l_ba <- within_corr(b, lead1[[a_code]])
    ha <- hosts$hosts[[match(a_code, hosts$sp_code)]]
    hb <- hosts$hosts[[match(b_code, hosts$sp_code)]]
    tibble(
      code_A = a_code, code_B = b_code,
      shared_regions = n_distinct(nuts[both]),
      shared_region_years = sum(both),
      both_damaged = sum(dmg_a & dmg_b),
      damage_overlap = if (either > 0) sum(dmg_a & dmg_b) / either else NA_real_,
      corr_within_region = unname(cw["median"]),
      corr_regions = as.integer(cw["n"]),
      corr_positive_share = unname(cw["pos"]),
      lag_A_then_B = unname(l_ab["median"]),
      lag_B_then_A = unname(l_ba["median"]),
      shared_hosts = paste(intersect(ha, hb), collapse = "; ")
    )
  })
  bind_rows(out) |>
    left_join(info |> rename_with(~ paste0(.x, "_A"), -sp_code), by = c("code_A" = "sp_code")) |>
    left_join(info |> rename_with(~ paste0(.x, "_B"), -sp_code), by = c("code_B" = "sp_code")) |>
    mutate(share_host = if_else(shared_hosts != "", "yes", "no"),
           same_guild = if_else(guild_A == guild_B, "yes", "no"),
           same_L1 = if_else(L1_A == L1_B, "yes", "no"),
           same_L2 = if_else(L2_A == L2_B & !startsWith(L2_A, "other"), "yes", "no"),
           both_single_species = if_else(single_species_A == "yes" & single_species_B == "yes", "yes", "no"),
           across(c(damage_overlap, corr_within_region, corr_positive_share,
                    lag_A_then_B, lag_B_then_A), ~ round(.x, 3))) |>
    select(insect_A, insect_B, guild_A, guild_B, shared_regions, shared_region_years,
           both_damaged, damage_overlap, corr_within_region, corr_regions, corr_positive_share,
           lag_A_then_B, lag_B_then_A, share_host, shared_hosts, same_guild, same_L1, same_L2,
           both_single_species, unit_A, unit_B, L1_A, L1_B, L2_A, L2_B, code_A, code_B) |>
    arrange(desc(corr_regions > 0), desc(corr_within_region))
}

pairs_columns <- function() {
  tibble::tribble(
    ~column, ~meaning,
    "insect_A / insect_B", "The two insects (or mixed-species codes) compared.",
    "shared_regions", "Regions where both are recorded.",
    "shared_region_years", "Region-years where both are recorded (a missing record is never counted as 0).",
    "both_damaged", "Region-years where both caused damage (> 0).",
    "damage_overlap", "both_damaged / region-years where at least one caused damage (0 = never together, 1 = always together).",
    "corr_within_region", "Do their yearly damages rise and fall together? Median over regions of the Spearman correlation of yearly damage in each region (-1 to 1). Rank-based, so m3 vs ha does not matter.",
    "corr_regions", "Number of regions behind corr_within_region (regions with at least 6 shared years and variation in both).",
    "corr_positive_share", "Share of those regions where the correlation is positive.",
    "lag_A_then_B", "Same correlation with B one year later: does damage by A tend to be followed by damage by B?",
    "lag_B_then_A", "Damage by B followed by damage by A one year later.",
    "share_host / shared_hosts", "Whether, and on which trees, both live (host tables, incl. the supervisor's matrix).",
    "same_guild / same_L1 / same_L2", "Same feeding guild / same paper group at L1 or L2.",
    "both_single_species", "yes = both are named species; no = at least one is a mixed-species code.",
    "Caution", "Correlations from few regions (corr_regions 1-2) are uncertain; sort or filter on corr_regions too. Correlation is not causation."
  )
}

write_pairs_workbook <- function(pairs, path) {
  library(openxlsx2)
  wb <- wb_workbook() |>
    wb_add_worksheet("pairs", tab_color = wb_color("7030A0")) |>
    wb_add_data_table(x = pairs, table_name = "pairs", na = "", table_style = "TableStyleMedium12") |>
    wb_freeze_pane(first_active_row = 2, first_active_col = 3) |>
    wb_set_col_widths(cols = seq_len(ncol(pairs)), widths = c(34, 34, rep(12, ncol(pairs) - 2))) |>
    # colour the correlation columns: red = move together, blue = opposite
    wb_add_conditional_formatting(dims = wb_dims(x = pairs, cols = match(c("corr_within_region", "lag_A_then_B", "lag_B_then_A"), names(pairs))[1], from_row = 2),
                                  type = "colorScale", style = c("5B9BD5", "FFFFFF", "F8696B"), rule = c(-1, 0, 1)) |>
    wb_add_conditional_formatting(dims = wb_dims(x = pairs, cols = match("lag_A_then_B", names(pairs)), from_row = 2),
                                  type = "colorScale", style = c("5B9BD5", "FFFFFF", "F8696B"), rule = c(-1, 0, 1)) |>
    wb_add_conditional_formatting(dims = wb_dims(x = pairs, cols = match("lag_B_then_A", names(pairs)), from_row = 2),
                                  type = "colorScale", style = c("5B9BD5", "FFFFFF", "F8696B"), rule = c(-1, 0, 1)) |>
    wb_add_worksheet("what the columns mean") |>
    wb_add_data_table(x = pairs_columns(), table_name = "pair_columns") |>
    wb_set_col_widths(cols = 1:2, widths = c(32, 140))
  wb_save(wb, path, overwrite = TRUE)
  invisible(path)
}
