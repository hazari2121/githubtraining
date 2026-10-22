# Finishing the correlation metrics
#
# (1) Multiple testing. For every measurable pair, a p-value from the same
#     region block bootstrap as the 95% ranges (whole regions resampled, 9,999
#     draws): p = 2 x the share of draws on the other side of 0. Then the
#     Benjamini-Hochberg false discovery rate (FDR) over all measurable pairs:
#     q < 0.05 means at most 5% of the links called "sure" are expected to be
#     chance.
# (2) One summary table of the usable pairs for the thesis.

boot_p <- function(x, y, g, B = 9999, seed = 1) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]; g <- g[ok]
  if (length(x) < min_n_metric || stats::sd(x) == 0 || stats::sd(y) == 0 || dplyr::n_distinct(g) < 5) return(NA_real_)
  G <- dplyr::n_distinct(g)
  st <- rowsum(cbind(1, x, y, x * x, y * y, x * y), g)
  set.seed(seed)
  S <- t(stats::rmultinom(B, G, rep(1 / G, G))) %*% st
  rb <- (S[, 1] * S[, 6] - S[, 2] * S[, 3]) / sqrt((S[, 1] * S[, 4] - S[, 2]^2) * (S[, 1] * S[, 5] - S[, 3]^2))
  rb <- rb[is.finite(rb)]
  min(1, 2 * (min(sum(rb <= 0), sum(rb >= 0)) + 1) / (length(rb) + 1))
}

pair_fdr <- function(tr, species, verdict) {
  rows <- dplyr::distinct(tr, country, nuts_id, year) |> dplyr::arrange(nuts_id, year)
  keys <- paste(rows$nuts_id, rows$year)
  Z <- wide_on(tr, "z_within", keys, species)
  A <- wide_on(tr, "anomaly", keys, species)
  g <- rows$nuts_id
  v <- dplyr::filter(verdict, verdict != "not enough data")
  v$p <- purrr::map2_dbl(v$species_a, v$species_b, function(a, b) boot_p(Z[, a], Z[, b], g, seed = match(paste(a, b), paste(v$species_a, v$species_b))))
  v$p_regional <- purrr::map2_dbl(v$species_a, v$species_b, function(a, b) boot_p(A[, a], A[, b], g, seed = 1e5 + match(paste(a, b), paste(v$species_a, v$species_b))))
  v |> dplyr::mutate(q = stats::p.adjust(p, "BH"), q_regional = stats::p.adjust(p_regional, "BH"),
                     sure_after_fdr = q < 0.05, regional_sure_after_fdr = q_regional < 0.05) |>
    dplyr::select(species_a, species_b, verdict, r, p, q, sure_after_fdr, r_regional_part, p_regional, q_regional, regional_sure_after_fdr)
}

fdr_summary <- function(fd) {
  strong <- c("usable", "promising", "unreliable")
  tibble::tibble(
    measure = c("Measurable pairs", "Sure before correction (95% range excludes 0)", "Sure after FDR correction (q < 0.05)",
                "Expected chance links before correction (5% of measurable)",
                "Usable pairs", "Usable pairs still sure after FDR", "Usable pairs whose regional part is sure after FDR"),
    value = c(nrow(fd), sum(fd$verdict %in% c(strong, "weak")), sum(fd$sure_after_fdr, na.rm = TRUE),
              round(0.05 * nrow(fd)), sum(fd$verdict == "usable"),
              sum(fd$verdict == "usable" & fd$sure_after_fdr, na.rm = TRUE),
              sum(fd$verdict == "usable" & fd$regional_sure_after_fdr, na.rm = TRUE)))
}

# r of a pair without the regions that stop reporting (all insects 0 from
# 2006-07 on: Hesse, Saxony-Anhalt; and Lower Saxony, almost always 0)
r_without_regions <- function(tr, usable, drop = c("DE7", "DEE", "DE9")) {
  z <- dplyr::filter(tr, !is.na(z_within), !nuts_id %in% drop) |> dplyr::select(nuts_id, year, species, z_within)
  purrr::map2_dbl(usable$species_a, usable$species_b, function(a, b) {
    j <- dplyr::inner_join(dplyr::filter(z, species == a), dplyr::filter(z, species == b), by = c("nuts_id", "year"))
    if (nrow(j) < 20) NA_real_ else stats::cor(j$z_within.x, j$z_within.y)
  })
}

# Summary table of the usable pairs, labelled by simple rules:
#   thin data            : few regions (< 10) AND weaker (< 0.3) without the regions that stop reporting
#   regional link        : regional part >= 0.25 and sure after FDR
#   mostly shared years  : regional part < 0.20 (the link is mostly years bad all over Europe)
#   partly shared years  : in between
# plus two warnings: few regions (< 10) and weaker without the regions that stop reporting (< 0.3)
usable_summary <- function(usable, fd, r_wo) {
  usable |>
    dplyr::left_join(dplyr::select(fd, species_a, species_b, q, q_regional, sure_after_fdr, regional_sure_after_fdr),
                     by = c("species_a", "species_b")) |>
    dplyr::mutate(r_without_stopping_regions = r_wo,
                  type = dplyr::case_when(
                    n_regions < 10 & abs(r_without_stopping_regions) < 0.3 ~ "thin data",
                    abs(r_regional_part) >= 0.25 & regional_sure_after_fdr ~ "regional link",
                    abs(r_regional_part) < 0.20 ~ "mostly shared years",
                    TRUE ~ "partly shared years"),
                  warning = trimws(paste(dplyr::if_else(n_regions < 10, "few regions;", ""),
                                         dplyr::if_else(abs(r_without_stopping_regions) < 0.3, "weaker without the regions that stop reporting;", ""),
                                         dplyr::if_else(!sure_after_fdr, "not sure after FDR;", ""))),
                  warning = sub(";$", "", warning)) |>
    dplyr::transmute(group, Pair, Hosts, r = round(r, 2), r_regional_part = round(r_regional_part, 2),
                     r_without_stopping_regions = round(r_without_stopping_regions, 2),
                     q = signif(q, 2), countries = Countries, n_countries, n_regions, type, warning) |>
    dplyr::arrange(group, dplyr::desc(abs(r)))
}

plot_usable_summary <- function(us) {
  d <- dplyr::mutate(us, group = sub("^[0-9]+\\. ", "", group)) |>
    dplyr::transmute(Group = group, Pair, Hosts, r, `r regional part` = r_regional_part, Countries = countries,
                     Type = type, Warning = dplyr::if_else(warning == "", "-", warning))
  fill <- dplyr::case_when(d$Type == "regional link" ~ "#e6f2e6", d$Type == "partly shared years" ~ "#fdf6e3",
                           d$Type == "thin data" ~ "#ececec", TRUE ~ "#f6e9e6")
  th <- gridExtra::ttheme_minimal(base_size = 8.5,
    core = list(bg_params = list(fill = fill, col = "white"), fg_params = list(hjust = 0, x = 0.03)),
    colhead = list(bg_params = list(fill = "#16302B"), fg_params = list(col = "white", fontface = "bold", hjust = 0, x = 0.03)))
  g <- gridExtra::tableGrob(d, rows = NULL, theme = th)
  ttl <- grid::textGrob("The 28 usable pairs: summary", x = 0.01, hjust = 0, gp = grid::gpar(fontsize = 13, fontface = "bold"))
  note <- grid::textGrob(paste("Green = regional link (regional part >= 0.25 and sure after FDR); yellow = partly shared years; red = mostly shared years (regional part < 0.20);",
                               "grey = thin data (fewer than 10 regions and below 0.3 without the regions that stop reporting).",
                               "\nAll 28 stay sure after the FDR correction (q < 0.05 over 786 tested pairs). r = same-year r within regions; r regional part = each value minus the same insect's mean in all other regions that year.",
                               "\nWarnings: few regions (< 10); weaker (< 0.3) without Hesse, Saxony-Anhalt and Lower Saxony, which report 0 for every insect for many years."),
                         x = 0.01, hjust = 0, gp = grid::gpar(fontsize = 8, col = "#555555"))
  gridExtra::arrangeGrob(ttl, g, note, ncol = 1, heights = grid::unit.c(grid::unit(0.5, "in"), sum(g$heights), grid::unit(0.7, "in")))
}

# ---- Questions for the data providers ---------------------------------------------

# "2000, 2001, 2002, 2005" -> "2000-2002, 2005"
year_ranges <- function(txt) {
  y <- suppressWarnings(as.integer(strsplit(txt, ",\\s*")[[1]]))
  if (anyNA(y)) return(txt)
  runs <- split(y, cumsum(c(1, diff(y) != 1)))
  paste(vapply(runs, function(r) if (length(r) == 1) as.character(r) else paste0(r[1], "-", r[length(r)]), ""), collapse = ", ")
}

data_questions <- function(susp) {
  fixed <- tibble::tribble(
    ~topic, ~where, ~question,
    "Units of growing stock", "Forest appendix, BG and LT sheets",
    "Single-species growing stocks are about 1000x smaller than their areas suggest (e.g. BG spruce 31,157 m3 on 133,737 ha). Are they in 1000 m3? (Read as 1000 m3 for now.)",
    "Host 0 but damage reported", "Forest appendix, Vienna (AT13)",
    "The appendix gives 0 spruce for Vienna, but I. typographus and P. chalcographus damage is reported every year (3,191 m3 in total). Is there spruce that is not split out?",
    "Not separately in the appendix", "Forest appendix",
    "Larch, birch, elm and poplar have no area or growing stock. Is there a source for them? (All conifers / all broadleaves used for now.)",
    "Forest area total", "Forest appendix vs 2025 paper",
    "The appendix adds up to 87.7 million ha in the reporting regions; the paper gives 102.75 million ha. Which forest area is the paper's figure?",
    "Very large single values", "DE DE2 Bavaria, P. abietina (2000-2003)",
    "Pristiphora abietina 58,333 / 76,667 / 113,333 / 200,000 ha in 2000-2003, then 0 from 2004 on. Are these ha defoliated, and are they 3-year averages?")
  auto <- susp |>
    dplyr::mutate(topic = dplyr::case_when(check == "all insects 0 in these years" ~ "Every insect reported as 0",
                                           check == "values ending in .333 / .667" ~ "Values that look like 3-year totals divided by 3",
                                           TRUE ~ "Sudden drop to (almost) 0"),
                  where = paste0(country, " ", nuts_id, " (", vapply(years, year_ranges, ""), ")"),
                  question = dplyr::case_when(
                    check == "all insects 0 in these years" ~ paste(detail, "- not monitored, or really no damage?"),
                    check == "values ending in .333 / .667" ~ paste(detail, "- are these multi-year totals divided evenly?"),
                    TRUE ~ paste(detail, "- typing error (e.g. in 1000 m3) or missing year?"))) |>
    dplyr::select(topic, where, question)
  dplyr::bind_rows(fixed, auto)
}

# ---- Robustness of the usable pairs ----------------------------------------------
# (a) Time-shift test: inside each region the second insect's series is moved
#     round in time by a random number of years. That keeps each series' own
#     trend and run of good/bad years but breaks which years line up, so it asks
#     "is r bigger than two trending series give by chance?" (999 shifts, seeded).
# (b) Leave one country out: r again without each country in turn; the lowest
#     value and the country whose removal gives it.
usable_robustness <- function(tr, usable, B = 999, seed = 1) {
  z <- dplyr::filter(tr, !is.na(z_within)) |> dplyr::select(country, nuts_id, year, species, z_within)
  set.seed(seed)
  purrr::map2_dfr(usable$species_a, usable$species_b, function(a, b) {
    j <- dplyr::inner_join(dplyr::filter(z, species == a), dplyr::filter(z, species == b), by = c("country", "nuts_id", "year"))
    r0 <- stats::cor(j$z_within.x, j$z_within.y)
    idx <- split(seq_len(nrow(j)), j$nuts_id)
    null <- replicate(B, {
      yb <- j$z_within.y
      for (i in idx) if (length(i) > 1) { k <- sample.int(length(i) - 1, 1); yb[i] <- yb[i][(seq_along(i) + k - 1) %% length(i) + 1] }
      stats::cor(j$z_within.x, yb)
    })
    loo <- vapply(unique(j$country), function(cn) { k <- j[j$country != cn, ]; if (nrow(k) < 20) NA_real_ else stats::cor(k$z_within.x, k$z_within.y) }, 0)
    tibble::tibble(species_a = a, species_b = b, r = r0,
                   p_time_shift = (sum(abs(null) >= abs(r0)) + 1) / (B + 1), r_by_chance_95 = stats::quantile(abs(null), 0.95),
                   lowest_r_one_country_out = if (r0 > 0) min(loo, na.rm = TRUE) else max(loo, na.rm = TRUE),
                   country_dropped = names(loo)[if (r0 > 0) which.min(loo) else which.max(loo)],
                   share_region_years_DE = mean(j$country == "DE"))
  })
}
