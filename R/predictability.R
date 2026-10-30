# Predictability across levels of aggregation
#
# Question: knowing one insect's (or group's) damage in a region-year, how well
# can another's be predicted, and does this change when insects are grouped?
# Levels: species (all single insects), host tree (the paper's level 2), host
# type (the paper's level 1), guild (all borers x all defoliators).
#
# Same data as the pair graphs: each region's log(1 + damage) series as a
# z-score within the region; all region-years where both are recorded.
#   R2        = r^2, share of one series' variation explained by the other
#   R2_tested = predict each country from a straight line fitted on the other
#               countries only: 1 - sum(error^2) / sum((z - mean)^2), averaged
#               over both directions (A -> B and B -> A). Can be below 0 when the
#               prediction is worse than just guessing the mean.
#   r_next    = r between one series this year and the other next year (same
#               region), averaged over both directions: can this year predict next year?

pair_predict <- function(j) {
  if (nrow(j) < min_n_metric || stats::sd(j$z_a) == 0 || stats::sd(j$z_b) == 0) return(NULL)
  r <- stats::cor(j$z_a, j$z_b)
  cv <- function(x, y) {
    cs <- unique(j$country)
    if (length(cs) < 2) return(NA_real_)
    pred <- rep(NA_real_, nrow(j))
    for (c in cs) {
      tr <- j$country != c
      b <- stats::coef(stats::lm(y[tr] ~ x[tr]))
      pred[!tr] <- b[1] + b[2] * x[!tr]
    }
    1 - sum((y - pred)^2) / sum((y - mean(y))^2)
  }
  nxt <- function(a, b) {
    k <- dplyr::inner_join(dplyr::select(j, nuts_id, year, a = dplyr::all_of(a)),
                           dplyr::transmute(j, nuts_id, year = year - 1, b = .data[[b]]), by = c("nuts_id", "year"))
    if (nrow(k) < min_n_metric) NA_real_ else stats::cor(k$a, k$b)
  }
  tibble::tibble(r = r, R2 = r^2, R2_tested = mean(c(cv(j$z_a, j$z_b), cv(j$z_b, j$z_a))),
                 r_next = mean(c(nxt("z_a", "z_b"), nxt("z_b", "z_a")), na.rm = TRUE),
                 n_region_years = nrow(j), n_regions = dplyr::n_distinct(j$nuts_id), n_countries = dplyr::n_distinct(j$country))
}

# All pairs of the series in tr (species or groups); guild_of: name -> "Borers"/"Defoliators"
level_predictability <- function(tr, keep, guild_of, level) {
  z <- dplyr::filter(tr, !is.na(z_within), species %in% keep) |> dplyr::select(country, nuts_id, year, species, z_within)
  pairs <- utils::combn(sort(unique(z$species)), 2, simplify = FALSE)
  purrr::map_dfr(pairs, function(p) {
    j <- dplyr::inner_join(dplyr::filter(z, species == p[1]) |> dplyr::select(-species, z_a = z_within),
                           dplyr::filter(z, species == p[2]) |> dplyr::select(-species, z_b = z_within),
                           by = c("country", "nuts_id", "year"))
    out <- pair_predict(j)
    if (is.null(out)) NULL else dplyr::mutate(out, level, a = p[1], b = p[2], .before = 1)
  }) |>
    dplyr::mutate(pair_type = dplyr::if_else(guild_of[a] == guild_of[b], "same guild", "borer x defoliator"))
}

predictability_levels <- c(species = "Species", level2 = "Host tree\n(fir, spruce, pine)", level1 = "Host type\n(gymno- / angiosperms)",
                           total = "Guild\n(all borers x all defoliators)")

predictability_summary <- function(pp) {
  pp |> dplyr::group_by(level, pair_type) |>
    dplyr::summarise(pairs = dplyr::n(), median_R2 = stats::median(R2), median_R2_tested = stats::median(R2_tested, na.rm = TRUE),
                     best_R2 = max(R2), share_pairs_R2_over_0.1 = mean(R2 >= 0.1), median_r_next = stats::median(r_next, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(level = factor(level, names(predictability_levels))) |> dplyr::arrange(pair_type, level)
}

plot_predictability <- function(pp) {
  d <- pp |> dplyr::mutate(level = factor(predictability_levels[level], predictability_levels),
                           pair_type = factor(pair_type, c("same guild", "borer x defoliator")))
  med <- d |> dplyr::group_by(level, pair_type) |> dplyr::summarise(m = stats::median(R2_tested, na.rm = TRUE), .groups = "drop")
  ggplot2::ggplot(d, ggplot2::aes(level, R2_tested)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_jitter(ggplot2::aes(colour = pair_type), width = 0.18, height = 0, alpha = 0.45, size = 1.6, show.legend = FALSE) +
    ggplot2::geom_point(data = med, ggplot2::aes(y = m), shape = 95, size = 14, colour = pal$ink) +
    ggplot2::scale_colour_manual(values = c(`same guild` = "#184f95", `borer x defoliator` = "#eb6834")) +
    ggplot2::facet_wrap(~ pair_type, ncol = 2) +
    ggplot2::labs(x = NULL, y = "Tested R-squared: share of the other series predicted\nin countries not used to fit the line",
                  title = "How well does one insect or group predict another, at each level of aggregation?",
                  subtitle = "One dot = one pair; black bar = median. Left: both in the same guild (borers with borers, defoliators with defoliators). Right: a borer with a defoliator.\nSpecies: pairs recorded together in at least 2 countries (needed for the test).",
                  caption = paste("Each region's log(1 + damage) series standardised within the region; all region-years where both are recorded.",
                                  "\nTested R-squared: each country predicted by a straight line fitted on the other countries only, averaged over both directions (0 = no better than the mean).",
                                  "\nGroups as in the 2025 EU-ForDAM paper (Table 1); group value = sum of all recorded member codes. Reported data, no gap-filling.")) +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
}

# Like-for-like comparison: each group pair against the species pairs inside
# the same two groups (e.g. borers on pine x borers on spruce against every
# pine-borer species x spruce-borer species pair).
predictability_compare <- function(pp, gm) {
  sp <- dplyr::filter(pp, level == "species", !is.na(R2_tested))
  grp <- dplyr::filter(pp, level != "species")
  purrr::pmap_dfr(dplyr::select(grp, level, a, b, R2_tested, pair_type), function(level, a, b, R2_tested, pair_type) {
    ma <- gm$code[gm$level == level & gm$group == a]; mb <- gm$code[gm$level == level & gm$group == b]
    s <- dplyr::filter(sp, (a %in% ma & b %in% mb) | (a %in% mb & b %in% ma))
    dplyr::bind_rows(
      tibble::tibble(what = "group", value = R2_tested),
      tibble::tibble(what = "species pair", value = s$R2_tested)) |>
      dplyr::mutate(level, group_pair = paste(a, "x", b), pair_type, n_species_pairs = nrow(s),
                    group_value = R2_tested, species_median = if (nrow(s)) stats::median(s$R2_tested) else NA_real_)
  })
}

plot_predictability_compare <- function(pc) {
  lv <- c(level2 = "Host tree (fir, spruce, pine)", level1 = "Host type (gymnosperms / angiosperms)", total = "Guild (all borers x all defoliators)")
  d <- pc |> dplyr::mutate(level = factor(lv[level], lv),
                           lab = paste0(group_pair, "  (", n_species_pairs, " species pairs)"),
                           lab = forcats::fct_reorder(lab, group_value))
  grp <- dplyr::filter(d, what == "group")
  med <- dplyr::distinct(d, level, lab, species_median) |> dplyr::filter(!is.na(species_median))
  ggplot2::ggplot(d, ggplot2::aes(value, lab)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#c9c8c3", linewidth = 0.4) +
    ggplot2::geom_point(data = dplyr::filter(d, what == "species pair"), colour = "#9c9b97", alpha = 0.55, size = 1.4,
                        position = ggplot2::position_jitter(height = 0.12, width = 0, seed = 1)) +
    ggplot2::geom_point(data = med, ggplot2::aes(x = species_median), shape = 124, size = 5, colour = "#5c5b57") +
    ggplot2::geom_point(data = grp, ggplot2::aes(colour = pair_type), size = 4) +
    ggplot2::scale_colour_manual(values = c(`same guild` = "#184f95", `borer x defoliator` = "#eb6834"),
                                 labels = c(`same guild` = "group pair, same guild", `borer x defoliator` = "group pair, borer x defoliator"), name = NULL) +
    ggplot2::facet_grid(level ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "Tested R-squared: share of one series predicted in countries not used to fit the line (0 = no better than the mean)", y = NULL,
                  title = "Does grouping insects make them more predictable?",
                  subtitle = paste("Big dot = the group pair. Grey dots = the species pairs inside the same two groups; grey bar = their median.",
                                   "\nWhere a group is to the right of its species, grouping predicts better than the single insects it contains."),
                  caption = paste("Each region's log(1 + damage) series standardised within the region; all region-years where both are recorded.",
                                  "\nTested R-squared: each country predicted by a straight line fitted on the other countries only, averaged over both directions.",
                                  "\nSpecies pairs: only those recorded together in at least 2 countries (needed for the test). Groups as in the 2025 EU-ForDAM paper (Table 1).")) +
    theme_eu(9) + ggplot2::theme(legend.position = "top", strip.text.y = ggplot2::element_text(angle = 0, face = "bold", hjust = 0))
}
