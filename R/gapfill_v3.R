# Gap-filling, aligned with the correlation metrics (Oct 2026).
#
# The earlier gap-filling (v1) used the insect codes as reported; the
# correlation metrics count the CZ spruce bark-borer group as I. typographus
# (as in Hlasny et al. 2025). The v3 targets rebuild the factor-model inputs
# from the same data as the correlation metrics (cb_cells / cb_tr), so both
# parts of the thesis rest on one dataset. Model settings (K, ridge penalties,
# group hint) are the ones chosen earlier; they are not re-tuned here.
#
# model_vs_correlation(): does the fitted factor model reproduce the
# correlation metrics? For every pair, the same within-region Pearson r is
# computed on the model's fitted values (expected log damage = P(any) x
# log amount) over the same region-years where both insects are recorded,
# and compared with the r measured on the data.

model_vs_correlation <- function(raw, verdict) {
  d <- raw |>
    dplyr::filter(!is.na(y)) |>
    dplyr::mutate(fit = p_any * amount_log) |>
    dplyr::select(country, nuts_id, year, species, fit)
  w <- tidyr::pivot_wider(d, names_from = species, values_from = fit)
  reg <- paste(w$country, w$nuts_id)
  zfun <- function(v) {   # standardise within each region series
    stats::ave(v, reg, FUN = function(x) {
      ok <- !is.na(x)
      if (sum(ok) < 3 || stats::sd(x[ok]) == 0) return(rep(NA_real_, length(x)))
      (x - mean(x[ok])) / stats::sd(x[ok])
    })
  }
  sp <- intersect(unique(c(verdict$species_a, verdict$species_b)), names(w))
  Z <- vapply(sp, function(s) zfun(w[[s]]), numeric(nrow(w)))
  r_model <- purrr::map2_dbl(verdict$species_a, verdict$species_b, function(a, b) {
    if (!a %in% sp || !b %in% sp) return(NA_real_)
    x <- Z[, a]; y <- Z[, b]; ok <- !is.na(x) & !is.na(y)
    if (sum(ok) < 20) NA_real_ else stats::cor(x[ok], y[ok])
  })
  verdict |>
    dplyr::select(species_a, species_b, verdict, r_data = r) |>
    dplyr::mutate(r_model = r_model)
}


# ---- Gap-filled dataset v2: factor model only, with honest ranges ------------
#
# 1. The final method is tested exactly as it is used: for each of the 10
#    folds, hide the country x insect blocks, refit the whole final model
#    (gapfill_final) on the rest, and keep the predictions for the hidden cells.
# 2. Ranges come from that test (empirical, not from model assumptions): for
#    every hidden country x insect x year total we know the real value, so the
#    spread of log(real / predicted) says how far off a filled total typically
#    is. The 10% and 90% points of that spread give an 80% range for any
#    filled total. Errors within a country x insect block are strongly shared
#    (a whole country's level can be off), so ranges are built for totals,
#    not for single cells added up as if independent.
#    Calibration is checked by learning the spread on half the folds and
#    counting how often the other half's real totals fall inside.
# 3. Fill rules:
#      reported damage / reported zero : kept
#      zero: host tree absent          : host tree not in the region (forest table)
#      not filled: aggregate code      : aggregate reporting codes (e.g. CambXyl_con&MD,
#                                        Def_dec) are country-specific reporting
#                                        categories, not insects; filling them in
#                                        other countries would double-count damage
#                                        already reported under species codes
#      not fillable                    : nothing recorded in that region-year
#      filled                          : everything else, with a confidence:
#        reliable : insect tested and the model beats the simple average for it
#                   and separates damage from no damage (AUC >= 0.7)
#        weak     : tested, but not both
#        untested : insect recorded in one country only (cannot be tested)
#    Europe-wide totals use reported + reliable + weak fills (weak flagged);
#    untested fills are reported separately and not added to the headline.

final_method_cv <- function(L, blocks, hs, group, K1, lambda1, K2, lambda2, tau = 50) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ltr <- L; Ltr$Y[H] <- NA
    raw <- gapfill_final(Ltr, hs, group, K1, lambda1, K2, lambda2, tau = tau)
    idx <- which(H, arr.ind = TRUE)
    k <- (idx[, 2] - 1) * nrow(L$Y) + idx[, 1]       # raw is laid out column by column
    out[[length(out) + 1]] <- tibble::tibble(
      fold = f, country = raw$country[k], nuts_id = raw$nuts_id[k], year = raw$year[k],
      species = raw$species[k], observed = L$Y[idx], pred = raw$factor_fill[k],
      pred_log = ifelse(raw$absent[k], 0, raw$p_any[k] * raw$amount_log[k]),   # log-scale prediction
      p_any = raw$p_any[k], absent = raw$absent[k])
  }
  dplyr::bind_rows(out)
}

# Hidden totals per fold x country x insect x year, with log(real / predicted)
cv_block_totals <- function(fc) {
  fc |>
    dplyr::filter(!is.na(pred)) |>
    dplyr::group_by(fold, country, species, year) |>
    dplyr::summarise(real = sum(expm1(observed)), pred = sum(pred), .groups = "drop") |>
    dplyr::mutate(lr = log((real + 1) / (pred + 1)))
}

# Per-insect test scores of the final method (for the confidence labels)
final_method_scores <- function(fc, gf_species_simple) {
  fc |>
    dplyr::filter(!is.na(pred)) |>
    dplyr::group_by(species) |>
    dplyr::summarise(n_cells = dplyr::n(),
                     error_log = sqrt(mean((pred_log - observed)^2)),
                     auc = auc(pred_log, as.numeric(observed > 0)), .groups = "drop") |>
    dplyr::left_join(gf_species_simple, by = "species") |>
    dplyr::mutate(confidence = dplyr::if_else(error_log < error_log_simple & !is.na(auc) & auc >= 0.7,
                                              "reliable", "weak"))
}

# Error spread of totals: per insect when it has >= min_n hidden totals,
# otherwise the pooled spread of all insects
range_table <- function(bt, min_n = 30) {
  pooled <- stats::quantile(bt$lr, c(0.1, 0.5, 0.9), names = FALSE)
  bt |>
    dplyr::group_by(species) |>
    dplyr::summarise(n_totals = dplyr::n(),
                     q10 = stats::quantile(lr, 0.1, names = FALSE),
                     q50 = stats::quantile(lr, 0.5, names = FALSE),
                     q90 = stats::quantile(lr, 0.9, names = FALSE), .groups = "drop") |>
    dplyr::mutate(source = dplyr::if_else(n_totals >= min_n, "this insect", "all insects"),
                  q10 = dplyr::if_else(n_totals >= min_n, q10, pooled[1]),
                  q50 = dplyr::if_else(n_totals >= min_n, q50, pooled[2]),
                  q90 = dplyr::if_else(n_totals >= min_n, q90, pooled[3]))
}

# Calibration check: learn the spread on odd folds, test on even folds, and back
range_calibration <- function(bt, min_n = 30) {
  one <- function(train, test) {
    rt <- range_table(train, min_n)
    pooled <- stats::quantile(train$lr, c(0.1, 0.9), names = FALSE)
    test |>
      dplyr::left_join(dplyr::select(rt, species, q10, q90), by = "species") |>
      dplyr::mutate(q10 = dplyr::coalesce(q10, pooled[1]), q90 = dplyr::coalesce(q90, pooled[2]),
                    inside = lr >= q10 & lr <= q90)
  }
  odd <- bt$fold %% 2 == 1
  dplyr::bind_rows(one(bt[odd, ], bt[!odd, ]), one(bt[!odd, ], bt[odd, ])) |>
    dplyr::summarise(n_totals = dplyr::n(), share_inside_80pct_range = mean(inside),
                     share_below = mean(lr < q10), share_above = mean(lr > q90))
}

gapfill_dataset_v2 <- function(raw, conf, unit_lookup, all_species) {
  conf_all <- dplyr::bind_rows(
    dplyr::select(conf, species, confidence),
    tibble::tibble(species = setdiff(all_species, conf$species), confidence = "untested"))
  raw |>
    dplyr::left_join(conf_all, by = "species") |>
    dplyr::left_join(unit_lookup, by = "species") |>
    dplyr::mutate(
      value_type = dplyr::case_when(
        !is.na(y) & y > 0 ~ "reported damage",
        !is.na(y) ~ "reported zero",
        absent ~ "zero: host tree absent",
        code_type(species) == "aggregate group" ~ "not filled: aggregate reporting code",
        empty | is.na(factor_fill) ~ "not fillable (nothing recorded in this region-year)",
        TRUE ~ "filled"),
      damage = dplyr::case_when(
        value_type %in% c("reported damage", "reported zero") ~ expm1(y),
        value_type == "zero: host tree absent" ~ 0,
        value_type == "filled" ~ factor_fill),
      p_any = dplyr::if_else(value_type == "filled", p_any, NA_real_),
      confidence = dplyr::if_else(value_type == "filled", confidence, NA_character_)) |>
    dplyr::select(country, nuts_id, year, species, unit, value_type, damage, confidence, p_any, host_area_ha)
}

# Country x insect x year filled totals with 80% ranges, and Europe-wide
# insect x year totals: reported + filled (reliable and weak), with an 80%
# range from 2000 draws: each country's filled total is multiplied by a test
# error log(real / predicted) resampled from that insect's hidden totals
# (all insects pooled when it has < min_n), countries drawn independently.
gapfill_totals_v2 <- function(gd, rt, bt, n_draw = 2000, min_n = 30, seed = 20261015) {
  set.seed(seed)
  ct <- gd |>
    dplyr::group_by(country, species, unit, year) |>
    dplyr::summarise(
      reported = sum(damage[value_type %in% c("reported damage", "reported zero")]),
      filled_tested = sum(damage[value_type == "filled" & confidence %in% c("reliable", "weak")]),
      filled_untested = sum(damage[value_type == "filled" & confidence == "untested"]),
      .groups = "drop") |>
    dplyr::left_join(dplyr::select(rt, species, q10, q90), by = "species") |>
    dplyr::mutate(filled_lo80 = pmax((filled_tested + 1) * exp(q10) - 1, 0),
                  filled_hi80 = (filled_tested + 1) * exp(q90) - 1)
  pool_all <- bt$lr
  pools <- split(bt$lr, bt$species)
  pool_of <- function(s) if (!is.null(pools[[s]]) && length(pools[[s]]) >= min_n) pools[[s]] else pool_all
  eu <- ct |>
    dplyr::group_by(species, unit, year) |>
    dplyr::group_modify(function(d, key) {
      f <- d$filled_tested[d$filled_tested > 0]
      draws <- if (length(f) > 0) {
        e <- matrix(sample(pool_of(key$species), n_draw * length(f), replace = TRUE), n_draw)
        rowSums(pmax(exp(e) * matrix(f + 1, n_draw, length(f), byrow = TRUE) - 1, 0))
      } else rep(0, n_draw)
      tibble::tibble(reported = sum(d$reported), filled = sum(d$filled_tested),
                     filled_untested = sum(d$filled_untested),
                     total = sum(d$reported) + sum(d$filled_tested),
                     total_lo80 = sum(d$reported) + stats::quantile(draws, 0.1, names = FALSE),
                     total_mid = sum(d$reported) + stats::quantile(draws, 0.5, names = FALSE),
                     total_hi80 = sum(d$reported) + stats::quantile(draws, 0.9, names = FALSE))
    }) |>
    dplyr::ungroup()
  list(country = ct, europe = eu)
}
