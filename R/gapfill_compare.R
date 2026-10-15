# Gap-filling: three methods tested head to head (Oct 2026).
#
# Gaps = insects a country did not report (unknown, never treated as 0).
# Test: hide whole country x insect blocks that WERE reported (same blocks as
# R/lfm.R), predict them with each method from the remaining data only, and
# compare with the real values.
#
#   1. simple average  : the insect's mean log(1 + damage) in that year, over
#                        all regions that reported it (the yardstick)
#   2. partner method  : the insect's partners = other insects whose damage
#                        correlates with it (r >= 0.3) in the training data,
#                        co-recorded in >= 20 region-years and >= 2 countries.
#                        Each partner recorded in the hidden region-year gives
#                        a prediction by linear regression; predictions are
#                        averaged with weights r^2. No partner recorded there:
#                        falls back to the simple average.
#   3. factor model    : the two-part latent factor model (R/hurdle.R, R/lfm.R),
#                        P(any damage) x amount if damaged, with the insect's
#                        level pulled towards its broad group (strength 50),
#                        the best version found earlier.
# Each method gives two predictions per hidden cell:
#   *_log : on the log(1 + damage) scale, used for the per-cell error and for
#           telling damage from no damage (for the factor model:
#           P(any) x log amount, its expected log value)
#   plain : expected damage in the original unit, summed for block totals.

partner_predict <- function(Ytr, H, rows, base, min_r = 0.3, min_n = 20, min_countries = 2) {
  P <- base
  used <- matrix(FALSE, nrow(Ytr), ncol(Ytr))
  for (s in which(colSums(H) > 0)) {
    hid <- which(H[, s])
    num <- rep(0, length(hid)); den <- rep(0, length(hid))
    for (p in setdiff(seq_len(ncol(Ytr)), s)) {
      ok <- !is.na(Ytr[, s]) & !is.na(Ytr[, p])
      if (sum(ok) < min_n || dplyr::n_distinct(rows$country[ok]) < min_countries) next
      if (stats::sd(Ytr[ok, s]) == 0 || stats::sd(Ytr[ok, p]) == 0) next
      r <- stats::cor(Ytr[ok, s], Ytr[ok, p])
      if (r < min_r) next
      b <- stats::coef(stats::lm(Ytr[ok, s] ~ Ytr[ok, p]))
      xp <- Ytr[hid, p]
      has <- !is.na(xp)
      num[has] <- num[has] + r^2 * (b[1] + b[2] * xp[has])
      den[has] <- den[has] + r^2
    }
    got <- den > 0
    P[hid[got], s] <- pmax(num[got] / den[got], 0)
    used[hid[got], s] <- TRUE
  }
  list(P = P, used = used)
}

#   4. factor model + forest area (optional, when Lh / absent are given):
#                        the same model, plus the NUTS forest table: damage per
#                        1000 ha of host forest in the amount part, log host
#                        area in the any-damage part, host tree absent -> 0
#                        (R/hosts.R). Regions without area use version 3.
#   5. factor model + forest area (amount only): presence from version 3,
#                        amount from version 4.
gapfill_compare_cv <- function(L, blocks, group, K1, lambda1, K2, lambda2, tau = 50,
                               Lh = NULL, absent = NULL) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ytr <- L$Y; Ytr[H] <- NA
    idx <- which(H, arr.ind = TRUE)
    # 1. simple average
    base <- baseline_species_year(Ytr, L$rows$year)
    # 2. partner method
    pm <- partner_predict(Ytr, H, L$rows, base)
    # 3. factor model with level hint
    hp <- hurdle_parts(Ytr)
    f1 <- logit_fit(hp$B, K1, lambda1, group = group, tau = tau, tau_links = 0)
    f2 <- lfm_fit(hp$Y2, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
    p_any <- logit_predict(f1)[idx]; a <- lfm_predict(f2)[idx]
    fm <- p_any * pmax(expm1(a) * smearing(f2, hp$Y2), 0)
    if (!is.null(Lh)) {
      Ys <- Lh$Y; Ys[H] <- NA
      Y2s <- hurdle_parts(Ys)$Y2
      g1 <- logit_fit(hp$B, K1, lambda1, X = Lh$X, group = group, tau = tau, tau_links = 0)
      g2 <- lfm_fit(Y2s, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
      p_h <- logit_predict(g1)[idx]; a_s <- lfm_predict(g2)[idx]
      area <- Lh$area[idx]; ab <- absent[idx]
      fh <- host_combine(p_h, a_s, smearing(g2, Y2s), a, smearing(f2, hp$Y2), area, ab)
      # log-scale prediction: amount on the original scale when area is known
      a_nat <- ifelse(!is.na(area) & !is.na(a_s), log1p(pmax(expm1(a_s) * area / 1000, 0)), a)
      fh_log <- ifelse(ab, 0, p_h * a_nat)
      # 5. forest area for the amount only: presence from version 3
      fa <- host_combine(p_any, a_s, smearing(g2, Y2s), a, smearing(f2, hp$Y2), area, ab)
      fa_log <- ifelse(ab, 0, p_any * a_nat)
    } else { fh <- fh_log <- fa <- fa_log <- NA_real_ }
    out[[length(out) + 1]] <- tibble::tibble(
      fold = f, row = idx[, 1], col = idx[, 2],
      country = L$rows$country[idx[, 1]], nuts_id = L$rows$nuts_id[idx[, 1]],
      year = L$rows$year[idx[, 1]], species = L$species[idx[, 2]],
      observed = L$Y[idx],
      `simple average` = pmax(expm1(base[idx]), 0),
      `partner method` = pmax(expm1(pm$P[idx]), 0),
      `factor model` = fm,
      `factor model + forest area` = fh,
      `factor model + forest area (amount only)` = fa,
      `simple average_log` = base[idx], `partner method_log` = pm$P[idx], `factor model_log` = p_any * a,
      `factor model + forest area_log` = fh_log,
      `factor model + forest area (amount only)_log` = fa_log,
      partner_used = pm$used[idx])
  }
  dplyr::bind_rows(out)
}

gapfill_compare_scores <- function(cvr) {
  methods <- c("simple average", "partner method", "factor model", "factor model + forest area",
               "factor model + forest area (amount only)", "hybrid (factor level + partner ups and downs)",
               "blend: factor model + 25% notes equation")
  methods <- methods[methods %in% names(cvr) & vapply(methods, function(m) any(!is.na(cvr[[m]])), TRUE)]
  lg <- paste0(methods, "_log")
  d0 <- cvr |> dplyr::filter(dplyr::if_all(dplyr::all_of(c(methods, lg)), ~ !is.na(.x)))
  d <- d0 |>
    tidyr::pivot_longer(dplyr::all_of(methods), names_to = "method", values_to = "pred") |>
    dplyr::select(-dplyr::all_of(lg)) |>
    dplyr::left_join(d0 |> tidyr::pivot_longer(dplyr::all_of(lg), names_to = "method", values_to = "pred_log") |>
                       dplyr::mutate(method = sub("_log$", "", method)) |>
                       dplyr::select(fold, row, col, method, pred_log),
                     by = c("fold", "row", "col", "method")) |>
    dplyr::mutate(method = factor(method, methods), real = expm1(observed),
                  err = pred_log - observed)
  tot <- d |> dplyr::group_by(method, fold, country, species, year) |>
    dplyr::summarise(real = sum(real), pred = sum(pred), .groups = "drop") |>
    dplyr::filter(real > 0) |>
    dplyr::mutate(lr = abs(log((pred + 1) / (real + 1))))
  overall <- d |> dplyr::group_by(method) |>
    dplyr::summarise(n_cells = dplyr::n(),
                     error_log = sqrt(mean(err^2)),
                     damage_vs_none_auc = auc(pred_log, as.numeric(observed > 0)),
                     .groups = "drop") |>
    dplyr::left_join(tot |> dplyr::group_by(method) |>
                       dplyr::summarise(totals_within_2x = mean(lr <= log(2)),
                                        typical_total_error_x = exp(stats::median(lr))), by = "method")
  by_species <- d |> dplyr::group_by(species, method) |>
    dplyr::summarise(n_cells = dplyr::n(), error_log = sqrt(mean(err^2)),
                     auc = auc(pred_log, as.numeric(observed > 0)), .groups = "drop") |>
    dplyr::group_by(species) |>
    dplyr::mutate(best = method[which.min(error_log)],
                  gain_vs_simple = 1 - error_log / error_log[method == "simple average"]) |>
    dplyr::ungroup()
  partner_cov <- cvr |> dplyr::summarise(share_cells_with_partner = mean(partner_used))
  list(overall = overall, by_species = by_species, partner_coverage = partner_cov)
}

# ---- Final gap-filled dataset (version 1) ---------------------------------------
# Every region x year x insect cell:
#   reported damage / reported zero : kept exactly as reported
#   zero: host tree absent          : not reported, and the insect's host tree
#                                     does not grow in the region (forest table)
#   filled                          : not reported (unknown) -> estimated
#   not fillable                    : nothing at all recorded in that region-year
# Method: factor model + forest area for the amount (version 5 of the test);
# partner method for the insects where it tested best, where a partner is
# recorded in that region-year (otherwise the factor model).
# Confidence of a filled value, from the hide-and-predict test of its method:
#   reliable : beats the simple average and separates damage from no damage
#              (AUC >= 0.7) for this insect
#   weak     : tested, but not both of the above
#   untested : insect reported by one country only, so it cannot be tested

gapfill_final <- function(L, hs, group, K1, lambda1, K2, lambda2, tau = 50, partner_species = character()) {
  Lh <- host_scaled_L(L, hs); absent <- host_absent_matrix(L, hs)
  hp <- hurdle_parts(L$Y); Y2s <- hurdle_parts(Lh$Y)$Y2
  f1 <- logit_fit(hp$B, K1, lambda1, group = group, tau = tau, tau_links = 0)
  f2 <- lfm_fit(hp$Y2, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
  g2 <- lfm_fit(Y2s, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
  P1 <- logit_predict(f1); P2 <- lfm_predict(f2); P2s <- lfm_predict(g2)
  sm <- smearing(f2, hp$Y2); sms <- smearing(g2, Y2s)
  # 80% range of the amount if damaged, from the spread of the amount model's errors
  ok <- !is.na(hp$Y2) & !is.na(P2); sd2 <- stats::sd(hp$Y2[ok] - P2[ok])
  oks <- !is.na(Y2s) & !is.na(P2s); sds <- stats::sd(Y2s[oks] - P2s[oks])
  scaled <- !is.na(Lh$area) & !is.na(P2s)
  amount_log <- ifelse(scaled, log1p(pmax(expm1(P2s) * sms, 0) * Lh$area / 1000), log1p(pmax(expm1(P2) * sm, 0)))
  spread <- ifelse(scaled, sds, sd2)
  E <- host_combine(P1, P2s, sms, P2, sm, Lh$area, absent)
  # partner method on all data for the chosen insects
  H <- is.na(L$Y); H[, !L$species %in% partner_species] <- FALSE
  pm <- partner_predict(L$Y, H, L$rows, baseline_species_year(L$Y, L$rows$year))
  n <- nrow(L$Y); p <- ncol(L$Y); ix <- cbind(rep(seq_len(n), p), rep(seq_len(p), each = n))
  tibble::tibble(
    country = L$rows$country[ix[, 1]], nuts_id = L$rows$nuts_id[ix[, 1]], year = L$rows$year[ix[, 1]],
    species = L$species[ix[, 2]], y = L$Y[ix], absent = absent[ix], empty = is.na(P1[ix]),
    p_any = P1[ix], amount_log = amount_log[ix], spread = spread[ix], factor_fill = E[ix],
    partner_used = pm$used[ix], partner_fill = pmax(expm1(pm$P[ix]), 0), host_area_ha = Lh$area[ix]
  )
}

gapfill_dataset <- function(raw_fill, gf_species, all_species, unit_lookup) {
  z <- stats::qnorm(0.9)
  conf <- gf_species |>
    dplyr::filter(as.character(method) == as.character(best)) |>
    dplyr::transmute(species, method_used = as.character(best),
                     confidence = dplyr::if_else(gain_vs_simple > 0 & !is.na(auc) & auc >= 0.7, "reliable", "weak"))
  conf <- dplyr::bind_rows(conf, tibble::tibble(species = setdiff(all_species, conf$species),
                                                method_used = "factor model + forest area (amount only)",
                                                confidence = "untested"))
  raw_fill |>
    dplyr::left_join(conf, by = "species") |>
    dplyr::left_join(unit_lookup, by = "species") |>
    dplyr::mutate(
      use_partner = method_used == "partner method" & partner_used,
      cand = dplyr::if_else(use_partner, partner_fill, factor_fill),
      value_type = dplyr::case_when(
        !is.na(y) & y > 0 ~ "reported damage",
        !is.na(y) ~ "reported zero",
        absent ~ "zero: host tree absent",
        (empty & !use_partner) | is.na(cand) ~ "not fillable (nothing recorded in this region-year)",
        TRUE ~ "filled"),
      fl = value_type == "filled",
      fill_method = dplyr::case_when(!fl ~ NA_character_, use_partner ~ "partner method",
                                     TRUE ~ "factor model + forest area"),
      damage = dplyr::case_when(
        value_type %in% c("reported damage", "reported zero") ~ expm1(y),
        value_type == "zero: host tree absent" ~ 0,
        fl ~ cand),
      p_any = dplyr::if_else(fl & !use_partner, p_any, NA_real_),
      amount_if_damaged = dplyr::if_else(fl & !use_partner, expm1(amount_log), NA_real_),
      amount_lo80 = dplyr::if_else(fl & !use_partner, pmax(expm1(amount_log - z * spread), 0), NA_real_),
      amount_hi80 = dplyr::if_else(fl & !use_partner, expm1(amount_log + z * spread), NA_real_),
      confidence = dplyr::if_else(fl, confidence, NA_character_)) |>
    dplyr::select(country, nuts_id, year, species, unit, value_type, damage, fill_method, confidence,
                  p_any, amount_if_damaged, amount_lo80, amount_hi80, host_area_ha)
}

# Europe-wide totals per insect and year: reported, and filled by confidence
gapfill_totals <- function(gd) {
  gd |>
    dplyr::group_by(species, unit, year) |>
    dplyr::summarise(
      reported = sum(damage[value_type %in% c("reported damage", "reported zero")]),
      filled_reliable = sum(damage[value_type == "filled" & confidence == "reliable"]),
      filled_weak = sum(damage[value_type == "filled" & confidence == "weak"]),
      filled_untested = sum(damage[value_type == "filled" & confidence == "untested"]),
      cells_not_fillable = sum(grepl("^not fillable", value_type)),
      .groups = "drop") |>
    dplyr::mutate(total_reported_plus_reliable = reported + filled_reliable)
}
