# Hybrid gap-filler (Oct 2026): the notes' equation log(B+1) = a + b log(A+1),
# with the level problem fixed by the factor model.
#
#   log(B+1)[i] = level_B,region  +  sum_A w_A * b_A * ( log(A+1)[i] - normal_A,region )
#                                    -----------------------------------------------
#                                                    sum_A w_A
#   level_B,region  : the factor model's average prediction for B in that
#                     region (B is never recorded there, so its level must
#                     come from the other insects, via the factor model)
#   normal_A,region : A's own average in that region (A IS recorded there)
#   b_A             : within-region slope of B on A, i.e. each region against
#                     its own mean, learnt from regions where both are recorded
#   w_A = r_A^2     : partners weighted by the strength of the within-region link
# Partners as in the partner method: within-region r >= 0.3, >= 20 shared
# region-years, link seen in >= 2 countries, A recorded in that region-year.
# No partner available -> the factor model's prediction is kept.
# Natural scale: the factor model's expected damage, shifted by the same
# amount on the log scale (keeps its any-damage probability and smearing).
#
# Uses the same hidden blocks and the same factor-model predictions as the
# method test (gf_cv), so the comparison is like for like. No randomness.

hybrid_from_cv <- function(L, blocks, cvr, min_r = 0.3, min_n = 20, min_countries = 2) {
  reg <- paste(L$rows$country, L$rows$nuts_id)
  out <- list()
  for (f in sort(unique(cvr$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ytr <- L$Y; Ytr[H] <- NA
    # each region's own mean per insect (training data), and values minus it
    RM <- apply(Ytr, 2, function(y) stats::ave(y, reg, FUN = function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)))
    C <- Ytr - RM
    cf <- dplyr::filter(cvr, fold == f)
    # factor-model level of B in each hidden region (mean of its predictions there)
    cf <- cf |>
      dplyr::mutate(region = reg[row]) |>
      dplyr::group_by(region, col) |>
      dplyr::mutate(level = mean(`factor model_log`)) |>
      dplyr::ungroup()
    num <- den <- rep(0, nrow(cf))
    for (s in unique(cf$col)) {
      ii <- which(cf$col == s); rows <- cf$row[ii]
      for (p in setdiff(seq_len(ncol(Ytr)), s)) {
        ok <- !is.na(C[, s]) & !is.na(C[, p])
        if (sum(ok) < min_n || dplyr::n_distinct(L$rows$country[ok]) < min_countries) next
        if (stats::sd(C[ok, s]) == 0 || stats::sd(C[ok, p]) == 0) next
        r <- stats::cor(C[ok, s], C[ok, p])
        if (r < min_r) next
        b <- stats::cov(C[ok, s], C[ok, p]) / stats::var(C[ok, p])
        anom <- C[rows, p]                       # A this year minus A's normal in that region
        has <- !is.na(anom)
        num[ii[has]] <- num[ii[has]] + r^2 * b * anom[has]
        den[ii[has]] <- den[ii[has]] + r^2
      }
    }
    got <- den > 0
    h_log <- ifelse(got, pmax(cf$level + num / pmax(den, 1e-12), 0), cf$`factor model_log`)
    out[[length(out) + 1]] <- cf |>
      dplyr::transmute(fold, row, col,
                       hybrid_used = got,
                       `hybrid_log` = h_log,
                       hybrid = `factor model` * exp(h_log - `factor model_log`))
  }
  dplyr::bind_rows(out) |>
    dplyr::rename(`hybrid (factor level + partner ups and downs)_log` = hybrid_log,
                  `hybrid (factor level + partner ups and downs)` = hybrid)
}

# Blend (variant): where the partner method has a partner, mix its log
# prediction into the factor model's: log = (1 - w) * factor + w * partner.
# w = 0.25 was picked from two tried values (0.25, 0.5) on these same hidden
# cells, so its score is slightly optimistic until checked in a nested test.
blend_from_cv <- function(cvr, w = 0.25) {
  cvr |>
    dplyr::transmute(fold, row, col,
      `blend: factor model + 25% notes equation_log` =
        ifelse(partner_used, (1 - w) * `factor model_log` + w * `partner method_log`, `factor model_log`),
      `blend: factor model + 25% notes equation` =
        `factor model` * exp(`blend: factor model + 25% notes equation_log` - `factor model_log`))
}
