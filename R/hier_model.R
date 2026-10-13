# Option 3 (soft hierarchy, "guild hint") vs the plain two-part model.
#
# Both are the two-part factor model of R/hurdle.R (part 1: any damage?
# part 2: how much, if damaged?), with the same sizes (K, lambda) chosen
# earlier. The only difference: with the hint, each species' level and
# loadings are pulled towards the average of its broad group (Hlasny et al.
# 2025's four groups: borers / defoliators on conifers / broadleaves) with
# strength tau. tau = 0 is the plain model.
# Test: the same hidden country x species blocks as before (R/lfm.R), so every
# version predicts exactly the same hidden cells.

hier_model_cv <- function(L, blocks, group, K1, lambda1, K2, lambda2, taus = c(0, 5, 20, 50)) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ytr <- L$Y; Ytr[H] <- NA
    hp <- hurdle_parts(Ytr)
    idx <- which(H, arr.ind = TRUE)
    base_sy <- baseline_species_year(Ytr, L$rows$year)
    for (tau in taus) {
      f1 <- logit_fit(hp$B, K1, lambda1, group = group, tau = tau)
      f2 <- lfm_fit(hp$Y2, K = K2, lambda = lambda2, group = group, tau = tau)
      out[[length(out) + 1]] <- tibble::tibble(
        fold = f, tau = tau, row = idx[, 1], col = idx[, 2],
        country = L$rows$country[idx[, 1]], year = L$rows$year[idx[, 1]], species = L$species[idx[, 2]],
        observed = L$Y[idx], base_sy = base_sy[idx],
        p_any = logit_predict(f1)[idx], amount_log = lfm_predict(f2)[idx], smear = smearing(f2, hp$Y2))
    }
  }
  dplyr::bind_rows(out)
}

# Scores per tau on the hidden cells that every version could predict
hier_model_scores <- function(cvr) {
  ok <- cvr |> dplyr::group_by(fold, row, col) |>
    dplyr::filter(all(!is.na(p_any) & !is.na(amount_log))) |> dplyr::ungroup()
  d <- ok |> dplyr::mutate(real = expm1(observed), y1 = as.numeric(observed > 0),
                           pred = p_any * pmax(expm1(amount_log) * smear, 0),
                           two_log = p_any * amount_log)
  tot <- d |> dplyr::group_by(tau, fold, country, species, year) |>
    dplyr::summarise(real = sum(real), pred = sum(pred), .groups = "drop") |>
    dplyr::filter(real > 0) |>
    dplyr::mutate(lr = abs(log((pred + 1) / (real + 1))))
  pos <- d$observed > 0
  overall <- d |> dplyr::group_by(tau) |>
    dplyr::summarise(
      n_cells = dplyr::n(),
      auc_any = auc(p_any, y1),
      log_loss = -mean(y1 * log(pmin(pmax(p_any, 1e-4), 1 - 1e-4)) + (1 - y1) * log(1 - pmin(pmax(p_any, 1e-4), 1 - 1e-4))),
      rmse_amount = sqrt(mean((amount_log[observed > 0] - observed[observed > 0])^2)),
      zeros_called_zero = mean(p_any[observed == 0] < 0.5),
      .groups = "drop") |>
    dplyr::left_join(tot |> dplyr::group_by(tau) |>
                       dplyr::summarise(totals_within_2x = mean(lr <= log(2)), typical_total_error_x = exp(stats::median(lr))),
                     by = "tau")
  by_species <- d |> dplyr::group_by(tau, species) |>
    dplyr::summarise(n_cells = dplyr::n(), auc_any = auc(p_any, y1),
                     rmse_two = sqrt(mean((two_log - observed)^2)),
                     rmse_base = sqrt(mean((base_sy - observed)^2, na.rm = TRUE)), .groups = "drop") |>
    dplyr::mutate(works = rmse_two < rmse_base & !is.na(auc_any) & auc_any >= 0.7)
  overall <- overall |> dplyr::left_join(by_species |> dplyr::group_by(tau) |>
                                           dplyr::summarise(species_tested = dplyr::n(), species_works = sum(works, na.rm = TRUE)), by = "tau") |>
    dplyr::mutate(model = dplyr::if_else(tau == 0, "plain (no hint)", paste0("guild hint, strength ", tau)), .before = 1)
  list(overall = overall, by_species = by_species)
}
