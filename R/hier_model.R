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

# `settings`: one row per version, columns label, tau (level), tau_links.
hier_model_cv <- function(L, blocks, group, K1, lambda1, K2, lambda2,
                          settings = tibble::tibble(label = c("t0", "t50"), tau = c(0, 50), tau_links = c(0, 50))) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ytr <- L$Y; Ytr[H] <- NA
    hp <- hurdle_parts(Ytr)
    idx <- which(H, arr.ind = TRUE)
    base_sy <- baseline_species_year(Ytr, L$rows$year)
    for (k in seq_len(nrow(settings))) {
      st <- settings[k, ]
      f1 <- logit_fit(hp$B, K1, lambda1, group = group, tau = st$tau, tau_links = st$tau_links)
      f2 <- lfm_fit(hp$Y2, K = K2, lambda = lambda2, group = group, tau = st$tau, tau_links = st$tau_links)
      out[[length(out) + 1]] <- tibble::tibble(
        fold = f, version = st$label, tau = st$tau, tau_links = st$tau_links, row = idx[, 1], col = idx[, 2],
        country = L$rows$country[idx[, 1]], year = L$rows$year[idx[, 1]], species = L$species[idx[, 2]],
        observed = L$Y[idx], base_sy = base_sy[idx],
        p_any = logit_predict(f1)[idx], amount_log = lfm_predict(f2)[idx], smear = smearing(f2, hp$Y2))
    }
  }
  dplyr::bind_rows(out)
}

# Scores per tau on the hidden cells that every version could predict
hier_model_scores <- function(cvr) {
  if (!"version" %in% names(cvr)) cvr$version <- paste0("t", cvr$tau)
  ok <- cvr |> dplyr::group_by(fold, row, col) |>
    dplyr::filter(all(!is.na(p_any) & !is.na(amount_log))) |> dplyr::ungroup()
  d <- ok |> dplyr::mutate(real = expm1(observed), y1 = as.numeric(observed > 0),
                           pred = p_any * pmax(expm1(amount_log) * smear, 0),
                           two_log = p_any * amount_log)
  tot <- d |> dplyr::group_by(version, fold, country, species, year) |>
    dplyr::summarise(real = sum(real), pred = sum(pred), .groups = "drop") |>
    dplyr::filter(real > 0) |>
    dplyr::mutate(lr = abs(log((pred + 1) / (real + 1))))
  pos <- d$observed > 0
  overall <- d |> dplyr::group_by(version) |>
    dplyr::summarise(
      n_cells = dplyr::n(),
      auc_any = auc(p_any, y1),
      log_loss = -mean(y1 * log(pmin(pmax(p_any, 1e-4), 1 - 1e-4)) + (1 - y1) * log(1 - pmin(pmax(p_any, 1e-4), 1 - 1e-4))),
      rmse_amount = sqrt(mean((amount_log[observed > 0] - observed[observed > 0])^2)),
      zeros_called_zero = mean(p_any[observed == 0] < 0.5),
      .groups = "drop") |>
    dplyr::left_join(tot |> dplyr::group_by(version) |>
                       dplyr::summarise(totals_within_2x = mean(lr <= log(2)), typical_total_error_x = exp(stats::median(lr))),
                     by = "version")
  by_species <- d |> dplyr::group_by(version, species) |>
    dplyr::summarise(n_cells = dplyr::n(), auc_any = auc(p_any, y1),
                     rmse_two = sqrt(mean((two_log - observed)^2)),
                     rmse_base = sqrt(mean((base_sy - observed)^2, na.rm = TRUE)), .groups = "drop") |>
    dplyr::mutate(works = rmse_two < rmse_base & !is.na(auc_any) & auc_any >= 0.7)
  overall <- overall |> dplyr::left_join(by_species |> dplyr::group_by(version) |>
                                           dplyr::summarise(species_tested = dplyr::n(), species_works = sum(works, na.rm = TRUE)), by = "version")
  list(overall = overall, by_species = by_species)
}

# Stricter check (nested selection): for each test fold, the hint strength is
# chosen using only the OTHER folds' hidden cells (part 1 by log loss, part 2 by
# RMSE), then scored on the test fold. The test fold's values are never used to
# pick the strength. Returns the combined predictions in the same layout as
# hier_model_cv(), labelled "nested".
hier_nested_select <- function(cvr, strengths) {
  cv <- cvr |> dplyr::filter(version %in% strengths)
  ll <- function(p, y) { p <- pmin(pmax(p, 1e-4), 1 - 1e-4); -mean(y * log(p) + (1 - y) * log(1 - p)) }
  purrr::map_dfr(sort(unique(cv$fold)), function(f) {
    inner <- dplyr::filter(cv, fold != f, !is.na(p_any))
    s1 <- inner |> dplyr::group_by(version) |> dplyr::summarise(v = ll(p_any, as.numeric(observed > 0)))
    s2 <- inner |> dplyr::filter(observed > 0, !is.na(amount_log)) |> dplyr::group_by(version) |>
      dplyr::summarise(v = sqrt(mean((amount_log - observed)^2)))
    v1 <- s1$version[which.min(s1$v)]; v2 <- s2$version[which.min(s2$v)]
    a <- dplyr::filter(cv, fold == f, version == v1)
    b <- dplyr::filter(cv, fold == f, version == v2)
    a |> dplyr::mutate(amount_log = b$amount_log[match(paste(row, col), paste(b$row, b$col))],
                       smear = b$smear[1], version = "nested", chosen_part1 = v1, chosen_part2 = v2)
  })
}
