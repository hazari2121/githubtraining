# Latent factor model (prototype) for harmonising / gap-filling EU-ForDAM.
#
# Data matrix Y: rows = region-years (all NUTS units x 2000-2022),
# columns = species; values = log(damage + 1) for observed cells (zeros are real
# observations: monitored, no damage), NA where not monitored.
#
# Model (low-rank matrix factorisation with biases):
#   y[i, s] = mu[s] + c[i] + sum_k U[i, k] * V[s, k] + error
#   mu[s] : species level             c[i] : overall damage intensity of region-year i
#   U[i,] : region-year scores on K hidden factors
#   V[s,] : how strongly species s responds to each factor
# Fitted by alternating ridge regressions (ALS) on observed cells only.
# K = 0 gives the purely additive model (region-year intensity + species level).
#
# A gap cell (i, s) is predicted from region-year i's factor scores, estimated
# from the OTHER species recorded there, and species s's loadings, estimated
# from regions where s IS recorded.

build_lfm_matrix <- function(cells, tr) {
  rows <- cells |> dplyr::distinct(country, nuts_id, year) |> dplyr::arrange(country, nuts_id, year)
  sp <- sort(unique(tr$species))
  Y <- matrix(NA_real_, nrow(rows), length(sp), dimnames = list(NULL, sp))
  key <- paste(rows$nuts_id, rows$year)
  idx_r <- match(paste(tr$nuts_id, tr$year), key)
  idx_c <- match(tr$species, sp)
  Y[cbind(idx_r, idx_c)] <- tr$log_damage
  list(Y = Y, rows = rows, species = sp)
}

# Fit the model on the observed (non-NA) cells of Y.
lfm_fit <- function(Y, K, lambda, lambda_c = 1, n_iter = 200, tol = 1e-6, seed = 20261009) {
  set.seed(seed)
  n <- nrow(Y); p <- ncol(Y); M <- !is.na(Y)
  mu <- colMeans(Y, na.rm = TRUE); mu[is.na(mu)] <- 0
  cc <- rep(0, n)
  U <- matrix(0, n, K)
  V <- matrix(stats::rnorm(p * K, sd = 0.1), p, K)
  # rows sharing the same set of observed species are solved together
  pat <- apply(M, 1, function(m) paste(which(m), collapse = ","))
  groups <- split(seq_len(n), pat)
  empty_rows <- which(rowSums(M) == 0)
  pen_row <- diag(c(lambda_c, rep(lambda, K)), K + 1)
  pen_col <- diag(c(1e-8, rep(lambda, K)), K + 1)
  prev <- Inf
  for (it in seq_len(n_iter)) {
    for (g in groups) {
      cols <- M[g[1], ]
      if (!any(cols)) next
      X <- cbind(1, V[cols, , drop = FALSE])
      R <- t(Y[g, cols, drop = FALSE]) - mu[cols]
      B <- solve(crossprod(X) + pen_row, crossprod(X, R))
      cc[g] <- B[1, ]
      if (K > 0) U[g, ] <- t(B[-1, , drop = FALSE])
    }
    for (s in seq_len(p)) {
      rr <- M[, s]
      if (!any(rr)) next
      X <- cbind(1, U[rr, , drop = FALSE])
      b <- solve(crossprod(X) + pen_col, crossprod(X, Y[rr, s] - cc[rr]))
      mu[s] <- b[1]
      if (K > 0) V[s, ] <- b[-1]
    }
    fit <- outer(cc, mu, "+") + U %*% t(V)
    loss <- mean((Y[M] - fit[M])^2)
    if (abs(prev - loss) < tol) break
    prev <- loss
  }
  list(mu = mu, c = cc, U = U, V = V, K = K, lambda = lambda, iter = it,
       train_rmse = sqrt(loss), empty_rows = empty_rows)
}

lfm_predict <- function(fit) {
  P <- outer(fit$c, fit$mu, "+") + fit$U %*% t(fit$V)
  P[fit$empty_rows, ] <- NA  # no information about these region-years
  P
}

# ---- Cross-validation: hide whole country x species blocks -----------------
# Eligible block: the country records the species (usable series) and at least
# one other species, so the region-years still carry information, and the
# species is recorded in at least one other country, so there is something
# to learn its behaviour from (species recorded in one country cannot be tested).
# Blocks are spread round-robin over folds within each country, so a country
# never loses all its species in one fold.
make_cv_blocks <- function(L, n_folds = 10, seed = 20261009) {
  set.seed(seed)
  obs <- which(!is.na(L$Y), arr.ind = TRUE)
  blocks <- tibble::tibble(country = L$rows$country[obs[, 1]], species = L$species[obs[, 2]]) |>
    dplyr::distinct()
  n_sp <- blocks |> dplyr::count(country, name = "n_species_country")
  n_ct <- blocks |> dplyr::count(species, name = "n_countries_species")
  blocks |>
    dplyr::left_join(n_sp, by = "country") |>
    dplyr::left_join(n_ct, by = "species") |>
    dplyr::filter(n_species_country >= 2, n_countries_species >= 2) |>
    dplyr::group_by(country) |>
    dplyr::mutate(fold = ((sample.int(n_folds, 1) + sample(dplyr::n())) %% n_folds) + 1) |>
    dplyr::ungroup()
}

held_mask <- function(L, blocks_f) {
  H <- matrix(FALSE, nrow(L$Y), ncol(L$Y))
  for (j in seq_len(nrow(blocks_f))) {
    r <- L$rows$country == blocks_f$country[j]
    s <- match(blocks_f$species[j], L$species)
    H[r, s] <- !is.na(L$Y[r, s])
  }
  H
}

# Baselines, using training data only
baseline_species_mean <- function(Ytr) matrix(colMeans(Ytr, na.rm = TRUE), nrow(Ytr), ncol(Ytr), byrow = TRUE)

baseline_species_year <- function(Ytr, year) {
  P <- baseline_species_mean(Ytr)
  for (y in unique(year)) {
    r <- year == y
    m <- colMeans(Ytr[r, , drop = FALSE], na.rm = TRUE)
    P[r, ] <- matrix(ifelse(is.na(m), P[which(r)[1], ], m), sum(r), ncol(Ytr), byrow = TRUE)
  }
  P
}

# Best single helper: for each hidden block (country c, species s), choose the
# species recorded in c whose training correlation with s (other countries,
# >= 30 shared region-years) is highest; predict by linear regression.
# Falls back to the species-year baseline where the helper is missing.
baseline_best_helper <- function(Ytr, rows, blocks_f, species, fallback) {
  P <- fallback
  for (j in seq_len(nrow(blocks_f))) {
    in_c <- rows$country == blocks_f$country[j]
    s <- match(blocks_f$species[j], species)
    cand <- setdiff(which(colSums(!is.na(Ytr[in_c, , drop = FALSE])) > 0), s)
    best <- NULL; best_r <- -Inf
    for (h in cand) {
      ok <- !in_c & !is.na(Ytr[, s]) & !is.na(Ytr[, h])
      if (sum(ok) < 30 || stats::sd(Ytr[ok, h]) == 0 || stats::sd(Ytr[ok, s]) == 0) next
      r <- stats::cor(Ytr[ok, s], Ytr[ok, h])
      if (r > best_r) { best_r <- r; best <- list(h = h, fit = stats::lm(Ytr[ok, s] ~ Ytr[ok, h])) }
    }
    if (is.null(best)) next
    tgt <- in_c & !is.na(Ytr[, best$h])
    P[tgt, s] <- stats::coef(best$fit)[1] + stats::coef(best$fit)[2] * Ytr[tgt, best$h]
  }
  P
}

# Run CV for all model configurations and baselines. Returns one row per
# held-out cell and model.
lfm_cv <- function(L, blocks, grid) {
  folds <- sort(unique(blocks$fold))
  out <- list()
  for (f in folds) {
    bf <- dplyr::filter(blocks, fold == f)
    H <- held_mask(L, bf)
    Ytr <- L$Y; Ytr[H] <- NA
    idx <- which(H, arr.ind = TRUE)
    cell <- tibble::tibble(fold = f, row = idx[, 1], col = idx[, 2],
                           country = L$rows$country[idx[, 1]], nuts_id = L$rows$nuts_id[idx[, 1]],
                           year = L$rows$year[idx[, 1]], species = L$species[idx[, 2]],
                           observed = L$Y[idx])
    sy <- baseline_species_year(Ytr, L$rows$year)
    preds <- list(
      "Baseline: species average" = baseline_species_mean(Ytr),
      "Baseline: species x year average" = sy,
      "Baseline: best single helper" = baseline_best_helper(Ytr, L$rows, bf, L$species, sy)
    )
    for (g in seq_len(nrow(grid))) {
      fit <- lfm_fit(Ytr, K = grid$K[g], lambda = grid$lambda[g])
      preds[[grid$model[g]]] <- lfm_predict(fit)
    }
    for (m in names(preds)) out[[length(out) + 1]] <- dplyr::mutate(cell, model = m, predicted = preds[[m]][idx])
  }
  dplyr::bind_rows(out)
}

# Scores: RMSE on log(damage + 1); "dynamics r" = mean correlation between
# predicted and observed over time within each held-out region-species series
# (>= 6 years); skill = 1 - MSE / MSE of the species x year baseline.
lfm_scores <- function(cv, by = character()) {
  # compare models on the same cells: those every model could predict
  common <- cv |> dplyr::group_by(fold, row, col) |>
    dplyr::summarise(ok = all(!is.na(predicted)), .groups = "drop") |> dplyr::filter(ok)
  cv <- dplyr::semi_join(cv, common, by = c("fold", "row", "col"))
  ser <- cv |>
    dplyr::filter(!is.na(predicted)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model", by))), nuts_id, species) |>
    dplyr::summarise(r = if (dplyr::n() >= 6 && stats::sd(predicted) > 0 && stats::sd(observed) > 0)
                       stats::cor(predicted, observed) else NA_real_, .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model", by)))) |>
    dplyr::summarise(dynamics_r = mean(r, na.rm = TRUE), n_series = sum(!is.na(r)), .groups = "drop")
  base <- cv |>
    dplyr::filter(model == "Baseline: species x year average", !is.na(predicted)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(mse_base = mean((predicted - observed)^2), .groups = "drop")
  sc <- cv |>
    dplyr::filter(!is.na(predicted)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model", by)))) |>
    dplyr::summarise(n_cells = dplyr::n(), rmse = sqrt(mean((predicted - observed)^2)),
                     mse = mean((predicted - observed)^2), bias = mean(predicted - observed),
                     .groups = "drop") |>
    dplyr::left_join(ser, by = c("model", by))
  if (length(by)) sc <- dplyr::left_join(sc, base, by = by) else sc$mse_base <- base$mse_base
  sc |> dplyr::mutate(skill_vs_year_baseline = 1 - mse / mse_base) |> dplyr::select(-mse, -mse_base)
}

# Final fit on all data with the chosen configuration; factors rotated with
# varimax for interpretation (rotation does not change predictions).
lfm_final <- function(L, K, lambda) {
  fit <- lfm_fit(L$Y, K = K, lambda = lambda)
  if (K >= 2) {
    vm <- stats::varimax(fit$V, normalize = FALSE)
    fit$V <- fit$V %*% vm$rotmat
    fit$U <- fit$U %*% vm$rotmat
  }
  # orient each factor so that its largest loading is positive
  if (K >= 1) for (k in seq_len(K)) {
    sg <- sign(fit$V[which.max(abs(fit$V[, k])), k])
    fit$V[, k] <- fit$V[, k] * sg; fit$U[, k] <- fit$U[, k] * sg
  }
  fit
}

lfm_loadings <- function(fit, L, species_meta) {
  tibble::as_tibble(stats::setNames(as.data.frame(fit$V), paste0("F", seq_len(fit$K)))) |>
    dplyr::mutate(species = L$species, species_level = fit$mu) |>
    dplyr::left_join(dplyr::select(species_meta, species, guild = guild_from_file, host_tree), by = "species") |>
    dplyr::relocate(species, guild, host_tree)
}

lfm_scores_by_year <- function(fit, L) {
  tibble::as_tibble(stats::setNames(as.data.frame(fit$U), paste0("F", seq_len(fit$K)))) |>
    dplyr::bind_cols(L$rows, intensity = fit$c) |>
    dplyr::filter(!seq_len(nrow(L$rows)) %in% fit$empty_rows)
}

# Gap-filled table: every not-monitored cell in a region-year that records at
# least one other species. PROTOTYPE: host presence is not yet checked, so a
# filled value may refer to a species whose host tree is absent.
lfm_fill <- function(fit, L) {
  P <- lfm_predict(fit)
  gap <- which(is.na(L$Y) & !is.na(P), arr.ind = TRUE)
  tibble::tibble(country = L$rows$country[gap[, 1]], nuts_id = L$rows$nuts_id[gap[, 1]],
                 year = L$rows$year[gap[, 1]], species = L$species[gap[, 2]],
                 log_damage_pred = P[gap], damage_pred = pmax(expm1(P[gap]), 0))
}

# Pick example hidden series for plotting: for a spread of species, the series
# with the median dynamics r (typical case, not cherry-picked).
lfm_example_pick <- function(cv, best_model, n = 5) {
  cv |>
    dplyr::filter(model == best_model, !is.na(predicted)) |>
    dplyr::group_by(country, nuts_id, species) |>
    dplyr::filter(dplyr::n() >= 15) |>
    dplyr::summarise(r = stats::cor(predicted, observed), .groups = "drop") |>
    dplyr::filter(species %in% c("pha_cya", "ips_typ", "tom_spp", "lym_mon", "pit_spp", "geo_db")) |>
    dplyr::group_by(species) |>
    dplyr::arrange(r, .by_group = TRUE) |>
    dplyr::slice(ceiling(dplyr::n() / 2)) |>
    dplyr::ungroup() |>
    dplyr::slice_head(n = n) |>
    dplyr::transmute(nuts_id, species, title = paste0(species, ", ", nuts_id, " (", country, ")"))
}

