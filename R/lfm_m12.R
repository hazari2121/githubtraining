# Factor model, steps M1 and M2 (Oct 2026). New functions alongside the
# existing ones (R/lfm.R, R/hurdle.R), so earlier results stay reproducible.
#
# M1 - region + year structure for the region-year intensity.
#   Before (M0): every region-year i has its own intensity c_i, pulled
#   towards 0 (ridge lambda_c = 1).
#   Now: c_i is pulled towards a_region + b_year,
#       penalty  lambda_rc * (c_i - a_r(i) - b_t(i))^2,
#   where a_r (region level: size, reporting style) and b_t (Europe-wide year)
#   are themselves estimated from all region-years (mild ridge, k0 = 1).
#   Region-years with few insects recorded then borrow their intensity from
#   their region's usual level and that year's Europe-wide level, instead of
#   from a handful of records. Same in the any-damage part (logit_fit_rs).
#
# M2 - more hidden factors, with each insect's sensitivities (loadings)
#   pulled towards its broad group's (tau_links > 0), so extra factors can
#   pick up insect-specific links without overfitting.
#
# Seeds fixed as in the originals.

region_year_prior <- function(cc, region, year, keep, k0 = 1, sweeps = 5) {
  a <- stats::setNames(rep(0, length(unique(region))), unique(region))
  b <- stats::setNames(rep(0, length(unique(year))), unique(year))
  r <- as.character(region); t <- as.character(year)
  for (s in seq_len(sweeps)) {
    ra <- tapply((cc - b[t])[keep], r[keep], sum); na <- tapply(rep(1, sum(keep)), r[keep], sum)
    a[names(ra)] <- ra / (na + k0)
    rb <- tapply((cc - a[r])[keep], t[keep], sum); nb <- tapply(rep(1, sum(keep)), t[keep], sum)
    b[names(rb)] <- rb / (nb + k0)
  }
  list(prior = unname(a[r] + b[t]), a = a, b = b)
}

lfm_fit_rs <- function(Y, K, lambda, region, year, lambda_rc = 5, n_iter = 200, tol = 1e-6,
                       seed = 20261009, group = NULL, tau = 0, tau_links = tau) {
  set.seed(seed)
  n <- nrow(Y); p <- ncol(Y); M <- !is.na(Y)
  mu <- colMeans(Y, na.rm = TRUE); mu[is.na(mu)] <- 0
  cc <- rep(0, n); pr <- rep(0, n)
  U <- matrix(0, n, K)
  V <- matrix(stats::rnorm(p * K, sd = 0.1), p, K)
  pat <- apply(M, 1, function(m) paste(which(m), collapse = ","))
  groups <- split(seq_len(n), pat)
  empty_rows <- which(rowSums(M) == 0); keep <- rowSums(M) > 0
  pen_row <- diag(c(lambda_rc, rep(lambda, K)), K + 1)
  pen_col <- diag(c(1e-8, rep(lambda, K)), K + 1)
  hier <- !is.null(group) && (tau > 0 || tau_links > 0)
  if (hier) {
    tvec <- c(tau, rep(tau_links, K))
    pen_col <- pen_col + diag(tvec, K + 1)
    gi <- split(seq_len(p), group)
  }
  prev <- Inf
  for (it in seq_len(n_iter)) {
    for (g in groups) {
      cols <- M[g[1], ]
      if (!any(cols)) next
      X <- cbind(1, V[cols, , drop = FALSE])
      R <- t(Y[g, cols, drop = FALSE]) - mu[cols]
      rhs <- crossprod(X, R); rhs[1, ] <- rhs[1, ] + lambda_rc * pr[g]   # pull towards region + year
      B <- solve(crossprod(X) + pen_row, rhs)
      cc[g] <- B[1, ]
      if (K > 0) U[g, ] <- t(B[-1, , drop = FALSE])
    }
    ry <- region_year_prior(cc, region, year, keep); pr <- ry$prior
    if (hier) {
      Th <- cbind(mu, V)
      for (gg in gi) Th[gg, ] <- matrix(colMeans(Th[gg, , drop = FALSE]), length(gg), K + 1, byrow = TRUE)
    }
    for (s in seq_len(p)) {
      rr <- M[, s]
      if (!any(rr)) next
      X <- cbind(1, U[rr, , drop = FALSE])
      rhs <- crossprod(X, Y[rr, s] - cc[rr])
      if (hier) rhs <- rhs + tvec * Th[s, ]
      b <- solve(crossprod(X) + pen_col, rhs)
      mu[s] <- b[1]
      if (K > 0) V[s, ] <- b[-1]
    }
    fit <- outer(cc, mu, "+") + U %*% t(V)
    loss <- mean((Y[M] - fit[M])^2)
    if (abs(prev - loss) < tol) break
    prev <- loss
  }
  list(mu = mu, c = cc, U = U, V = V, K = K, lambda = lambda, iter = it, a_region = ry$a, b_year = ry$b,
       train_rmse = sqrt(loss), empty_rows = empty_rows)
}

logit_fit_rs <- function(B, K, lambda, region, year, lambda_rb = 5, n_iter = 600, lr = 0.05,
                         seed = 20261012, group = NULL, tau = 0, tau_links = tau, update_every = 25) {
  set.seed(seed)
  M <- !is.na(B); Bz <- B; Bz[!M] <- 0
  n <- nrow(B); p <- ncol(B); keep <- rowSums(M) > 0
  rate <- pmin(pmax(colMeans(B, na.rm = TRUE), 0.01), 0.99); rate[is.na(rate)] <- 0.5
  pars <- list(a = stats::qlogis(rate), b = rep(0, n),
               U = matrix(stats::rnorm(n * K, sd = 0.01), n, K),
               V = matrix(stats::rnorm(p * K, sd = 0.01), p, K))
  pr <- rep(0, n)
  hier <- !is.null(group) && (tau > 0 || tau_links > 0)
  gmean <- function(x) { x <- as.matrix(x); for (g in unique(group)) { i <- group == g
    x[i, ] <- matrix(colMeans(x[i, , drop = FALSE]), sum(i), ncol(x), byrow = TRUE) }; x }
  m <- lapply(pars, function(x) x * 0); v <- m
  b1 <- 0.9; b2 <- 0.999; eps <- 1e-8
  for (t in seq_len(n_iter)) {
    if (t %% update_every == 0) pr <- region_year_prior(pars$b, region, year, keep)$prior
    eta <- outer(pars$b, pars$a, "+") + if (K > 0) pars$U %*% t(pars$V) else 0
    G <- (sigmoid(eta) - Bz) * M
    grads <- list(
      a = colSums(G),
      b = rowSums(G) + 2 * lambda_rb * (pars$b - pr),
      U = if (K > 0) G %*% pars$V + 2 * lambda * pars$U else pars$U,
      V = if (K > 0) t(G) %*% pars$U + 2 * lambda * pars$V else pars$V)
    if (hier) {
      grads$a <- grads$a + 2 * tau * (pars$a - as.numeric(gmean(pars$a)))
      if (K > 0) grads$V <- grads$V + 2 * tau_links * (pars$V - gmean(pars$V))
    }
    for (nm in names(pars)) {
      m[[nm]] <- b1 * m[[nm]] + (1 - b1) * grads[[nm]]
      v[[nm]] <- b2 * v[[nm]] + (1 - b2) * grads[[nm]]^2
      pars[[nm]] <- pars[[nm]] - lr * (m[[nm]] / (1 - b1^t)) / (sqrt(v[[nm]] / (1 - b2^t)) + eps)
    }
  }
  pars$c <- rep(0, p); pars$X <- matrix(0, n, p)
  pars$K <- K; pars$empty_rows <- which(rowSums(M) == 0)
  pars
}

# The final gap-filling model (as gapfill_final, factor model only) with the
# M1/M2 options. spec: list(K1, lambda1, K2, lambda2, tau, tau_links, lambda_rc)
# lambda_rc = 0 and tau_links = 0 reproduce the M0 structure (intensity
# pulled to 0 instead of region + year).
gapfill_final_rs <- function(L, hs, group, spec) {
  Lh <- host_scaled_L(L, hs); absent <- host_absent_matrix(L, hs)
  hp <- hurdle_parts(L$Y); Y2s <- hurdle_parts(Lh$Y)$Y2
  reg <- paste(L$rows$country, L$rows$nuts_id); yr <- L$rows$year
  lrc <- if (spec$lambda_rc > 0) spec$lambda_rc else 1
  if (spec$lambda_rc > 0) {
    f1 <- logit_fit_rs(hp$B, spec$K1, spec$lambda1, reg, yr, lambda_rb = spec$lambda_rc,
                       group = group, tau = spec$tau, tau_links = spec$tau_links)
    f2 <- lfm_fit_rs(hp$Y2, spec$K2, spec$lambda2, reg, yr, lambda_rc = spec$lambda_rc,
                     group = group, tau = spec$tau, tau_links = spec$tau_links)
    g2 <- lfm_fit_rs(Y2s, spec$K2, spec$lambda2, reg, yr, lambda_rc = spec$lambda_rc,
                     group = group, tau = spec$tau, tau_links = spec$tau_links)
  } else {
    f1 <- logit_fit(hp$B, spec$K1, spec$lambda1, group = group, tau = spec$tau, tau_links = spec$tau_links)
    f2 <- lfm_fit(hp$Y2, K = spec$K2, lambda = spec$lambda2, group = group, tau = spec$tau, tau_links = spec$tau_links)
    g2 <- lfm_fit(Y2s, K = spec$K2, lambda = spec$lambda2, group = group, tau = spec$tau, tau_links = spec$tau_links)
  }
  P1 <- logit_predict(f1); P2 <- lfm_predict(f2); P2s <- lfm_predict(g2)
  sm <- smearing(f2, hp$Y2); sms <- smearing(g2, Y2s)
  scaled <- !is.na(Lh$area) & !is.na(P2s)
  amount_log <- ifelse(scaled, log1p(pmax(expm1(P2s) * sms, 0) * Lh$area / 1000), log1p(pmax(expm1(P2) * sm, 0)))
  E <- host_combine(P1, P2s, sms, P2, sm, Lh$area, absent)
  list(P1 = P1, amount_log = amount_log, E = E, absent = absent, V2 = g2$V, V1 = f1$V)
}

# Hide-and-predict test of one model spec (same blocks as before)
variant_cv <- function(L, blocks, hs, group, spec) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ltr <- L; Ltr$Y[H] <- NA
    r <- gapfill_final_rs(Ltr, hs, group, spec)
    idx <- which(H, arr.ind = TRUE)
    out[[length(out) + 1]] <- tibble::tibble(
      fold = f, country = L$rows$country[idx[, 1]], species = L$species[idx[, 2]], year = L$rows$year[idx[, 1]],
      observed = L$Y[idx], pred = r$E[idx],
      pred_log = ifelse(r$absent[idx], 0, r$P1[idx] * r$amount_log[idx]))
  }
  dplyr::bind_rows(out)
}

variant_scores <- function(fc, name) {
  d <- dplyr::filter(fc, !is.na(pred))
  bt <- d |> dplyr::group_by(fold, country, species, year) |>
    dplyr::summarise(real = sum(expm1(observed)), pred = sum(pred), .groups = "drop") |>
    dplyr::filter(real > 0) |> dplyr::mutate(lr = abs(log((pred + 1) / (real + 1))))
  tibble::tibble(model = name, n_cells = nrow(d),
                 error_log = sqrt(mean((d$pred_log - d$observed)^2)),
                 damage_vs_none_auc = auc(d$pred_log, as.numeric(d$observed > 0)),
                 totals_within_2x = mean(bt$lr <= log(2)),
                 typical_total_error_x = exp(stats::median(bt$lr)))
}

# How many of the usable pairs does the model's amount part link (loading
# cosine >= 0.3), and how well do its links track the measured r?
variant_links <- function(L, hs, group, spec, verdict, name) {
  r <- gapfill_final_rs(L, hs, group, spec)
  V <- r$V2; rownames(V) <- L$species
  nv <- sqrt(rowSums(V^2)); C <- (V %*% t(V)) / (nv %o% nv)
  rm <- mapply(function(a, b) if (a %in% rownames(C) && b %in% rownames(C)) C[a, b] else NA,
               verdict$species_a, verdict$species_b)
  u <- verdict$verdict == "usable"
  tibble::tibble(model = name, usable_linked = sum(rm[u] >= 0.3, na.rm = TRUE), usable_n = sum(u),
                 agreement_with_r = stats::cor(verdict$r, rm, use = "pair"))
}

# Full-data fit of a spec, laid out like gapfill_final()'s output (one row per
# region-year x insect, column by column), so the v2 dataset functions apply.
gapfill_raw_rs <- function(L, hs, group, spec) {
  r <- gapfill_final_rs(L, hs, group, spec)
  Lh <- host_scaled_L(L, hs)
  n <- nrow(L$Y); p <- ncol(L$Y); ix <- cbind(rep(seq_len(n), p), rep(seq_len(p), each = n))
  tibble::tibble(
    country = L$rows$country[ix[, 1]], nuts_id = L$rows$nuts_id[ix[, 1]], year = L$rows$year[ix[, 1]],
    species = L$species[ix[, 2]], y = L$Y[ix], absent = r$absent[ix], empty = is.na(r$P1[ix]),
    p_any = r$P1[ix], amount_log = r$amount_log[ix], factor_fill = r$E[ix], host_area_ha = Lh$area[ix])
}
