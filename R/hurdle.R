# Two-part (hurdle) latent factor model for gap-filling.
#
# 64% of recorded values are zeros (0 = monitored, no damage), so each gap is
# split into two questions:
#   Part 1  "Any damage?"   P(damage > 0) from a logistic factor model fitted to
#                           all recorded values (1 = damage, 0 = recorded zero).
#   Part 2  "How much?"     log(damage + 1) given damage > 0, from the linear
#                           factor model (R/lfm.R) fitted to positive values only.
#   Combined: expected damage = P(any) x expected amount if damaged.
#
# Both parts share the structure of R/lfm.R: insect level + region-year
# intensity + region-year scores x insect loadings on K hidden factors.

sigmoid <- function(x) 1 / (1 + exp(-x))

# Logistic low-rank model fitted by gradient descent (Adam) on recorded cells.
#   logit P[i, s] = a[s] + b[i] + U[i, ] . V[s, ]
# Ridge penalties: lambda on U and V, lambda_b on the row intensity b.
# Optional covariate matrix X (same shape as B, e.g. log host area) enters with
# one slope per insect: + c[s] * X[i, s] (ridge penalty 1 on c).
logit_fit <- function(B, K, lambda, lambda_b = 1, n_iter = 600, lr = 0.05, seed = 20261012, X = NULL) {
  set.seed(seed)
  M <- !is.na(B); Bz <- B; Bz[!M] <- 0
  n <- nrow(B); p <- ncol(B)
  rate <- pmin(pmax(colMeans(B, na.rm = TRUE), 0.01), 0.99); rate[is.na(rate)] <- 0.5
  a <- stats::qlogis(rate); b <- rep(0, n)
  U <- matrix(stats::rnorm(n * K, sd = 0.01), n, K)
  V <- matrix(stats::rnorm(p * K, sd = 0.01), p, K)
  # Adam state
  pars <- list(a = a, b = b, U = U, V = V, c = rep(0, p))
  if (is.null(X)) X <- matrix(0, n, p)
  m <- lapply(pars, function(x) x * 0); v <- m
  b1 <- 0.9; b2 <- 0.999; eps <- 1e-8
  for (t in seq_len(n_iter)) {
    eta <- outer(pars$b, pars$a, "+") + X * rep(pars$c, each = n) + if (K > 0) pars$U %*% t(pars$V) else 0
    G <- (sigmoid(eta) - Bz) * M                     # d loss / d eta on recorded cells
    grads <- list(
      a = colSums(G),
      b = rowSums(G) + 2 * lambda_b * pars$b,
      U = if (K > 0) G %*% pars$V + 2 * lambda * pars$U else pars$U,
      V = if (K > 0) t(G) %*% pars$U + 2 * lambda * pars$V else pars$V,
      c = colSums(G * X) + 2 * pars$c
    )
    for (nm in names(pars)) {
      m[[nm]] <- b1 * m[[nm]] + (1 - b1) * grads[[nm]]
      v[[nm]] <- b2 * v[[nm]] + (1 - b2) * grads[[nm]]^2
      pars[[nm]] <- pars[[nm]] - lr * (m[[nm]] / (1 - b1^t)) / (sqrt(v[[nm]] / (1 - b2^t)) + eps)
    }
  }
  pars$K <- K; pars$X <- X; pars$empty_rows <- which(rowSums(M) == 0)
  pars
}

logit_predict <- function(fit) {
  n <- length(fit$b)
  P <- sigmoid(outer(fit$b, fit$a, "+") + fit$X * rep(fit$c, each = n) + if (fit$K > 0) fit$U %*% t(fit$V) else 0)
  P[fit$empty_rows, ] <- NA
  P
}

# Split the log(damage + 1) matrix into the two parts
hurdle_parts <- function(Y) {
  B <- ifelse(is.na(Y), NA_real_, as.numeric(Y > 0))
  Y2 <- Y; Y2[!is.na(Y2) & Y2 <= 0] <- NA
  list(B = B, Y2 = Y2)
}

# Smearing factor (Duan) to turn a mean of log(x + 1) into a mean of x:
# mean of exp(residuals) of part 2 on its training cells.
smearing <- function(fit2, Y2) {
  P <- lfm_predict(fit2); ok <- !is.na(Y2) & !is.na(P)
  mean(exp(Y2[ok] - P[ok]))
}

# Cross-validation on the same hidden country x insect blocks as R/lfm.R.
# Returns one row per hidden cell with predictions from every candidate.
hurdle_cv <- function(L, blocks, grid1, grid2, one_part) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    bf <- dplyr::filter(blocks, fold == f)
    H <- held_mask(L, bf)
    Ytr <- L$Y; Ytr[H] <- NA
    hp <- hurdle_parts(Ytr)
    idx <- which(H, arr.ind = TRUE)
    base <- tibble::tibble(fold = f, row = idx[, 1], col = idx[, 2],
                           country = L$rows$country[idx[, 1]], nuts_id = L$rows$nuts_id[idx[, 1]],
                           year = L$rows$year[idx[, 1]], species = L$species[idx[, 2]],
                           observed = L$Y[idx])
    # baselines for "any damage?": insect rate, insect x year rate (training data)
    rate_s <- matrix(colMeans(hp$B, na.rm = TRUE), nrow(Ytr), ncol(Ytr), byrow = TRUE)
    rate_sy <- baseline_species_year(hp$B, L$rows$year)
    p1 <- list(`P1 baseline: insect rate` = rate_s, `P1 baseline: insect x year rate` = rate_sy)
    for (g in seq_len(nrow(grid1))) p1[[grid1$model[g]]] <- logit_predict(logit_fit(hp$B, grid1$K[g], grid1$lambda[g]))
    p2 <- list(`P2 baseline: insect x year mean` = baseline_species_year(hp$Y2, L$rows$year))
    sm <- list(`P2 baseline: insect x year mean` = 1)
    for (g in seq_len(nrow(grid2))) {
      fit2 <- lfm_fit(hp$Y2, K = grid2$K[g], lambda = grid2$lambda[g])
      p2[[grid2$model[g]]] <- lfm_predict(fit2)
      sm[[grid2$model[g]]] <- smearing(fit2, hp$Y2)
    }
    one <- lfm_predict(lfm_fit(Ytr, K = one_part$K, lambda = one_part$lambda))
    base_sy <- baseline_species_year(Ytr, L$rows$year)   # one-part insect x year baseline
    rows <- base |> dplyr::mutate(one_part = one[idx], base_sy = base_sy[idx])
    for (nm in names(p1)) rows[[paste0("p1|", nm)]] <- p1[[nm]][idx]
    for (nm in names(p2)) { rows[[paste0("p2|", nm)]] <- p2[[nm]][idx]; rows[[paste0("sm|", nm)]] <- sm[[nm]] }
    out[[length(out) + 1]] <- rows
  }
  dplyr::bind_rows(out)
}

# Scores for each part, and choice of the best configuration of each
auc <- function(p, y) {
  ok <- !is.na(p) & !is.na(y); p <- p[ok]; y <- y[ok]
  if (length(unique(y)) < 2) return(NA_real_)
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

hurdle_part_scores <- function(hcv) {
  y1 <- as.numeric(hcv$observed > 0)
  s1 <- purrr::map_dfr(grep("^p1\\|", names(hcv), value = TRUE), function(cl) {
    p <- pmin(pmax(hcv[[cl]], 1e-4), 1 - 1e-4); ok <- !is.na(p)
    tibble::tibble(part = "1: any damage?", model = sub("^p1\\|", "", cl),
                   auc = auc(p, y1), brier = mean((p[ok] - y1[ok])^2),
                   log_loss = -mean(y1[ok] * log(p[ok]) + (1 - y1[ok]) * log(1 - p[ok])))
  })
  pos <- hcv$observed > 0
  s2 <- purrr::map_dfr(grep("^p2\\|", names(hcv), value = TRUE), function(cl) {
    e <- hcv[[cl]][pos] - hcv$observed[pos]; ok <- !is.na(e)
    tibble::tibble(part = "2: how much, if damaged?", model = sub("^p2\\|", "", cl),
                   rmse_log = sqrt(mean(e[ok]^2)))
  })
  dplyr::bind_rows(s1, s2)
}

# Combined comparison, two-part vs one-part, on the same hidden cells:
#  - RMSE of log(damage + 1): two-part uses P(any) x predicted log amount,
#    which is the expected value of log(damage + 1)
#  - zero calls: share of truly zero cells predicted as "probably no damage"
#  - block totals: |log ratio| of predicted vs real total damage per hidden
#    country x insect x year (what Europe-wide totals depend on)
hurdle_compare <- function(hcv, best1, best2) {
  p <- hcv[[paste0("p1|", best1)]]; a <- hcv[[paste0("p2|", best2)]]; sm <- hcv[[paste0("sm|", best2)]]
  d <- hcv |>
    dplyr::mutate(
      p_any = p, amount_log = a,
      two_part_log = p * a,
      two_part_damage = p * pmax(expm1(a) * sm, 0),
      one_part_damage = pmax(expm1(one_part), 0),
      real_damage = expm1(observed)
    ) |>
    dplyr::filter(!is.na(p_any), !is.na(amount_log), !is.na(one_part), !is.na(base_sy))
  tot <- d |>
    dplyr::group_by(fold, country, species, year) |>
    dplyr::summarise(real = sum(real_damage), two = sum(two_part_damage), one = sum(one_part_damage), .groups = "drop") |>
    dplyr::filter(real > 0)
  list(
    cells = d,
    summary = tibble::tibble(
      model = c("One-part factor model", "Two-part factor model"),
      rmse_log = c(sqrt(mean((d$one_part - d$observed)^2)), sqrt(mean((d$two_part_log - d$observed)^2))),
      zeros_called_zero = c(mean(d$one_part[d$observed == 0] < log1p(1)),
                            mean(d$p_any[d$observed == 0] < 0.5)),
      median_abs_log_ratio_totals = c(stats::median(abs(log((tot$one + 1) / (tot$real + 1)))),
                                      stats::median(abs(log((tot$two + 1) / (tot$real + 1))))),
      share_totals_within_2x = c(mean(abs(log((tot$one + 1) / (tot$real + 1))) <= log(2)),
                                 mean(abs(log((tot$two + 1) / (tot$real + 1))) <= log(2)))
    ),
    by_species = d |>
      dplyr::group_by(species) |>
      dplyr::summarise(
        n_cells = dplyr::n(),
        auc_any = auc(p_any, as.numeric(observed > 0)),
        rmse_two = sqrt(mean((two_part_log - observed)^2)),
        rmse_one = sqrt(mean((one_part - observed)^2)),
        rmse_base = sqrt(mean((base_sy - observed)^2, na.rm = TRUE)),
        .groups = "drop"
      )
  )
}
# ---- Final fit and gap-filled dataset ---------------------------------------

hurdle_final <- function(L, K1, lambda1, K2, lambda2) {
  hp <- hurdle_parts(L$Y)
  f1 <- logit_fit(hp$B, K1, lambda1)
  f2 <- lfm_fit(hp$Y2, K = K2, lambda = lambda2)
  P2 <- lfm_predict(f2); ok <- !is.na(hp$Y2) & !is.na(P2)
  list(P1 = logit_predict(f1), P2 = P2, smear = mean(exp(hp$Y2[ok] - P2[ok])),
       sigma2 = stats::sd(hp$Y2[ok] - P2[ok]), f1 = f1, f2 = f2)
}

# Confidence per insect from the cross-validation:
#   "works"    : tested, and the two-part model beats the insect x year baseline
#                and separates damage / no damage reasonably (AUC >= 0.7)
#   "weak"     : tested, but not better than the baseline or AUC < 0.7
#   "untested" : insect recorded in only one country, so it could not be tested
hurdle_confidence <- function(by_species, species) {
  tibble::tibble(species = species) |>
    dplyr::left_join(by_species, by = "species") |>
    dplyr::mutate(confidence = dplyr::case_when(
      is.na(n_cells) ~ "untested",
      rmse_two < rmse_base & !is.na(auc_any) & auc_any >= 0.7 ~ "works",
      TRUE ~ "weak"
    ))
}

# Harmonised table: every region-year-insect cell, observed or filled.
# Filled values are expected damage = P(any) x mean amount if damaged, with an
# 80% range for the amount if damaged. Host presence is not yet known, so a
# filled value may refer to an insect whose host tree is absent.
hurdle_fill <- function(L, fin, conf, cells, unit_lookup) {
  n <- nrow(L$Y); p <- ncol(L$Y)
  grid <- tibble::tibble(row = rep(seq_len(n), p), col = rep(seq_len(p), each = n))
  z <- stats::qnorm(0.9)
  mon_cs <- cells |> dplyr::group_by(country, species) |>
    dplyr::summarise(ever = any(status != "not_monitored"), .groups = "drop")
  grid |>
    dplyr::mutate(
      country = L$rows$country[row], nuts_id = L$rows$nuts_id[row], year = L$rows$year[row],
      species = L$species[col],
      y = L$Y[cbind(row, col)], p_any = fin$P1[cbind(row, col)], a = fin$P2[cbind(row, col)]
    ) |>
    dplyr::left_join(mon_cs, by = c("country", "species")) |>
    dplyr::left_join(dplyr::select(conf, species, confidence), by = "species") |>
    dplyr::left_join(unit_lookup, by = "species") |>
    dplyr::mutate(
      value_type = dplyr::case_when(
        !is.na(y) & y > 0 ~ "observed damage",
        !is.na(y) ~ "observed zero",
        is.na(p_any) | is.na(a) ~ "not fillable (nothing recorded in this region-year)",
        TRUE ~ "filled"
      ),
      gap_type = dplyr::case_when(
        value_type != "filled" ~ NA_character_,
        ever ~ "country monitors this insect, missed this year/region",
        TRUE ~ "country never monitored this insect"
      ),
      amount_if_damaged = dplyr::if_else(value_type == "filled", pmax(expm1(a) * fin$smear, 0), NA_real_),
      amount_lo80 = dplyr::if_else(value_type == "filled", pmax(expm1(a - z * fin$sigma2), 0), NA_real_),
      amount_hi80 = dplyr::if_else(value_type == "filled", expm1(a + z * fin$sigma2), NA_real_),
      p_any = dplyr::if_else(value_type == "filled", p_any, NA_real_),
      damage = dplyr::case_when(
        value_type %in% c("observed damage", "observed zero") ~ expm1(y),
        value_type == "filled" ~ p_any * amount_if_damaged,
        TRUE ~ NA_real_
      ),
      confidence = dplyr::if_else(value_type == "filled", confidence, NA_character_),
      host_presence = dplyr::if_else(value_type == "filled", "unknown (needs host-tree data)", NA_character_)
    ) |>
    dplyr::select(country, nuts_id, year, species, unit, value_type, damage, p_any,
                  amount_if_damaged, amount_lo80, amount_hi80, gap_type, confidence, host_presence)
}

# Europe-wide totals per insect and year: observed, plus filled (by confidence)
hurdle_totals <- function(filled) {
  filled |>
    dplyr::group_by(species, unit, year) |>
    dplyr::summarise(
      observed = sum(damage[value_type %in% c("observed damage", "observed zero")]),
      filled_works = sum(damage[value_type == "filled" & confidence == "works"]),
      filled_weak = sum(damage[value_type == "filled" & confidence == "weak"]),
      filled_untested = sum(damage[value_type == "filled" & confidence == "untested"]),
      cells_not_fillable = sum(value_type == "not fillable (nothing recorded in this region-year)"),
      .groups = "drop"
    ) |>
    dplyr::mutate(total_with_reliable_fill = observed + filled_works,
                  share_filled_reliable = filled_works / pmax(total_with_reliable_fill, 1e-9))
}
