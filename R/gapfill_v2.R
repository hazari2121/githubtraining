# Gap-filling v2: adding memory, neighbouring regions, a country reporting
# effect and a distribution built for zero-heavy damage, each tested separately.
#
# Starting point: the factor model + forest area (amount only), version 5 of
# R/gapfill_compare.R. On top of it, a second-stage regression ("stacker")
# combines the factor model's prediction with extra information:
#   memory     : how active the region's OTHER insects were last year
#                (mean standardised damage); the hidden insect itself has no
#                history in a country that never reported it
#   neighbours : the same insect in nearby regions (other countries, < 600 km,
#                weight exp(-distance / 200 km)): its anomaly this year and
#                last year (bad year around here?) and its level per km2
#   reporting  : the country's reporting level for the insect's broad group,
#                = mean residual (reported - factor model) of the country's
#                other insects in that group
#   tweedie    : stacker fitted as a Tweedie GLM (log link, power 1.5) on the
#                original damage instead of a normal model on log(1 + damage)
# Honest test: features of hidden cells use training data of their own fold
# only; the stacker for fold f is fitted on the hidden cells of the OTHER folds
# and then applied to fold f.

# ---- Region positions ------------------------------------------------------------
# NUTS label points and areas (Eurostat Nuts2json, NUTS 2021). Approximations:
# FI forest-centre codes -> parent NUTS2 point (area shared equally);
# PL forest directorates (RDLP) -> directorate city (area = PL area / 17);
# "DEB; DEC" -> mean of DEB and DEC points, areas summed.
region_positions <- function(regions, dir = "data/external") {
  pts <- dplyr::bind_rows(lapply(0:3, function(l) {
    f <- jsonlite::fromJSON(file.path(dir, sprintf("nutspt_%d.json", l)))$features
    tibble::tibble(id = f$properties$id, area_km2 = f$properties$ar / 1e6,
                   lon = vapply(f$geometry$coordinates, `[`, 0, 1),
                   lat = vapply(f$geometry$coordinates, `[`, 0, 2))
  })) |> dplyr::distinct(id, .keep_all = TRUE)
  pl <- tibble::tribble(~id, ~lon, ~lat,
    "BIA", 23.16, 53.13, "GDA", 18.65, 54.35, "KAT", 19.02, 50.26, "KRA", 19.94, 50.06,
    "KRO", 21.77, 49.69, "LOD", 19.46, 51.76, "LUB", 22.57, 51.25, "OLS", 20.48, 53.78,
    "PIL", 16.74, 53.15, "POZ", 16.93, 52.41, "RAD", 21.15, 51.40, "SZ1", 14.55, 53.43,
    "SZ2", 16.70, 53.71, "TOR", 18.60, 53.01, "WAR", 21.01, 52.23, "WRO", 17.04, 51.11,
    "ZIE", 15.51, 51.94) |>
    dplyr::mutate(area_km2 = pts$area_km2[pts$id == "PL"] / 17)
  out <- regions |>
    dplyr::left_join(pts, by = c("nuts_id" = "id")) |>
    dplyr::left_join(pl, by = c("nuts_id" = "id"), suffix = c("", "_pl")) |>
    dplyr::mutate(lon = dplyr::coalesce(lon, lon_pl), lat = dplyr::coalesce(lat, lat_pl),
                  area_km2 = dplyr::coalesce(area_km2, area_km2_pl), source = dplyr::if_else(!is.na(lon_pl), "PL directorate city", "NUTS 2021")) |>
    dplyr::select(country, nuts_id, lon, lat, area_km2, source)
  # merged DE region
  dd <- dplyr::filter(pts, id %in% c("DEB", "DEC"))
  i <- out$nuts_id == "DEB; DEC"
  out$lon[i] <- mean(dd$lon); out$lat[i] <- mean(dd$lat); out$area_km2[i] <- sum(dd$area_km2); out$source[i] <- "NUTS 2021 (DEB+DEC)"
  # Finnish forest-centre codes -> parent NUTS2
  for (k in which(is.na(out$lon) & out$country == "FI")) {
    par <- pts[pts$id == substr(out$nuts_id[k], 1, 4), ]
    sib <- sum(is.na(out$lon) & substr(out$nuts_id, 1, 4) == substr(out$nuts_id[k], 1, 4))
    out$lon[k] <- par$lon; out$lat[k] <- par$lat
    out$area_km2[k] <- par$area_km2 / max(sib, 1); out$source[k] <- "parent NUTS2 (FI, approximate)"
  }
  out
}

haversine_km <- function(lon, lat) {
  r <- pi / 180; n <- length(lon)
  outer(seq_len(n), seq_len(n), function(i, j) {
    dlat <- (lat[j] - lat[i]) * r; dlon <- (lon[j] - lon[i]) * r
    a <- sin(dlat / 2)^2 + cos(lat[i] * r) * cos(lat[j] * r) * sin(dlon / 2)^2
    6371 * 2 * asin(pmin(1, sqrt(a)))
  })
}

# ---- Features for hidden cells (training data of the fold only) -------------------
v2_features <- function(L, Ytr, idx, FMlog_all, pos, group, max_km = 600, scale_km = 200) {
  rows <- L$rows
  regs <- unique(rows$nuts_id); yrs <- sort(unique(rows$year))
  ri <- match(rows$nuts_id, regs); yi <- match(rows$year, yrs)
  rowof <- matrix(NA_integer_, length(regs), length(yrs)); rowof[cbind(ri, yi)] <- seq_len(nrow(rows))
  p <- match(regs, pos$nuts_id); D <- haversine_km(pos$lon[p], pos$lat[p])
  W <- exp(-D / scale_km); W[D > max_km] <- 0; diag(W) <- 0
  ctry <- rows$country[match(regs, rows$nuts_id)]
  W[outer(ctry, ctry, "==")] <- 0                       # neighbours in other countries only
  area <- pos$area_km2[p]
  # standardised damage per insect (training data) and region-series means
  mu <- colMeans(Ytr, na.rm = TRUE); sdv <- apply(Ytr, 2, stats::sd, na.rm = TRUE); sdv[!is.finite(sdv) | sdv == 0] <- 1
  Z <- sweep(sweep(Ytr, 2, mu), 2, sdv, "/")
  rowZ <- rowMeans(Z, na.rm = TRUE); rowZ[!is.finite(rowZ)] <- NA
  RM <- apply(Ytr, 2, function(v) stats::ave(v, rows$nuts_id, FUN = function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)))
  Anom <- Ytr - RM
  Dens <- log1p(1000 * expm1(Ytr) / area[ri])           # damage per 1000 km2 of region
  # reporting effect: mean residual of the country's other insects in the same group
  Res <- Ytr - FMlog_all
  rep_tab <- tibble::tibble(country = rows$country[which(!is.na(Res), arr.ind = TRUE)[, 1]],
                            g = group[which(!is.na(Res), arr.ind = TRUE)[, 2]], res = Res[!is.na(Res)]) |>
    dplyr::group_by(country, g) |> dplyr::summarise(rep = mean(res), n = dplyr::n(), .groups = "drop")
  nb <- function(M, r, t, s) {
    if (is.na(t) || t < 1) return(NA_real_)
    rr <- rowof[, t]; v <- ifelse(is.na(rr), NA, M[cbind(ifelse(is.na(rr), 1L, rr), s)])
    w <- W[r, ]; ok <- !is.na(v) & w > 0
    if (!any(ok)) NA_real_ else sum(w[ok] * v[ok]) / sum(w[ok])
  }
  out <- tibble::tibble(i = idx[, 1], s = idx[, 2]) |>
    dplyr::mutate(r = ri[i], t = yi[i], country = rows$country[i], g = group[s])
  out$mem_region_last_year <- vapply(seq_len(nrow(out)), function(k) {
    t <- out$t[k] - 1; if (t < 1) return(NA_real_); rr <- rowof[out$r[k], t]; if (is.na(rr)) NA_real_ else rowZ[rr] }, 0)
  out$nb_anom_this_year <- vapply(seq_len(nrow(out)), function(k) nb(Anom, out$r[k], out$t[k], out$s[k]), 0)
  out$nb_anom_last_year <- vapply(seq_len(nrow(out)), function(k) nb(Anom, out$r[k], out$t[k] - 1, out$s[k]), 0)
  out$nb_level <- vapply(seq_len(nrow(out)), function(k) nb(Dens, out$r[k], out$t[k], out$s[k]), 0)
  out$log_area <- log(area[out$r])
  out |> dplyr::left_join(dplyr::select(rep_tab, country, g, rep_effect = rep), by = c("country", "g")) |>
    dplyr::select(-r, -t, -g, -country)
}

# ---- Cross-validation: features + factor model per fold ------------------------------
gapfill_v2_cv <- function(L, blocks, group, K1, lambda1, K2, lambda2, hs, pos, tau = 50) {
  Lh <- host_scaled_L(L, hs); absent <- host_absent_matrix(L, hs)
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ytr <- L$Y; Ytr[H] <- NA
    idx <- which(H, arr.ind = TRUE)
    hp <- hurdle_parts(Ytr); Ys <- Lh$Y; Ys[H] <- NA; Y2s <- hurdle_parts(Ys)$Y2
    f1 <- logit_fit(hp$B, K1, lambda1, group = group, tau = tau, tau_links = 0)
    f2 <- lfm_fit(hp$Y2, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
    g2 <- lfm_fit(Y2s, K = K2, lambda = lambda2, group = group, tau = tau, tau_links = 0)
    P1 <- logit_predict(f1); P2 <- lfm_predict(f2); P2s <- lfm_predict(g2)
    Anat <- ifelse(!is.na(Lh$area) & !is.na(P2s), log1p(pmax(expm1(P2s), 0) * Lh$area / 1000), P2)
    FMlog <- ifelse(absent, 0, P1 * Anat)
    FMnat <- host_combine(P1, P2s, smearing(g2, Y2s), P2, smearing(f2, hp$Y2), Lh$area, absent)
    feats <- v2_features(L, Ytr, idx, FMlog, pos, group)
    out[[length(out) + 1]] <- dplyr::bind_cols(
      tibble::tibble(fold = f, row = idx[, 1], col = idx[, 2],
                     country = L$rows$country[idx[, 1]], year = L$rows$year[idx[, 1]], species = L$species[idx[, 2]],
                     observed = L$Y[idx], fm_log = FMlog[idx], fm_nat = FMnat[idx], absent = absent[idx],
                     fm_p = P1[idx], fm_amt = Anat[idx]),
      dplyr::select(feats, -i, -s))
  }
  dplyr::bind_rows(out)
}

# ---- Stacker variants, fitted on other folds' hidden cells -----------------------------
# Two parts, like the factor model itself:
#   presence : logistic regression on logit(factor model P(any)) + extras
#   amount   : linear regression of log amount (damaged cells only) on the
#              factor model's log amount + extras; smearing back-transform
# prediction = P(any) x amount. The factor model's prediction enters as an
# offset (slope fixed at 1), so the extras only correct it. "recalibrated only"
# (intercept + region size, no extras) separates the effect of the second stage
# from the effect of the extras.
v2_variants <- list(
  `factor model (v1)`            = NULL,
  `recalibrated only`            = character(),
  `+ memory`                     = c("mem_region_last_year"),
  `+ neighbours`                 = c("nb_anom_this_year", "nb_anom_last_year", "nb_level"),
  `+ reporting effect`           = c("rep_effect"),
  `+ all three`                  = c("mem_region_last_year", "nb_anom_this_year", "nb_anom_last_year", "nb_level", "rep_effect"),
  `+ all three, Tweedie`         = c("mem_region_last_year", "nb_anom_this_year", "nb_anom_last_year", "nb_level", "rep_effect")
)

v2_design <- function(d, vars) {
  X <- data.frame(lp = stats::qlogis(pmin(pmax(d$fm_p, 1e-4), 1 - 1e-4)), amt = d$fm_amt, log_area = d$log_area)
  for (v in vars) { x <- d[[v]]; X[[paste0(v, "_na")]] <- as.numeric(is.na(x)); x[is.na(x)] <- 0; X[[v]] <- x }
  X
}

gapfill_v2_predict <- function(cvd) {
  d <- dplyr::filter(cvd, !is.na(fm_log), !is.na(fm_nat), !is.na(fm_p), !is.na(fm_amt))
  res <- list()
  for (nm in names(v2_variants)) {
    vars <- v2_variants[[nm]]
    pr <- rep(NA_real_, nrow(d)); prl <- rep(NA_real_, nrow(d))
    for (f in sort(unique(d$fold))) {
      tr <- d$fold != f; te <- d$fold == f
      if (is.null(vars)) { pr[te] <- d$fm_nat[te]; prl[te] <- d$fm_log[te]; next }
      X <- v2_design(d, vars)
      if (grepl("Tweedie", nm)) {
        y <- expm1(d$observed)
        # scale each insect's damage so units (m3, ha) and sizes are comparable in one GLM
        scl <- tapply(y[tr], d$species[tr], function(v) max(mean(v), 1e-6))
        s_all <- as.numeric(scl[d$species]); s_all[is.na(s_all)] <- 1
        dat <- cbind(X, y = y / s_all)
        m <- stats::glm(y ~ ., data = dat[tr, ], family = statmod::tweedie(var.power = 1.5, link.power = 0),
                        control = stats::glm.control(maxit = 100))
        mu <- stats::predict(m, newdata = dat[te, ], type = "response") * s_all[te]
        pr[te] <- mu; prl[te] <- log1p(mu)
      } else {
        # the factor model's prediction is kept as an offset (slope fixed at 1);
        # the extras can only shift it up or down. Refitting the slope pulls large
        # predictions towards the average, which ruins regional totals.
        b <- as.numeric(d$observed > 0)
        Xp <- X[, setdiff(names(X), c("lp", "amt")), drop = FALSE]
        f_p <- stats::as.formula(paste("b ~", paste(c(names(Xp), "offset(lp)"), collapse = " + ")))
        f_a <- stats::as.formula(paste("y ~", paste(c(names(Xp), "offset(amt)"), collapse = " + ")))
        m1 <- stats::glm(f_p, data = cbind(Xp, lp = X$lp, b = b)[tr, ], family = stats::binomial())
        p <- stats::predict(m1, newdata = cbind(Xp, lp = X$lp)[te, ], type = "response")
        pos <- tr & d$observed > 0
        m2 <- stats::lm(f_a, data = cbind(Xp, amt = X$amt, y = d$observed)[pos, ])
        a <- stats::predict(m2, newdata = cbind(Xp, amt = X$amt)[te, ])
        sm <- mean(exp(stats::resid(m2)))
        pr[te] <- p * pmax(expm1(a) * sm, 0); prl[te] <- p * pmax(a, 0)
      }
      pr[te][d$absent[te]] <- 0; prl[te][d$absent[te]] <- 0
    }
    res[[nm]] <- d |> dplyr::select(fold, row, col, country, year, species, observed) |>
      dplyr::mutate(method = nm, pred = pr, pred_log = prl)
  }
  dplyr::bind_rows(res) |> dplyr::mutate(method = factor(method, names(v2_variants)))
}

gapfill_v2_scores <- function(pred) {
  d <- pred |> dplyr::mutate(real = expm1(observed), err = pred_log - observed)
  tot <- d |> dplyr::group_by(method, fold, country, species, year) |>
    dplyr::summarise(real = sum(real), pred = sum(pred), .groups = "drop") |>
    dplyr::filter(real > 0) |> dplyr::mutate(lr = abs(log((pred + 1) / (real + 1))))
  d |> dplyr::group_by(method) |>
    dplyr::summarise(n_cells = dplyr::n(), error_log = sqrt(mean(err^2)),
                     damage_vs_none_auc = auc(pred_log, as.numeric(observed > 0)), .groups = "drop") |>
    dplyr::left_join(tot |> dplyr::group_by(method) |>
                       dplyr::summarise(totals_within_2x = mean(lr <= log(2)),
                                        typical_total_error_x = exp(stats::median(lr))), by = "method")
}
