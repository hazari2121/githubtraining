# Gap-filling: what to do with host trees the forest appendix has no column for
#
# v4b (stop-gap): larch, birch, elm and poplar / willow insects are no longer
#   scaled by a stand-in area (all conifers / all broadleaves). Their amounts
#   come from the model's plain (unscaled) part, as already done for regions
#   without forest data. The "host absent" rule is unchanged.
#
# v5 (host as a predictor, the 2025 paper's way): no scaling at all. The amount
#   part is fitted on log(1 + damage) with log(host area) as an extra input
#   whose effect (beta) is learned for each insect, pulled towards its group's
#   (shrinkage k = 20). beta = 1 would mean damage rises in step with host
#   area, beta = 0 that host area does not matter. Fitted by back-fitting:
#   fit the factor model to damage minus the host effect, re-estimate beta from
#   what is left, repeat (3 rounds). Insects with a stand-in host get no host
#   input (x = 0).

proxy_hosts <- c("larch", "birch", "elm", "poplar / willow")

host_status_v4b <- function(hs4) {
  dplyr::mutate(hs4, host_area_ha = dplyr::if_else(host_tree %in% proxy_hosts, NA_real_, host_area_ha))
}

# Host input: log(host area), centred for each insect over the regions with data; 0 where unknown
host_input <- function(L, hs) {
  A <- host_area_matrix(L, hs)
  X <- log(A); X[!is.finite(X)] <- NA
  X <- sweep(X, 2, colMeans(X, na.rm = TRUE))
  X[is.na(X)] <- 0
  X
}

gapfill_final_hp <- function(L, hs, group, spec, rounds = 3, k = 20) {
  absent <- host_absent_matrix(L, hs); X <- host_input(L, hs)
  hp <- hurdle_parts(L$Y); M <- !is.na(hp$Y2)
  reg <- paste(L$rows$country, L$rows$nuts_id); yr <- L$rows$year
  f1 <- logit_fit_rs(hp$B, spec$K1, spec$lambda1, reg, yr, lambda_rb = spec$lambda_rc,
                     group = group, tau = spec$tau, tau_links = spec$tau_links)
  beta <- rep(0, ncol(L$Y)); g <- ifelse(is.na(group), "none", group)
  for (r in seq_len(rounds)) {
    Yadj <- hp$Y2 - sweep(X, 2, beta, `*`)
    f2 <- lfm_fit_rs(Yadj, spec$K2, spec$lambda2, reg, yr, lambda_rc = spec$lambda_rc,
                     group = group, tau = spec$tau, tau_links = spec$tau_links)
    R <- hp$Y2 - lfm_predict(f2); R[!M] <- 0; XM <- X; XM[!M] <- 0
    sxy <- colSums(R * XM) + beta * colSums(XM^2)   # partial residual: effect of x with the current beta added back
    sxx <- colSums(XM^2)
    raw <- ifelse(sxx > 0, sxy / sxx, NA)
    bg <- tapply(raw, g, function(v) if (all(is.na(v))) 0 else stats::median(v, na.rm = TRUE))
    beta <- (sxy + k * bg[g]) / (sxx + k)
    beta[sxx == 0] <- 0
  }
  P1 <- logit_predict(f1)
  P2 <- lfm_predict(f2) + sweep(X, 2, beta, `*`)
  ok <- M & !is.na(P2); sm <- mean(exp(hp$Y2[ok] - P2[ok]))
  amount <- pmax(expm1(P2) * sm, 0)
  list(P1 = P1, amount_log = log1p(amount), E = ifelse(absent, 0, P1 * amount), absent = absent,
       V2 = f2$V, V1 = f1$V, beta = stats::setNames(unname(beta), L$species))
}

variant_cv_hp <- function(L, blocks, hs, group, spec) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ltr <- L; Ltr$Y[H] <- NA
    r <- gapfill_final_hp(Ltr, hs, group, spec)
    idx <- which(H, arr.ind = TRUE)
    out[[length(out) + 1]] <- tibble::tibble(
      fold = f, country = L$rows$country[idx[, 1]], species = L$species[idx[, 2]], year = L$rows$year[idx[, 1]],
      observed = L$Y[idx], pred = r$E[idx],
      pred_log = ifelse(r$absent[idx], 0, r$P1[idx] * r$amount_log[idx]))
  }
  dplyr::bind_rows(out)
}

# Usable pairs linked by the model (as variant_links), from an already fitted model
links_of_fit <- function(r, L, verdict, name) {
  V <- r$V2; rownames(V) <- L$species
  nv <- sqrt(rowSums(V^2)); C <- (V %*% t(V)) / (nv %o% nv)
  rm <- mapply(function(a, b) if (a %in% rownames(C) && b %in% rownames(C)) C[a, b] else NA,
               verdict$species_a, verdict$species_b)
  u <- verdict$verdict == "usable"
  tibble::tibble(model = name, usable_linked = sum(rm[u] >= 0.3, na.rm = TRUE), usable_n = sum(u),
                 agreement_with_r = stats::cor(verdict$r, rm, use = "pair"))
}

# Full-data output laid out like gapfill_raw_rs()
gapfill_raw_from_fit <- function(r, L, hs) {
  A <- host_area_matrix(L, hs)
  n <- nrow(L$Y); p <- ncol(L$Y); ix <- cbind(rep(seq_len(n), p), rep(seq_len(p), each = n))
  tibble::tibble(
    country = L$rows$country[ix[, 1]], nuts_id = L$rows$nuts_id[ix[, 1]], year = L$rows$year[ix[, 1]],
    species = L$species[ix[, 2]], y = L$Y[ix], absent = r$absent[ix], empty = is.na(r$P1[ix]),
    p_any = r$P1[ix], amount_log = r$amount_log[ix], factor_fill = r$E[ix], host_area_ha = A[ix])
}

host_effects <- function(r, species_meta, hs) {
  tibble::tibble(species = names(r$beta), beta = round(unname(r$beta), 3)) |>
    dplyr::left_join(dplyr::distinct(hs, species, host_tree), by = "species")
}

# Filled totals per insect for several versions side by side (single insects)
gapfill_versions <- function(tots, species_keep) {
  purrr::imap_dfr(tots, function(t, nm) t$europe |> dplyr::filter(species %in% species_keep) |>
                    dplyr::group_by(species) |> dplyr::summarise(reported = sum(reported), filled = sum(filled), .groups = "drop") |>
                    dplyr::mutate(version = nm)) |>
    dplyr::mutate(filled_vs_reported = round(filled / reported, 2)) |>
    tidyr::pivot_wider(id_cols = c(species, reported), names_from = version, values_from = c(filled, filled_vs_reported))
}
