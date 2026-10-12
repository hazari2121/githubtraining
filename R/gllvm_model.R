# Latent factor model with the gllvm package (Niku et al.), agent-group level.
# Exploratory step towards PhD objective 3 (patterns and drivers).
#
#   rows    = region x year
#   columns = agent groups (guild x host tree) reported by >= 3 countries
#   y       = log(1 + damage) of RECORDED values only. Assumed zeros (never
#             reported, host absent) and back-casts are left out (NA): they
#             would make groups look as if they rose and fell together.
#
# Two models:
#  1. Presence ("any damage?", binomial, probit link):
#       P(damage > 0)[i, j] = group level + region effect + year effect
#                             + latent factors (region-year scores x group loadings)
#     Region and year effects are shared by all groups (region size and
#     reporting; Europe-wide bad years).
#  2. Timing (gaussian): y centred within each region x group series, so region
#     size and long-run level drop out; what is left is each series' good and
#     bad years. Year effect shared by all groups + latent factors.
# The latent factors capture which groups move TOGETHER beyond those shared
# effects; their correlation is the "residual correlation".

gllvm_data <- function(g_cells, min_countries = 3, min_years = 5) {
  d <- g_cells |>
    dplyr::filter(status %in% c("observed_positive", "observed_zero")) |>
    dplyr::transmute(country, nuts_id, year, group = species, y = log1p(damage))
  keep <- d |> dplyr::group_by(group) |>
    dplyr::summarise(nc = dplyr::n_distinct(country)) |>
    dplyr::filter(nc >= min_countries) |> dplyr::pull(group) |> sort()
  w <- d |> dplyr::filter(group %in% keep) |>
    tidyr::pivot_wider(names_from = group, values_from = y) |>
    dplyr::arrange(country, nuts_id, year)
  Y <- as.matrix(w[, keep])
  # timing data: anomalies within each region x group series with >= min_years
  # recorded years (shorter series set to NA)
  A <- Y
  for (r in unique(w$nuts_id)) {
    ii <- which(w$nuts_id == r)
    for (j in seq_len(ncol(A))) {
      v <- A[ii, j]
      A[ii, j] <- if (sum(!is.na(v)) >= min_years) v - mean(v, na.rm = TRUE) else NA
    }
  }
  list(Y = Y, B = (Y > 0) * 1, A = A,
       design = data.frame(region = factor(w$nuts_id), year = factor(w$year), country = w$country))
}

# Fit models with 0..max_k latent factors; family "gaussian" (amount, log
# scale) or "binomial" (any damage). Seeded for reproducibility.
gllvm_fit_set <- function(gd, model = c("presence", "timing"), ks = 0:3, seed = 20261012) {
  model <- match.arg(model)
  y <- if (model == "presence") gd$B else gd$A
  ok <- rowSums(!is.na(y)) > 0
  y <- y[ok, , drop = FALSE]; des <- gd$design[ok, c("region", "year")]
  des$region <- droplevels(des$region); des$year <- droplevels(des$year)
  fam <- if (model == "presence") stats::binomial(link = "probit") else "gaussian"
  re <- if (model == "presence") ~ (1 | region) + (1 | year) else ~ (1 | year)
  fits <- lapply(stats::setNames(ks, paste0("K", ks)), function(k) {
    set.seed(seed)
    gllvm::gllvm(y, family = fam, num.lv = k, studyDesign = des,
                 row.eff = re, seed = seed, sd.errors = FALSE)
  })
  attr(fits, "years") <- as.integer(as.character(des$year))
  fits
}

gllvm_ic <- function(fits, model) {
  tibble::tibble(model = model, K = as.integer(sub("K", "", names(fits))),
                 logLik = vapply(fits, function(f) as.numeric(stats::logLik(f)), 0),
                 AIC = vapply(fits, stats::AIC, 0), BIC = vapply(fits, stats::BIC, 0)) |>
    dplyr::mutate(chosen_by_BIC = BIC == min(BIC))
}

gllvm_best <- function(fits, ic) fits[[paste0("K", ic$K[ic$chosen_by_BIC])]]

# Residual correlation between groups (from the latent factors)
gllvm_rescor <- function(fit) {
  if (fit$num.lv == 0) return(NULL)
  gllvm::getResidualCor(fit)
}

# Year pattern of each factor: mean region-year score per year
gllvm_year_scores <- function(fit, years) {
  if (fit$num.lv == 0) return(tibble::tibble())
  S <- gllvm::getLV(fit)
  tibble::as_tibble(S, .name_repair = ~ paste0("factor ", seq_along(.x))) |>
    dplyr::mutate(year = years) |>
    tidyr::pivot_longer(-year, names_to = "factor", values_to = "score") |>
    dplyr::group_by(factor, year) |>
    dplyr::summarise(score = mean(score), .groups = "drop")
}

gllvm_loadings <- function(fit) {
  if (fit$num.lv == 0) return(tibble::tibble())
  L <- gllvm::getLoadings(fit)
  tibble::as_tibble(L, rownames = "group", .name_repair = ~ c(paste0("factor ", seq_along(.x)))) |>
    tidyr::pivot_longer(-group, names_to = "factor", values_to = "loading")
}

# Variance partitioning per group (share of the EXPLAINED variance):
# region, year, latent factors
gllvm_vp <- function(fit) {
  vp <- gllvm::varPartitioning(fit)
  tibble::as_tibble(vp$PropExplainedVarSp, rownames = "group") |>
    tidyr::pivot_longer(-group, names_to = "component", values_to = "share") |>
    dplyr::mutate(component = dplyr::case_when(
      grepl("region", component) ~ "region (shared by all groups)",
      grepl("year", component) ~ "year (Europe-wide, shared by all groups)",
      TRUE ~ paste("latent", sub("LV", "factor ", component))))
}

# Does the timing model agree with the earlier pairwise correlations?
# Compare its residual correlations with plain correlations of the same group
# pairs, computed directly from the anomalies after removing each year's mean
# (the model's shared year effect).
gllvm_vs_pairwise <- function(R, gd) {
  if (is.null(R)) return(tibble::tibble())
  yr <- gd$design$year
  ym <- tapply(gd$A, list(rep(yr, ncol(gd$A))), mean, na.rm = TRUE)
  A <- gd$A - as.numeric(ym[as.character(yr)])
  g <- colnames(R); out <- list()
  for (a in seq_along(g)) for (b in seq_along(g)) if (a < b) {
    ok <- !is.na(A[, a]) & !is.na(A[, b])
    r <- if (sum(ok) >= 20) stats::cor(A[ok, a], A[ok, b]) else NA_real_
    out[[length(out) + 1]] <- tibble::tibble(group_a = g[a], group_b = g[b], n = sum(ok),
                                             r_pairwise = r, r_model = R[a, b])
  }
  dplyr::bind_rows(out)
}
