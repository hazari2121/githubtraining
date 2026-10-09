# Cross-species correlation metrics, version 3 (aligned with the PhD aims:
# how much does each species tell us about every other species?).
#
# For every pair of species (all 1,176 pairs), using only region-years where
# both are observed (series with >= 6 damage years; Hlasny et al. 2025 rule):
#   same_year   : r of z_within (standardised within each region-species series)
#   beyond_year : r of anomaly (each species' Europe-wide yearly mean removed),
#                 i.e. the link that is not just shared bad years
#   lag_ab      : does A's year t predict B's year t+1 BEYOND B's own year t?
#                 Partial correlation of A(t) with B(t+1), both adjusted for
#                 B(t) (anomaly scale; Granger-type). Plain lagged correlations
#                 are misleading because outbreaks last several years, so any
#                 linked pair looks linked across years in both directions.
#   lag_ba      : the same with A and B swapped
#   co_occur    : r of damage > 0 indicators (phi), on all observed series
#                 (no >= 6 filter), i.e. do the two tend to occur together?
#
# Uncertainty: 95% intervals from a region block bootstrap (whole regions are
# resampled, 999 times), which respects autocorrelation within a region's
# series. Intervals are NA when fewer than 5 regions contribute (too few
# blocks to resample reliably).

min_n_metric <- 20      # minimum co-observed region-years for a metric
boot_reps <- 999

# Pearson r with a region block bootstrap interval, using per-region sums
boot_r <- function(x, y, g, B = boot_reps, seed = 1) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]; g <- g[ok]
  n <- length(x)
  out <- c(r = NA_real_, lo = NA_real_, hi = NA_real_, n = n, n_regions = dplyr::n_distinct(g))
  if (n < min_n_metric || stats::sd(x) == 0 || stats::sd(y) == 0) return(out)
  out["r"] <- stats::cor(x, y)
  G <- out[["n_regions"]]
  if (G < 5) return(out)
  st <- rowsum(cbind(1, x, y, x * x, y * y, x * y), g)       # G x 6 sufficient stats
  set.seed(seed)
  w <- stats::rmultinom(B, G, rep(1 / G, G))                  # G x B region counts
  S <- t(w) %*% st                                            # B x 6
  num <- S[, 1] * S[, 6] - S[, 2] * S[, 3]
  den <- sqrt((S[, 1] * S[, 4] - S[, 2]^2) * (S[, 1] * S[, 5] - S[, 3]^2))
  rb <- num / den
  q <- stats::quantile(rb[is.finite(rb)], c(0.025, 0.975), names = FALSE)
  out["lo"] <- q[1]; out["hi"] <- q[2]
  out
}

# Partial correlation of x with y adjusting both for z (residuals of linear
# regressions on z, fitted on rows where x, y and z are all observed).
# The adjustment is estimated once, not inside each bootstrap draw.
boot_r_partial <- function(x, y, z, g, ...) {
  ok <- is.finite(x) & is.finite(y) & is.finite(z)
  if (sum(ok) < min_n_metric || stats::sd(z[ok]) == 0)
    return(c(r = NA_real_, lo = NA_real_, hi = NA_real_, n = sum(ok), n_regions = dplyr::n_distinct(g[ok])))
  rx <- ry <- rep(NA_real_, length(x))
  rx[ok] <- stats::resid(stats::lm(x[ok] ~ z[ok]))
  ry[ok] <- stats::resid(stats::lm(y[ok] ~ z[ok]))
  boot_r(rx, ry, g, ...)
}

# Wide table aligned to a fixed set of region-year rows
wide_on <- function(d, var, keys, species) {
  W <- matrix(NA_real_, length(keys), length(species), dimnames = list(NULL, species))
  k <- match(paste(d$nuts_id, d$year), keys)
  s <- match(d$species, species)
  ok <- !is.na(k) & !is.na(s)
  W[cbind(k[ok], s[ok])] <- d[[var]][ok]
  W
}

correlation_metrics <- function(tr, tr_nofilter, species, ts) {
  rows <- dplyr::distinct(tr_nofilter, country, nuts_id, year) |> dplyr::arrange(nuts_id, year)
  keys <- paste(rows$nuts_id, rows$year)
  Z  <- wide_on(tr, "z_within", keys, species)
  A  <- wide_on(tr, "anomaly", keys, species)
  A1 <- wide_on(dplyr::mutate(tr, year = year - 1L), "anomaly", keys, species)  # value of year t+1
  Pz <- wide_on(dplyr::mutate(tr_nofilter, pos = as.numeric(damage > 0)), "pos", keys, species)
  Pf <- wide_on(dplyr::mutate(tr, pos = as.numeric(damage > 0)), "pos", keys, species)
  g <- rows$nuts_id

  pairs <- species_pairs(species)
  res <- purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
    a <- pairs$species_a[i]; b <- pairs$species_b[i]
    sy <- boot_r(Z[, a], Z[, b], g, seed = i)
    by <- boot_r(A[, a], A[, b], g, seed = i + 1e5)
    lab <- boot_r_partial(A[, a], A1[, b], A[, b], g, seed = i + 2e5)
    lba <- boot_r_partial(A[, b], A1[, a], A[, a], g, seed = i + 3e5)
    co <- boot_r(Pz[, a], Pz[, b], g, seed = i + 4e5)
    both <- is.finite(Z[, a]) & is.finite(Z[, b])
    tibble::tibble(
      species_a = a, species_b = b,
      n = sy[["n"]], n_regions = sy[["n_regions"]],
      n_countries = dplyr::n_distinct(rows$country[both]),
      n_both_damage = sum(both & Pf[, a] == 1 & Pf[, b] == 1, na.rm = TRUE),
      same_year = sy[["r"]], same_year_lo = sy[["lo"]], same_year_hi = sy[["hi"]],
      beyond_year = by[["r"]], beyond_year_lo = by[["lo"]], beyond_year_hi = by[["hi"]],
      lag_ab = lab[["r"]], lag_ab_lo = lab[["lo"]], lag_ab_hi = lab[["hi"]],
      lag_ba = lba[["r"]], lag_ba_lo = lba[["lo"]], lag_ba_hi = lba[["hi"]],
      co_occur = co[["r"]], co_occur_lo = co[["lo"]], co_occur_hi = co[["hi"]], n_co_occur = co[["n"]]
    )
  })

  excl0 <- function(lo, hi) !is.na(lo) & (lo > 0 | hi < 0)
  res |>
    dplyr::left_join(ts |> dplyr::filter(version == "z_within") |>
                       dplyr::select(species_a, species_b, transferability, countries_n10),
                     by = c("species_a", "species_b")) |>
    dplyr::mutate(
      same_year_sure = excl0(same_year_lo, same_year_hi),
      beyond_year_sure = excl0(beyond_year_lo, beyond_year_hi),
      lag_ab_sure = excl0(lag_ab_lo, lag_ab_hi),
      lag_ba_sure = excl0(lag_ba_lo, lag_ba_hi),
      # one-line verdict per pair
      link = dplyr::case_when(
        is.na(same_year) ~ "not enough data",
        same_year_sure & abs(same_year) >= 0.3 & beyond_year_sure & abs(beyond_year) >= 0.2 ~ "strong, beyond shared bad years",
        same_year_sure & abs(same_year) >= 0.3 ~ "strong, mostly shared bad years",
        same_year_sure ~ "weak but real",
        TRUE ~ "no clear link"
      )
    )
}

# Per species: how much do the other species tell us about it?
pick_best <- function(v, r, ok) if (any(ok)) v[ok][which.max(abs(r[ok]))] else v[NA_integer_][1]

species_information <- function(cm, species_meta) {
  both <- dplyr::bind_rows(
    dplyr::rename(cm, focal = species_a, partner = species_b),
    dplyr::rename(cm, focal = species_b, partner = species_a)
  )
  both |>
    dplyr::group_by(species = focal) |>
    dplyr::summarise(
      partners_tested = sum(!is.na(same_year)),
      strong_links = sum(link == "strong, beyond shared bad years"),
      strong_but_shared_years = sum(link == "strong, mostly shared bad years"),
      weak_links = sum(link == "weak but real"),
      # best partners only among links with a reliable interval (>= 5 regions)
      best_partner = pick_best(partner, same_year, !is.na(same_year_lo)),
      best_r = pick_best(same_year, same_year, !is.na(same_year_lo)),
      best_partner_beyond_years = pick_best(partner, beyond_year, !is.na(beyond_year_lo)),
      best_r_beyond_years = pick_best(beyond_year, beyond_year, !is.na(beyond_year_lo)),
      .groups = "drop"
    ) |>
    dplyr::left_join(dplyr::select(species_meta, species, guild = guild_from_file, host_tree), by = "species") |>
    dplyr::arrange(dplyr::desc(strong_links), dplyr::desc(abs(best_r)))
}

# Lagged pairs in ordered form: "leader in year t -> follower in year t+1"
lag_pairs <- function(cm) {
  dplyr::bind_rows(
    cm |> dplyr::transmute(leader = species_a, follower = species_b, r = lag_ab, lo = lag_ab_lo, hi = lag_ab_hi,
                           sure = lag_ab_sure, same_year, n_regions),
    cm |> dplyr::transmute(leader = species_b, follower = species_a, r = lag_ba, lo = lag_ba_lo, hi = lag_ba_hi,
                           sure = lag_ba_sure, same_year, n_regions)
  ) |>
    dplyr::filter(!is.na(r)) |>
    dplyr::arrange(dplyr::desc(sure), dplyr::desc(abs(r)))
}
