# Steps 4-6: pairwise correlations, temporal vs spatial, transferability.
#
# All correlations use pairwise-complete region-years: only rows where BOTH
# species have a value in the version being analysed.
# NOTE: series are autocorrelated and region-years are not independent, so
# p-values (and FDR-adjusted p-values) are optimistic. Interpret mainly by
# effect size, n and consistency across countries.

min_n_interpret <- 10   # pairs / countries with fewer co-observed region-years are flagged

species_pairs <- function(species) {
  cmb <- utils::combn(sort(unique(species)), 2)
  tibble::tibble(species_a = cmb[1, ], species_b = cmb[2, ])
}

# Wide table: one row per region-year, one column per species
make_wide <- function(tr, var) {
  tr |>
    dplyr::select(country, nuts_id, year, species, value = dplyr::all_of(var)) |>
    tidyr::pivot_wider(names_from = species, values_from = value)
}

get_col <- function(W, sp) if (sp %in% names(W)) W[[sp]] else rep(NA_real_, nrow(W))

# Pearson + Spearman for two vectors (pairwise complete)
pair_stats <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  out <- list(n = sum(ok), r_pearson = NA_real_, p_pearson = NA_real_,
              r_spearman = NA_real_, p_spearman = NA_real_)
  if (out$n >= 4) {
    x <- x[ok]; y <- y[ok]
    if (stats::sd(x) > 0 && stats::sd(y) > 0) {
      pt <- stats::cor.test(x, y, method = "pearson")
      st <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
      out$r_pearson <- unname(pt$estimate);  out$p_pearson <- pt$p.value
      out$r_spearman <- unname(st$estimate); out$p_spearman <- st$p.value
    }
  }
  out
}

# Step 4: all pairs x versions, with BH-FDR across all pairs within version x method
pairwise_correlations <- function(tr, species,
                                  versions = c("z_within", "detrended", "diff", "anomaly", "log_damage")) {
  pairs <- species_pairs(species)
  # damage > 0 indicator on the same rows (all versions share the row set/order)
  P <- make_wide(dplyr::mutate(tr, pos = as.numeric(damage > 0)), "pos")
  res <- purrr::map(versions, function(v) {
    W <- make_wide(tr, v)
    stopifnot(identical(W$nuts_id, P$nuts_id), identical(W$year, P$year))
    reg <- paste(W$country, W$nuts_id)
    st <- purrr::map2(pairs$species_a, pairs$species_b, function(a, b) {
      x <- get_col(W, a); y <- get_col(W, b)
      ok <- is.finite(x) & is.finite(y)
      c(pair_stats(x, y),
        # co-observed region-years with damage > 0 in both species; low values
        # mean the correlation is driven mainly by shared zeros
        n_both_positive = sum(ok & get_col(P, a) == 1 & get_col(P, b) == 1, na.rm = TRUE),
        n_regions = dplyr::n_distinct(reg[ok]),
        n_countries = dplyr::n_distinct(W$country[ok]),
        countries = paste(sort(unique(W$country[ok])), collapse = ","))
    })
    dplyr::bind_cols(pairs, version = v, dplyr::bind_rows(st))
  })
  dplyr::bind_rows(res) |>
    dplyr::group_by(version) |>
    dplyr::mutate(
      fdr_pearson  = stats::p.adjust(p_pearson, method = "BH"),
      fdr_spearman = stats::p.adjust(p_spearman, method = "BH")
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      low_n = n < min_n_interpret,
      reference_only = version == "log_damage"
    )
}

# Step 5: spatial correlation = correlation of region means of log_damage
# across regions, using only the years in which both species were observed.
spatial_correlations <- function(tr, species, min_regions = 4) {
  pairs <- species_pairs(species)
  W <- make_wide(tr, "log_damage")
  reg <- paste(W$country, W$nuts_id)
  st <- purrr::map2(pairs$species_a, pairs$species_b, function(a, b) {
    x <- get_col(W, a); y <- get_col(W, b)
    ok <- is.finite(x) & is.finite(y)
    out <- list(n_regions_spatial = 0L, r_spatial_pearson = NA_real_, p_spatial_pearson = NA_real_,
                r_spatial_spearman = NA_real_)
    if (!any(ok)) return(out)
    m <- tibble::tibble(reg = reg[ok], x = x[ok], y = y[ok]) |>
      dplyr::group_by(reg) |>
      dplyr::summarise(mx = mean(x), my = mean(y), .groups = "drop")
    out$n_regions_spatial <- nrow(m)
    if (nrow(m) >= min_regions) {
      s <- pair_stats(m$mx, m$my)
      out$r_spatial_pearson <- s$r_pearson; out$p_spatial_pearson <- s$p_pearson
      out$r_spatial_spearman <- s$r_spearman
    }
    out
  })
  dplyr::bind_cols(pairs, dplyr::bind_rows(st))
}

# Combine temporal (pooled within-series z_within / detrended) and spatial.
# Linked = |r| >= 0.3; "insufficient" when temporal n < 10 or < 5 regions.
temporal_vs_spatial <- function(pw, sp, link_threshold = 0.3, min_regions_spatial = 5) {
  temporal <- pw |>
    dplyr::filter(version %in% c("z_within", "detrended")) |>
    dplyr::select(species_a, species_b, version, n, r_pearson) |>
    tidyr::pivot_wider(names_from = version, values_from = c(n, r_pearson)) |>
    dplyr::rename(n_temporal = n_z_within, r_temporal_z = r_pearson_z_within,
                  r_temporal_detrended = r_pearson_detrended) |>
    dplyr::select(-n_detrended)

  temporal |>
    dplyr::left_join(sp, by = c("species_a", "species_b")) |>
    dplyr::mutate(
      temporal_link = abs(r_temporal_z) >= link_threshold,
      spatial_link  = abs(r_spatial_pearson) >= link_threshold,
      linked_by = dplyr::case_when(
        is.na(r_temporal_z) | n_temporal < min_n_interpret ~ "insufficient data",
        is.na(r_spatial_pearson) | n_regions_spatial < min_regions_spatial ~
          dplyr::if_else(temporal_link, "timing (place untestable)", "neither (place untestable)"),
        temporal_link & spatial_link ~ "both",
        temporal_link ~ "timing",
        spatial_link ~ "place",
        TRUE ~ "neither"
      )
    )
}

# Step 6a: per-country correlations (countries with n >= 10 for the pair)
per_country_correlations <- function(tr, species, versions = c("z_within", "detrended", "anomaly")) {
  pairs <- species_pairs(species)
  purrr::map_dfr(versions, function(v) {
    W <- make_wide(tr, v)
    idx <- split(seq_len(nrow(W)), W$country)
    purrr::map2_dfr(pairs$species_a, pairs$species_b, function(a, b) {
      x <- get_col(W, a); y <- get_col(W, b)
      ok <- is.finite(x) & is.finite(y)
      if (sum(ok) < min_n_interpret) return(NULL)
      purrr::imap_dfr(idx, function(i, ctry) {
        if (sum(ok[i]) < min_n_interpret) return(NULL)
        s <- pair_stats(x[i], y[i])
        tibble::tibble(species_a = a, species_b = b, version = v, country = ctry,
                       n = s$n, r_pearson = s$r_pearson, p_pearson = s$p_pearson,
                       r_spearman = s$r_spearman)
      })
    })
  })
}

# Step 6b: leave-one-country-out pooled Pearson r
loco_correlations <- function(tr, species, version = "z_within") {
  pairs <- species_pairs(species)
  W <- make_wide(tr, version)
  purrr::map2_dfr(pairs$species_a, pairs$species_b, function(a, b) {
    x <- get_col(W, a); y <- get_col(W, b)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < min_n_interpret) return(NULL)
    ctrs <- sort(unique(W$country[ok]))
    purrr::map_dfr(ctrs, function(ctry) {
      keep <- W$country != ctry
      s <- pair_stats(x[keep], y[keep])
      tibble::tibble(species_a = a, species_b = b, version = version, dropped_country = ctry,
                     n_without = s$n, r_without = s$r_pearson)
    })
  })
}

# Step 6c: per-pair transferability summary and flag.
#   consistent   : >= 2 countries (n >= 10 each), all with the pooled sign,
#                  SD of country r <= 0.2, and no leave-one-country-out sign flip
#   inconsistent : >= 2 countries but any of the above fails
#   insufficient data : pooled n < 10 or < 2 countries with n >= 10
transferability_summary <- function(pw, pc, loco, version = "z_within", max_sd = 0.2) {
  pooled <- pw |>
    dplyr::filter(version == !!version) |>
    dplyr::select(species_a, species_b, n_pooled = n, r_pooled = r_pearson,
                  fdr_pooled = fdr_pearson, countries_pooled = countries)

  by_country <- pc |>
    dplyr::filter(version == !!version, !is.na(r_pearson)) |>
    dplyr::left_join(pooled, by = c("species_a", "species_b")) |>
    dplyr::group_by(species_a, species_b) |>
    dplyr::summarise(
      n_countries = dplyr::n(),
      countries_n10 = paste(country, collapse = ","),
      r_mean = mean(r_pearson), r_sd = stats::sd(r_pearson),
      r_min = min(r_pearson), r_max = max(r_pearson),
      sign_consistency = mean(sign(r_pearson) == sign(dplyr::first(r_pooled))),
      .groups = "drop"
    )

  loco_sum <- loco |>
    dplyr::filter(version == !!version) |>
    dplyr::left_join(pooled, by = c("species_a", "species_b")) |>
    dplyr::group_by(species_a, species_b) |>
    dplyr::summarise(
      loco_max_abs_change = suppressWarnings(max(abs(r_without - r_pooled), na.rm = TRUE)),
      loco_most_influential = dropped_country[which.max(abs(r_without - r_pooled))][1],
      loco_sign_flip = any(sign(r_without) != sign(r_pooled), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(loco_max_abs_change = dplyr::if_else(is.finite(loco_max_abs_change),
                                                       loco_max_abs_change, NA_real_))

  pooled |>
    dplyr::left_join(by_country, by = c("species_a", "species_b")) |>
    dplyr::left_join(loco_sum, by = c("species_a", "species_b")) |>
    dplyr::mutate(
      version = version,
      n_countries = dplyr::coalesce(n_countries, 0L),
      transferability = dplyr::case_when(
        is.na(r_pooled) | n_pooled < min_n_interpret | n_countries < 2 ~ "insufficient data",
        sign_consistency == 1 & r_sd <= max_sd & !dplyr::coalesce(loco_sign_flip, FALSE) ~ "consistent",
        TRUE ~ "inconsistent"
      ),
      strength = cut(abs(r_pooled), c(-Inf, 0.1, 0.3, 0.5, Inf),
                     labels = c("negligible", "weak", "moderate", "strong"))
    )
}
