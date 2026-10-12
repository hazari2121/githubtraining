# Host-tree abundance per region (Appendix "NUTS_Forests" of Hlasny et al. 2025,
# received Oct 2026) and its use in gap-filling.
#
# The appendix gives forest area (ha) and growing stock (m3) for Norway spruce,
# Scots pine, oaks and fir, plus totals for all conifers and all broadleaves,
# per NUTS region (CZ, DE, PL, AT, FI) or per country (other countries).
#
# Decisions (to confirm with the data compilers):
#  - Area (ha) is the host measure. Volumes are not used: for BG and LT the
#    species volumes are about 1000x too small relative to area (probably
#    reported in 1000 m3).
#  - "-" is read as "host tree absent" (area 0). This can be switched off with
#    dash_means_absent = FALSE, in which case "-" means "no data".
#  - Definitions of area differ: FI counts only stands where the species
#    dominates (>75%), SE areas are derived from volume, SI species areas look
#    like "area where present". Use areas as approximate host amounts.
#  - Code fixes: CR -> HR, SL -> SI; DEB + DEC summed to match "DEB; DEC".
#    DE4 holds Brandenburg + Berlin (DE3 has no own row); CZ01, DE5, DE6 are missing.

host_cols <- c("spruce", "pine", "oak", "fir", "conifers_all", "broadleaves_all")

read_host_appendix <- function(path) {
  sheets <- setdiff(readxl::excel_sheets(path), "DataOrigin_Quality")
  purrr::map_dfr(sheets, function(sh) {
    x <- readxl::read_excel(path, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal")
    x <- x[-(1:2), 1:14]
    names(x) <- c("region_name", "code", paste0(rep(host_cols, each = 2), c("_ha", "_m3")))
    x |>
      dplyr::filter(!is.na(code), !is.na(spruce_ha), !grepl("^(Source|Notes)", region_name)) |>
      dplyr::mutate(sheet = sh, code = trimws(code), region_name = trimws(gsub("\\*", "", region_name)))
  })
}

host_area_long <- function(appx, dash_means_absent = TRUE) {
  ha <- appx |>
    dplyr::select(sheet, region_name, code, dplyr::ends_with("_ha")) |>
    tidyr::pivot_longer(dplyr::ends_with("_ha"), names_to = "host_group", values_to = "raw") |>
    dplyr::mutate(
      host_group = sub("_ha$", "", host_group),
      dash = trimws(raw) %in% c("-", "–"),
      area_ha = suppressWarnings(as.numeric(raw)),
      area_ha = dplyr::if_else(dash & dash_means_absent, 0, area_ha),
      code = dplyr::recode(code, CR = "HR", SL = "SI", DEB = "DEB; DEC", DEC = "DEB; DEC")
    )
  # sum Rheinland-Pfalz (DEB) and Saarland (DEC) into the merged region
  ha |>
    dplyr::group_by(code, host_group) |>
    dplyr::summarise(area_ha = if (all(is.na(area_ha))) NA_real_ else sum(area_ha, na.rm = TRUE),
                     dash = all(dash), sheet = dplyr::first(sheet), .groups = "drop") |>
    dplyr::rename(nuts_id = code)
}

# Which host group of the appendix each insect's host tree corresponds to.
# Larch and minor broadleaves are not in the appendix: the conifer/broadleaf
# totals are used for scaling, but presence cannot be judged ("not specific").
host_group_of <- function(host_tree) {
  dplyr::case_when(
    host_tree == "Picea abies" ~ "spruce",
    host_tree == "Pinus sylvestris" ~ "pine",
    host_tree == "Quercus spp." ~ "oak",
    host_tree == "Abies alba" ~ "fir",
    host_tree == "Larix decidua" ~ "conifers_all",
    TRUE ~ "broadleaves_all"
  )
}

# Region x insect host information:
#   host_status: "present", "absent" (area 0, no recorded damage on that host),
#                "not specific" (larch / minor broadleaves), "no host data"
#   host_area_ha: area used to scale damage (specific host, else group total)
# Recorded damage on the same host tree in the region overrides "absent".
host_status_table <- function(cells, species_meta, host_long) {
  sp <- species_meta |>
    dplyr::transmute(species, host_tree, host_group = host_group_of(host_tree),
                     specific = host_group %in% c("spruce", "pine", "oak", "fir"))
  evidence <- cells |>
    dplyr::left_join(dplyr::select(species_meta, species, host_tree), by = "species") |>
    dplyr::group_by(nuts_id, host_tree) |>
    dplyr::summarise(damage_recorded_on_host = any(status == "observed_positive"), .groups = "drop")
  regions <- dplyr::distinct(cells, country, nuts_id)
  tidyr::crossing(regions, sp) |>
    dplyr::left_join(host_long |> dplyr::select(nuts_id, host_group, area_ha), by = c("nuts_id", "host_group")) |>
    dplyr::left_join(evidence, by = c("nuts_id", "host_tree")) |>
    dplyr::mutate(
      damage_recorded_on_host = dplyr::coalesce(damage_recorded_on_host, FALSE),
      host_status = dplyr::case_when(
        !nuts_id %in% host_long$nuts_id ~ "no host data",
        !specific ~ "not specific",
        damage_recorded_on_host ~ "present",
        is.na(area_ha) ~ "no host data",
        area_ha > 0 ~ "present",
        TRUE ~ "absent"
      ),
      host_area_ha = area_ha
    ) |>
    dplyr::select(country, nuts_id, species, host_tree, host_group, host_status, host_area_ha, damage_recorded_on_host)
}

# Host-scaled matrix: log(1 + 1000 * damage / host area) = damage per 1000 ha
# of host forest. Rows of regions without a usable host area are left empty.
host_scaled_L <- function(L, hs) {
  A <- host_area_matrix(L, hs)
  Ys <- log1p(1000 * expm1(L$Y) / A)
  Ys[!is.finite(Ys)] <- NA
  L2 <- L; L2$Y <- Ys; L2$area <- A
  # covariate for "any damage?": standardised log host area, 0 where unknown
  X <- log(A); X[!is.finite(X)] <- NA
  X <- (X - mean(X, na.rm = TRUE)) / stats::sd(X, na.rm = TRUE); X[is.na(X)] <- 0
  L2$X <- X
  L2
}

host_area_matrix <- function(L, hs) {
  key <- paste(hs$nuts_id, hs$species)
  area <- stats::setNames(hs$host_area_ha, key)
  A <- matrix(NA_real_, nrow(L$Y), ncol(L$Y))
  for (s in seq_along(L$species)) A[, s] <- area[paste(L$rows$nuts_id, L$species[s])]
  A[!is.na(A) & A <= 0] <- NA   # absent host: damage is structurally zero, not scaled
  A
}

# Mark "impossible" cells: not monitored and host absent
apply_host_status <- function(cells, hs) {
  cells |>
    dplyr::left_join(dplyr::select(hs, nuts_id, species, host_status), by = c("nuts_id", "species")) |>
    dplyr::mutate(
      host_absent = host_status == "absent",
      status = dplyr::if_else(status == "not_monitored" & host_absent, "impossible", status)
    )
}

# ---- Host-aware two-part model ----------------------------------------------
# Same two parts as R/hurdle.R, with host information added:
#   - absent host tree  -> damage is 0 (no prediction needed)
#   - Part 1 "any damage?" also sees log host area (one slope per insect)
#   - Part 2 "how much?" models damage per 1000 ha of host forest, then
#     multiplies back by the region's host area. Where host area is unknown,
#     the plain Part 2 prediction (R/hurdle.R) is used.
host_absent_matrix <- function(L, hs) {
  st <- stats::setNames(hs$host_status, paste(hs$nuts_id, hs$species))
  Z <- matrix(FALSE, nrow(L$Y), ncol(L$Y))
  for (s in seq_along(L$species)) Z[, s] <- st[paste(L$rows$nuts_id, L$species[s])] %in% "absent"
  Z
}

# Natural-scale prediction of the host-aware model.
host_combine <- function(p1, a_scaled, sm_scaled, a_plain, sm_plain, area, absent) {
  amount <- ifelse(!is.na(area) & !is.na(a_scaled),
                   pmax(expm1(a_scaled) * sm_scaled, 0) * area / 1000,
                   pmax(expm1(a_plain) * sm_plain, 0))
  ifelse(absent, 0, p1 * amount)
}

# Cross-validation on the same hidden blocks as h_cv (R/hurdle.R), with the
# chosen sizes of each part. Returns h_cv's hidden cells with host columns added.
host_cv <- function(L, Lh, absent, blocks, h_cv, best1, best2) {
  out <- list()
  for (f in sort(unique(blocks$fold))) {
    H <- held_mask(L, dplyr::filter(blocks, fold == f))
    Ytr <- L$Y; Ytr[H] <- NA
    B <- hurdle_parts(Ytr)$B
    Ys <- Lh$Y; Ys[H] <- NA
    Y2s <- hurdle_parts(Ys)$Y2
    f1 <- logit_fit(B, best1$K, best1$lambda, X = Lh$X)
    f2 <- lfm_fit(Y2s, K = best2$K, lambda = best2$lambda)
    idx <- which(H, arr.ind = TRUE)
    out[[length(out) + 1]] <- tibble::tibble(
      fold = f, row = idx[, 1], col = idx[, 2],
      p1_host = logit_predict(f1)[idx], p2_scaled = lfm_predict(f2)[idx],
      sm_scaled = smearing(f2, Y2s), area = Lh$area[idx], absent = absent[idx])
  }
  dplyr::bind_rows(out) |> dplyr::inner_join(h_cv, by = c("fold", "row", "col"))
}

# Natural-scale comparison of: one-part, two-part (no hosts), host-aware two-part.
host_compare <- function(hc, best1, best2, hs) {
  d <- hc |>
    dplyr::mutate(
      p1 = .data[[paste0("p1|", best1)]], a = .data[[paste0("p2|", best2)]], sm = .data[[paste0("sm|", best2)]],
      real = expm1(observed),
      one = pmax(expm1(one_part), 0),
      two = p1 * pmax(expm1(a) * sm, 0),
      host = host_combine(p1_host, p2_scaled, sm_scaled, a, sm, area, absent)
    ) |>
    dplyr::filter(!is.na(one), !is.na(two), !is.na(host)) |>
    dplyr::left_join(dplyr::select(hs, nuts_id, species, host_status), by = c("nuts_id", "species"))
  tot <- d |>
    dplyr::group_by(fold, country, species, year) |>
    dplyr::summarise(dplyr::across(c(real, one, two, host), sum), .groups = "drop") |>
    dplyr::filter(real > 0)
  lr <- function(x) abs(log((x + 1) / (tot$real + 1)))
  models <- c("One-part factor model", "Two-part model", "Two-part model + host trees")
  list(
    cells = d,
    summary = tibble::tibble(
      model = models,
      zeros_called_zero = c(mean(d$one[d$real == 0] < 1), mean(d$p1[d$real == 0] < 0.5),
                            mean(d$absent[d$real == 0] | d$p1_host[d$real == 0] < 0.5)),
      auc_any = c(auc(d$one, as.numeric(d$real > 0)), auc(d$p1, as.numeric(d$real > 0)),
                  auc(ifelse(d$absent, 0, d$p1_host), as.numeric(d$real > 0))),
      median_abs_log_ratio_totals = c(stats::median(lr(tot$one)), stats::median(lr(tot$two)), stats::median(lr(tot$host))),
      share_totals_within_2x = c(mean(lr(tot$one) <= log(2)), mean(lr(tot$two) <= log(2)), mean(lr(tot$host) <= log(2))),
      n_cells = nrow(d), n_blocks = nrow(tot)
    ),
    by_species = d |>
      dplyr::group_by(species) |>
      dplyr::summarise(
        n_cells = dplyr::n(),
        auc_host = auc(ifelse(absent, 0, p1_host), as.numeric(real > 0)),
        rmse_host = sqrt(mean((log1p(host) - observed)^2)),
        rmse_two = sqrt(mean((log1p(two) - observed)^2)),
        rmse_base = sqrt(mean((base_sy - observed)^2, na.rm = TRUE)),
        .groups = "drop"
      )
  )
}

# Final host-aware fit on all recorded data
host_final <- function(L, Lh, h_final, best1, best2) {
  f1 <- logit_fit(hurdle_parts(L$Y)$B, best1$K, best1$lambda, X = Lh$X)
  Y2s <- hurdle_parts(Lh$Y)$Y2
  f2 <- lfm_fit(Y2s, K = best2$K, lambda = best2$lambda)
  P2s <- lfm_predict(f2); ok <- !is.na(Y2s) & !is.na(P2s)
  list(P1 = logit_predict(f1), P2s = P2s, smear_s = mean(exp(Y2s[ok] - P2s[ok])),
       sigma_s = stats::sd(Y2s[ok] - P2s[ok]), plain = h_final, area = Lh$area)
}

# Confidence per insect, same rule as hurdle_confidence() but for this model
host_confidence <- function(by_species, species) {
  tibble::tibble(species = species) |>
    dplyr::left_join(by_species, by = "species") |>
    dplyr::mutate(confidence = dplyr::case_when(
      is.na(n_cells) ~ "untested",
      rmse_host < rmse_base & !is.na(auc_host) & auc_host >= 0.7 ~ "works",
      TRUE ~ "weak"
    ))
}

# Harmonised table with host information (replaces hurdle_fill output)
host_fill <- function(L, fin, absent, conf, cells, hs, unit_lookup) {
  n <- nrow(L$Y); p <- ncol(L$Y); z <- stats::qnorm(0.9)
  mon_cs <- cells |> dplyr::group_by(country, species) |>
    dplyr::summarise(ever = any(status != "not_monitored"), .groups = "drop")
  ix <- cbind(rep(seq_len(n), p), rep(seq_len(p), each = n))
  tibble::tibble(row = ix[, 1], col = ix[, 2]) |>
    dplyr::mutate(
      country = L$rows$country[row], nuts_id = L$rows$nuts_id[row], year = L$rows$year[row],
      species = L$species[col], y = L$Y[ix], absent = absent[ix],
      p_any = fin$P1[ix], area = fin$area[ix], as = fin$P2s[ix], ap = fin$plain$P2[ix]
    ) |>
    dplyr::left_join(mon_cs, by = c("country", "species")) |>
    dplyr::left_join(dplyr::select(conf, species, confidence), by = "species") |>
    dplyr::left_join(dplyr::select(hs, nuts_id, species, host_status, host_area_ha), by = c("nuts_id", "species")) |>
    dplyr::left_join(unit_lookup, by = "species") |>
    dplyr::mutate(
      scaled = !is.na(area) & !is.na(as),
      value_type = dplyr::case_when(
        !is.na(y) & y > 0 ~ "observed damage",
        !is.na(y) ~ "observed zero",
        absent ~ "zero: host tree absent",
        is.na(p_any) | (is.na(as) & is.na(ap)) ~ "not fillable (nothing recorded in this region-year)",
        TRUE ~ "filled"
      ),
      fl = value_type == "filled",
      gap_type = dplyr::case_when(!fl ~ NA_character_,
                                  ever ~ "country monitors this insect, missed this year/region",
                                  TRUE ~ "country never monitored this insect"),
      amount_if_damaged = dplyr::case_when(
        fl & scaled ~ pmax(expm1(as) * fin$smear_s, 0) * area / 1000,
        fl ~ pmax(expm1(ap) * fin$plain$smear, 0)),
      amount_lo80 = dplyr::case_when(
        fl & scaled ~ pmax(expm1(as - z * fin$sigma_s), 0) * area / 1000,
        fl ~ pmax(expm1(ap - z * fin$plain$sigma2), 0)),
      amount_hi80 = dplyr::case_when(
        fl & scaled ~ expm1(as + z * fin$sigma_s) * area / 1000,
        fl ~ expm1(ap + z * fin$plain$sigma2)),
      p_any = dplyr::if_else(fl, p_any, NA_real_),
      damage = dplyr::case_when(
        value_type %in% c("observed damage", "observed zero") ~ expm1(y),
        value_type == "zero: host tree absent" ~ 0,
        fl ~ p_any * amount_if_damaged),
      amount_basis = dplyr::case_when(fl & scaled ~ "per host area", fl ~ "no host area (plain model)"),
      confidence = dplyr::if_else(fl, confidence, NA_character_)
    ) |>
    dplyr::select(country, nuts_id, year, species, unit, value_type, damage, p_any,
                  amount_if_damaged, amount_lo80, amount_hi80, gap_type, confidence,
                  host_status, host_area_ha, amount_basis)
}
