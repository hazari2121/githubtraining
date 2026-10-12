# Harmonisation at agent-group level (guild x host tree).
#
# Countries report different insect lists, so most "gaps" at insect level are
# insects a country never monitors, not missing records. Groups that nearly all
# countries report are compared instead:
#   group value = sum of the recorded insects of that group in a region-year
#                 (NA if none of the group's insects was recorded).
# Units are the same within a guild (borers m3, defoliators ha). Defoliator
# areas of different insects may overlap, so group sums can double-count area.
# The grouping is PROVISIONAL (guild and host from the file's indicator
# columns) until confirmed by T. Hlasny.

host_label <- function(host_tree) {
  dplyr::case_when(
    host_tree == "Picea abies" ~ "spruce",
    host_tree == "Pinus sylvestris" ~ "pine",
    host_tree == "Abies alba" ~ "fir",
    host_tree == "Larix decidua" ~ "larch",
    host_tree == "Quercus spp." ~ "oak",
    TRUE ~ "other broadleaves"
  )
}

# species -> agent group, e.g. "borers on spruce"
agent_groups <- function(species_meta, guild_lookup) {
  species_meta |>
    dplyr::select(species, unit, host_tree) |>
    dplyr::left_join(dplyr::select(guild_lookup, species, guild), by = "species") |>
    dplyr::mutate(host = host_label(host_tree),
                  group = paste0(guild, "s on ", host))
}

# Region-year x group cells, with how many of the group's insects were recorded
group_cells <- function(cells, groups) {
  cells |>
    dplyr::left_join(dplyr::select(groups, species, group), by = "species") |>
    dplyr::group_by(country, nuts_id, year, group) |>
    dplyr::summarise(
      n_insects_recorded = sum(status != "not_monitored"),
      damage = if (any(status != "not_monitored")) sum(damage, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(status = dplyr::case_when(
      is.na(damage) ~ "not_monitored",
      damage == 0 ~ "observed_zero",
      TRUE ~ "observed_positive"
    ))
}

# Pseudo species table for the groups (host tree used for host status)
group_meta <- function(groups) {
  rep_host <- c(spruce = "Picea abies", pine = "Pinus sylvestris", fir = "Abies alba",
                larch = "Larix decidua", oak = "Quercus spp.", `other broadleaves` = "other broadleaves")
  groups |>
    dplyr::distinct(group, host, unit) |>
    dplyr::transmute(species = group, unit, host_tree = unname(rep_host[host]))
}

# tr-like table (log values) for build_lfm_matrix()
group_tr <- function(gcells) {
  gcells |>
    dplyr::filter(status != "not_monitored") |>
    dplyr::transmute(country, nuts_id, year, species = group, damage, log_damage = log1p(damage))
}

# Which country x group combinations are ever reported; what kind of gap each
# not-monitored cell is.
gap_types <- function(c) {
  c <- dplyr::mutate(c, rec = status %in% c("observed_positive", "observed_zero"))
  span <- c |>
    dplyr::group_by(country, species) |>
    dplyr::summarise(ever = any(rec),
                     first = if (any(rec)) min(year[rec]) else NA_real_,
                     last = if (any(rec)) max(year[rec]) else NA_real_,
                     .groups = "drop")
  rowany <- c |>
    dplyr::group_by(nuts_id, year) |>
    dplyr::summarise(row_has_data = any(rec), .groups = "drop")
  c |>
    dplyr::left_join(span, by = c("country", "species")) |>
    dplyr::left_join(rowany, by = c("nuts_id", "year")) |>
    dplyr::mutate(gap = dplyr::case_when(
      rec ~ "recorded",
      status == "impossible" ~ "host absent",
      !ever ~ "never reported by this country",
      year < first ~ "before the series starts",
      year > last ~ "after the series ends",
      TRUE ~ "missing year inside the series"
    )) |>
    dplyr::select(-rec)
}

gap_summary <- function(level_cells, level) {
  gap_types(level_cells) |>
    dplyr::count(gap, name = "cells") |>
    dplyr::mutate(level = level, share = cells / sum(cells))
}

# ---- Back-casting the years before a series starts --------------------------
# For a series (region r, unit u) starting late, the value in an earlier year t
#   = its own level in its first `ref_n` recorded years
#   + the change between year t and those reference years, averaged over the
#     regions (other countries) that recorded u in both.
# Baseline: own reference level, no change ("carry back").
backcast_predict <- function(Y, rows, target_rows, col, years_hide, ref_years, min_donors = 3) {
  y <- Y[, col]
  own <- mean(y[target_rows][rows$year[target_rows] %in% ref_years], na.rm = TRUE)
  donors <- which(!rows$country %in% rows$country[target_rows])   # other countries only
  dn <- split(donors, rows$nuts_id[donors])
  out <- sapply(years_hide, function(t) {
    d <- vapply(dn, function(ii) {
      yr <- rows$year[ii]
      a <- y[ii][yr == t]; b <- mean(y[ii][yr %in% ref_years], na.rm = TRUE)
      if (length(a) == 1 && !is.na(a) && is.finite(b)) a - b else NA_real_
    }, numeric(1))
    if (sum(!is.na(d)) >= min_donors) mean(d, na.rm = TRUE) else NA_real_
  })
  tibble::tibble(year = years_hide, carry_back = own, backcast = own + out)
}

# Test: for countries recorded from 2000, hide the first `n_hide` years of the
# whole country (as for AT, PL, FI) and predict them from other countries.
backcast_cv <- function(L, n_hide_set = c(2, 3, 12), ref_n = 5) {
  rows <- L$rows; out <- list()
  for (n_hide in n_hide_set) {
    hide <- 2000:(1999 + n_hide); ref <- (2000 + n_hide):(1999 + n_hide + ref_n)
    for (s in seq_along(L$species)) for (cn in unique(rows$country)) {
      ci <- which(rows$country == cn)
      for (r in unique(rows$nuts_id[ci])) {
        ri <- ci[rows$nuts_id[ci] == r]
        obs <- L$Y[ri, s]; yrs <- rows$year[ri]
        if (any(is.na(obs[yrs %in% c(hide, ref)]))) next
        p <- backcast_predict(L$Y, rows, ri, s, hide, ref)
        out[[length(out) + 1]] <- tibble::tibble(
          n_hide = n_hide, country = cn, nuts_id = r, unit = L$species[s], year = hide,
          observed = obs[match(hide, yrs)], carry_back = p$carry_back, backcast = p$backcast)
      }
    }
  }
  dplyr::bind_rows(out)
}

backcast_scores <- function(bcv) {
  bcv |>
    dplyr::filter(!is.na(backcast)) |>
    dplyr::group_by(n_hide) |>
    dplyr::summarise(
      cells = dplyr::n(), series = dplyr::n_distinct(paste(nuts_id, unit)),
      rmse_backcast = sqrt(mean((backcast - observed)^2)),
      rmse_carry_back = sqrt(mean((carry_back - observed)^2)),
      skill = 1 - rmse_backcast / rmse_carry_back,
      .groups = "drop")
}

# Back-cast every real "before the series starts" gap of L (any length).
# The test (backcast_cv) showed that adding the change seen in other countries
# does NOT beat carrying the series' own early level back, so the simpler
# carry-back is used; its test error gives the 80% range.
backcast_fill <- function(L, gt, ref_n = 5) {
  todo <- gt |> dplyr::filter(gap == "before the series starts") |>
    dplyr::distinct(country, nuts_id, species, first)
  purrr::pmap_dfr(todo, function(country, nuts_id, species, first) {
    ri <- which(L$rows$nuts_id == nuts_id); s <- match(species, L$species)
    if (is.na(s)) return(NULL)
    p <- backcast_predict(L$Y, L$rows, ri, s, 2000:(first - 1), first:(first + ref_n - 1))
    tibble::tibble(country = country, nuts_id = nuts_id, species = species,
                   year = p$year, backcast_log = p$carry_back)
  })
}

# ---- Harmonised group-level dataset -------------------------------------------
# Starts from host_fill() output (observed / host absent / factor-model fill)
# and replaces cells before a series starts with back-casts.
# Never-reported country x group combinations: decision (Oct 2026) is that a
# group a country never reports is "no problem there" -> damage 0. The model's
# guess for those cells is kept in `model_guess` as a sensitivity check only.
group_harmonised <- function(filled, gt, bc, bc_scores, z = stats::qnorm(0.9)) {
  # 80% range from the back-cast test error (log scale), by length of the gap
  sd_bc <- max(bc_scores$rmse_carry_back)
  filled |>
    dplyr::left_join(dplyr::select(gt, nuts_id, year, species, gap), by = c("nuts_id", "year", "species")) |>
    dplyr::left_join(bc, by = c("country", "nuts_id", "species", "year")) |>
    dplyr::mutate(
      use_bc = gap == "before the series starts" & !is.na(backcast_log),
      never = gap == "never reported by this country" & value_type != "zero: host tree absent",
      model_guess = dplyr::if_else(never & value_type == "filled", damage, NA_real_),
      value_type = dplyr::case_when(
        use_bc ~ "back-cast (before the series starts)",
        gap == "before the series starts" ~ "not fillable (no donor countries)",
        never ~ "zero: never reported (assumed no damage)",
        value_type == "filled" ~ "filled: missing year in a reported series",
        TRUE ~ value_type),
      damage = dplyr::if_else(use_bc, pmax(expm1(backcast_log), 0), damage),
      damage = dplyr::if_else(never, 0, damage),
      dplyr::across(c(p_any, amount_if_damaged, amount_lo80, amount_hi80, amount_basis, confidence),
                    ~ dplyr::if_else(never, NA, .x)),
      damage = dplyr::if_else(grepl("^not fillable", value_type), NA_real_, damage),
      p_any = dplyr::if_else(use_bc, NA_real_, p_any),
      amount_if_damaged = dplyr::if_else(use_bc, NA_real_, amount_if_damaged),
      amount_lo80 = dplyr::if_else(use_bc, pmax(expm1(backcast_log - z * sd_bc), 0), amount_lo80),
      amount_hi80 = dplyr::if_else(use_bc, expm1(backcast_log + z * sd_bc), amount_hi80),
      amount_basis = dplyr::if_else(use_bc, "series' own level in its first 5 years", amount_basis)
    ) |>
    dplyr::rename(group = species) |>
    dplyr::select(-use_bc, -never, -backcast_log, -gap_type)
}

# Europe-wide totals per group and year, split by where the numbers come from
group_totals <- function(gh) {
  gh |>
    dplyr::group_by(group, unit, year) |>
    dplyr::summarise(
      observed = sum(damage[value_type %in% c("observed damage", "observed zero")]),
      back_cast = sum(damage[value_type == "back-cast (before the series starts)"]),
      filled_missing_year = sum(damage[value_type == "filled: missing year in a reported series"], na.rm = TRUE),
      model_guess_never_reported = sum(model_guess, na.rm = TRUE),   # sensitivity only, not in totals
      .groups = "drop") |>
    dplyr::mutate(harmonised_total = observed + back_cast + filled_missing_year)
}
