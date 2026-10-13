# Correlation metrics along the hierarchy used in the data file and in
# Hlasny et al. (2025): the file's indicator columns group every insect code
#   level 1 (4 broad groups, as in the paper): guild x host type
#             borers on conifers / borers on broadleaves /
#             defoliators on conifers / defoliators on broadleaves
#   level 2 (host tree): borers on spruce / pine / fir / other conifers (larch),
#             defoliators on spruce / pine; broadleaf groups as in level 1
#   level 3: the insect codes themselves (species, genus, aggregate groups)
#
# Group value in a region-year = sum of the damage of its recorded member codes
# (NA if none recorded). Units are the same within a guild (borers m3,
# defoliators ha). Zeros are real observations, as everywhere else.
# Correlations at levels 1-2 use the same metrics as the species level
# (R/metrics.R): same-year r of z_within and beyond-shared-bad-years r of the
# anomaly, with region block-bootstrap intervals.

hierarchy_table <- function(species_meta) {
  f <- species_meta$flags
  has <- function(x) grepl(x, f, fixed = TRUE)
  level1 <- dplyr::case_when(
    has("Bark borers on conifers") ~ "borers on conifers",
    has("Bark borers on decidous") ~ "borers on broadleaves",
    has("Defoliators on conifers") ~ "defoliators on conifers",
    has("Defoliators on decidous") ~ "defoliators on broadleaves"
  )
  level2 <- dplyr::case_when(
    has("Bark borers on spruce") ~ "borers on spruce",
    has("Bark borers on pine") ~ "borers on pine",
    has("Bark borers on fir") ~ "borers on fir",
    level1 == "borers on conifers" ~ "borers on other conifers",
    has("Defoliators on spruce") ~ "defoliators on spruce",
    has("Defoliators on pine") ~ "defoliators on pine",
    TRUE ~ level1
  )
  tibble::tibble(species = species_meta$species, unit = species_meta$unit,
                 level1 = level1, level2 = level2)
}

# Sum member codes into group cells (cells-like table for make_transforms)
aggregate_cells <- function(cells, hier, level) {
  cells |>
    dplyr::left_join(dplyr::select(hier, species, group = dplyr::all_of(level)), by = "species") |>
    dplyr::filter(status != "not_monitored") |>
    dplyr::group_by(country, nuts_id, year, group, unit) |>
    dplyr::summarise(damage = sum(damage), n_codes = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(status = dplyr::if_else(damage > 0, "observed_positive", "observed_zero")) |>
    dplyr::rename(species = group)
}

# Pairwise metrics for one level (no transferability step at group level)
level_correlations <- function(cells, hier, level) {
  gc <- aggregate_cells(cells, hier, level)
  tr <- make_transforms(gc, min_nonzero = 0)
  groups <- sort(unique(gc$species))
  ts0 <- tibble::tibble(version = character(), species_a = character(), species_b = character(),
                        transferability = character(), countries_n10 = integer())
  correlation_metrics(tr, groups, ts0) |>
    dplyr::select(-transferability, -countries_n10) |>
    dplyr::mutate(level = level, .before = 1)
}

# Paper-style check: Europe-wide yearly totals (sum over all regions recorded
# that year), log(1 + x), Pearson r over the 23 years
hier_europe_correlations <- function(cells, hier, level) {
  w <- aggregate_cells(cells, hier, level) |>
    dplyr::group_by(species, year) |>
    dplyr::summarise(total = log1p(sum(damage)), .groups = "drop") |>
    tidyr::pivot_wider(names_from = species, values_from = total)
  g <- setdiff(names(w), "year")
  tidyr::expand_grid(species_a = g, species_b = g) |>
    dplyr::filter(species_a < species_b) |>
    dplyr::mutate(level = level,
                  r_europe_total = purrr::map2_dbl(species_a, species_b, function(a, b) {
                    ok <- !is.na(w[[a]]) & !is.na(w[[b]])
                    if (sum(ok) >= 10) stats::cor(w[[a]][ok], w[[b]][ok]) else NA_real_
                  }))
}

# Do insects in the same group move together more than insects in different
# groups? Species pairs are classed by the lowest level they share; the mean r
# per class is compared with 999 random relabellings of the codes (labels
# shuffled among codes, keeping group sizes).
hierarchy_pair_test <- function(cm, hier, var = "same_year", n_perm = 999, seed = 20261013) {
  d <- cm |> dplyr::filter(!is.na(.data[[var]])) |>
    dplyr::select(species_a, species_b, r = dplyr::all_of(var))
  cls <- function(h) {
    l1 <- stats::setNames(h$level1, h$species); l2 <- stats::setNames(h$level2, h$species)
    dplyr::case_when(
      l2[d$species_a] == l2[d$species_b] ~ "same host-tree group (level 2)",
      l1[d$species_a] == l1[d$species_b] ~ "same broad group only (level 1)",
      TRUE ~ "different broad groups"
    )
  }
  obs <- c(tapply(d$r, cls(hier), mean))
  set.seed(seed)
  perm <- replicate(n_perm, {
    h <- hier; i <- sample(nrow(h)); h$level1 <- h$level1[i]; h$level2 <- h$level2[i]
    m <- c(tapply(d$r, factor(cls(h), names(obs)), mean)); unname(m[names(obs)])
  })
  tibble::tibble(metric = var, pair_class = names(obs), n_pairs = as.integer(table(cls(hier))[names(obs)]),
                 mean_r = as.numeric(obs),
                 mean_r_random = rowMeans(perm, na.rm = TRUE),
                 p_value = (rowSums(perm >= obs, na.rm = TRUE) + 1) / (n_perm + 1))
}
