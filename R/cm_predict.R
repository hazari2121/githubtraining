# Predictive correlation metric (Oct 2026).
#
# Question: if a country never reported insect B, do its records of insect A
# help us guess B's ups and downs there? This is exactly what gap-filling asks.
#
# Test (leave one country out): for every ordered pair A -> B and every country
# c where both are recorded together, learn the link from the OTHER countries
# and predict B in c from A in the same region-years. Values are z_within
# (log(1 + damage), each region series against its own mean), so the test is
# about timing (which years are high), not about levels.
#
#   skill = 1 - error(guess using A) / error(guess "normal year")
#     > 0 : A helps;  0 : no help;  < 0 : A misleads (the link does not transfer)
#   skill_beyond_year = the same, but the baseline already knows B's
#     Europe-wide yearly mean (from the other countries), so it asks whether A
#     adds information beyond years that were bad all over Europe.
#
# predictive_by_country(): one row per ordered pair x held-out country.
# predictive_summary(): errors summed over all held-out countries (pooled
# skill) and the share of held-out countries where A helped. No randomness,
# so no seed is needed.

predictive_by_country <- function(tr, species, min_test = 10, min_train = 20) {
  w <- tr |>
    dplyr::filter(!is.na(z_within)) |>
    dplyr::select(country, nuts_id, year, species, z_within) |>
    tidyr::pivot_wider(names_from = species, values_from = z_within)
  pairs <- tidyr::expand_grid(predictor = species, target = species) |>
    dplyr::filter(predictor != target, predictor %in% names(w), target %in% names(w))

  purrr::pmap_dfr(pairs, function(predictor, target) {
    d <- tibble::tibble(country = w$country, year = w$year, a = w[[predictor]], b = w[[target]]) |>
      dplyr::filter(!is.na(a), !is.na(b))
    cs <- d |> dplyr::count(country) |> dplyr::filter(n >= min_test) |> dplyr::pull(country)
    if (length(cs) < 2) return(NULL)
    purrr::map_dfr(cs, function(c) {
      trn <- dplyr::filter(d, country != c); tst <- dplyr::filter(d, country == c)
      if (nrow(trn) < min_train || stats::sd(trn$a) == 0) return(NULL)
      # Europe-wide yearly mean of B, from the other countries only
      ym <- trn |> dplyr::group_by(year) |> dplyr::summarise(ym = mean(b), .groups = "drop")
      trn <- dplyr::left_join(trn, ym, by = "year")
      tst <- dplyr::left_join(tst, ym, by = "year") |> dplyr::mutate(ym = dplyr::coalesce(ym, 0))
      f1 <- stats::lm(b ~ a, trn)                 # A only
      f0 <- stats::lm(b ~ ym, trn)                # Europe-wide yearly mean only
      f2 <- stats::lm(b ~ a + ym, trn)            # A on top of the yearly mean
      tibble::tibble(predictor, target, held_out = c, n = nrow(tst),
                     e_normal = sum(tst$b^2),     # "normal year" guess = 0
                     e_a = sum((tst$b - stats::predict(f1, tst))^2),
                     e_year = sum((tst$b - stats::predict(f0, tst))^2),
                     e_year_a = sum((tst$b - stats::predict(f2, tst))^2),
                     skill = 1 - e_a / e_normal)
    })
  })
}

# One row per unordered pair (both directions pooled) with a plain verdict:
#   predicts well     : pooled skill >= 0.05 and A helped in >= 75% of held-out countries
#   predicts a little : pooled skill > 0 and A helped in at least half
#   misleads          : pooled skill < -0.05 (the link learnt elsewhere points the wrong way)
#   no help           : the rest;  cannot test : recorded together in < 2 countries
predictive_summary <- function(pc, verdict) {
  s <- pc |>
    dplyr::mutate(species_a = pmin(predictor, target), species_b = pmax(predictor, target)) |>
    dplyr::group_by(species_a, species_b) |>
    dplyr::summarise(n_countries_tested = dplyr::n_distinct(held_out),
                     skill = 1 - sum(e_a) / sum(e_normal),
                     skill_beyond_year = 1 - sum(e_year_a) / sum(e_year),
                     helped_share = mean(e_a < e_normal),
                     countries_tested = paste(sort(unique(held_out)), collapse = ","),
                     .groups = "drop")
  verdict |>
    dplyr::select(species_a, species_b, group_a, group_b, verdict, r) |>
    dplyr::left_join(s, by = c("species_a", "species_b")) |>
    dplyr::mutate(predictive = dplyr::case_when(
      is.na(skill) | n_countries_tested < 2 ~ "cannot test",
      skill >= 0.05 & helped_share >= 0.75 ~ "predicts well",
      skill > 0 & helped_share >= 0.5 ~ "predicts a little",
      skill < -0.05 ~ "misleads",
      TRUE ~ "no help"),
      predictive = factor(predictive, c("predicts well", "predicts a little", "no help", "misleads", "cannot test")))
}
