# Top five borers from I. typographus: dredge, GAM and random forest next to the
# linear models, the comparison with the 2025 paper, and the data sheet.
#
# Dredge: every sub-model of Sx ~ IT x VPD x latitude x longitude that keeps
# the hierarchy (an interaction only with all its lower-order terms, as
# MuMIn::dredge does with its default), fitted on all data and ranked by AICc;
# the best one is then tested. Written out here because MuMIn could not be
# installed in this environment; the ranking is the same as dredge's default.
# GAM (mgcv): Sx ~ s(IT) + s(VPD) + s(latitude) + s(longitude), basis size
# limited by the number of distinct values. Random forest (ranger, 500 trees):
# Sx ~ IT + VPD + latitude + longitude. All on log(damage + 1).

aicc <- function(m) { k <- length(stats::coef(m)) + 1; n <- stats::nobs(m); stats::AIC(m) + 2 * k * (k + 1) / (n - k - 1) }

dredge_terms <- function(full = predict_all_formula) {
  tt <- labels(stats::terms(full))
  parts <- lapply(tt, function(t) strsplit(t, ":", fixed = TRUE)[[1]])
  # lower-order terms of each term
  low <- lapply(parts, function(p) if (length(p) == 1) character() else
    unlist(lapply(seq_len(length(p) - 1), function(k) apply(utils::combn(p, k), 2, paste, collapse = ":"))))
  ok <- list()
  for (i in 0:(2^length(tt) - 1)) {
    s <- tt[bitwAnd(i, 2^(seq_along(tt) - 1)) > 0]
    if (all(unlist(low[match(s, tt)]) %in% s)) ok[[length(ok) + 1]] <- s
  }
  ok
}

dredge_fit <- function(d, sets = dredge_terms()) {
  res <- purrr::map_dfr(sets, function(s) {
    fo <- stats::as.formula(paste("sx ~", if (length(s)) paste(s, collapse = " + ") else "1"))
    m <- stats::lm(fo, d)
    tibble::tibble(formula = paste("Sx ~", if (length(s)) paste(s, collapse = " + ") else "1"), n_terms = length(s), AICc = aicc(m))
  })
  dplyr::arrange(dplyr::mutate(res, delta = AICc - min(AICc)), AICc)
}

# held-out predictions for one method; by = "nuts_id" (hide a region) or "country"
cv_method <- function(d, method, by, fo = NULL) {
  p <- rep(NA_real_, nrow(d))
  kk <- function(v, tr) max(3, min(5, dplyr::n_distinct(tr[[v]]) - 1))
  for (g in unique(d[[by]])) {
    i <- d[[by]] == g; tr <- d[!i, ]
    p[i] <- switch(method,
      lm = suppressWarnings(stats::predict(stats::lm(fo, tr), d[i, ])),
      gam = {
        if (dplyr::n_distinct(tr$nuts_id) < 6) NA else {
          f <- stats::as.formula(sprintf("sx ~ s(it, k = 5) + s(vpd, k = %d) + s(lat, k = %d) + s(lon, k = %d)", kk("vpd", tr), kk("lat", tr), kk("lon", tr)))
          as.numeric(stats::predict(mgcv::gam(f, data = tr, method = "REML"), d[i, ]))
        }
      },
      rf = stats::predict(ranger::ranger(sx ~ it + vpd + lat + lon, data = tr, num.trees = 500, seed = 1), d[i, ])$predictions)
  }
  p
}

compare_methods <- function(dmg, top, pos, clim, ph_hier) {
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  r2t <- function(p, y) { k <- !is.na(p); 1 - sum((y[k] - p[k])^2) / sum((y[k] - mean(y[k]))^2) }
  pred <- list(); dr <- list()
  sc <- purrr::map_dfr(top$species, function(s) {
    d <- predict_all_data(dmg, s, pos, clim)
    dd <- dredge_fit(d); dr[[s]] <<- dplyr::mutate(utils::head(dd, 10), species = s, name = unname(nm[s]), .before = 1)
    fw <- forward_terms(d)$formula
    meths <- list(`linear, IT only` = list("lm", sx ~ it), `linear, full equation` = list("lm", predict_all_formula),
                  `linear, built step by step` = list("lm", fw), `linear, dredge best (AICc)` = list("lm", stats::as.formula(sub("Sx", "sx", dd$formula[1]))),
                  GAM = list("gam", NULL), `random forest` = list("rf", NULL))
    purrr::imap_dfr(meths, function(m, lab) {
      purrr::map_dfr(c(`hide a region` = "nuts_id", `hide a country` = "country"), function(by) {
        if (by == "country" && dplyr::n_distinct(d$country) < 2) return(NULL)
        p <- cv_method(d, m[[1]], by, m[[2]])
        pred[[paste(s, lab, by)]] <<- dplyr::mutate(d, species = s, pred = p, method = lab, test = by)
        tibble::tibble(species = s, name = unname(nm[s]), method = lab, test = if (by == "nuts_id") "hide a region" else "hide a country",
                       formula = if (m[[1]] == "lm") paste(deparse(m[[2]], width.cutoff = 200), collapse = "") else NA_character_,
                       regions = dplyr::n_distinct(d$nuts_id), countries = dplyr::n_distinct(d$country), region_years = nrow(d),
                       R2_tested = r2t(p, d$sx), r_tested = stats::cor(p, d$sx, use = "complete.obs"))
      })
    })
  })
  list(scores = sc, dredge = dplyr::bind_rows(dr), pred = dplyr::bind_rows(pred))
}

# Paper Table 6: r2 of the Europe yearly totals (original scale) of I. typographus
# and the target. Ours: r2 of the Europe yearly totals of observed and predicted
# damage of the target (hide a region), back-transformed and summed over regions.
compare_with_paper <- function(pred, t6) {
  p6 <- dplyr::filter(t6, species_a == "ips_typ" | species_b == "ips_typ") |>
    dplyr::transmute(species = dplyr::if_else(species_a == "ips_typ", species_b, species_a), paper_r2 = paper, paper_r2_recomputed = r_squared)
  pred |> dplyr::filter(test == "nuts_id", !is.na(pred)) |>
    dplyr::group_by(species, method, year) |> dplyr::summarise(obs = sum(expm1(sx)), pred = sum(expm1(pred)), .groups = "drop") |>
    dplyr::group_by(species, method) |> dplyr::summarise(r2_europe_totals = stats::cor(obs, pred)^2, .groups = "drop") |>
    dplyr::left_join(p6, by = "species") |>
    dplyr::mutate(not_lower_than_paper = dplyr::if_else(is.na(paper_r2), NA, r2_europe_totals >= paper_r2))
}

# Data sheet (page 2 of the notes): one sheet per target, every region-year
# with I. typographus; target damage empty where the target is not recorded
write_pair_sheets <- function(dmg, top, pos, clim, ph_hier, path) {
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  sp <- function(s) dplyr::filter(dmg, species == s, !is.na(damage)) |> dplyr::select(country, nuts_id, year, damage)
  sheets <- lapply(top$species, function(s) {
    tg <- sp(s); with_target <- unique(tg$nuts_id)
    dplyr::rename(sp("ips_typ"), predictor_ips_typographus_m3 = damage) |>
      dplyr::left_join(dplyr::rename(tg, target_m3 = damage), by = c("country", "nuts_id", "year")) |>
      dplyr::left_join(dplyr::select(pos, nuts_id, lon, lat), by = "nuts_id") |>
      dplyr::left_join(dplyr::select(clim, nuts_id, vpd), by = "nuts_id") |>
      dplyr::mutate(region_status = dplyr::if_else(nuts_id %in% with_target, "both recorded", "target not recorded (to predict)")) |>
      dplyr::transmute(nuts_code = nuts_id, country, lon, lat, vpd_kPa = round(vpd, 4), year, target = unname(nm[s]), target_m3,
                       predictor = "Ips typographus", predictor_ips_typographus_m3, region_status) |>
      dplyr::arrange(region_status, nuts_code, year)
  })
  names(sheets) <- substr(gsub("[^A-Za-z ]", "", unname(nm[top$species])), 1, 31)
  overview <- dplyr::bind_rows(lapply(seq_along(sheets), function(i) sheets[[i]] |> dplyr::group_by(target = names(sheets)[i], region_status) |>
    dplyr::summarise(regions = dplyr::n_distinct(nuts_code), countries = paste(sort(unique(country)), collapse = " "), .groups = "drop")))
  writexl::write_xlsx(c(list(overview = overview), sheets), path)
  path
}

plot_compare_methods <- function(sc) {
  d <- sc |> dplyr::mutate(name = forcats::fct_reorder(name, -region_years),
                           method = factor(method, c("linear, IT only", "linear, built step by step", "linear, dredge best (AICc)", "linear, full equation", "GAM", "random forest")),
                           test = factor(test, c("hide a region", "hide a country")),
                           shown = pmax(R2_tested, -0.5))
  ggplot2::ggplot(d, ggplot2::aes(shown, method, colour = test)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#c9c8c3") +
    ggplot2::geom_point(size = 2.6, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::scale_colour_manual(values = c(`hide a region` = "#184f95", `hide a country` = "#eb6834"), name = NULL) +
    ggplot2::facet_wrap(~ name, ncol = 3) +
    ggplot2::labs(x = "Tested R-squared (values below -0.5 drawn at -0.5)", y = NULL,
                  title = "Top five borers predicted from Ips typographus: which method predicts best?",
                  subtitle = "Each region (blue) or each country (orange) predicted by a model fitted without it. 0 = no better than the overall mean.",
                  caption = "Predictors: I. typographus damage, VPD, latitude, longitude; log(damage + 1). EU-ForDAM value1, reported data. VPD: WorldClim 2.1.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", strip.text = ggplot2::element_text(face = "bold"))
}

# Predictability against the amount of data (notes, page 5: R ~ N)
predict_vs_n <- function(dmg, pos, clim, meta, ph_hier) {
  nm <- stats::setNames(ph_hier$name, ph_hier$species)
  r2t <- function(p, y) 1 - sum((y - p)^2) / sum((y - mean(y))^2)
  codes <- setdiff(unique(dmg$species[!is.na(dmg$damage)]), "ips_typ")
  purrr::map_dfr(codes, function(s) {
    d <- predict_all_data(dmg, s, pos, clim)
    if (nrow(d) < 30 || dplyr::n_distinct(d$nuts_id) < 3 || stats::sd(d$sx) == 0) return(NULL)
    tibble::tibble(species = s, name = unname(nm[s]), region_years = nrow(d), regions = dplyr::n_distinct(d$nuts_id),
                   countries = dplyr::n_distinct(d$country),
                   R2_tested_it_only = loro_r2(d, sx ~ it), R2_tested_step = loro_r2(d, forward_terms(d)$formula))
  }) |> dplyr::left_join(dplyr::select(meta, species, unit, guild = guild_from_file), by = "species")
}

plot_predict_vs_n <- function(vn) {
  d <- dplyr::mutate(vn, guild = dplyr::if_else(guild == "borer", "Bark and wood borers", "Defoliators"), shown = pmax(R2_tested_step, -0.5))
  ggplot2::ggplot(d, ggplot2::aes(region_years, shown, colour = guild)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#c9c8c3") +
    ggplot2::geom_point(size = 2.4) +
    ggrepel::geom_text_repel(ggplot2::aes(label = name), size = 2.4, show.legend = FALSE, seed = 1, max.overlaps = 30) +
    ggplot2::scale_colour_manual(values = c(`Bark and wood borers` = "#184f95", Defoliators = "#eb6834"), name = NULL) +
    ggplot2::labs(x = "Number of region-years where the insect and I. typographus are both recorded", y = "Tested R-squared (hide a region)",
                  title = "Predictability from Ips typographus against the amount of data",
                  subtitle = "Equation built up step by step from Sx ~ IT (terms of IT x VPD x latitude x longitude). Values below -0.5 drawn at -0.5.",
                  caption = "EU-ForDAM value1, reported data; log(damage + 1). Insects with at least 30 region-years in at least 3 regions.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top")
}
