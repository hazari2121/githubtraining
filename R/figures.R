# Figures. Colours: diverging blue <-> grey <-> red for correlations,
# single-hue blue ramp for magnitudes, fixed categorical slots for guild/host.

pal <- list(
  surface = "#fcfcfb", ink = "#0b0b0b", ink2 = "#52514e", na = "#e6e5e1",
  div_low = "#1c5cab", div_mid = "#f0efec", div_high = "#c23b3a",
  seq = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"),
  guild = c(borer = "#2a78d6", defoliator = "#eb6834", other = "#1baf7a",
            `bark borer` = "#2a78d6", `wood borer` = "#4a3aa7"),
  host = c(conifer = "#008300", broadleaf = "#eda100", both = "#e87ba4")
)

theme_eu <- function(base = 9) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = pal$surface, colour = NA),
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(colour = pal$ink),
      axis.text = ggplot2::element_text(colour = pal$ink2),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(colour = pal$ink2, hjust = 0)
    )
}

save_fig <- function(p, path, w, h) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, p, width = w, height = h, dpi = 200, bg = pal$surface)
  path
}

plot_coverage <- function(cov_cs) {
  sp_order <- cov_cs |> dplyr::group_by(species) |>
    dplyr::summarise(k = sum(pct_observed > 0)) |> dplyr::arrange(dplyr::desc(k), species) |>
    dplyr::pull(species)
  ct_order <- cov_cs |> dplyr::group_by(country) |>
    dplyr::summarise(k = sum(pct_observed > 0)) |> dplyr::arrange(k) |> dplyr::pull(country)
  d <- cov_cs |>
    dplyr::mutate(species = factor(species, sp_order), country = factor(country, ct_order),
                  pct_observed = dplyr::na_if(pct_observed, 0))
  ggplot2::ggplot(d, ggplot2::aes(species, country, fill = pct_observed)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.5) +
    ggplot2::scale_fill_gradientn(colours = pal$seq, limits = c(0, 100), na.value = pal$na,
                                  name = "% region-years\nobserved") +
    ggplot2::labs(x = NULL, y = NULL, title = "Monitoring coverage: country x species",
                  caption = "Grey = not monitored. Observed = positive or zero value reported, 2000-2022.") +
    theme_eu() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
}

# Heatmap of a species x species matrix in clustering order, with guild/host strips
plot_matrix <- function(M, order, lookup, fill_name, title, caption, type = c("r", "n")) {
  type <- match.arg(type)
  d <- as.data.frame(as.table(M[order, order]), stringsAsFactors = FALSE) |>
    stats::setNames(c("a", "b", "value")) |>
    dplyr::mutate(a = factor(a, order), b = factor(b, rev(order)))
  if (type == "n") d$value[d$value == 0] <- NA

  ann <- lookup |> dplyr::filter(species %in% order) |>
    dplyr::mutate(a = factor(species, order))
  n <- length(order)

  p <- ggplot2::ggplot(d, ggplot2::aes(a, b)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = pal$surface, linewidth = 0.3)
  p <- if (type == "r") {
    # mark missing cells explicitly: the NA grey is too close to the r = 0 midpoint
    p + ggplot2::geom_point(data = dplyr::filter(d, is.na(value), as.character(a) != as.character(b)),
                            shape = 4, size = 1, colour = pal$ink2) +
      ggplot2::scale_fill_gradient2(low = pal$div_low, mid = pal$div_mid, high = pal$div_high,
                                      midpoint = 0, limits = c(-1, 1), na.value = pal$na, name = fill_name)
  } else {
    p + ggplot2::scale_fill_gradientn(colours = pal$seq, trans = "log10", na.value = pal$na, name = fill_name)
  }
  p +
    ggnewscale_guild(ann, n) +
    ggplot2::coord_equal(clip = "off", ylim = c(0.5, n + 2.6), expand = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = title, caption = caption) +
    theme_eu(8) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
}

# Guild and host strips above the matrix drawn as coloured points with a legend
# encoded by shape, so identity never relies on colour alone.
ggnewscale_guild <- function(ann, n) {
  list(
    ggplot2::geom_point(data = ann, ggplot2::aes(a, n + 1.2, colour = guild, shape = guild),
                        size = 2.2, inherit.aes = FALSE),
    ggplot2::geom_point(data = ann, ggplot2::aes(a, n + 2.1, colour = host, shape = host),
                        size = 2.2, inherit.aes = FALSE),
    ggplot2::scale_colour_manual(values = c(pal$guild, pal$host), na.value = pal$ink2,
                                 name = "Guild (row 1) / host (row 2)"),
    ggplot2::scale_shape_manual(values = c(borer = 15, defoliator = 17, other = 18,
                                           `bark borer` = 15, `wood borer` = 3,
                                           conifer = 16, broadleaf = 1, both = 10),
                                na.value = 4, name = "Guild (row 1) / host (row 2)"),
    ggplot2::annotate("text", x = 0.3, y = c(n + 1.2, n + 2.1), label = c("guild", "host"),
                      hjust = 1, size = 2.5, colour = pal$ink2)
  )
}

# Country-level r for the strongest pairs (with >= 2 countries)
plot_top_pairs <- function(ts, pc, k = 15, version = "z_within") {
  top <- ts |>
    dplyr::filter(n_countries >= 2, !is.na(r_pooled)) |>
    dplyr::arrange(dplyr::desc(abs(r_pooled))) |>
    dplyr::slice_head(n = k) |>
    dplyr::mutate(pair = paste(species_a, "x", species_b))
  lev <- rev(top$pair)
  d <- pc |> dplyr::filter(version == !!version) |>
    dplyr::inner_join(dplyr::select(top, species_a, species_b, pair, transferability),
                      by = c("species_a", "species_b")) |>
    dplyr::mutate(pair = factor(pair, lev)) |>
    # alternate labels above/below the dot so neighbouring countries stay legible
    dplyr::group_by(pair) |> dplyr::arrange(r_pearson, .by_group = TRUE) |>
    dplyr::mutate(lab_v = dplyr::if_else(dplyr::row_number() %% 2 == 1, -1.1, 2.1)) |>
    dplyr::ungroup()
  top <- top |> dplyr::mutate(pair = factor(pair, lev))
  ggplot2::ggplot(d, ggplot2::aes(r_pearson, pair)) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(size = n), colour = pal$seq[3], alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = country, vjust = lab_v), size = 2.3, colour = pal$ink2) +
    ggplot2::geom_point(data = top, ggplot2::aes(r_pooled, pair), shape = 124, size = 6,
                        colour = pal$ink, inherit.aes = FALSE) +
    ggplot2::geom_text(data = top, ggplot2::aes(1.02, pair, label = transferability),
                       hjust = 0, size = 2.5, colour = pal$ink2, inherit.aes = FALSE) +
    ggplot2::scale_x_continuous(limits = c(-1, 1.45), breaks = seq(-1, 1, 0.5)) +
    ggplot2::scale_size_area(max_size = 5, name = "n region-years") +
    ggplot2::labs(x = paste0("Pearson r (", version, ")"), y = NULL,
                  title = "Strongest pairs: correlation within each country",
                  caption = "Dots = countries (n >= 10); black bar = pooled r; right = transferability flag.") +
    theme_eu() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# Species ranking: regions where the species occurs (dark) out of regions where
# it is monitored (light), one panel per guild.
plot_species_ranking <- function(sp_rank) {
  d <- sp_rank |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other"),
                  species = factor(species, rev(species)),
                  lab = paste0(n_regions_occurring, " (", n_countries_occurring, " ctry)"))
  ggplot2::ggplot(d, ggplot2::aes(y = species)) +
    ggplot2::geom_col(ggplot2::aes(x = n_regions_monitored), fill = "#e6e5e1", width = 0.75) +
    ggplot2::geom_col(ggplot2::aes(x = n_regions_occurring, fill = guild), width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(x = n_regions_monitored, label = lab), hjust = -0.1,
                       size = 2.4, colour = pal$ink2) +
    ggplot2::scale_fill_manual(values = pal$guild, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::facet_grid(guild ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "Number of NUTS units", y = NULL,
                  title = "Where does each insect occur?",
                  subtitle = "Coloured bar = units with damage in at least one year; grey bar = units where monitored",
                  caption = "Label: units with damage (number of countries). National-level countries count as one unit.") +
    theme_eu(9) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# Region x species map: years with damage. Blank = not monitored,
# grey = monitored but never damaged, blue = years with damage.
plot_occurrence_map <- function(rs, sp_rank) {
  d <- rs |>
    dplyr::mutate(
      species = factor(species, sp_rank$species),
      region = paste0(nuts_id, " "),
      yrs = dplyr::na_if(years_with_damage, 0L)
    )
  reg_order <- d |> dplyr::distinct(country, region) |> dplyr::arrange(dplyr::desc(country), dplyr::desc(region))
  d$region <- factor(d$region, reg_order$region)
  ggplot2::ggplot(d, ggplot2::aes(species, region, fill = yrs)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.3) +
    ggplot2::scale_fill_gradientn(colours = pal$seq, limits = c(1, 23), na.value = "#cfcec9",
                                  name = "Years with\ndamage") +
    ggplot2::facet_grid(country ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL, title = "Insect occurrence by NUTS unit, 2000-2022",
                  caption = paste("Blank = not monitored. Grey = monitored, no damage recorded. Insects ordered by number of units where they occur.",
                                  "PL rows are district codes aggregated to NUTS2.")) +
    theme_eu(7) +
    ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5),
                   strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold"),
                   panel.spacing.y = ggplot2::unit(1.5, "pt"))
}

# Rank-occupancy curve: species (y, ranked) against number of NUTS units where
# they occur (x), joined by a line from the rarest to the most widespread.
plot_occupancy_curve <- function(sp_rank) {
  d <- sp_rank |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other")) |>
    dplyr::arrange(n_regions_occurring, dplyr::desc(species)) |>
    dplyr::mutate(species = factor(species, species), y = as.integer(species))
  n_units <- max(sp_rank$n_regions_monitored)
  ggplot2::ggplot(d, ggplot2::aes(n_regions_occurring, y)) +
    ggplot2::geom_path(colour = pal$ink2, linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(colour = guild, shape = guild), size = 2.6) +
    ggplot2::geom_text(ggplot2::aes(label = n_regions_occurring), hjust = 0, nudge_x = 1.3,
                       size = 2.4, colour = pal$ink2) +
    ggplot2::scale_y_continuous(breaks = d$y, labels = as.character(d$species),
                                expand = ggplot2::expansion(add = 0.8)) +
    ggplot2::scale_x_continuous(limits = c(0, n_units), breaks = seq(0, 80, 10)) +
    ggplot2::scale_colour_manual(values = pal$guild, name = "Guild") +
    ggplot2::scale_shape_manual(values = c(borer = 16, defoliator = 17, other = 15), name = "Guild") +
    ggplot2::labs(x = "Number of NUTS units where the species occurs (damage > 0 in at least one year)",
                  y = NULL, title = "Species occupancy across NUTS units",
                  subtitle = "Few widespread species, many species found in few units",
                  caption = paste0("Out of ", n_units, " NUTS units in the dataset. National-level countries count as one unit; PL district codes aggregated to NUTS2.")) +
    theme_eu(9) +
    ggplot2::theme(legend.position = c(0.82, 0.15),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3),
                   panel.grid.major.y = ggplot2::element_line(colour = "#f3f2ee", linewidth = 0.2))
}

# Two rank-occupancy curves side by side: NUTS units where each species is
# monitored, and NUTS units where it occurs. Each panel is ranked on its own
# measure, so species order differs between panels.
plot_occupancy_curves_monitored <- function(sp_rank) {
  n_units <- max(sp_rank$n_regions_monitored)
  long <- dplyr::bind_rows(
    sp_rank |> dplyr::transmute(species, guild, n = n_regions_monitored,
                                panel = "Monitored (value reported, 0 or > 0)"),
    sp_rank |> dplyr::transmute(species, guild, n = n_regions_occurring,
                                panel = "Occurring (damage > 0 in at least one year)")
  ) |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other"),
                  panel = factor(panel, unique(panel))) |>
    dplyr::group_by(panel) |>
    dplyr::arrange(n, dplyr::desc(species), .by_group = TRUE) |>
    dplyr::mutate(key = paste(species, as.integer(panel), sep = "___")) |>
    dplyr::ungroup()
  long$key <- factor(long$key, long$key)

  ggplot2::ggplot(long, ggplot2::aes(n, key, group = panel)) +
    ggplot2::geom_path(colour = pal$ink2, linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(colour = guild, shape = guild), size = 2.2) +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = 0, nudge_x = 1.5, size = 2.2, colour = pal$ink2) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", nrow = 1) +
    ggplot2::scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
    ggplot2::scale_x_continuous(limits = c(0, n_units + 4), breaks = seq(0, 80, 10)) +
    ggplot2::scale_colour_manual(values = pal$guild, name = "Guild") +
    ggplot2::scale_shape_manual(values = c(borer = 16, defoliator = 17, other = 15), name = "Guild") +
    ggplot2::labs(x = "Number of NUTS units", y = NULL,
                  title = "Species-NUTS ranking: monitored vs occurring",
                  subtitle = "Each panel ranked on its own measure (species order differs between panels)",
                  caption = paste0("Out of ", n_units, " NUTS units. National-level countries count as one unit; PL district codes aggregated to NUTS2.")) +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "bottom",
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   panel.spacing.x = ggplot2::unit(14, "pt"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3),
                   panel.grid.major.y = ggplot2::element_line(colour = "#f3f2ee", linewidth = 0.2))
}

# Explainer: what a correlation looks like. For each example pair and country,
# the yearly z-score of both species (mean over the country's regions).
# Lines that rise and fall together = high r.
plot_correlation_explainer <- function(tr, pc, examples) {
  d <- purrr::pmap_dfr(examples, function(a, b, country, label) {
    tr |>
      dplyr::filter(country == !!country, species %in% c(a, b), !is.na(z_within)) |>
      dplyr::group_by(species, year) |>
      dplyr::summarise(z = mean(z_within), .groups = "drop") |>
      dplyr::mutate(role = dplyr::if_else(species == a, "I. typographus", "Partner species"),
                    country = country, label = label, pair_b = b)
  })
  rr <- examples |>
    dplyr::left_join(dplyr::filter(pc, version == "z_within") |>
                       dplyr::select(a = species_a, b = species_b, country, r_pearson),
                     by = c("a", "b", "country")) |>
    dplyr::mutate(panel = paste0(country, ":  r = ", formatC(r_pearson, format = "f", digits = 2)))
  d <- d |> dplyr::left_join(dplyr::select(rr, label, country, panel), by = c("label", "country"))
  d$label <- factor(d$label, unique(examples$label))
  ggplot2::ggplot(d, ggplot2::aes(year, z, colour = role)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#d9d8d4", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = c(`I. typographus` = pal$guild[["borer"]],
                                            `Partner species` = pal$guild[["defoliator"]]), name = NULL) +
    ggplot2::facet_wrap(label ~ panel, ncol = 3, labeller = ggplot2::labeller(.multi_line = FALSE)) +
    ggplot2::labs(x = NULL, y = "Damage, standardised (0 = that region's average)",
                  title = "What a correlation looks like",
                  subtitle = "Lines that rise and fall together = high r. Top row: the link holds in every country. Bottom row: strong in one country, absent in others.",
                  caption = "z_within averaged over each country's regions. r = correlation across region-years in that country.") +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   panel.grid.major.y = ggplot2::element_line(colour = "#f3f2ee", linewidth = 0.2))
}

# ---- Latent factor model figures -------------------------------------------

# Model comparison: skill vs the species x year baseline and dynamics r
plot_lfm_comparison <- function(score, grid, best_model) {
  d <- score |>
    dplyr::left_join(grid, by = "model") |>
    dplyr::filter(is.na(K) | lambda == 50 | (K == 0)) |>
    dplyr::mutate(
      label = dplyr::case_when(
        is.na(K) ~ sub("Baseline: ", "", model),
        K == 0 ~ "Additive (region-year intensity, no factors)",
        TRUE ~ paste0(K, " hidden factor", ifelse(K > 1, "s", ""))),
      type = dplyr::case_when(is.na(K) ~ "Baseline", model == best_model ~ "Chosen model", TRUE ~ "Factor model"),
      label = factor(label, rev(unique(label[order(type != "Baseline", K)])))
    ) |>
    tidyr::pivot_longer(c(skill_vs_year_baseline, dynamics_r), names_to = "metric") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(metric = dplyr::recode(metric,
      skill_vs_year_baseline = "Error reduction vs 'species x year' baseline",
      dynamics_r = "Gets the ups and downs right (r)"))
  ggplot2::ggplot(d, ggplot2::aes(value, label, colour = type)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#d9d8d4") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = value, yend = label), linewidth = 0.6, alpha = 0.5) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(value, format = "f", digits = 2)),
                       hjust = -0.4, size = 2.6, colour = pal$ink2) +
    ggplot2::facet_wrap(~ metric, scales = "free_x") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.25))) +
    ggplot2::scale_colour_manual(values = c(Baseline = "#9c9b97", `Factor model` = pal$seq[3],
                                            `Chosen model` = pal$guild[["defoliator"]]), name = NULL) +
    ggplot2::labs(x = NULL, y = NULL, title = "Does the factor model fill gaps better than simple rules?",
                  subtitle = "Whole country x species blocks hidden and predicted (10-fold cross-validation, 108 blocks)",
                  caption = "Error = mean squared error on log(damage + 1). r = correlation between predicted and real yearly series.") +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# Per species: how well can the chosen model fill this species' gaps?
plot_lfm_species <- function(score_sp, best_model, species_meta) {
  d <- score_sp |>
    dplyr::filter(model == best_model) |>
    dplyr::left_join(dplyr::select(species_meta, species, guild = guild_from_file), by = "species") |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other")) |>
    dplyr::arrange(skill_vs_year_baseline) |>
    dplyr::mutate(species = factor(species, species))
  ggplot2::ggplot(d, ggplot2::aes(skill_vs_year_baseline, species, fill = guild)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("r = ", formatC(dynamics_r, format = "f", digits = 2)),
                                    x = pmax(skill_vs_year_baseline, 0)), hjust = -0.15, size = 2.4,
                       colour = pal$ink2) +
    ggplot2::scale_fill_manual(values = pal$guild, name = "Guild") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.2))) +
    ggplot2::labs(x = "Error reduction vs 'species x year' baseline (right = model helps)", y = NULL,
                  title = "Which insects can the factor model fill?",
                  subtitle = "Chosen model, hidden country x species blocks. Label: r for the ups and downs.",
                  caption = "Only species recorded in at least 2 countries can be tested.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", legend.justification = "left")
}

# Examples: real vs predicted for hidden series
plot_lfm_examples <- function(cv, best_model, examples) {
  d <- cv |>
    dplyr::filter(model %in% c(best_model, "Baseline: species x year average")) |>
    dplyr::inner_join(examples, by = c("nuts_id", "species")) |>
    dplyr::mutate(model = dplyr::if_else(model == best_model, "Factor model prediction", "Species x year baseline"))
  obs <- dplyr::distinct(d, title, year, observed)
  ggplot2::ggplot(d, ggplot2::aes(year)) +
    ggplot2::geom_line(data = obs, ggplot2::aes(y = observed, colour = "Real (hidden from the model)"), linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = predicted, colour = model, linetype = model), linewidth = 0.7) +
    ggplot2::facet_wrap(~ title, ncol = 3, scales = "free_y") +
    ggplot2::scale_colour_manual(values = c(`Real (hidden from the model)` = pal$ink,
                                            `Factor model prediction` = pal$guild[["defoliator"]],
                                            `Species x year baseline` = "#9c9b97"), name = NULL) +
    ggplot2::scale_linetype_manual(values = c(`Factor model prediction` = "solid",
                                              `Species x year baseline` = "22"), guide = "none") +
    ggplot2::labs(x = NULL, y = "log(damage + 1)", title = "Hidden series: real vs predicted",
                  subtitle = "The model never saw these values; it predicted them from other species in the same region and the species elsewhere.") +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   panel.grid.major.y = ggplot2::element_line(colour = "#f3f2ee", linewidth = 0.2))
}

# What the model learned: species loadings on the hidden factors, and the
# Europe-wide yearly pattern of overall intensity and of each factor
plot_lfm_factors <- function(load, year_scores) {
  K <- sum(grepl("^F[0-9]+$", names(load)))
  ld <- load |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other")) |>
    tidyr::pivot_longer(dplyr::matches("^F[0-9]+$"), names_to = "factor", values_to = "loading") |>
    dplyr::group_by(species) |> dplyr::mutate(ord = loading[factor == "F2"]) |> dplyr::ungroup() |>
    dplyr::arrange(guild, ord) |>
    dplyr::mutate(species = factor(species, unique(species)),
                  factor = paste("Factor", sub("F", "", factor)))
  p1 <- ggplot2::ggplot(ld, ggplot2::aes(loading, species, fill = guild)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::facet_grid(guild ~ factor, scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_manual(values = pal$guild, guide = "none") +
    ggplot2::labs(x = "Loading (how strongly the species follows the factor)", y = NULL,
                  title = "What the hidden factors represent") +
    theme_eu(8) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  ys <- year_scores |>
    dplyr::group_by(year) |>
    dplyr::summarise(dplyr::across(c(intensity, dplyr::matches("^F[0-9]+$")), mean), .groups = "drop") |>
    tidyr::pivot_longer(-year) |>
    dplyr::mutate(name = dplyr::recode(name, intensity = "Overall damage intensity",
                                       !!!stats::setNames(paste("Factor", seq_len(K)), paste0("F", seq_len(K)))))
  p2 <- ggplot2::ggplot(ys, ggplot2::aes(year, value)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#d9d8d4") +
    ggplot2::geom_line(colour = pal$seq[4], linewidth = 0.9) +
    ggplot2::facet_wrap(~ name, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "Mean score across regions",
                  title = "Europe-wide yearly pattern",
                  caption = "Overall intensity peaks in 2003-06 and 2018-19 (drought years).") +
    theme_eu(8) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
  list(loadings = p1, years = p2)
}

# ---- Correlation metrics v3 figures ------------------------------------------

# Strongest links with 95% bootstrap intervals: same-year r and r beyond
# shared bad years, for pairs with a reliable interval (>= 5 regions)
plot_cm_top <- function(cm, k = 25) {
  top <- cm |>
    dplyr::filter(!is.na(same_year_lo)) |>
    dplyr::arrange(dplyr::desc(abs(same_year))) |>
    dplyr::slice_head(n = k) |>
    dplyr::mutate(pair = paste(species_a, "&", species_b))
  d <- dplyr::bind_rows(
    top |> dplyr::transmute(pair, what = "Same year", r = same_year, lo = same_year_lo, hi = same_year_hi),
    top |> dplyr::transmute(pair, what = "Beyond shared bad years", r = beyond_year, lo = beyond_year_lo, hi = beyond_year_hi)
  ) |>
    dplyr::mutate(pair = factor(pair, rev(top$pair)), what = factor(what, c("Same year", "Beyond shared bad years")))
  ggplot2::ggplot(d, ggplot2::aes(r, pair, colour = what)) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.6,
                            position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::scale_colour_manual(values = c(`Same year` = pal$seq[4], `Beyond shared bad years` = pal$guild[["defoliator"]]),
                                 name = NULL) +
    ggplot2::scale_x_continuous(limits = c(-0.2, 1), breaks = seq(-0.2, 1, 0.2)) +
    ggplot2::labs(x = "Correlation r (dot) with 95% range (line)", y = NULL,
                  title = "Strongest links between insect species",
                  subtitle = "Blue: do they have bad years together? Orange: still linked after removing Europe-wide bad years?",
                  caption = "Ranges from resampling whole regions 999 times; only pairs observed together in at least 5 regions.") +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# How much do the other species tell us about each species?
plot_cm_species <- function(cms) {
  d <- cms |>
    dplyr::filter(strong_links + strong_but_shared_years + weak_links > 0) |>
    dplyr::transmute(species, guild = dplyr::coalesce(guild, "other"),
                     `Strong, beyond shared bad years` = strong_links,
                     `Strong, mostly shared bad years` = strong_but_shared_years,
                     `Weak but real` = weak_links) |>
    tidyr::pivot_longer(-c(species, guild), names_to = "type", values_to = "n") |>
    dplyr::mutate(type = factor(type, rev(c("Strong, beyond shared bad years", "Strong, mostly shared bad years", "Weak but real"))))
  ord <- d |> dplyr::group_by(species) |>
    dplyr::summarise(s = sum(n * c(3, 2, 1)[as.integer(factor(type, levels(d$type)[3:1]))]), t = sum(n)) |>
    dplyr::arrange(t, s)
  d$species <- factor(d$species, ord$species)
  ggplot2::ggplot(d, ggplot2::aes(n, species, fill = type)) +
    ggplot2::geom_col(width = 0.7, colour = pal$surface, linewidth = 0.3) +
    ggplot2::facet_grid(guild ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_manual(values = c(`Strong, beyond shared bad years` = pal$seq[5],
                                          `Strong, mostly shared bad years` = pal$seq[3],
                                          `Weak but real` = pal$seq[1]), name = NULL,
                               guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::labs(x = "Number of other species clearly linked to it", y = NULL,
                  title = "How much do other insects tell us about each insect?",
                  subtitle = "Links whose 95% range excludes zero. Strong = |r| >= 0.3; 'beyond' = still |r| >= 0.2 after removing Europe-wide bad years.",
                  caption = "Species with no clear link, or recorded too rarely together with others to test, are not shown.") +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# Lead effects: does one species' bad year predict another's next year,
# beyond that species' own previous year?
plot_cm_lags <- function(lags, k = 20) {
  d <- lags |>
    dplyr::filter(sure) |>
    dplyr::arrange(dplyr::desc(abs(r))) |>
    dplyr::slice_head(n = k) |>
    dplyr::mutate(pair = paste(leader, "then", follower), dir = ifelse(r > 0, "more damage next year", "less damage next year"))
  d$pair <- factor(d$pair, rev(d$pair))
  ggplot2::ggplot(d, ggplot2::aes(r, pair, colour = dir)) +
    ggplot2::geom_vline(xintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.6) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = c(`more damage next year` = pal$div_high, `less damage next year` = pal$div_low),
                                 name = "A bad year for the first species means...") +
    ggplot2::labs(x = "Partial correlation (dot) with 95% range (line)", y = NULL,
                  title = "Does one insect's bad year come before another's?",
                  subtitle = "First species in year t vs second species in year t+1, after accounting for the second species' own year t",
                  caption = paste("Strongest links whose 95% range excludes zero; >= 5 regions. About 1 in 20 tested links",
                                  "would pass by chance, so treat single links as hypotheses.")) +
    theme_eu(9) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}
