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
  # One graph: insects ranked by the number of NUTS units with damage (filled
  # dot); the hollow dot on the same row is the number of units that reported
  # the insect at all (0 or > 0). The gap between them = checked but never found.
  n_units <- max(sp_rank$n_regions_monitored)
  d <- sp_rank |>
    dplyr::mutate(guild = dplyr::coalesce(guild, "other")) |>
    dplyr::arrange(n_regions_occurring, n_regions_monitored, dplyr::desc(species)) |>
    dplyr::mutate(species = factor(species, species),
                  lab = paste0(n_regions_occurring, " / ", n_regions_monitored))
  pts <- dplyr::bind_rows(
    d |> dplyr::transmute(species, guild, n = n_regions_monitored, what = "reported (0 or > 0)"),
    d |> dplyr::transmute(species, guild, n = n_regions_occurring, what = "damage > 0 in at least one year")
  ) |>
    dplyr::mutate(what = factor(what, c("damage > 0 in at least one year", "reported (0 or > 0)")))
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = d, ggplot2::aes(x = n_regions_occurring, xend = n_regions_monitored,
                                                 y = species, yend = species, colour = guild),
                          linewidth = 1.6, alpha = 0.25) +
    ggplot2::geom_point(data = pts, ggplot2::aes(n, species, colour = guild, shape = what), size = 2.4, stroke = 0.9) +
    ggplot2::geom_text(data = d, ggplot2::aes(x = n_units + 2, y = species, label = lab),
                       hjust = 0, size = 2.4, colour = pal$ink2) +
    ggplot2::annotate("text", x = n_units + 2, y = nrow(d) + 1.2, label = "damage / reported",
                      hjust = 0, size = 2.4, fontface = "bold", colour = pal$ink2) +
    ggplot2::scale_shape_manual(values = c(`damage > 0 in at least one year` = 16, `reported (0 or > 0)` = 1),
                                name = "NUTS units where the insect was") +
    ggplot2::scale_colour_manual(values = c(pal$guild, other = pal$ink2), name = "Guild") +
    ggplot2::scale_x_continuous(limits = c(0, n_units + 12), breaks = seq(0, 80, 10)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Number of NUTS units", y = NULL,
                  title = "Species-NUTS occurrence ranking",
                  subtitle = "Ranked by units with damage (filled dot). Hollow dot = units that reported the insect; the gap = reported but never damaged",
                  caption = paste0("Out of ", n_units, " NUTS units, 2000-2022. 0 = monitored, no damage; no value = not monitored. ",
                                   "National-level countries count as one unit; PL district codes aggregated to NUTS2.")) +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical", legend.box.just = "left",
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
plot_lfm_comparison <- function(score, grid, best_model, n_blocks) {
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
                  subtitle = paste0("Whole country x species blocks hidden and predicted (10-fold cross-validation, ", n_blocks, " blocks)"),
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
                  caption = "Factor scores averaged over all region-years with data.") +
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
    # axis covers every dot and range, so negative links are not cut off
    ggplot2::scale_x_continuous(limits = c(min(-0.2, floor(min(d$lo, d$r, na.rm = TRUE) * 5) / 5), 1),
                                breaks = seq(-1, 1, 0.2)) +
    ggplot2::labs(x = "Correlation r (dot) with 95% range (line). Negative = one up when the other is down", y = NULL,
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

# ---- Two-part model figures ----------------------------------------------------

status_cols <- c(`observed` = "#184f95", `filled: works` = "#008300", `filled: weak` = "#eda100",
                 `filled: untested` = "#9c9b97", `zero: host absent` = "#8a5a44", `not fillable` = "#e6e5e1")

plot_hurdle_compare <- function(summary) {
  d <- summary |>
    dplyr::transmute(model,
      `Real zeros called 'probably no damage'` = zeros_called_zero,
      `Block totals within 2x of the truth` = share_totals_within_2x) |>
    tidyr::pivot_longer(-model, names_to = "metric") |>
    dplyr::mutate(model = factor(model, rev(summary$model)))
  ggplot2::ggplot(d, ggplot2::aes(value, model, fill = model)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(100 * value), "%")), hjust = -0.2, size = 3.2, colour = pal$ink) +
    ggplot2::facet_wrap(~ metric, ncol = 1) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(100 * x, "%"), limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_fill_manual(values = c(`One-part factor model` = "#9c9b97", `Two-part factor model` = pal$guild[["defoliator"]],
                                          `Two-part model` = pal$guild[["defoliator"]], `Two-part model + host trees` = "#008300"), guide = "none") +
    ggplot2::labs(x = NULL, y = NULL, title = "Gap-filling models compared",
                  subtitle = "Hidden country x insect blocks, predicted and compared with the real values",
                  caption = "Block total = predicted vs real total damage of one hidden country x insect x year.") +
    theme_eu(10) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# Harmonised dataset at a glance: country x insect, coloured by the dominant
# status of its region-year cells
plot_harmonised_status <- function(filled, sp_order) {
  d <- filled |>
    dplyr::mutate(status = dplyr::case_when(
      value_type %in% c("observed damage", "observed zero") ~ "observed",
      value_type == "filled" ~ paste0("filled: ", confidence),
      value_type == "zero: host tree absent" ~ "zero: host absent",
      TRUE ~ "not fillable")) |>
    dplyr::count(country, species, status) |>
    dplyr::group_by(country, species) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(species = factor(species, sp_order),
                  status = factor(status, names(status_cols)),
                  country = factor(country, rev(sort(unique(country)))))
  ggplot2::ggplot(d, ggplot2::aes(species, country, fill = status)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = status_cols, name = NULL, drop = FALSE) +
    ggplot2::labs(x = NULL, y = NULL, title = "Harmonised dataset: what is observed and what is filled",
                  subtitle = "Each tile shows the most common status of that country x insect's region-years",
                  caption = paste("Filled confidence from the hidden-block test: works = beats the baseline and separates damage from no damage;",
                                  "\nweak = does not; untested = insect recorded in one country only. Host absent = host tree not in the region's forest inventory.")) +
    theme_eu(8) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
                   legend.position = "top", legend.justification = "left")
}

# How much of each insect's Europe-wide total would come from filled values
plot_totals_composition <- function(totals, k = 20) {
  d <- totals |>
    dplyr::group_by(species, unit) |>
    dplyr::summarise(observed = sum(observed), `filled: works` = sum(filled_works),
                     `filled: weak` = sum(filled_weak), `filled: untested` = sum(filled_untested), .groups = "drop") |>
    dplyr::slice_max(observed, n = k) |>
    dplyr::mutate(lab = paste0(species, "  (observed ", formatC(observed / 1e6, format = "f", digits = 1), " M ", unit, ")"))
  ord <- d |> dplyr::arrange(observed) |> dplyr::pull(lab)
  d <- d |>
    tidyr::pivot_longer(c(observed, `filled: works`, `filled: weak`, `filled: untested`), names_to = "source") |>
    dplyr::group_by(lab) |> dplyr::mutate(share = value / sum(value)) |> dplyr::ungroup() |>
    dplyr::mutate(lab = factor(lab, ord), source = factor(source, rev(c("observed", "filled: works", "filled: weak", "filled: untested"))))
  ggplot2::ggplot(d, ggplot2::aes(share, lab, fill = source)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0.5, colour = pal$surface, linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = status_cols[c("observed", "filled: works", "filled: weak", "filled: untested")], name = NULL,
                               guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(100 * x, "%"), expand = c(0, 0)) +
    ggplot2::labs(x = "Share of Europe-wide total 2000-2022", y = NULL,
                  title = "How much of each insect's Europe-wide total would be filled in?",
                  subtitle = "20 insects with the largest observed damage. Large orange/grey shares = totals not yet trustworthy.",
                  caption = "Cells where the host tree is absent are set to zero. Larch and minor broadleaf hosts cannot be checked (not in the host table).") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", legend.justification = "left")
}

# ---- Agent-group harmonisation -------------------------------------------------

group_cols <- c(`observed` = "#184f95", `back-cast` = "#5b8fd6", `filled: works` = "#008300",
                `filled: weak` = "#eda100", `filled: untested` = "#9c9b97",
                `zero: host absent` = "#8a5a44", `zero: never reported` = "#c9c3b4", `not fillable` = "#e6e5e1")

plot_group_status <- function(gh) {
  d <- gh |>
    dplyr::mutate(status = dplyr::case_when(
      value_type %in% c("observed damage", "observed zero") ~ "observed",
      grepl("^back-cast", value_type) ~ "back-cast",
      grepl("^filled", value_type) ~ paste0("filled: ", confidence),
      value_type == "zero: host tree absent" ~ "zero: host absent",
      grepl("^zero: never reported", value_type) ~ "zero: never reported",
      TRUE ~ "not fillable")) |>
    dplyr::count(country, group, status) |>
    dplyr::group_by(country, group) |> dplyr::slice_max(n, n = 1, with_ties = FALSE) |> dplyr::ungroup() |>
    dplyr::mutate(status = factor(status, names(group_cols)), country = factor(country, rev(sort(unique(country)))))
  ggplot2::ggplot(d, ggplot2::aes(group, country, fill = status)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = group_cols, name = NULL, drop = TRUE) +
    ggplot2::labs(x = NULL, y = NULL, title = "Harmonised by agent group: observed, back-cast or zero",
                  subtitle = "Each tile = most common status of that country x group's region-years, 2000-2022",
                  caption = "Back-cast = years before a country's series starts. Never reported = the country never reports this group; assumed no damage (0).") +
    theme_eu(9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1), legend.position = "top", legend.justification = "left")
}

plot_group_totals <- function(gt) {
  d <- gt |>
    dplyr::group_by(group, unit) |>
    dplyr::summarise(Observed = sum(observed), `Back-cast early years` = sum(back_cast),
                     `Model guess if never-reported meant 'unknown'` = sum(model_guess_never_reported), .groups = "drop") |>
    dplyr::mutate(lab = paste0(group, " (", unit, ")")) |>
    tidyr::pivot_longer(c(Observed, `Back-cast early years`, `Model guess if never-reported meant 'unknown'`), names_to = "source") |>
    dplyr::mutate(source = factor(source, rev(c("Observed", "Back-cast early years", "Model guess if never-reported meant 'unknown'"))))
  ord <- d |> dplyr::filter(source == "Observed") |> dplyr::arrange(value) |> dplyr::pull(lab)
  ggplot2::ggplot(d, ggplot2::aes(value / 1e6, factor(lab, ord), fill = source)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~ unit, scales = "free", ncol = 1) +
    ggplot2::scale_fill_manual(values = c(Observed = "#184f95", `Back-cast early years` = "#5b8fd6",
                                          `Model guess if never-reported meant 'unknown'` = "#eda100"),
                               name = NULL, guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::labs(x = "Million (m3 or ha), 2000-2022", y = NULL, title = "Europe-wide damage by agent group",
                  subtitle = "Harmonised total = observed + back-cast (never-reported = 0). Orange = sensitivity: what the model would add instead.",
                  caption = "Grouping provisional (guild x host tree). Defoliator areas may overlap between insects.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", legend.justification = "left",
                                 strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# ---- gllvm (latent factor model, agent groups) --------------------------------

plot_gllvm_cor <- function(Rp, Rt) {
  tidy <- function(R, lab) {
    if (is.null(R)) return(NULL)
    tibble::as_tibble(R, rownames = "a") |> tidyr::pivot_longer(-a, names_to = "b", values_to = "r") |>
      dplyr::mutate(model = lab)
  }
  d <- dplyr::bind_rows(tidy(Rp, "Presence: damage in the same places and years"),
                        tidy(Rt, "Timing: bad years at the same time"))
  ord <- sort(unique(d$a))
  d <- d |> dplyr::mutate(a = factor(a, ord), b = factor(b, rev(ord)),
                          lab = dplyr::if_else(a == b, "", formatC(r, format = "f", digits = 1)))
  ggplot2::ggplot(d, ggplot2::aes(a, b, fill = r)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = lab), size = 2.6, colour = pal$ink) +
    ggplot2::facet_wrap(~ model) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", limits = c(-1, 1), name = "correlation") +
    ggplot2::labs(x = NULL, y = NULL, title = "Which insect groups go together? (gllvm latent factor model)",
                  subtitle = "Correlation left after removing shared region and Europe-wide year effects",
                  caption = "Red = go up together, blue = one up when the other is down. Recorded data only; groups reported by >= 3 countries.") +
    theme_eu(9) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1),
                                 strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

plot_gllvm_years <- function(ys, ld) {
  top <- ld |> dplyr::group_by(factor) |>
    dplyr::summarise(lab = paste0("+ ", paste(group[order(-loading)][1:2], collapse = ", "),
                                  "\n- ", paste(group[order(loading)][1:2], collapse = ", ")), .groups = "drop")
  d <- ys |> dplyr::left_join(top, by = "factor") |> dplyr::mutate(facet = paste0(factor, "\n", lab))
  ggplot2::ggplot(d, ggplot2::aes(year, score)) +
    ggplot2::geom_hline(yintercept = 0, colour = pal$ink2, linewidth = 0.3) +
    ggplot2::geom_line(colour = "#184f95", linewidth = 0.8) + ggplot2::geom_point(colour = "#184f95", size = 1.4) +
    ggplot2::facet_wrap(~ facet, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "mean factor score", title = "Timing factors over time",
                  subtitle = "High = years when the '+' groups had worse damage than usual relative to the '-' groups",
                  caption = "Average over regions of each region-year's factor score (timing model).") +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = 8))
}

plot_gllvm_vp <- function(vp) {
  ggplot2::ggplot(vp, ggplot2::aes(share, group, fill = component)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~ model) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(100 * x, "%"), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = c(`region (shared by all groups)` = "#9c9b97",
                                          `year (Europe-wide, shared by all groups)` = "#eda100",
                                          `latent factor 1` = "#184f95", `latent factor 2` = "#5b8fd6",
                                          `latent factor 3` = "#a9c6ec"), name = NULL) +
    ggplot2::labs(x = "share of explained variation", y = NULL, title = "What drives each group's variation?",
                  caption = "Timing model has no region effect: each series is measured around its own average.") +
    theme_eu(9) + ggplot2::theme(legend.position = "top", legend.justification = "left",
                                 strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# Species-NUTS occurrence ranking (version 2): one dot per code = number of NUTS
# units with damage; colour = guild, shape = code type.
plot_occurrence_ranking_v2 <- function(occ2) {
  n_units <- 77
  d <- occ2 |>
    dplyr::arrange(n_units_with_damage, dplyr::desc(species)) |>
    dplyr::mutate(species = factor(species, species),
                  code_type = factor(code_type, c("single insect", "aggregate group")))
  ggplot2::ggplot(d, ggplot2::aes(n_units_with_damage, species)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = n_units_with_damage, yend = species, colour = guild),
                          linewidth = 0.5, alpha = 0.35) +
    ggplot2::geom_point(ggplot2::aes(colour = guild, shape = code_type), size = 2.6, stroke = 1) +
    ggplot2::geom_text(ggplot2::aes(label = n_units_with_damage), hjust = 0, nudge_x = 1.2, size = 2.5, colour = pal$ink2) +
    ggplot2::scale_colour_manual(values = pal$guild, name = "Guild") +
    ggplot2::scale_shape_manual(values = c(`single insect` = 16, `aggregate group` = 15),
                                labels = c(`single insect` = "single insect (species or genus)",
                                           `aggregate group` = "aggregate group (several insects lumped together)"),
                                name = "Code type", drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, n_units), breaks = seq(0, 70, 10), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(x = "Number of NUTS units with damage (at least one year with damage > 0, 2000-2022)", y = NULL,
                  title = "Species-NUTS occurrence ranking",
                  subtitle = "EU-ForDAM insect codes as reported, ranked by the number of NUTS units where they caused damage",
                  caption = paste("77 NUTS units in total; 11 countries report one national unit; PL district codes are NUTS2.",
                                  "\nAggregate groups combine several insects, and their content can differ between countries.",
                                  "\nCodes as reported: the CZ spruce bark-borer group (CambXyl_con&BB_con&SM) is not merged into ips_typ.")) +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical", legend.box.just = "left",
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}

# ---- Correlations along the hierarchy ------------------------------------------

# Heatmap of same-year r between groups at one level (lower triangle); the
# number is r, bold when its 95% range excludes 0.
plot_hier_heatmap <- function(hier_cm, hier, level, title) {
  d <- dplyr::filter(hier_cm, level == !!level)
  ord <- hier |> dplyr::distinct(level1, g = .data[[level]]) |>
    dplyr::arrange(factor(level1, c("borers on conifers", "borers on broadleaves",
                                    "defoliators on conifers", "defoliators on broadleaves")), g) |>
    dplyr::pull(g) |> unique()
  both <- dplyr::bind_rows(d, dplyr::rename(d, species_a = species_b, species_b = species_a)) |>
    dplyr::mutate(ia = match(species_a, ord), ib = match(species_b, ord)) |>
    dplyr::filter(ia > ib) |>
    dplyr::mutate(a = factor(species_a, rev(ord)), b = factor(species_b, ord),
                  lab = formatC(same_year, format = "f", digits = 2),
                  face = dplyr::if_else(same_year_sure, "bold", "plain"))
  ggplot2::ggplot(both, ggplot2::aes(b, a, fill = same_year)) +
    ggplot2::geom_tile(colour = pal$surface, linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = lab, fontface = face), size = 3, colour = pal$ink) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", limits = c(-0.6, 0.6),
                                  oob = scales::squish, name = "r (same year)") +
    ggplot2::labs(x = NULL, y = NULL, title = title,
                  subtitle = "Do the groups have bad years together? Pooled over all regions where both are recorded",
                  caption = "Bold = 95% range (whole regions resampled 999 times) excludes zero.\nGroup value = sum of its recorded insect codes per region-year.") +
    theme_eu(9) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
                                 panel.grid = ggplot2::element_blank())
}

# Mean correlation of insect pairs by how close they are in the hierarchy,
# against random group labels
plot_hier_test <- function(hier_test) {
  d <- hier_test |>
    dplyr::mutate(metric = dplyr::recode(metric, same_year = "Bad years together (same year)",
                                         beyond_year = "Beyond Europe-wide bad years"),
                  pair_class = factor(pair_class, c("same host-tree group (level 2)",
                                                    "same broad group only (level 1)", "different broad groups")),
                  lab = paste0("n = ", n_pairs, ", p = ", formatC(p_value, format = "f", digits = 3)))
  ggplot2::ggplot(d, ggplot2::aes(mean_r, pair_class)) +
    ggplot2::geom_col(fill = "#184f95", width = 0.6) +
    ggplot2::geom_point(ggplot2::aes(x = mean_r_random), shape = 124, size = 7, colour = pal$guild[["defoliator"]]) +
    ggplot2::geom_text(ggplot2::aes(label = lab), x = 0.005, hjust = 0, colour = "white", size = 2.8) +
    ggplot2::facet_wrap(~ metric, ncol = 1) +
    ggplot2::labs(x = "Mean correlation of insect pairs", y = NULL,
                  title = "Are insects in the same group more correlated?",
                  subtitle = "Insect pairs grouped by the closest level they share. Orange mark = average with random group labels",
                  caption = "p = share of 999 random relabellings with a mean at least as high as observed.") +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

# ---- Option 3 (guild hint) vs plain two-part model ------------------------------

plot_hint_strength <- function(overall, chosen = 50) {
  d <- overall |>
    dplyr::transmute(tau, `Presence: AUC (0.5 = guessing, 1 = perfect)` = auc_any,
                     `Real zeros called 'no damage'` = zeros_called_zero,
                     `Hidden totals within 2x` = totals_within_2x,
                     `Share of tested species passing the reliability rule` = species_works / species_tested) |>
    tidyr::pivot_longer(-tau) |>
    dplyr::mutate(x = factor(tau, sort(unique(tau))))
  ggplot2::ggplot(d, ggplot2::aes(x, value, group = name)) +
    ggplot2::geom_line(colour = "#184f95", linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(colour = tau == chosen | tau == 0), size = 2.6) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = pal$guild[["defoliator"]], `FALSE` = "#184f95"), guide = "none") +
    ggplot2::facet_wrap(~ name, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = "Strength of the guild hint (0 = plain model we had before)", y = NULL,
                  title = "Does the guild hint improve gap-filling?",
                  subtitle = paste0("Same hidden country x insect blocks for every version. Orange = plain model (0) and chosen strength (", chosen, ")"),
                  caption = "Hint = each insect's level and links pulled towards the average of its broad group (borers/defoliators on conifers/broadleaves).") +
    theme_eu(9) + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

plot_hint_species <- function(by_species, hier, chosen = 50) {
  d <- by_species |> dplyr::filter(tau %in% c(0, chosen)) |>
    dplyr::select(tau, species, auc_any) |>
    tidyr::pivot_wider(names_from = tau, values_from = auc_any, names_prefix = "t") |>
    dplyr::rename(plain = t0, hint = !!paste0("t", chosen)) |>
    dplyr::filter(!is.na(plain), !is.na(hint)) |>
    dplyr::left_join(dplyr::select(hier, species, level1), by = "species") |>
    dplyr::arrange(hint) |> dplyr::mutate(species = factor(species, species))
  ggplot2::ggplot(d) +
    ggplot2::geom_vline(xintercept = c(0.5, 0.7), colour = pal$ink2, linewidth = 0.3, linetype = c("solid", "dashed")) +
    ggplot2::geom_segment(ggplot2::aes(x = plain, xend = hint, y = species, yend = species), colour = "#c9c3b4", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(plain, species), shape = 1, size = 2.4, colour = pal$ink2) +
    ggplot2::geom_point(ggplot2::aes(hint, species, colour = level1), size = 2.6) +
    ggplot2::scale_colour_manual(values = c(`borers on conifers` = "#184f95", `borers on broadleaves` = "#5b8fd6",
                                            `defoliators on conifers` = "#eb6834", `defoliators on broadleaves` = "#f2a33a"),
                                 name = "Broad group") +
    ggplot2::labs(x = "Presence prediction for hidden blocks (AUC; 0.5 = guessing, dashed = 0.7)", y = NULL,
                  title = "Per insect: plain model (hollow) vs guild hint (filled)",
                  subtitle = paste0("Insects recorded in at least 2 countries, so they can be tested. Hint strength ", chosen),
                  caption = "AUC = how well the model ranks hidden region-years with damage above those without.") +
    theme_eu(8.5) + ggplot2::theme(legend.position = "bottom")
}

# ---- Correlation metrics, back to basics ---------------------------------------

# All codes vs all codes: same-year r (Q1). Coloured only where the 95% range
# excludes zero; grey dot = tested, no clear link; blank = never recorded together.
plot_cm_basic_matrix <- function(cm, hier, ord) {
  both <- dplyr::bind_rows(cm, dplyr::rename(cm, species_a = species_b, species_b = species_a)) |>
    dplyr::mutate(a = factor(species_a, rev(ord)), b = factor(species_b, ord),
                  show = dplyr::if_else(same_year_sure, same_year, NA_real_))
  cuts <- hier |> dplyr::mutate(i = match(species, ord)) |> dplyr::group_by(level1) |>
    dplyr::summarise(last = max(i)) |> dplyr::filter(last < length(ord)) |> dplyr::pull(last)
  n <- length(ord)
  ggplot2::ggplot(both, ggplot2::aes(b, a)) +
    ggplot2::geom_tile(ggplot2::aes(fill = show), colour = "white", linewidth = 0.2) +
    ggplot2::geom_point(data = dplyr::filter(both, !is.na(same_year), !same_year_sure), size = 0.4, colour = "#9c9b97") +
    ggplot2::geom_vline(xintercept = cuts + 0.5, colour = pal$ink, linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = n - cuts + 0.5, colour = pal$ink, linewidth = 0.4) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", limits = c(-0.8, 0.8),
                                  oob = scales::squish, na.value = "transparent", name = "r (same year)") +
    ggplot2::labs(x = NULL, y = NULL, title = "Which insects have bad years together? (all codes vs all codes)",
                  subtitle = "Coloured = clear link (95% range excludes 0). Grey dot = tested, no clear link. Blank = never recorded together",
                  caption = paste("Lines separate the 4 broad groups: borers on conifers | borers on broadleaves | defoliators on conifers | defoliators on broadleaves.",
                                  "\nCodes as reported; region series standardised against their own mean; ranges from resampling whole regions 999 times.")) +
    theme_eu(7.5) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
                   panel.grid = ggplot2::element_blank(), legend.position = "right")
}
