# Helper finder: for each target species, which other species could be used to
# fill its gaps? Three questions per target-helper pair:
#   1. Link        : do they move together? pooled r (z_within), and r after
#                    removing Europe-wide year effects (anomaly).
#   2. Transfer    : does the link hold across countries? (transferability flag)
#   3. Availability: is the helper observed where the target is missing?
#                    gap_coverage = share of the target's not-monitored
#                    region-years in which the helper is observed.
#
# Grade (thresholds are choices, not rules; see report):
#   A  recommended   : consistent across >= 2 countries and |r| >= 0.3, and the
#                      link survives removal of Europe-wide year effects
#                      (anomaly r same sign, |r| >= 0.2). Links that vanish are
#                      mostly shared bad years or opposite long-term trends,
#                      which a year effect in the gap-filling model captures anyway.
#   B  possible      : consistent and 0.2 <= |r| < 0.3, or |r| >= 0.3 but tested
#                      in only one country (transfer untested)
#   C  not suitable  : everything else (weak, inconsistent or no data)
# A helper that is never observed where the target is missing (gap_coverage = 0)
# cannot fill gaps and is graded C. So is any link resting on thin evidence:
# fewer than 2 regions or fewer than 10 region-years where both species had
# damage (e.g. r = 0.96 from one region with 6 shared damage years is chance).

min_support_regions <- 2
min_support_both_pos <- 10

min_anomaly_r <- 0.2

helper_grade <- function(r, flag, coverage, n_regions, n_both_positive, r_anomaly) {
  beyond_year <- !is.na(r_anomaly) & sign(r_anomaly) == sign(r) & abs(r_anomaly) >= min_anomaly_r
  dplyr::case_when(
    is.na(r) | coverage == 0 ~ "C",
    n_regions < min_support_regions | n_both_positive < min_support_both_pos ~ "C",
    flag == "consistent" & abs(r) >= 0.3 & beyond_year ~ "A",
    flag == "consistent" & abs(r) >= 0.3 ~ "B",
    flag == "consistent" & abs(r) >= 0.2 ~ "B",
    flag == "insufficient data" & abs(r) >= 0.3 ~ "B",
    TRUE ~ "C"
  )
}

grade_label <- c(A = "A: recommended", B = "B: possible", C = "C: not suitable")

helper_finder <- function(cells, ts, pw, species_meta) {
  # Both directions of every pair: target <- helper
  tz <- ts |> dplyr::filter(version == "z_within")
  ta <- ts |> dplyr::filter(version == "anomaly") |>
    dplyr::select(species_a, species_b, r_anomaly = r_pooled, transfer_anomaly = transferability)
  pairs <- tz |>
    dplyr::left_join(ta, by = c("species_a", "species_b")) |>
    dplyr::select(species_a, species_b, n = n_pooled, r = r_pooled, r_anomaly,
                  transferability, transfer_anomaly, n_countries, countries = countries_n10,
                  r_min, r_max)
  support <- pw |>
    dplyr::filter(version == "z_within") |>
    dplyr::select(species_a, species_b, n_regions, n_both_positive)
  pairs <- pairs |> dplyr::left_join(support, by = c("species_a", "species_b"))
  both <- dplyr::bind_rows(
    dplyr::rename(pairs, target = species_a, helper = species_b),
    dplyr::rename(pairs, target = species_b, helper = species_a)
  )

  # Availability: target not monitored, helper observed, in the same region-year
  obs <- cells |>
    dplyr::mutate(observed = status != "not_monitored") |>
    dplyr::select(country, nuts_id, year, species, observed)
  gaps <- obs |> dplyr::filter(!observed) |> dplyr::select(country, nuts_id, year, target = species)
  avail <- obs |> dplyr::filter(observed) |> dplyr::select(country, nuts_id, year, helper = species)
  n_gaps <- gaps |> dplyr::count(target, name = "target_gap_region_years")
  cover <- gaps |>
    dplyr::inner_join(avail, by = c("country", "nuts_id", "year"), relationship = "many-to-many") |>
    dplyr::group_by(target, helper) |>
    dplyr::summarise(gap_region_years_filled = dplyr::n(),
                     gap_countries_reached = paste(sort(unique(country)), collapse = ","),
                     .groups = "drop")

  both |>
    dplyr::left_join(n_gaps, by = "target") |>
    dplyr::left_join(cover, by = c("target", "helper")) |>
    dplyr::mutate(
      gap_region_years_filled = dplyr::coalesce(gap_region_years_filled, 0L),
      gap_coverage = 100 * gap_region_years_filled / target_gap_region_years,
      grade = helper_grade(r, transferability, gap_coverage, n_regions, n_both_positive, r_anomaly),
      link_beyond_year_effects = !is.na(r_anomaly) & sign(r_anomaly) == sign(r) & abs(r_anomaly) >= min_anomaly_r
    ) |>
    dplyr::left_join(dplyr::select(species_meta, target = species, target_guild = guild_from_file),
                     by = "target") |>
    dplyr::left_join(dplyr::select(species_meta, helper = species, helper_guild = guild_from_file,
                                   helper_is_aggregate = is_aggregate), by = "helper") |>
    dplyr::arrange(target, grade, dplyr::desc(abs(r))) |>
    dplyr::relocate(target, target_guild, grade, helper, helper_guild, r, r_anomaly,
                    transferability, countries, gap_coverage)
}

# Best helpers per target: up to k helpers with grade A or B, ranked by grade,
# then by share of the target's gaps they reach, then |r|.
# Targets without any A/B helper are kept with helper = NA.
helper_best <- function(hf, k = 3) {
  best <- hf |>
    dplyr::filter(grade %in% c("A", "B")) |>
    dplyr::group_by(target) |>
    dplyr::arrange(grade, dplyr::desc(gap_coverage), dplyr::desc(abs(r)), .by_group = TRUE) |>
    dplyr::slice_head(n = k) |>
    dplyr::mutate(helper_rank = dplyr::row_number()) |>
    dplyr::ungroup()
  none <- hf |>
    dplyr::distinct(target, target_guild, target_gap_region_years) |>
    dplyr::anti_join(best, by = "target") |>
    dplyr::mutate(grade = "C", helper_rank = NA_integer_)
  dplyr::bind_rows(best, none) |>
    dplyr::select(target, target_guild, helper_rank, helper, grade, r, r_anomaly,
                  transferability, countries, n_regions, n_both_positive, gap_coverage,
                  gap_countries_reached, target_gap_region_years) |>
    dplyr::arrange(target, helper_rank)
}

# Graph: one row per target species; dot = its best helper, x = |r| of the link,
# colour = grade, label = helper name and share of gaps it reaches.
plot_helper_finder <- function(best, sp_rank) {
  d <- best |>
    dplyr::group_by(target) |>
    dplyr::slice_min(helper_rank, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::left_join(dplyr::select(sp_rank, target = species, n_regions_occurring), by = "target") |>
    dplyr::mutate(
      grade_lab = factor(grade_label[grade], grade_label),
      x = dplyr::coalesce(abs(r), 0),
      lab = dplyr::case_when(
        !is.na(helper) ~ paste0(helper, "  (reaches ", round(gap_coverage), "% of gaps)"),
        target_gap_region_years < 300 ~ "hardly any gaps: monitored almost everywhere",
        TRUE ~ "no suitable helper"),
      target_guild = dplyr::coalesce(target_guild, "other")
    ) |>
    dplyr::arrange(n_regions_occurring) |>
    dplyr::mutate(target = factor(target, unique(target)))

  ggplot2::ggplot(d, ggplot2::aes(x, target)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = x, yend = target), colour = "#e6e5e1", linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(colour = grade_lab, shape = grade_lab), size = 2.6) +
    ggplot2::geom_text(ggplot2::aes(label = lab), hjust = 0, nudge_x = 0.02, size = 2.4, colour = pal$ink2) +
    ggplot2::scale_colour_manual(values = c(`A: recommended` = "#008300", `B: possible` = "#eda100",
                                            `C: not suitable` = "#9c9b97"), name = NULL, drop = FALSE) +
    ggplot2::scale_shape_manual(values = c(`A: recommended` = 16, `B: possible` = 17,
                                           `C: not suitable` = 4), name = NULL, drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 1.25), breaks = seq(0, 1, 0.2)) +
    ggplot2::facet_grid(target_guild ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = "Strength of the link with the best helper (|r|)", y = "Insect with gaps (target)",
                  title = "Helper finder: which insect can fill the gaps of each insect?",
                  subtitle = "Best helper per target. Targets ordered by how widespread they are (most widespread at top).",
                  caption = paste("A = link holds in >= 2 countries, |r| >= 0.3 and survives removing Europe-wide bad years.",
                                  "\nB = weaker, mostly shared bad years, or tested in one country only. C = none suitable.",
                                  "\nAll links need >= 2 regions and >= 10 years where both insects had damage.",
                                  "'% of gaps' = share of the target's missing region-years where the helper is recorded.")) +
    theme_eu(8.5) +
    ggplot2::theme(legend.position = "top", legend.justification = "left",
                   strip.text.y = ggplot2::element_text(angle = 0, face = "bold"),
                   panel.grid.major.x = ggplot2::element_line(colour = "#ecebe7", linewidth = 0.3))
}
