# Draft "could this insect occur here?" rule (Oct 2026), test round.
#
# The factor model fills an insect wherever its host tree grows, which puts
# e.g. the pine processionary moth (Mediterranean) into central European pine
# forests. Until an expert country x insect range table is available, a
# data-only draft rule decides where an insect may be filled:
#   in range  : the country reports damage (> 0) from the insect, or
#               shares a land border with a country that does;
#   otherwise : "zero: outside range (draft rule)", like host tree absent.
# Countries that report the insect only as zeros (monitored, never damaged)
# count as in range only through a neighbour. The table is written out so it
# can be corrected by hand (Tomas / Roman) and read back in later.

country_neighbours <- list(
  AT = c("CH", "CZ", "DE", "HU", "IT", "SI", "SK"), BG = c("RO", "RS"), CH = c("AT", "DE", "IT"),
  CZ = c("AT", "DE", "PL", "SK"), DE = c("AT", "CH", "CZ", "PL"), FI = "SE",
  HR = c("HU", "RS", "SI"), HU = c("AT", "HR", "RO", "RS", "SI", "SK"), IT = c("AT", "CH", "SI"),
  LT = "PL", PL = c("CZ", "DE", "LT", "SK"), RO = c("BG", "HU", "RS"),
  RS = c("BG", "HR", "HU", "RO"), SE = "FI", SI = c("AT", "HR", "HU", "IT"), SK = c("AT", "CZ", "HU", "PL"))

range_draft <- function(L) {
  countries <- sort(unique(L$rows$country))
  dmg <- tibble::tibble(country = rep(L$rows$country, ncol(L$Y)),
                        species = rep(L$species, each = nrow(L$Y)), y = as.vector(L$Y)) |>
    dplyr::group_by(country, species) |>
    dplyr::summarise(damage_reported = any(y > 0, na.rm = TRUE), .groups = "drop")
  tidyr::expand_grid(country = countries, species = L$species) |>
    dplyr::left_join(dmg, by = c("country", "species")) |>
    dplyr::mutate(damage_reported = dplyr::coalesce(damage_reported, FALSE)) |>
    dplyr::group_by(species) |>
    dplyr::mutate(neighbour_reports = purrr::map_lgl(country, function(c)
      any(country_neighbours[[c]] %in% country[damage_reported])),
      in_range = damage_reported | neighbour_reports,
      reason = dplyr::case_when(damage_reported ~ "reports damage",
                                neighbour_reports ~ "neighbour reports damage",
                                TRUE ~ "no evidence nearby")) |>
    dplyr::ungroup()
}

# Wide version for hand correction: 1 = in range, 0 = outside
range_draft_wide <- function(rd) {
  rd |> dplyr::transmute(species, country, in_range = as.integer(in_range)) |>
    tidyr::pivot_wider(names_from = country, values_from = in_range) |>
    dplyr::arrange(species)
}

# Apply to the v2 dataset: filled cells outside the range become 0
apply_range <- function(gd, rd) {
  gd |>
    dplyr::left_join(dplyr::select(rd, country, species, in_range), by = c("country", "species")) |>
    dplyr::mutate(
      outside = value_type == "filled" & !dplyr::coalesce(in_range, TRUE),
      value_type = dplyr::if_else(outside, "zero: outside range (draft rule)", value_type),
      damage = dplyr::if_else(outside, 0, damage),
      confidence = dplyr::if_else(outside, NA_character_, confidence),
      p_any = dplyr::if_else(outside, NA_real_, p_any)) |>
    dplyr::select(-in_range, -outside)
}

# Filled damage per insect (all years), before and after the draft range rule
range_effect <- function(tot_before, tot_after) {
  f <- function(t) t$europe |> dplyr::group_by(species, unit) |>
    dplyr::summarise(filled = sum(total_mid - reported), reported = sum(reported), .groups = "drop")
  dplyr::full_join(dplyr::rename(f(tot_before), filled_before = filled),
                   dplyr::select(f(tot_after), species, filled_after = filled), by = "species") |>
    dplyr::mutate(filled_vs_reported_before = filled_before / pmax(reported, 1),
                  filled_vs_reported_after = filled_after / pmax(reported, 1)) |>
    dplyr::arrange(dplyr::desc(filled_before))
}
