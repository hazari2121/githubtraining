# Data preparation decisions (agreed with SH, Oct 2026):
#  - value1 is the damage variable. value2 is kept in the raw long table only:
#    in most countries it is an exact constant fraction of value1 (a unit
#    conversion), and in DE it is a sub-part of value1; it is filled for ~12% of rows.
#  - CZ has 1-3 rows per NUTS2-year-species (sub-regions, probably kraje).
#    Damage (m3 / ha) is additive, so sub-rows are summed to NUTS2.
#  - Aggregate codes (containing "&", plus "Def_dec") are kept as their own units
#    and flagged; their content differs between countries. Exception: groups a
#    country reports in place of a dominant species (see group_to_species).

# Species metadata from the file itself. Guild is derived from the indicator
# columns ("Bark borers on ..." / "Defoliators on ..."), which are consistent
# across countries (unlike SP_CATEGORY, which is scrambled in the CZ rows).
species_meta_from_raw <- function(raw) {
  flag_cols <- names(raw)[13:ncol(raw)]
  flags <- raw |>
    dplyr::select(sp_code, dplyr::all_of(flag_cols)) |>
    tidyr::pivot_longer(-sp_code, names_to = "flag", values_to = "v") |>
    dplyr::filter(v == 1) |>
    dplyr::distinct(sp_code, flag) |>
    dplyr::group_by(species = sp_code) |>
    dplyr::summarise(flags = paste(sort(flag), collapse = "; "), .groups = "drop")

  raw |>
    dplyr::group_by(species = sp_code) |>
    dplyr::summarise(
      unit     = dplyr::first(stats::na.omit(sp_unit)),
      host_tree = paste(sort(unique(HOSTTREE)), collapse = "/"),
      treetype = dplyr::first(TREETYPE),
      .groups = "drop"
    ) |>
    dplyr::left_join(flags, by = "species") |>
    dplyr::mutate(
      is_aggregate = grepl("&", species) | species == "Def_dec",
      is_genus_level = grepl("_spp?$", species),
      guild_from_file = dplyr::case_when(
        grepl("Bark borers", flags) ~ "borer",
        grepl("Defoliators", flags) ~ "defoliator",
        TRUE ~ NA_character_
      ),
      host_from_file = dplyr::recode(treetype, con = "conifer", dec = "broadleaf")
    )
}

# Group codes treated as a single species, following Hlasny et al. (2025, GCB,
# Sect. 2.2): "if the national data providers indicated that a broader group was
# dominated by a single species (e.g. borers on gymnosperms is dominated by
# I. typographus), we considered this group in species-specific analyses".
# Only applied where the country does not report that species separately.
# This mapping reproduces the paper's Table 6 (see R/paper.R).
group_to_species <- tibble::tribble(
  ~country, ~from,                   ~to,       ~reason,
  "CZ",     "CambXyl_con&BB_con&SM", "ips_typ", "Spruce bark-borer group, dominated by I. typographus; CZ reports no separate ips_typ"
)

# One row per country-nuts-year-species with the damage value used downstream.
prepare_damage <- function(long, species_meta, recode = group_to_species) {
  long |>
    dplyr::left_join(dplyr::select(recode, country, species = from, to),
                     by = c("country", "species")) |>
    dplyr::mutate(species = dplyr::coalesce(to, species)) |>
    dplyr::select(-to) |>
    # BG/RS rows with missing value also miss the unit; units are fixed per species
    dplyr::select(-unit) |>
    dplyr::left_join(dplyr::select(species_meta, species, unit), by = "species") |>
    dplyr::group_by(country, nuts_id, year, species, unit) |>
    dplyr::summarise(
      # NA only if every sub-row is NA; otherwise sum of reported sub-rows
      damage = if (all(is.na(damage))) NA_real_ else sum(damage, na.rm = TRUE),
      n_sub_rows = dplyr::n(),
      .groups = "drop"
    )
}
