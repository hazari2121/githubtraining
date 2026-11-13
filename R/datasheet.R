# One workbook for people: the automated dataset, what every column means, and what
# was changed compared with the raw EU-ForDAM sheet. Written by run_pipeline().

suppressPackageStartupMessages({
  library(dplyr)
})

datasheet_columns <- function() {
  tribble(
    ~column, ~source, ~description,
    "country", "raw: Country", "Country code (ISO 2-letter).",
    "nuts", "raw: NUTS", "Spatial unit code: NUTS region, Polish forestry district code, or country code.",
    "region", "raw: Region", "Spatial unit name.",
    "year", "raw: d_year", "Year of damage.",
    "sp_code", "raw: sp_code", "Insect code. Codes with & or starting CambXyl_/Def_ are aggregates of several insects.",
    "insect", "added (reference/insects.csv)", "Insect name.",
    "guild", "added", "borer (recorded in m3) or defoliator (recorded in ha).",
    "is_aggregate", "added", "TRUE if sp_code combines several insects (e.g. CambXyl_con&BB_con&SM).",
    "host_tree", "raw: HOSTTREE", "Host tree as recorded (Latin).",
    "host", "added (reference/host_trees.csv)", "Common host name (pine, spruce, fir, larch, oak, birch, elm, horse chestnut, poplar/willow).",
    "host_type", "added", "conifers or broadleaves.",
    "unit", "raw: sp_unit", "Unit of value: m3 (damaged wood) or ha (defoliated area).",
    "value", "raw: value1", "Damage. 0 = monitored, no damage. Blank rows were removed (not recorded).",
    "damage_rel", "added", "value / host_amount: fraction of the host damaged that year. Blank if no host data.",
    "host_amount", "added (Data_Klim_Tree_NUTS)", "Area (ha) or growing stock (m3) of the insect's host in this unit.",
    "host_amount_columns", "added", "Which host columns were summed for host_amount (e.g. spruce_m3 + pine_m3).",
    "host_amount_from", "added", "Host amount definition, as in the host tables.",
    "vpd", "added (Data_Klim_Tree_NUTS)", "Mean vapour pressure deficit (kPa) of the unit.",
    "tas", "added (Data_Klim_Tree_NUTS)", "Mean air temperature (deg C) of the unit.",
    "pr", "added (Data_Klim_Tree_NUTS)", "Mean annual precipitation (mm) of the unit.",
    "value_alt", "raw: value2", "Same damage as value, in the other unit.",
    "unit_alt", "added", "Unit of value_alt (opposite of unit).",
    "series_id", "added", "Time series = insect code x host tree x unit x spatial unit.",
    "series_nonzero_years", "added", "Number of years with damage > 0 in this series.",
    "in_paper_series", "added", "TRUE if the series has >= 6 non-zero years (filter of Hlasny et al. 2025).",
    "tree_type", "raw: TREETYPE", "con / dec as recorded.",
    "sp_category", "raw: SP_CATEGORY", "Category as recorded (not always consistent; prefer the grp_ columns).",
    "sp_subcategory", "raw: SP_SUBCATEGORY", "Subcategory as recorded.",
    "grp_*", "raw: indicator columns", "TRUE if the record belongs to that group (raw 1/blank). Used for group totals."
  )
}

datasheet_changes <- function(res, raw_rows) {
  d  <- res$cleaned$data
  md <- res$model_data
  qc <- res$qc
  hc <- res$host_climate
  n_dup <- qc$overview$value[qc$overview$metric == "rows_in_duplicate_groups"]

  ch <- tribble(
    ~step, ~change, ~why, ~affected,
    "Clean", "Rows with blank value1 removed", "Blank = not recorded; never turned into 0", nrow(res$cleaned$dropped),
    "Clean", "Zeros kept as real zero damage", "0 = monitored, no damage found", sum(d$value == 0),
    "Clean", "No rows added for missing insect/region/year combinations", "Missing = not recorded; never filled with 0", 0,
    "Clean", "Repeated rows kept and counted", "Valid separate records", n_dup,
    "Clean", "Columns renamed to snake_case (value1 -> value, value2 -> value_alt, ...)", "Consistent names for analysis", ncol(md),
    "Clean", "Labels tidied: hidden non-breaking space removed, 'decidous' -> 'deciduous'", "Spelling / invisible characters", 2,
    "Clean", "Group indicator columns 1/blank -> TRUE/FALSE (grp_*)", "Easier filtering", nrow(res$cleaned$groups),
    "Add", "Insect name, guild, common host name, host type", "Readable and groupable records", nrow(md),
    "Add", "Aggregate codes marked (is_aggregate)", "Kept in group totals, left out of species tables", sum(md$is_aggregate),
    "Add", "Time-series filter of the paper (in_paper_series)", "Series with < 6 non-zero years marked, not deleted", sum(!md$in_paper_series),
    "Rule", "Ips typographus includes CambXyl_con&BB_con&SM (via its group column)", "Matches paper Tables 1-2 (77 regions, 16 countries, 586.7 Mm3)", sum(md$sp_code == "CambXyl_con&BB_con&SM")
  )

  if (!is.null(hc)) ch <- bind_rows(ch, tribble(
    ~step, ~change, ~why, ~affected,
    "Host/climate", "Host amount, damage_rel, VPD, temperature, precipitation added", "Damage relative to available host; climate drivers", sum(!is.na(md$host_amount)),
    "Host/climate", "Poland: swapped code/name columns in host file fixed", "Codes must match damage data (BIA, GDA, ...)", sum(hc$country == "PL"),
    "Host/climate", "Czechia: host data NUTS3 summed to NUTS2; climate forest-area weighted", "Damage data are NUTS2", sum(hc$country == "CZ"),
    "Host/climate", "Slovenia code SL -> SI", "Match damage data", sum(hc$country == "SI"),
    "Host/climate", "'-' in host file -> blank (not available), never 0", "Not available is not zero", nrow(qc$host_not_available),
    "Host/climate", "No host data for Berlin (DE3), Bremen (DE5), Hamburg (DE6): host columns blank", "Not in source; Berlin lumped with Brandenburg", sum(is.na(md$host_amount)),
    "Flag only", "BG and LT spruce/pine/oak m3 look like thousands of m3 (0.03-0.7 m3/ha)", "Not changed; waiting for source check", nrow(qc$host_density_suspicious),
    "Flag only", "Records with damage_rel > 1 (damage larger than host)", "Not changed; mainly BG/LT host values and L. monacha on spruce (kept as in paper)", sum(md$damage_rel > 1, na.rm = TRUE)
  ))
  ch |> mutate(affected = as.integer(affected))
}

datasheet_about <- function(res, inputs) {
  md <- res$model_data
  tibble(item = c("What", "Generated", "Damage input", "Host/climate input", "Rows",
                  "Countries", "Spatial units", "Years", "Insect codes", "Regenerate"),
         value = c("EU-ForDAM damage records, cleaned and joined with insect, host and climate information. One row = one record.",
                   format(Sys.time(), "%Y-%m-%d %H:%M"),
                   basename(inputs$damage), if (is.null(inputs$host_climate)) "not used" else basename(inputs$host_climate),
                   format(nrow(md), big.mark = ","), n_distinct(md$country), n_distinct(md$nuts),
                   paste(range(md$year), collapse = "-"), n_distinct(md$sp_code),
                   "Rscript run_pipeline.R <damage.xlsx> output <host_climate.xlsx>"))
}

write_datasheet <- function(res, path, inputs) {
  md <- res$model_data |>
    relocate(any_of(c("damage_rel", "host_amount", "host_amount_columns", "host_amount_from",
                      "vpd", "tas", "pr")), .after = value) |>
    relocate(starts_with("grp_"), .after = last_col())
  sheets <- list(about = datasheet_about(res, inputs),
                 changes = datasheet_changes(res, NULL),
                 columns = datasheet_columns(),
                 data = md)
  if (!is.null(res$host_climate)) sheets$spatial_units <- res$host_climate
  writexl::write_xlsx(sheets, path)
}
