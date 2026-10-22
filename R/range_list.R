# Range list for expert correction: is each insect present in each country?
#
# One row per single insect, one column per country. Cells:
#   R  = the country reports damage from this insect (in range, from the data)
#   0  = the country reports this insect but only ever 0 damage
#   y? / n? = not reported; the draft rule's guess (filled only where the country
#             or a neighbour reports damage). To be overwritten with Y or N.
# read_range_list() turns the returned file into the in_range table used by
# apply_range(): Y, R and 0 count as in range, N as outside, anything else
# keeps the draft guess.

insect_full_names <- c(
  agr_big = "Agrilus biguttatus", agr_vir = "Agrilus viridis", bup_pin = "Bupalus piniaria", cep_spp = "Cephalcia spp.",
  clo_ana = "Clostera anastomosis", den_mic = "Dendroctonus micans", den_pin = "Dendrolimus pini", dip_sp = "Diprion spp.",
  era_def = "Erannis defoliaria", geo_db = "Geometridae on broadleaves", ips_acu = "Ips acuminatus", ips_ami = "Ips amitinus",
  ips_cem = "Ips cembrae", ips_dup = "Ips duplicatus", ips_sex = "Ips sexdentatus", ips_typ = "Ips typographus",
  lym_dis = "Lymantria dispar", lym_mon = "Lymantria monacha", mel_spp = "Melolontha spp.", neo_ser = "Neodiprion sertifer",
  ope_spp = "Operophtera spp.", pan_fla = "Panolis flammea", pha_cya = "Phaenops cyanea", pis_pin = "Pissodes pini",
  pit_bid = "Pityogenes bidentatus", pit_cha = "Pityogenes chalcographus", pit_spp = "Pityokteines spp.",
  pol_pol = "Polygraphus poligraphus", pri_abi = "Pristiphora abietina", sap_car = "Saperda carcharias",
  sco_int = "Scolytus intricatus", sco_rat = "Scolytus ratzeburgi", sco_sco = "Scolytus scolytus",
  tet_gab = "Tetropium gabrieli", tet_spp = "Tetropium spp.", tha_pit = "Thaumetopoea pityocampa",
  tha_pro = "Thaumetopoea processionea", tom_spp = "Tomicus spp.", tor_vir = "Tortrix viridana",
  uro_sir_spp = "Urocerus / Sirex spp.", xyl_lin = "Trypodendron (Xyloterus) lineatum", xyl_spp = "Xyleborus spp.",
  zei_gri = "Zeiraphera griseana")

range_list_long <- function(gd, rd, species_keep, species_meta) {
  st <- gd |> dplyr::filter(species %in% species_keep) |>
    dplyr::group_by(country, species) |>
    dplyr::summarise(rep_dmg = any(value_type == "reported damage"), rep_zero = any(value_type == "reported zero"),
                     filled = sum(damage[value_type == "filled"], na.rm = TRUE), .groups = "drop")
  st |> dplyr::left_join(dplyr::select(rd, country, species, in_range), by = c("country", "species")) |>
    dplyr::mutate(cell = dplyr::case_when(rep_dmg ~ "R", rep_zero ~ "0", dplyr::coalesce(in_range, FALSE) ~ "y?", TRUE ~ "n?"),
                  insect = unname(insect_full_names[species]),
                  host_tree = host_tree_of(species, species_meta),
                  guild = species_meta$guild_from_file[match(species, species_meta$species)],
                  unit = species_meta$unit[match(species, species_meta$species)])
}

write_range_list <- function(rl, path) {
  countries <- sort(unique(rl$country))
  wide <- rl |> dplyr::select(guild, host_tree, insect, code = species, unit, country, cell) |>
    tidyr::pivot_wider(names_from = country, values_from = cell) |>
    dplyr::left_join(rl |> dplyr::group_by(species) |> dplyr::summarise(`countries reporting damage` = sum(cell == "R")),
                     by = c(code = "species")) |>
    dplyr::arrange(guild, host_tree, insect) |>
    dplyr::relocate(`countries reporting damage`, .after = unit)
  stake <- rl |> dplyr::filter(cell %in% c("y?", "n?"), filled > 0) |>
    dplyr::transmute(insect, code = species, country, unit, `current guess` = cell,
                     `filled damage 2000-2022 if in range` = round(filled)) |>
    dplyr::arrange(dplyr::desc(`filled damage 2000-2022 if in range`))
  howto <- data.frame(`How to fill the range list` = c(
    "One row per insect, one column per country. Question for each cell: does this insect occur and cause damage in this country?",
    "R  = the country reports damage from this insect (from the data, leave as is).",
    "0  = the country reports this insect but only ever 0 damage (leave as is).",
    "y? = not reported; current guess: in range (the country or a neighbour reports damage).",
    "n? = not reported; current guess: outside range.",
    "Please overwrite every y? and n? with Y (in range) or N (outside range). Leave ? if unsure: the guess is then kept.",
    "Not in range means the gap-filling sets that country's damage to 0 instead of estimating it.",
    "Sheet 'what is at stake' lists the not-reported cells with the most filled damage first: those matter most.",
    "Note 1: Czechia and Germany report a spruce bark-borer group that is counted as I. typographus (as in the 2025 paper). For Czechia it may already include P. chalcographus, so P. chalcographus in CZ may be better set to N to avoid counting it twice.",
    "Note 2: Germany reports damage under tha_pit (T. pityocampa is Mediterranean). Is this T. pinivora, the northern pine processionary moth? It does not change the filling (reported values are kept), but the name matters for the range list.",
    "Countries: AT Austria, BG Bulgaria, CH Switzerland, CZ Czechia, DE Germany, FI Finland, HR Croatia, HU Hungary, IT Italy, LT Lithuania, PL Poland, RO Romania, RS Serbia, SE Sweden, SI Slovenia, SK Slovakia."),
    check.names = FALSE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "how to fill"); openxlsx::writeData(wb, "how to fill", howto)
  openxlsx::setColWidths(wb, "how to fill", 1, 140)
  openxlsx::addWorksheet(wb, "range list")
  openxlsx::writeData(wb, "range list", wide, headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = "#16302B", fontColour = "white"))
  openxlsx::freezePane(wb, "range list", firstActiveRow = 2, firstActiveCol = 7)
  openxlsx::setColWidths(wb, "range list", 1:6, c(11, 15, 30, 12, 5, 11))
  openxlsx::setColWidths(wb, "range list", 6 + seq_along(countries), 5)
  fills <- c(R = "#bfe3bf", `0` = "#e6f2e6", `y?` = "#fff2b3", `n?` = "#e0e0e0")
  for (k in seq_along(countries)) {
    col <- 6 + k; v <- wide[[countries[k]]]
    for (f in names(fills)) {
      rows <- which(v == f) + 1
      if (length(rows)) openxlsx::addStyle(wb, "range list", openxlsx::createStyle(fgFill = fills[[f]], halign = "center"), rows = rows, cols = col)
    }
    open_rows <- which(v %in% c("y?", "n?")) + 1
    for (r in open_rows) openxlsx::dataValidation(wb, "range list", cols = col, rows = r, type = "list", value = '"Y,N,?,y?,n?"')
  }
  openxlsx::addWorksheet(wb, "what is at stake"); openxlsx::writeData(wb, "what is at stake", stake)
  openxlsx::setColWidths(wb, "what is at stake", 1:6, c(30, 11, 8, 6, 14, 32))
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

# Read the returned file into the in_range table used by apply_range()
read_range_list <- function(path, rd) {
  w <- openxlsx::read.xlsx(path, sheet = "range list", check.names = FALSE)
  countries <- intersect(names(w), unique(rd$country))
  long <- tidyr::pivot_longer(w[, c("code", countries)], -code, names_to = "country", values_to = "cell") |>
    dplyr::mutate(cell = toupper(trimws(cell)))
  rd |> dplyr::left_join(long, by = c(species = "code", "country")) |>
    dplyr::mutate(in_range = dplyr::case_when(cell %in% c("Y", "R", "0") ~ TRUE, cell == "N" ~ FALSE, TRUE ~ in_range),
                  reason = dplyr::case_when(cell == "Y" ~ "expert: in range", cell == "N" ~ "expert: outside range", TRUE ~ reason)) |>
    dplyr::select(-cell)
}
