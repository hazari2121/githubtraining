"""Tables for the data request to the national experts (2023-2025 update).

1. outputs/tables/REQUEST_data_inventory.xlsx
   What each country has provided (spatial unit, years, agents) and what to ask for:
   countries with NUTS-level data add 2023-2025; countries without NUTS-level data send
   the full 2000-2025 series at NUTS level. Abiotic damage: to be filled in from the
   Incoming sheets on Teams (not in our files).
2. outputs/tables/REQUEST_species_presence_by_NUTS.xlsx
   One sheet per country: species in rows, regions in columns; X = reported in
   EU-ForDAM; empty cells for the experts to give their confidence that the species
   occurs there although it is not recorded.

Built from the committed summaries of the EU-ForDAM data (value1, 2000-2022):
outputs/tables/step1_country_species_coverage.csv (country x agent: unit, regions,
years), outputs/tables/occurrence_matrix_region_species.csv (region x agent: years with
damage; empty = not recorded), the insect list of the overlap workbook, and the NUTS
2021 codes in data/external/nutspt_2.json.
Run from the project root:  python3 scripts/data_request_tables.py
"""
import json
import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.datavalidation import DataValidation

COV = "outputs/tables/step1_country_species_coverage.csv"
OCC = "outputs/tables/occurrence_matrix_region_species.csv"
OVERLAP = "outputs/tables/OVERLAP_target_and_predictor_species.xlsx"
NUTS2 = "data/external/nutspt_2.json"
YEARS = list(range(2000, 2023))
NEW = "2023, 2024, 2025"

COUNTRY = {"AT": "Austria", "BG": "Bulgaria", "CH": "Switzerland", "CZ": "Czechia", "DE": "Germany", "FI": "Finland",
           "HR": "Croatia", "HU": "Hungary", "IT": "Italy", "LT": "Lithuania", "PL": "Poland", "RO": "Romania",
           "RS": "Serbia", "SE": "Sweden", "SI": "Slovenia", "SK": "Slovakia"}

# Spatial unit of the data each country sent, and whether it counts as NUTS-level
UNIT = {
    "AT": ("NUTS2", "yes"), "CZ": ("NUTS2", "yes"), "DE": ("NUTS1 (Saarland and Rhineland-Palatinate together)", "yes"),
    "FI": ("Finnish forest centres (part NUTS3 codes, part forest-centre codes)", "unclear - discuss with Roman"),
    "PL": ("Regional Directorates of State Forests (RDLP), not NUTS", "unclear - discuss with Roman"),
    "IT": ("one region only: Friuli Venezia Giulia (ITH4)", "no (one NUTS2 region of the country)"),
}
for c in ["BG", "CH", "HR", "HU", "LT", "RO", "RS", "SE", "SI", "SK"]:
    UNIT[c] = ("whole country (national total)", "no")


def names():
    ins = pd.read_excel(OVERLAP, "insects")
    agg = pd.read_excel(OVERLAP, "aggregate_codes")
    nm = dict(zip(ins.code, ins.insect))
    nm.update(dict(zip(agg.code, agg.what)))
    nm["CambXyl_con&BB_con&SM"] = "Borers on spruce (aggregate; counted as Ips typographus)"
    return nm, ins, set(agg.code) | {"CambXyl_con&BB_con&SM"}


def nuts2_codes():
    f = json.load(open(NUTS2))["features"]
    return sorted(x["properties"]["id"] for x in f)


def inventory():
    cov = pd.read_csv(COV)
    nm, _, agg = names()
    occ = pd.read_csv(OCC)
    rows = []
    for c, g in cov.groupby("country"):
        unit, is_nuts = UNIT[c]
        n_units = occ[occ.country == c].shape[0]
        y0, y1 = int(g.year_min.min()), int(g.year_max.max())
        notes = []
        if y0 > 2000:
            notes.append(f"series start in {y0}")
        if y1 < 2022:
            notes.append(f"series end in {y1}")
        if c in ("CZ", "DE"):
            notes.append("spruce bark beetles reported as one group code (counted as Ips typographus)")
        if c == "IT":
            notes.append("data are for Friuli Venezia Giulia only, not all of Italy")
        if is_nuts == "yes":
            req = f"Add {NEW} for every agent and region already in the data (3 values per agent and region)"
        elif is_nuts.startswith("unclear"):
            req = f"Either add {NEW} for the current units, or the full 2000-2025 series at NUTS level - to agree with Roman"
        else:
            req = "Send the full 2000-2025 series at NUTS level (NUTS2), for every agent; not only the new years"
        rows.append({"country": c, "country_name": COUNTRY[c], "spatial_unit_provided": unit, "units": n_units,
                     "NUTS_level_data": is_nuts, "years_provided": f"{y0}-{y1}",
                     "agents_provided": g.species.nunique(),
                     "agent_groups_among_them": sum(s in agg for s in g.species.unique()),
                     "request_damage_data": req,
                     "abiotic_damage_provided (check Incoming sheets on Teams)": "",
                     "abiotic_spatial_unit (NUTS / national)": "",
                     "request_abiotic_data": "",
                     "notes": "; ".join(notes)})
    inv = pd.DataFrame(rows)

    # one row per country and agent: rows that differ only by unit (BG 2000-2004 and RS
    # I. typographus 2000-2002 have no unit) are merged, with a note
    def merge(g):
        units = sorted(set(u for u in g.unit if isinstance(u, str)))
        no_unit = g[g.unit.isna() & g.year_min.notna()]
        note = "; ".join(f"unit missing for {int(r.year_min)}-{int(r.year_max)}: ask for it" for r in no_unit.itertuples())
        yrs = g.dropna(subset=["year_min"])
        return pd.Series({"unit": " / ".join(units) if units else None, "n_regions": g.n_regions.max(),
                          "year_min": yrs.year_min.min() if len(yrs) else float("nan"),
                          "year_max": yrs.year_max.max() if len(yrs) else float("nan"),
                          "n_years": yrs.n_years.sum() if len(yrs) else 0, "unit_note": note})
    cov = cov.groupby(["country", "species"]).apply(merge, include_groups=False).reset_index()
    for c in cov.country[cov.unit_note != ""].unique():
        inv.loc[inv.country == c, "notes"] = (inv.loc[inv.country == c, "notes"] + "; " if inv.loc[inv.country == c, "notes"].iloc[0] else "") + \
            "unit missing in the early years for some agents (see by_agent): ask which unit"
    ag = []
    for _, r in cov.iterrows():
        unit, is_nuts = UNIT[r.country]
        if pd.isna(r.year_min):
            have, missing_txt, n_y = "none (agent listed but no values)", "2000-2022", 0
        else:
            y0, y1, n_y = int(r.year_min), int(r.year_max), int(r.n_years)
            have = f"{y0}-{y1}"
            gaps = (y1 - y0 + 1) - n_y
            parts = []
            if y0 > 2000:
                parts.append(f"2000-{y0 - 1}" if y0 - 1 > 2000 else "2000")
            if gaps > 0:
                parts.append(f"{gaps} year(s) missing inside {y0}-{y1}")
            if y1 < 2022:
                parts.append(f"{y1 + 1}-2022" if y1 + 1 < 2022 else "2022")
            missing_txt = "; ".join(parts) if parts else "none"
        if is_nuts == "yes":
            req = NEW + ("" if missing_txt == "none" else f" (and, if available, the missing years: {missing_txt})")
        elif is_nuts.startswith("unclear"):
            req = f"{NEW}, or 2000-2025 at NUTS level - to agree with Roman"
        else:
            req = "2000-2025 at NUTS2 level"
        ag.append({"country": r.country, "agent_code": r.species, "agent": nm.get(r.species, r.species),
                   "agent_group_code": r.species in agg, "unit": r.unit, "regions": int(r.n_regions),
                   "years_provided": have, "years_with_values": n_y, "years_missing_2000_2022": missing_txt,
                   "request": req, "note": r.unit_note})
    return inv, pd.DataFrame(ag)


def presence_species():
    # Species (not group codes) recorded in at least 2 countries
    _, ins, _ = names()
    sp = ins[(~ins.aggregate_code) & (ins.countries_recorded >= 2)].copy()
    sp["guild_order"] = (sp.guild != "borer").astype(int)
    return sp.sort_values(["guild_order", "countries_recorded", "regions_recorded"], ascending=[True, False, False])


def units_for(c, occ, nuts2):
    """Columns of a country's sheet: regions in its data, or its NUTS2 regions where data are national"""
    reported = occ[occ.country == c]
    if UNIT[c][1] == "yes" or UNIT[c][1].startswith("unclear"):
        return list(reported.nuts_id), None
    return [n for n in nuts2 if n.startswith(c)], "national"


HEAD = Font(bold=True)
XFILL = PatternFill("solid", fgColor="B1CF82")
NOTE = Font(italic=True, color="5C5B57")


def autosize(ws, widths):
    for i, w in widths.items():
        ws.column_dimensions[get_column_letter(i)].width = w


def write_inventory(inv, ag, path):
    wb = Workbook()
    ws = wb.active
    ws.title = "how to read"
    lines = [
        "Inventory of the EU-ForDAM data we hold (2000-2022) and what to request for 2023-2025.",
        "",
        "by_country: one row per country.",
        "  spatial_unit_provided / NUTS_level_data: the regions the country's data are given for.",
        "  request_damage_data: countries with NUTS-level data add 2023, 2024, 2025 (three values per agent and region);",
        "  countries without NUTS-level data send the full 2000-2025 series at NUTS2 level, not only the new years.",
        "  Poland (forest directorates) and Finland (forest centres) are sub-national but not plain NUTS: to agree with Roman.",
        "  Abiotic damage columns: to fill in from the Incoming sheets on Teams (not in the files used here).",
        "  Same rule: abiotic data at NUTS level -> add 2023-2025; national or none -> full 2000-2025 series at NUTS level.",
        "by_agent: one row per country and agent (insect or group code): years we have, years missing within 2000-2022, what to request.",
        "  agent_group_code = TRUE: damage reported for a group of insects together, not a single species.",
        "",
        "Source: EU-ForDAM database (value1), 2000-2022, as used in the analyses.",
    ]
    for i, l in enumerate(lines, 1):
        ws.cell(i, 1, l)
    autosize(ws, {1: 130})
    for title, df, widths in [("by_country", inv, {1: 8, 2: 13, 3: 42, 4: 7, 5: 22, 6: 13, 7: 9, 8: 10, 9: 70, 10: 26, 11: 22, 12: 30, 13: 60}),
                              ("by_agent", ag, {1: 8, 2: 24, 3: 42, 4: 10, 5: 6, 6: 8, 7: 13, 8: 10, 9: 40, 10: 60, 11: 40})]:
        ws = wb.create_sheet(title)
        ws.append(list(df.columns))
        for c in ws[1]:
            c.font = HEAD
            c.alignment = Alignment(wrap_text=True, vertical="top")
        for r in df.itertuples(index=False):
            ws.append(list(r))
        for row in ws.iter_rows(min_row=2):
            for c in row:
                c.alignment = Alignment(wrap_text=True, vertical="top")
        ws.freeze_panes = "C2"
        autosize(ws, widths)
        if title == "by_country":
            fill = PatternFill("solid", fgColor="FAEED9")
            for row in ws.iter_rows(min_row=2, min_col=10, max_col=12):
                for c in row:
                    c.fill = fill
    wb.save(path)


def write_presence(sp, occ, path):
    nuts2 = nuts2_codes()
    wb = Workbook()
    ws = wb.active
    ws.title = "how to read"
    lines = [
        "Which species may occur in a region although they are not recorded in EU-ForDAM, and how sure you are.",
        "",
        "One sheet per country: species in rows, regions in columns.",
        "  X = the species is reported in the EU-ForDAM data for that region (damage recorded, including years with 0).",
        "  Empty cell = not recorded. Please enter your confidence that the species occurs there:",
        "    3 = certainly present   2 = probably present   1 = possibly present   0 = absent",
        "  Leave empty if you do not know.",
        "Countries whose data are national totals: the columns are the country's NUTS2 regions; the column",
        "  'whole country (reported)' shows which species are in the national data.",
        "Poland: forest directorates (RDLP); Finland: forest centres; Germany: NUTS1; Italy: data for Friuli Venezia Giulia only.",
        "",
        f"Species: the {len(sp)} species (not group codes) recorded in at least 2 countries of EU-ForDAM (borers first, then defoliators).",
        "Damage units: m3 for borers, ha for defoliators.",
    ]
    for i, l in enumerate(lines, 1):
        ws.cell(i, 1, l)
    autosize(ws, {1: 120})
    for c in sorted(COUNTRY):
        ws = wb.create_sheet(c)
        cols, kind = units_for(c, occ, nuts2)
        lead = ["species", "guild", "countries recorded (EU-ForDAM)"]
        extra = ["whole country (reported)"] if kind == "national" else []
        ws.append(lead + extra + cols)
        ws.row_dimensions[1].height = 60
        for cell in ws[1]:
            cell.font = HEAD
            cell.alignment = Alignment(text_rotation=90 if cell.column > len(lead) else 0, wrap_text=True, vertical="bottom")
        rep = occ[occ.country == c].set_index("nuts_id")
        for s in sp.itertuples():
            row = [s.insect, s.guild, int(s.countries_recorded)]
            if kind == "national":
                v = rep[s.code].iloc[0] if s.code in rep.columns and len(rep) else float("nan")
                row.append("X" if pd.notna(v) else "")
                if c == "IT":
                    row += ["X" if (n == "ITH4" and pd.notna(v)) else "" for n in cols]
                else:
                    row += [""] * len(cols)
            else:
                row += ["X" if (s.code in rep.columns and pd.notna(rep.loc[n, s.code])) else "" for n in cols]
            ws.append(row)
        first = len(lead) + 1
        last = len(lead) + len(extra) + len(cols)
        dv = DataValidation(type="list", formula1='"X,3,2,1,0"', allow_blank=True)
        ws.add_data_validation(dv)
        rng = f"{get_column_letter(first + len(extra))}2:{get_column_letter(last)}{len(sp) + 1}"
        dv.add(rng)
        for row in ws.iter_rows(min_row=2, min_col=first, max_col=last):
            for cell in row:
                cell.alignment = Alignment(horizontal="center")
                if cell.value == "X":
                    cell.fill = XFILL
        ws.freeze_panes = ws.cell(2, first)
        autosize(ws, {1: 30, 2: 11, 3: 10, **{i: 5 for i in range(first, last + 1)}})
        if kind == "national":
            ws.column_dimensions[get_column_letter(first)].width = 9
        ws.cell(len(sp) + 3, 1, "X = reported in EU-ForDAM. Fill empty cells: 3 certainly, 2 probably, 1 possibly present, 0 absent; empty = unknown.").font = NOTE
    wb.save(path)


if __name__ == "__main__":
    inv, ag = inventory()
    write_inventory(inv, ag, "outputs/tables/REQUEST_data_inventory.xlsx")
    occ = pd.read_csv(OCC)
    sp = presence_species()
    write_presence(sp, occ, "outputs/tables/REQUEST_species_presence_by_NUTS.xlsx")
    print(inv[["country", "spatial_unit_provided", "units", "NUTS_level_data", "years_provided", "agents_provided", "notes"]].to_string())
    print(len(sp), "species in the presence tables")
