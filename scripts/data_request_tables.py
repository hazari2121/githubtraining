"""Tables for the data request to the national experts (2023-2025 update).

1. outputs/tables/REQUEST_data_inventory.xlsx
   summary   one row per country: what we hold and what to ask for, colour-coded
             (green = add 2023-2025; orange = send the full 2000-2025 series at NUTS level;
             grey = to agree with Roman). Abiotic columns to fill from the Incoming sheets.
   by_agent  one row per country and agent: years we hold, years missing, what to ask.
2. outputs/tables/REQUEST_species_presence_by_NUTS.xlsx
   One sheet per country: species in rows, regions (code and name) in columns.
   X = reported in EU-ForDAM; empty cells for the expert's confidence that the
   species occurs there (3 certainly, 2 probably, 1 possibly present, 0 absent).

Built from the committed summaries of the EU-ForDAM data (value1, 2000-2022):
outputs/tables/step1_country_species_coverage.csv, outputs/tables/occurrence_matrix_region_species.csv,
the insect list of outputs/tables/OVERLAP_target_and_predictor_species.xlsx, region names
from data/raw/DataEUForDam_1.xlsx and the NUTS 2021 codes and names in data/external/nutspt_2.json.
Run from the project root:  python3 scripts/data_request_tables.py
"""
import json
import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Alignment, Font, PatternFill, Border, Side
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.datavalidation import DataValidation

COV = "outputs/tables/step1_country_species_coverage.csv"
OCC = "outputs/tables/occurrence_matrix_region_species.csv"
OVERLAP = "outputs/tables/OVERLAP_target_and_predictor_species.xlsx"
RAW = "data/raw/DataEUForDam_1.xlsx"
NUTS2 = "data/external/nutspt_2.json"
NEW = "2023-2025"

COUNTRY = {"AT": "Austria", "BG": "Bulgaria", "CH": "Switzerland", "CZ": "Czechia", "DE": "Germany", "FI": "Finland",
           "HR": "Croatia", "HU": "Hungary", "IT": "Italy", "LT": "Lithuania", "PL": "Poland", "RO": "Romania",
           "RS": "Serbia", "SE": "Sweden", "SI": "Slovenia", "SK": "Slovakia"}

# What each country sent (spatial unit) and which request applies:
#   update = NUTS-level data: add 2023-2025;  full = no NUTS-level data: 2000-2025 at NUTS2;
#   discuss = sub-national but not plain NUTS: to agree with Roman
LEVEL = {
    "AT": ("NUTS2", "update"), "CZ": ("NUTS2", "update"),
    "DE": ("NUTS1 (Saarland + Rhineland-Palatinate merged)", "update"),
    "IT": ("NUTS2, one region only (Friuli Venezia Giulia)", "update"),
    "FI": ("forest centres (part NUTS3 codes, part own codes)", "discuss"),
    "PL": ("State Forest directorates (RDLP), not NUTS", "discuss"),
}
for _c in ["BG", "CH", "HR", "HU", "LT", "RO", "RS", "SE", "SI", "SK"]:
    LEVEL[_c] = ("national total only", "full")

ASK = {"update": f"UPDATE: add {NEW} (3 values per agent and region)",
       "full": "FULL SERIES: send 2000-2025 at NUTS2, every agent",
       "discuss": f"DISCUSS WITH ROMAN: add {NEW} for current units, or 2000-2025 at NUTS"}
FILL = {"update": "A9D18E", "full": "F4B183", "discuss": "BFBFBF"}   # green, orange, grey

EXTRA_NOTE = {
    "CH": "data come from 673 forest districts (appendix), so NUTS-level data should exist",
    "IT": "ask whether other Italian regions have data (then 2000-2025 for those)",
    "DE": "NUTS1 only, ask whether NUTS2 is possible; spruce bark beetles reported as one group code (we count it as Ips typographus)",
    "CZ": "spruce bark beetles reported as one group code (we count it as Ips typographus)",
    "SE": "only Ips typographus, estimated during outbreaks",
    "FI": "only one agent; area affected from the National Forest Inventory",
}


def names():
    ins = pd.read_excel(OVERLAP, "insects")
    agg = pd.read_excel(OVERLAP, "aggregate_codes")
    nm = dict(zip(ins.code, ins.insect))
    nm.update(dict(zip(agg.code, agg.what)))
    nm["CambXyl_con&BB_con&SM"] = "Borers on spruce (group code; counted as Ips typographus)"
    return nm, ins, set(agg.code) | {"CambXyl_con&BB_con&SM"}


def region_names():
    """code -> name: regions in the data (raw file), else NUTS 2021 names"""
    out = {}
    for lv in range(4):
        out.update({x["properties"]["id"]: x["properties"]["na"] for x in json.load(open(f"data/external/nutspt_{lv}.json"))["features"]})
    raw = pd.read_excel(RAW, "EUForDam", usecols=["Country", "Region", "NUTS"]).dropna().drop_duplicates("NUTS")
    raw = raw[raw.Region != raw.NUTS]          # AT, DE, FI give the code as the name
    out.update(dict(zip(raw.NUTS, raw.Region)))
    out["DEB; DEC"] = "Rheinland-Pfalz + Saarland"
    return out


def nuts2_codes(c):
    return sorted(x["properties"]["id"] for x in json.load(open(NUTS2))["features"] if x["properties"]["id"].startswith(c))


def agent_table(cov, nm, agg):
    # rows that differ only by unit (BG 2000-2004 and RS I. typographus 2000-2002 have no unit) merged
    def merge(g):
        units = sorted(set(u for u in g.unit if isinstance(u, str)))
        no_unit = g[g.unit.isna() & g.year_min.notna()]
        note = "; ".join(f"no unit given for {int(r.year_min)}-{int(r.year_max)}: ask which unit" for r in no_unit.itertuples())
        yrs = g.dropna(subset=["year_min"])
        return pd.Series({"unit": " / ".join(units) if units else "", "n_regions": int(g.n_regions.max()),
                          "year_min": yrs.year_min.min(), "year_max": yrs.year_max.max(),
                          "n_years": int(yrs.n_years.sum()), "unit_note": note})
    cov = cov[cov.year_min.notna()].groupby(["country", "species"]).apply(merge, include_groups=False).reset_index()
    rows = []
    for r in cov.itertuples():
        kind = LEVEL[r.country][1]
        y0, y1 = int(r.year_min), int(r.year_max)
        gaps = (y1 - y0 + 1) - r.n_years
        miss = ([f"2000-{y0 - 1}" if y0 > 2001 else "2000"] if y0 > 2000 else []) + \
               ([f"{gaps} year(s) inside {y0}-{y1}"] if gaps > 0 else []) + \
               ([f"{y1 + 1}-2022" if y1 < 2021 else "2022"] if y1 < 2022 else [])
        ask = {"update": NEW, "full": "2000-2025 at NUTS2", "discuss": f"{NEW} (or 2000-2025 at NUTS, see Roman)"}[kind]
        if kind == "update" and miss:
            ask += " + if possible the missing years"
        rows.append({"country": r.country, "agent": nm.get(r.species, r.species), "agent_code": r.species,
                     "group_code": "yes" if r.species in agg else "", "unit": r.unit, "regions": r.n_regions,
                     "years_we_have": f"{y0}-{y1}", "years_missing_2000_2022": "; ".join(miss) or "-",
                     "ask_for": ask, "values_to_request": 3 * r.n_regions if kind == "update" else "",
                     "note": r.unit_note})
    return pd.DataFrame(rows).sort_values(["country", "group_code", "agent"])


def inventory():
    cov = pd.read_csv(COV)
    nm, _, agg = names()
    ag = agent_table(cov, nm, agg)
    occ = pd.read_csv(OCC)
    rows = []
    for c in sorted(COUNTRY):
        a = ag[ag.country == c]
        unit, kind = LEVEL[c]
        years = f"{a.years_we_have.str[:4].astype(int).min()}-{a.years_we_have.str[-4:].astype(int).max()}"
        notes = [n for n in [EXTRA_NOTE.get(c, "")] if n]
        if (a.note != "").any():
            notes.append("early years without a unit for some agents: ask which unit")
        if a.years_missing_2000_2022.ne("-").any() and kind == "update":
            notes.append("some agents miss years in 2000-2022 (see by_agent)")
        rows.append({"country": f"{c} {COUNTRY[c]}", "data_we_have": unit, "regions": int((occ.country == c).sum()),
                     "years": years, "agents": len(a), "of_which_group_codes": int((a.group_code == "yes").sum()),
                     "what_to_ask": ASK[kind],
                     "values_to_request": int(a.values_to_request.replace("", 0).sum()) if kind == "update" else "",
                     "abiotic_provided?": "", "abiotic_level (NUTS / national)": "", "abiotic_what_to_ask": "",
                     "notes": "; ".join(notes), "kind_": kind})
    return pd.DataFrame(rows), ag


HEAD = Font(bold=True, color="FFFFFF")
HEADFILL = PatternFill("solid", fgColor="2B4334")
THIN = Border(bottom=Side(style="thin", color="C9C8C3"))
XFILL = PatternFill("solid", fgColor="B1CF82")
NATFILL = PatternFill("solid", fgColor="EAF2DC")
TODO = PatternFill("solid", fgColor="DDEBF7")   # light blue: still to fill in


def header(ws, row, rotate_from=None):
    for cell in ws[row]:
        cell.font, cell.fill = HEAD, HEADFILL
        rot = 90 if rotate_from and cell.column >= rotate_from else 0
        cell.alignment = Alignment(wrap_text=True, vertical="bottom" if rot else "center", text_rotation=rot)


def widths(ws, w):
    for i, v in w.items():
        ws.column_dimensions[get_column_letter(i)].width = v


def write_inventory(inv, ag, path):
    wb = Workbook()
    ws = wb.active
    ws.title = "summary"
    intro = ["EU-ForDAM data we hold (2000-2022) and what to ask each country for, for 2023-2025",
             "Green = UPDATE: the country gave NUTS-level data, so it only adds 2023, 2024, 2025 (3 values per agent and region).   "
             "Orange = FULL SERIES: only national totals, so it sends 2000-2025 at NUTS2 for every agent.   Grey = to agree with Roman.",
             "Blue columns: abiotic damage, to fill in from the Incoming sheets on Teams (same rule: NUTS level -> add 2023-2025; national or none -> 2000-2025 at NUTS2).",
             "Details per agent (years we have, years missing, what to ask): sheet by_agent."]
    for i, t in enumerate(intro, 1):
        ws.cell(i, 1, t).font = Font(bold=(i == 1), size=12 if i == 1 else 10)
    cols = [c for c in inv.columns if c != "kind_"]
    start = len(intro) + 2
    for j, c in enumerate(cols, 1):
        ws.cell(start, j, c.replace("_", " "))
    header(ws, start)
    ws.row_dimensions[start].height = 32
    for i, d in enumerate(inv.to_dict("records"), start + 1):
        for j, c in enumerate(cols, 1):
            cell = ws.cell(i, j, d[c])
            cell.alignment = Alignment(wrap_text=True, vertical="top")
            cell.border = THIN
            if c in ("abiotic_provided?", "abiotic_level (NUTS / national)", "abiotic_what_to_ask"):
                cell.fill = TODO
            else:
                cell.fill = PatternFill("solid", fgColor=FILL[d["kind_"]])
        ws.cell(i, 1).font = Font(bold=True)
    ws.freeze_panes = ws.cell(start + 1, 2)
    ws.page_setup.orientation = "landscape"; ws.page_setup.fitToWidth = 1; ws.page_setup.fitToHeight = 0
    ws.sheet_properties.pageSetUpPr.fitToPage = True
    widths(ws, {1: 16, 2: 30, 3: 8, 4: 11, 5: 7, 6: 9, 7: 44, 8: 10, 9: 11, 10: 13, 11: 18, 12: 55})

    ws = wb.create_sheet("by_agent")
    ws.append([c.replace("_", " ") for c in ag.columns])
    header(ws, 1)
    ws.row_dimensions[1].height = 32
    prev = None
    for r in ag.itertuples(index=False):
        ws.append(list(r))
        row = ws.max_row
        for cell in ws[row]:
            cell.alignment = Alignment(wrap_text=True, vertical="top")
            if r.country != prev:
                cell.border = Border(top=Side(style="medium", color="2B4334"))
        for j in (1, ag.columns.get_loc("ask_for") + 1):
            ws.cell(row, j).fill = PatternFill("solid", fgColor=FILL[LEVEL[r.country][1]])
        prev = r.country
    ws.freeze_panes = "C2"
    ws.page_setup.orientation = "landscape"; ws.page_setup.fitToWidth = 1; ws.page_setup.fitToHeight = 0
    ws.sheet_properties.pageSetUpPr.fitToPage = True
    widths(ws, {1: 8, 2: 40, 3: 22, 4: 7, 5: 6, 6: 8, 7: 11, 8: 26, 9: 30, 10: 9, 11: 38})
    wb.save(path)


def presence_species():
    _, ins, _ = names()
    sp = ins[(~ins.aggregate_code) & (ins.countries_recorded >= 2)].copy()
    sp["o"] = (sp.guild != "borer").astype(int)
    return sp.sort_values(["o", "countries_recorded", "regions_recorded"], ascending=[True, False, False])


def write_presence(sp, occ, path):
    rn = region_names()
    wb = Workbook()
    ws = wb.active
    ws.title = "how to read"
    lines = ["Which species may occur in a region although EU-ForDAM has no record of it, and how sure you are.",
             "",
             "One sheet per country: species in rows, regions in columns (code and name).",
             "X (green) = the species is reported in the EU-ForDAM data for that region.",
             "Empty cell = not reported. Please enter how confident you are that the species occurs there:",
             "    3 = certainly present     2 = probably present     1 = possibly present     0 = absent     (leave empty if you do not know)",
             "",
             "Countries that sent national totals: the columns are the country's NUTS2 regions. Species in the national data are marked",
             "in the column 'reported nationally' and their row is shaded: please mark in which regions they occur.",
             "Regions are the units of the data: NUTS2 (AT, CZ), NUTS1 (DE), State Forest directorates (PL), forest centres (FI).",
             "",
             f"Species: the {len(sp)} species reported by at least 2 countries in EU-ForDAM (borers first, then defoliators);",
             "group codes (damage of several insects reported together) and species reported by one country only are left out."]
    for i, t in enumerate(lines, 1):
        ws.cell(i, 1, t).font = Font(bold=(i == 1))
    widths(ws, {1: 130})
    for c in sorted(COUNTRY):
        ws = wb.create_sheet(f"{c} {COUNTRY[c]}"[:31])
        kind = LEVEL[c][1]
        rep = occ[occ.country == c].set_index("nuts_id")
        if kind == "full":
            regions, national = nuts2_codes(c), True
        else:
            regions, national = list(rep.index), False
        ws.cell(1, 1, f"{COUNTRY[c]}: is the species present in the region?").font = Font(bold=True, size=12)
        ws.cell(2, 1, "X = reported in EU-ForDAM.  Fill empty cells: 3 certainly, 2 probably, 1 possibly present, 0 absent; empty = unknown.")
        if national:
            ws.cell(3, 1, "We have national totals only: shaded species are in your national data, please mark the regions where they occur.")
        hdr = ["species", "borer / defoliator"] + (["reported nationally"] if national else []) + \
              [f"{r}  {rn.get(r, '')}".strip() for r in regions]
        H = 5
        for j, v in enumerate(hdr, 1):
            ws.cell(H, j, v)
        first = 3 + (1 if national else 0)
        header(ws, H, rotate_from=3)
        ws.row_dimensions[H].height = 150
        for i, s in enumerate(sp.itertuples(), H + 1):
            ws.cell(i, 1, s.insect).font = Font(italic=True)
            ws.cell(i, 2, s.guild)
            if national:
                v = rep[s.code].iloc[0] if s.code in rep.columns and len(rep) else float("nan")
                x = pd.notna(v)
                ws.cell(i, 3, "X" if x else "")
                if x:
                    for j in range(1, len(hdr) + 1):
                        ws.cell(i, j).fill = NATFILL
                    ws.cell(i, 3).fill = XFILL
                for j, r in enumerate(regions, first):
                    if c == "IT" and r == "ITH4" and x:
                        ws.cell(i, j, "X").fill = XFILL
            else:
                for j, r in enumerate(regions, first):
                    if s.code in rep.columns and pd.notna(rep.loc[r, s.code]):
                        ws.cell(i, j, "X").fill = XFILL
            for j in range(3, len(hdr) + 1):
                ws.cell(i, j).alignment = Alignment(horizontal="center")
                ws.cell(i, j).border = Border(left=Side(style="thin", color="E7E6E2"), bottom=Side(style="thin", color="E7E6E2"))
        dv = DataValidation(type="list", formula1='"X,3,2,1,0"', allow_blank=True)
        ws.add_data_validation(dv)
        dv.add(f"{get_column_letter(first)}{H + 1}:{get_column_letter(len(hdr))}{H + len(sp)}")
        ws.freeze_panes = ws.cell(H + 1, first)
        ws.page_setup.orientation = "landscape"; ws.page_setup.fitToWidth = 1; ws.page_setup.fitToHeight = 0
        ws.sheet_properties.pageSetUpPr.fitToPage = True
        widths(ws, {1: 30, 2: 11, **{j: 6 for j in range(3, len(hdr) + 1)}})
    wb.save(path)


if __name__ == "__main__":
    inv, ag = inventory()
    write_inventory(inv, ag, "outputs/tables/REQUEST_data_inventory.xlsx")
    occ = pd.read_csv(OCC)
    sp = presence_species()
    write_presence(sp, occ, "outputs/tables/REQUEST_species_presence_by_NUTS.xlsx")
    print(inv.drop(columns="kind_")[["country", "data_we_have", "regions", "years", "agents", "what_to_ask", "values_to_request", "notes"]].to_string())
    print(len(sp), "species in the presence sheets")
