"""Build the professor's deliverables from the checked inventory data.

  1. EUForDam_Inventory_and_Requests.xlsx  – what we have, from whom, and what we ask each country
  2. EUForDam_Species_Presence.xlsx        – species × NUTS table, one sheet per country (X = reported, confidence 0–3)
     species_presence/EUForDam_Species_Presence_XX.xlsx – the same sheet per country, to attach to individual emails

Usage: python build_deliverables.py <build_inventory.py> <EUForDam_REW.xlsx> <old_inventory.xlsx> <Data_Klim_Tree_NUTS.xlsx> <out_dir>
All content comes from EUForDam_REW, the Incoming sheets (as recorded in build_inventory.py) and the host data we hold.
"""
import os
import sys

import pandas as pd
from openpyxl import Workbook
from openpyxl.comments import Comment
from openpyxl.styles import Alignment, Border, Font, PatternFill, Side
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.datavalidation import DataValidation

BUILD, REW, OLD, HOSTFILE, OUT = sys.argv[1:6]
src = open(BUILD, encoding="utf-8").read()
src = src[: src.index("# ---------------------------------------------------------------- styles")]
sys.argv = [BUILD, REW, OLD, "unused.xlsx"]
ns = {}
exec(compile(src, BUILD, "exec"), ns)
d, AGENT, COUNTRY, TARGET, SUBNAT = ns["d"], ns["AGENT"], ns["COUNTRY"], ns["TARGET"], ns["SUBNAT"]
ABIO, INCOMING, FLAGS, UNIT_NAMES, ser = ns["ABIO"], ns["INCOMING"], ns["FLAGS"], ns["UNIT_NAMES"], ns["ser"]
os.makedirs(OUT, exist_ok=True)
os.makedirs(os.path.join(OUT, "species_presence"), exist_ok=True)

ORDER = ["AT", "CZ", "DE", "FI", "PL", "BG", "CH", "HR", "HU", "IT", "LT", "RO", "RS", "SE", "SI", "SK"]
EXPERT = {  # names as given in the Incoming sheets ('–' = no name in the file)
    "AT": "Gernot Hoch", "BG": "Georgi Georgiev, Margarita Georgieva (FRI-BAS)", "CH": "–", "CZ": "Roman Modlinger (CULS)",
    "DE": "–", "FI": "Tiina Ylioja, Markus Melin (Luke)", "HR": "Milan Pernek", "HU": "Anikó Hirka, György Csóka (Univ. Sopron)",
    "IT": "–", "LT": "Gediminas Brazaitis", "PL": "–", "RO": "Mihai-Leonard Duduman", "RS": "Slobodan Milanović",
    "SE": "Martin Schroeder", "SI": "Maarten de Groot, Marija Kolšek", "SK": "Jozef Vakula (NLC Zvolen)"}
LEVEL_TXT = {"AT": "NUTS 2", "CZ": "NUTS 2", "DE": "NUTS 1 (federal states)", "FI": "NUTS 3", "PL": "RDLP forest regions (treated as NUTS)"}
FRIENDLY = {"CambXyl_con&BB_con&BO": "other bark beetles on pine", "CambXyl_con&BO": "other bark/wood borers on conifers",
            "CambXyl_con&MD": "bark/wood borers on larch (mixed)", "Def_dec&DB": "other defoliators on broadleaves",
            "Def_dec": "defoliators on Aesculus", "CambXyl_con&BB_con&SM": "spruce bark beetles (mixed)"}
CFRIENDLY = {("CZ", "CambXyl_con&BB_con&SM"): "Ips typographus (spruce bark beetles)", ("SK", "ips_dup"): "other bark beetles"}

# ---------------------------------------------------------------- host data we hold (Data_Klim_Tree_NUTS)
k = pd.read_excel(HOSTFILE, sheet_name="DATA_CLEAN")
k["COUNTRY"] = k["COUNTRY"].replace({"SL": "SI"})
HOST_HELD = {}
for c, g in k.groupby("COUNTRY"):
    HOST_HELD[c] = {"units": len(g), "codes": list(g["NUTS_region"] if c == "PL" else g["NUTS_CODE"].astype(str)),
                    "species": [h for h in ("spruce", "pine", "oak", "fir")
                                if pd.to_numeric(g[f"{h}_m3"], errors="coerce").fillna(0).gt(0).any()
                                or pd.to_numeric(g[f"{h}_ha"], errors="coerce").fillna(0).gt(0).any()]}
HOSTKEY = {"Picea abies": "spruce", "Pinus sylvestris": "pine", "Quercus spp.": "oak", "Abies alba": "fir",
           "Larix decidua": "larch", "Betula spp.": "birch", "Populus, Salix": "poplar", "Populus, Salix ?": "poplar",
           "Ulmus spp.": "elm", "Aesculus spp.": "aesculus"}
host_of = d.drop_duplicates("sp_code").set_index("sp_code")["HOSTTREE"].astype(str).str.strip()


def yr(a, b):
    return f"{a}–{b}" if a != b else f"{a}"


def units_for(c):
    if c in SUBNAT:
        return [(u, UNIT_NAMES.get(u, u)) for u in sorted(d[d.Country == c].NUTS.unique())]
    return TARGET[c][1]


def level_for(c):
    return LEVEL_TXT[c] if c in SUBNAT else TARGET[c][0].split(" (")[0]


def agents_for(c):
    s = ser[ser[0] == c]
    out = []
    for _, r in s.iterrows():
        code = r[2]
        name = CFRIENDLY.get((c, code)) or FRIENDLY.get(code) or AGENT[code][0]
        out.append({"code": code, "name": name, "unit": r[6], "y0": int(r[8]), "y1": int(r[9]), "blank": r[10],
                    "nz": int(r[11]), "units": int(r[7])})
    out.sort(key=lambda a: (a["code"] in FRIENDLY or (c, a["code"]) in CFRIENDLY and c != "CZ", -a["nz"], a["name"]))
    return out


def abio_held(c, i0):
    a = ABIO[c]
    if a[i0] != "Y":
        return "not provided"
    lev = level_for(c) if a[i0 + 1] == "NUTS" else "national"
    return f"{lev}, {yr(a[i0 + 2], a[i0 + 3])}"


def abio_req(c, i0):
    a = ABIO[c]
    if a[i0] == "Y" and a[i0 + 1] == "NUTS":
        return f"Update {a[i0 + 3] + 1}–2025 by {level_for(c)}"
    return f"Full series 2000–2025 by {level_for(c)}"


def host_status(c):
    h = HOST_HELD.get(c)
    if c in SUBNAT and h and h["units"] > 1:
        missing = [u for u, _ in units_for(c) if u not in h["codes"] and c not in ("CZ", "PL")]
        held = f"by region ({h['units']} units): {', '.join(h['species'])}"
        req = f"Missing regions: {', '.join(missing)}" if missing else "– (held)"
        return held, req
    held = f"national: {', '.join(h['species'])}" if h else "none"
    return held, f"Growing stock / area of main tree species by {level_for(c)} (with the district data)"


# ---------------------------------------------------------------- styles
F = "Arial"
H = Font(name=F, bold=True, color="FFFFFF")
HF = PatternFill("solid", fgColor="305496")
B = Font(name=F, size=10)
BB = Font(name=F, size=10, bold=True)
T = Font(name=F, size=14, bold=True)
YEL = PatternFill("solid", fgColor="FFF2CC")
GRY = PatternFill("solid", fgColor="D9D9D9")
GRN = PatternFill("solid", fgColor="C6EFCE")
RED = PatternFill("solid", fgColor="F8CBAD")
thin = Side(style="thin", color="BFBFBF")
BOX = Border(left=thin, right=thin, top=thin, bottom=thin)
CEN = Alignment(horizontal="center", vertical="center", wrap_text=True)
WRAP = Alignment(wrap_text=True, vertical="top")


def table(ws, r0, cols, widths, rows):
    for j, (h_, w) in enumerate(zip(cols, widths), 1):
        c_ = ws.cell(r0, j, h_)
        c_.font, c_.fill, c_.alignment, c_.border = H, HF, CEN, BOX
        ws.column_dimensions[get_column_letter(j)].width = w
    for i, row in enumerate(rows, r0 + 1):
        for j, v in enumerate(row, 1):
            c_ = ws.cell(i, j, v)
            c_.font, c_.alignment, c_.border = B, WRAP, BOX
    ws.freeze_panes = ws.cell(r0 + 1, 3)
    ws.auto_filter.ref = f"A{r0}:{get_column_letter(len(cols))}{r0 + len(rows)}"


# =====================================================================================
# 1. Inventory and requests
# =====================================================================================
wb = Workbook()
ov = wb.active
ov.title = "Overview"
ov["A1"] = "EUForDam – what we have, from whom, and what we ask"
ov["A1"].font = T
ov["A2"] = ("Green = NUTS-level data held → add 2023–2025 only.  Red = national data only → full series 2000–2025 at NUTS level.  "
            "Abiotic: same logic, from the Incoming sheets.  Sources: EUForDam_REW (insects), Incoming sheets (abiotic, experts), "
            "Data_Klim_Tree_NUTS (host data).")
ov["A2"].font = B
rows = []
for c in ORDER:
    sub = c in SUBNAT
    ag = agents_for(c)
    gy = d[(d.Country == c) & d.v.notna()].d_year
    hh, hr = host_status(c)
    rows.append([c, COUNTRY[c], EXPERT[c],
                 f"{level_for(c) if sub else 'National'} ({len(units_for(c)) if sub else 1} unit{'s' if sub else ''}), "
                 f"{gy.min()}–{gy.max()}, {len(ag)} agents",
                 "YES" if sub else "NO",
                 "Add 2023–2025 (3 values per agent × region)" if sub else
                 f"Full series 2000–2025 by {level_for(c)} ({len(units_for(c))} regions), all agents",
                 abio_held(c, 0), abio_req(c, 0), abio_held(c, 4), abio_req(c, 4), hh, hr])
table(ov, 4, ["Code", "Country", "Expert (from Incoming)", "Insects – we have", "NUTS data?", "Insects – we ask",
              "Wind – we have", "Wind – we ask", "Total abiotic – we have", "Total abiotic – we ask",
              "Host data – we have", "Host data – we ask"],
      [6, 12, 26, 30, 9, 34, 22, 26, 22, 26, 28, 34], rows)
for i, c in enumerate(ORDER, 5):
    for j in (1, 2, 5, 6):
        ov.cell(i, j).fill = GRN if c in SUBNAT else RED
    for j in (8, 10):
        ov.cell(i, j).fill = GRN if ov.cell(i, j).value.startswith("Update") else RED

# ---- agents per country
ag_ws = wb.create_sheet("Agents")
rows = []
for c in ORDER:
    for a in agents_for(c):
        rows.append([c, COUNTRY[c], a["name"], a["code"], a["unit"], a["units"], yr(a["y0"], a["y1"]), a["blank"],
                     "2023–2025" if c in SUBNAT else f"2000–2025 by {level_for(c)}"])
table(ag_ws, 1, ["Code", "Country", "Agent", "Agent code", "Unit", "# regions held", "Years held", "Blank years (2000–2022)",
                 "We ask"], [6, 12, 34, 22, 6, 9, 11, 16, 24], rows)

# ---- abiotic: who provided what in the Incoming sheets (professor's 3rd point)
ABIO_UNIT = {  # units as found in each Incoming sheet: (wind, total abiotic)
    "AT": ("–", "m3"), "CZ": ("–", "m3"), "DE": ("–", "m3 / ha mixed"), "FI": ("ha (NFI area)", "ha (NFI area)"),
    "PL": ("m3 (proxy: broken & fallen trees)", "–"), "BG": ("–", "ha"), "CH": ("m3", "m3 (sum of categories)"),
    "HR": ("–", "ha + m3"), "HU": ("ha ('Storm')", "ha"), "IT": ("m3 (Vaia storm only)", "ha + m3"),
    "LT": ("ha + m3", "ha"), "RO": ("ha (+ m3 with gaps)", "–"), "RS": ("–", "m3"),
    "SE": ("million m3 (storm years only)", "million m3 (= storm-felled)"), "SI": ("–", "m3"), "SK": ("–", "m3")}
abw = wb.create_sheet("Abiotic", 2)
abw["A1"] = "Abiotic damage – who provided what in the Incoming sheets, and what we ask"
abw["A1"].font = T
abw["A2"] = ("Rule: provided by NUTS → update the last years only; not provided or national only → full series 2000–2025 at NUTS level.")
abw["A2"].font = B
rows = []
for c in ORDER:
    a = ABIO[c]
    lev = lambda i0: ("—" if a[i0] != "Y" else (level_for(c) if a[i0 + 1] == "NUTS" else "National"))
    yrs = lambda i0: (yr(a[i0 + 2], a[i0 + 3]) if a[i0] == "Y" else "—")
    rows.append([c, COUNTRY[c],
                 "Yes" if a[0] == "Y" else "No", lev(0), yrs(0), ABIO_UNIT[c][0], abio_req(c, 0),
                 "Yes" if a[4] == "Y" else "No", lev(4), yrs(4), ABIO_UNIT[c][1], abio_req(c, 4),
                 a[8]])
table(abw, 4, ["Code", "Country",
               "Wind in Incoming?", "Wind level", "Wind years", "Wind unit", "Wind – we ask",
               "Total abiotic in Incoming?", "Total level", "Total years", "Total unit", "Total abiotic – we ask",
               "Notes from the Incoming sheet"],
      [6, 12, 9, 14, 11, 18, 26, 10, 14, 11, 16, 26, 70], rows)
for i, row in enumerate(rows, 5):
    for j in (7, 12):
        abw.cell(i, j).fill = GRN if str(abw.cell(i, j).value).startswith("Update") else RED
    for j in (3, 8):
        abw.cell(i, j).fill = GRN if abw.cell(i, j).value == "Yes" else RED
    abw.row_dimensions[i].height = 48

# ---- notes for Roman (internal)
nr = wb.create_sheet("For Roman (internal)")
rows = [[c, COUNTRY[c], FLAGS.get(c, ""), INCOMING.get(c, ("",) * 7)[6]] for c in ORDER]
table(nr, 1, ["Code", "Country", "Data gaps / to clarify", "Incoming vs EUForDam (conversions, errors)"], [6, 12, 70, 90], rows)
for i in range(2, 2 + len(rows)):
    nr.row_dimensions[i].height = 75
for s_ in wb.worksheets:
    s_.sheet_view.zoomScale = 90
wb.save(os.path.join(OUT, "EUForDam_Inventory_and_Requests.xlsx"))

# =====================================================================================
# 2. Species presence – species × NUTS, one sheet per country
# =====================================================================================
nzc = d[d.v > 0].groupby("sp_code").Country.nunique()
SPECIES = sorted([k_ for k_, v in AGENT.items() if v[1] != "group code" and nzc.get(k_, 0) >= 3],
                 key=lambda k_: (-nzc[k_], AGENT[k_][0]))


def species_sheet(ws, c):
    units = units_for(c) + ([("FI1D7", "Lappi"), ("FI200", "Åland")] if c == "FI" else [])
    g = d[d.Country == c].copy()
    if c == "SK":
        g = g[g.sp_code != "ips_dup"]            # SK 'ips_dup' is really 'other bark beetles'
    if c == "CZ":
        g.loc[g.sp_code == "CambXyl_con&BB_con&SM", "sp_code"] = "ips_typ"
    pos = g[g.v > 0]
    by_unit = pos.groupby(["sp_code", "NUTS"]).d_year.agg(lambda s: yr(int(s.min()), int(s.max()))).to_dict()
    by_nat = pos.groupby("sp_code").d_year.agg(lambda s: yr(int(s.min()), int(s.max()))).to_dict()
    # species whose host tree occurs in this country's data (agents' hosts or host data we hold)
    hosts = {HOSTKEY.get(host_of[s_]) for s_ in g.sp_code.unique()} | set(HOST_HELD.get(c, {}).get("species", []))
    species = [s_ for s_ in SPECIES if HOSTKEY.get(host_of[s_]) in hosts]
    sub = c in SUBNAT
    ws["A1"] = f"{COUNTRY[c]} – which species occur where?"
    ws["A1"].font = T
    ws["A2"] = ("X = reported in our data (hover for years).  Yellow cells: how confident are you that the species occurs in that region, "
                "even if no damage was recorded?  0 = absent · 1 = low · 2 = medium · 3 = high.  Leave blank if you do not know.")
    ws["A2"].font = B
    ws["A2"].alignment = Alignment(wrap_text=True, vertical="top")
    ws.merge_cells(start_row=2, start_column=1, end_row=2, end_column=min(3 + len(units), 14))
    ws.row_dimensions[2].height = 45
    fixed = ["Species", "Host tree"] + ([] if sub else ["Reported nationally"])
    nf = len(fixed)
    for j, h_ in enumerate(fixed, 1):
        ws.cell(4, j, h_)
        ws.merge_cells(start_row=4, start_column=j, end_row=5, end_column=j)
    for kk, (u, nm) in enumerate(units):
        ws.cell(4, nf + 1 + kk, u)
        ws.cell(5, nf + 1 + kk, nm)
    cc = nf + len(units) + 1
    ws.cell(4, cc, "Comment")
    ws.merge_cells(start_row=4, start_column=cc, end_row=5, end_column=cc)
    for r in (4, 5):
        for j in range(1, cc + 1):
            c_ = ws.cell(r, j)
            c_.font, c_.fill, c_.alignment, c_.border = H, HF, CEN, BOX
    ws.row_dimensions[5].height = 45
    dv = DataValidation(type="list", formula1='"0,1,2,3"', allow_blank=True, error="Enter 0, 1, 2 or 3 (or leave blank).",
                        showErrorMessage=True)
    ws.add_data_validation(dv)
    r = 6
    for s_ in species:
        ws.cell(r, 1, AGENT[s_][0]).font = Font(name=F, size=10, italic=True)
        ws.cell(r, 2, host_of[s_]).font = B
        if not sub:
            c_ = ws.cell(r, 3)
            c_.alignment, c_.font = CEN, BB
            if s_ in by_nat:
                c_.value, c_.fill = "X", GRY
                c_.comment = Comment(f"Reported nationally {by_nat[s_]}", "EUForDam")
        for kk, (u, _) in enumerate(units):
            c_ = ws.cell(r, nf + 1 + kk)
            c_.alignment, c_.font = CEN, BB
            if sub and (s_, u) in by_unit:
                c_.value, c_.fill = "X", GRY
                c_.comment = Comment(f"Reported {by_unit[(s_, u)]}", "EUForDam")
            else:
                c_.fill = YEL
                dv.add(c_.coordinate)
        ws.cell(r, cc).fill = YEL
        for j in range(1, cc + 1):
            ws.cell(r, j).border = BOX
        r += 1
    ws.column_dimensions["A"].width = 30
    ws.column_dimensions["B"].width = 16
    if not sub:
        ws.column_dimensions["C"].width = 11
    for kk in range(len(units)):
        ws.column_dimensions[get_column_letter(nf + 1 + kk)].width = 13
    ws.column_dimensions[get_column_letter(cc)].width = 35
    ws.freeze_panes = ws.cell(6, nf + 1)
    ws.sheet_view.zoomScale = 90
    return len(species), len(units)


def legend(ws):
    ws["A1"] = "EUForDam – species presence"
    ws["A1"].font = T
    lines = ["One sheet per country: species in rows, NUTS regions in columns.",
             "X (grey) = species reported in EUForDam with damage > 0 in that region (hover over X for the years).",
             "Yellow cells: confidence that the species occurs there, even if no damage was recorded:",
             "   0 = absent   1 = low (possible)   2 = medium (likely)   3 = high (confirmed)   blank = do not know",
             "Species listed: those reported in at least 3 countries in EUForDam whose host tree occurs in the country."]
    for i, t in enumerate(lines, 3):
        ws.cell(i, 1, t).font = B
    ws.column_dimensions["A"].width = 120


allw = Workbook()
legend(allw.active)
allw.active.title = "Legend"
summary = []
for c in ORDER:
    n_sp, n_u = species_sheet(allw.create_sheet(c), c)
    one = Workbook()
    species_sheet(one.active, c)
    one.active.title = c
    legend(one.create_sheet("Legend"))
    one.save(os.path.join(OUT, "species_presence", f"EUForDam_Species_Presence_{c}.xlsx"))
    summary.append(f"{c}:{n_sp}x{n_u}")
allw.save(os.path.join(OUT, "EUForDam_Species_Presence.xlsx"))
print("species × regions:", " ".join(summary))
