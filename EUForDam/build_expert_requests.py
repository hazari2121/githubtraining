"""Build one expert-facing data-request workbook per country.

Usage: python build_expert_requests.py <build_inventory.py> <EUForDam_REW.xlsx> <old_inventory.xlsx> <out_dir> [Data_Klim_Tree_NUTS.xlsx]
Reuses the data definitions in build_inventory.py (everything above its 'styles' section).
"""
import os
import sys

import pandas as pd

from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.datavalidation import DataValidation

BUILD, REW, OLD, OUTDIR = sys.argv[1:5]
HOSTFILE = sys.argv[5] if len(sys.argv) > 5 else None   # Data_Klim_Tree_NUTS_*.xlsx (host availability we hold)
src = open(BUILD, encoding="utf-8").read()
src = src[: src.index("# ---------------------------------------------------------------- styles")]
sys.argv = [BUILD, REW, OLD, "unused.xlsx"]
ns = {}
exec(compile(src, BUILD, "exec"), ns)
d, AGENT, COUNTRY, TARGET, SUBNAT, ABIO = ns["d"], ns["AGENT"], ns["COUNTRY"], ns["TARGET"], ns["SUBNAT"], ns["ABIO"]
UNIT_NAMES, host, unit_of, LEVEL = ns["UNIT_NAMES"], ns["host"], ns["unit_of"], ns["LEVEL"]
os.makedirs(OUTDIR, exist_ok=True)

# ---- host availability we already hold (Data_Klim_Tree_NUTS, sheet DATA_CLEAN)
HOSTS = [("spruce", "Norway spruce (Picea abies)"), ("pine", "Scots pine (Pinus sylvestris)"), ("oak", "Oaks (Quercus spp.)"),
         ("fir", "Silver fir (Abies alba)"), ("coniferous", "All conifers"), ("deciduous", "All broadleaves")]
HOSTVAL, HOST_UNITS = {}, {}
if HOSTFILE:
    import pandas as _pd
    _k = _pd.read_excel(HOSTFILE, sheet_name="DATA_CLEAN")
    _k["COUNTRY"] = _k["COUNTRY"].replace({"SL": "SI"})
    for _, row in _k.iterrows():
        cc_ = row["COUNTRY"]
        code_, name_ = (row["NUTS_region"], row["NUTS_CODE"]) if cc_ == "PL" else (str(row["NUTS_CODE"]).replace(";", "; ").replace("  ", " "), row["NUTS_region"])
        if cc_ not in SUBNAT:
            code_ = cc_
        for hk, _n in HOSTS:
            def num(v):
                v = _pd.to_numeric(v, errors="coerce")
                return None if _pd.isna(v) else float(v)
            HOSTVAL[(cc_, code_, hk)] = (num(row.get(f"{hk}_m3")), num(row.get(f"{hk}_ha")))
        if cc_ in SUBNAT:
            HOST_UNITS.setdefault(cc_, []).append((code_, UNIT_NAMES.get(code_, name_)))

# expert-friendly names for group codes / special cases
FRIENDLY = {
    "CambXyl_con&BB_con&BO": "Other bark beetles on pine",
    "CambXyl_con&BO": "Other bark and wood borers on conifers",
    "CambXyl_con&MD": "Bark and wood borers on larch (mixed)",
    "Def_dec&DB": "Other defoliators on broadleaves",
    "Def_dec": "Defoliators on Aesculus",
}
COUNTRY_FRIENDLY = {("CZ", "CambXyl_con&BB_con&SM"): "Ips typographus (spruce bark beetles)",
                    ("DE", "CambXyl_con&BB_con&SM"): "Spruce bark beetles – mixed",
                    ("SK", "ips_dup"): "Other bark beetles (your column 'Other bark beetles')"}

QUESTIONS = {
    "AT": ["Your abiotic total comes as several unlabelled rows per region and year. Can you label them by type (wind, snow, other)?",
           "Do insect data for 2000–2001 exist? (optional)"],
    "CZ": ["Your regional rows are NUTS 3 kraje filed under NUTS 2 codes. Can you report by NUTS 3 (kraj) code instead? If yes, add the NUTS 3 code in the Comment column.",
           "Is a wind series (separate from total abiotic) available?"],
    "DE": ["Can Rheinland-Pfalz (DEB) and Saarland (DEC) be reported separately?",
           "Where a state has no data, please leave the cell blank – do not enter 0."],
    "FI": ["Can Lapland (FI1D7) be included?", "Do Ips typographus data before 2012 exist? (optional)"],
    "PL": ["Is a labelled wind / storm series available? We currently use 'processed broken and fallen trees'.",
           "Do data for 2000–2002 exist? (optional)"],
    "SK": ["Which species are in your 'Other bark beetles' column?"],
    "SE": ["Years without outbreak: leave blank if there is no estimate; enter 0 only if damage was assessed and was zero."],
    "CH": ["The file you sent has 2023 bark beetle = 0. Please give the complete 2023 values here."],
    "IT": ["The data cover Friuli Venezia Giulia only. Are data for other regions available? (optional)"],
    "HU": ["Your values are in ha. If volumes (m3) are also available, please give them on the Volume (m3) lines."],
    "LT": ["Your data cover state forests. Please say in the Comment column if values are scaled to all forests."],
}

F = "Arial"
H = Font(name=F, bold=True, color="FFFFFF")
HF = PatternFill("solid", fgColor="305496")
B = Font(name=F, size=10)
BB = Font(name=F, size=10, bold=True)
T = Font(name=F, size=14, bold=True)
YEL = PatternFill("solid", fgColor="FFF2CC")
OPT = PatternFill("solid", fgColor="FFFBEB")
GRY = PatternFill("solid", fgColor="D9D9D9")
LGR = PatternFill("solid", fgColor="EDEDED")
thin = Side(style="thin", color="BFBFBF")
BOX = Border(left=thin, right=thin, top=thin, bottom=thin)
CEN = Alignment(horizontal="center", vertical="center", wrap_text=True)
WRAP = Alignment(wrap_text=True, vertical="top")


def units_for(c):
    if c in SUBNAT:
        return [(u, UNIT_NAMES.get(u, u)) for u in sorted(d[d.Country == c].NUTS.unique())]
    return TARGET[c][1]


def level_for(c):
    return LEVEL[c] if c in SUBNAT else TARGET[c][0]


def agents_for(c):
    g = d[d.Country == c]
    out = []
    for code in g.sp_code.unique():
        name = COUNTRY_FRIENDLY.get((c, code)) or FRIENDLY.get(code) or AGENT[code][0]
        out.append((name, host.loc[code, "HOSTTREE"], unit_of.get(code, "m3") or "m3", code))
    order = {"species": 0, "species group": 0, "group code": 1}
    ncty = d[d.v > 0].groupby("sp_code").Country.nunique()

    def key(a):
        if (c, a[3]) == ("CZ", "CambXyl_con&BB_con&SM"):
            return (0, -99, a[0])
        grp = 1 if (AGENT[a[3]][1] == "group code" or (c, a[3]) in COUNTRY_FRIENDLY) else 0
        return (grp, -ncty.get(a[3], 0), a[0])
    out.sort(key=key)
    return out


def abio_years(c, idx_in, idx_lev, idx_last):
    a = ABIO[c]
    if a[idx_in] == "Y" and a[idx_lev] == "NUTS":
        return list(range(a[idx_last] + 1, 2026)), "update"
    return list(range(2000, 2026)), "full"


TYPE_OF = {"CambXyl_con": "Bark/wood borer", "CambXyl_dec": "Bark/wood borer", "Def_con": "Defoliator", "Def_dec": "Defoliator"}
MEAS = {"m3": "Volume (m3)", "ha": "Area (ha)"}
HIDDEN = ["sp_code", "SP_CATEGORY", "SP_SUBCATEGORY", "HOSTTREE", "TREETYPE", "sp_unit"]
# Category per code: most common combination outside CZ (CZ category labels in EUForDam_REW are scrambled)
_m = d[d.Country != "CZ"] if (d.Country != "CZ").any() else d
_cols = ["SP_CATEGORY", "SP_SUBCATEGORY", "HOSTTREE", "TREETYPE"]
meta = (_m.groupby(["sp_code"] + _cols).size().reset_index(name="n").sort_values("n", ascending=False)
        .drop_duplicates("sp_code").set_index("sp_code")[_cols])
_cz = d[~d.sp_code.isin(meta.index)]
meta = pd.concat([meta, _cz.groupby(["sp_code"] + _cols).size().reset_index(name="n").sort_values("n", ascending=False)
                  .drop_duplicates("sp_code").set_index("sp_code")[_cols]])


def header(ws, r, cols, widths):
    for j, (h, w) in enumerate(zip(cols, widths), 1):
        c = ws.cell(r, j, h)
        c.font, c.fill, c.alignment, c.border = H, HF, CEN, BOX
        ws.column_dimensions[get_column_letter(j)].width = w
    ws.row_dimensions[r].height = 32


def grid_sheet(ws, title, note, fixed_cols, fixed_w, rows, years):
    """rows: dicts with 'fixed' (list), 'hidden' (list), 'status' (fn year -> req/opt/held),
    'inputs' (indices of fixed cols the expert fills), 'first' (bool: first line of a block)."""
    ws["A1"] = title
    ws["A1"].font = T
    ws["A2"] = note
    ws["A2"].font = B
    ws["A2"].alignment = Alignment(wrap_text=True, vertical="top")
    ws.merge_cells(start_row=2, start_column=1, end_row=2, end_column=min(len(fixed_cols) + len(years) + 1, 16))
    ws.row_dimensions[2].height = 62
    nf, ny = len(fixed_cols), len(years)
    cols = fixed_cols + [str(y) for y in years] + ["Comment"] + HIDDEN
    header(ws, 4, cols, fixed_w + [9] * ny + [40] + [14] * len(HIDDEN))
    dv_num = DataValidation(type="decimal", operator="greaterThanOrEqual", formula1="0", allow_blank=True,
                            error="Please enter a number ≥ 0 (leave blank if no data).", showErrorMessage=True)
    dv_meas = DataValidation(type="list", formula1='"Volume (m3),Area (ha)"', allow_blank=True)
    dv_type = DataValidation(type="list", formula1='"Bark/wood borer,Defoliator,Other"', allow_blank=True)
    for v in (dv_num, dv_meas, dv_type):
        ws.add_data_validation(v)
    r = 5
    for row in rows:
        for j, v in enumerate(row["fixed"], 1):
            c = ws.cell(r, j, v)
            c.font, c.border, c.alignment = B, BOX, WRAP
            if j - 1 in row.get("inputs", ()):
                c.fill = YEL
                if fixed_cols[j - 1] == "Measure":
                    dv_meas.add(c.coordinate)
                if fixed_cols[j - 1] == "Type":
                    dv_type.add(c.coordinate)
        if row.get("first"):
            ws.cell(r, 1).font = BB
            for j in range(1, nf + ny + 2):
                ws.cell(r, j).border = Border(left=thin, right=thin, bottom=thin, top=Side(style="medium", color="305496"))
        else:
            ws.cell(r, 1).font = Font(name=F, size=10, color="A6A6A6")
        for k, y in enumerate(years):
            cell = ws.cell(r, nf + 1 + k)
            if not row.get("first"):
                cell.border = BOX
            cell.alignment = CEN
            st = row["status"](y)
            if st == "held":
                cell.fill, cell.value = GRY, "held"
                cell.font = Font(name=F, size=8, color="808080")
            else:
                cell.fill = YEL if st == "req" else OPT
                dv_num.add(cell.coordinate)
        cm = ws.cell(r, nf + ny + 1)
        cm.fill = YEL
        if not row.get("first"):
            cm.border = BOX
        for j, v in enumerate(row["hidden"]):
            ws.cell(r, nf + ny + 2 + j, v).font = Font(name=F, size=8, color="808080")
        r += 1
    for j in range(len(HIDDEN)):
        ws.column_dimensions[get_column_letter(nf + ny + 2 + j)].hidden = True
    ws.freeze_panes = ws.cell(5, nf + 1)
    ws.auto_filter.ref = f"A4:{get_column_letter(nf + ny + 1)}{r - 1}"
    return r


for c in COUNTRY:
    sub = c in SUBNAT
    units = units_for(c)
    lvl = level_for(c)
    agents = agents_for(c)
    yrs_held = d[(d.Country == c) & d.v.notna()].d_year
    wb = Workbook()
    reg_units = units if sub else units + [(c, "National total (only if no split possible)")]

    # ---------------- Insect damage
    ins = wb.active
    ins.title = "1 Insect damage"
    if sub:
        years = [2023, 2024, 2025]
        note = (f"We hold your data by {lvl} up to 2022. Please add 2023, 2024 and 2025 for each agent and region below.")
    else:
        years = list(range(2000, 2026))
        note = (f"We hold national totals only. Please give the full series 2000–2025 by {lvl} for each agent below. "
                "If a series cannot be split by region, use the 'National total' row and explain in Comment.")
    note += ("  Each agent/region has TWO lines: Volume (m3) and Area (ha) – fill whichever you have, or both. "
             "The first line is the unit we use for that agent.  BLANK = no data / not monitored.  0 = monitored, no damage.")
    rows = []
    for name, hst, un, code in agents:
        m = meta.loc[code]
        main = un if un in MEAS else ("ha" if m.SP_CATEGORY.startswith("Def") else "m3")
        order = [main, "ha" if main == "m3" else "m3"]
        for u, nm in reg_units:
            for i, mu in enumerate(order):
                rows.append({"fixed": [name, TYPE_OF.get(m.SP_CATEGORY, ""), hst, MEAS[mu], u, nm],
                             "hidden": [code, m.SP_CATEGORY, m.SP_SUBCATEGORY, m.HOSTTREE, m.TREETYPE, main],
                             "status": lambda y: "req", "first": i == 0})
    for k in range(8):
        rows.append({"fixed": ["Other agent (name) →" if k == 0 else "", "", "", "", "", ""], "hidden": ["NEW", "", "", "", "", ""],
                     "status": lambda y: "req", "inputs": (0, 1, 2, 3, 4, 5), "first": k == 0})
    grid_sheet(ins, f"{COUNTRY[c]} – insect damage", note,
               ["Agent", "Type", "Host tree", "Measure", "Unit code", "Region"], [32, 13, 14, 12, 10, 22], rows, years)

    # ---------------- Abiotic damage
    ab = wb.create_sheet("2 Abiotic damage")
    wy, wmode = abio_years(c, 0, 1, 3)
    ty, tmode = abio_years(c, 4, 5, 7)
    a_ = ABIO[c]
    w_first = a_[2] if wmode == "update" else 2000   # first year we actually hold
    t_first = a_[6] if tmode == "update" else 2000
    years = list(range(2000, 2026))

    def stat(ry, first):
        return lambda y: "req" if y in ry else ("opt" if y < first else "held")
    rows = []
    for typ, code, ry, first in (("Wind (windthrow)", "ABIO_wind", set(wy), w_first),
                                 ("Total abiotic (wind, snow, ice, drought, fire …)", "ABIO_tot", set(ty), t_first)):
        for u, nm in reg_units:
            for i, mu in enumerate(("m3", "ha")):
                rows.append({"fixed": [typ, MEAS[mu], u, nm], "hidden": [code, "Abiotic", code, "", "", "m3"],
                             "status": stat(ry, first), "first": i == 0})
    note = ("Wind = windthrow / storm damage only.  Total abiotic = all abiotic causes together (wind, snow, ice, drought, fire, other).  "
            "Two lines per region: Volume (m3) and Area (ha) – fill whichever you have, or both.  "
            "Yellow = please fill · light yellow = optional (earlier years we do not have) · grey 'held' = we already have it.  "
            "BLANK = no data, 0 = no damage.")
    grid_sheet(ab, f"{COUNTRY[c]} – abiotic damage", note, ["Damage type", "Measure", "Unit code", "Region"],
               [34, 12, 10, 22], rows, years)

    # ---------------- Host availability
    hs = wb.create_sheet("3 Host availability")
    hs["A1"] = f"{COUNTRY[c]} – host availability (main tree species)"
    hs["A1"].font = T
    hs["A2"] = ("Growing stock (m3) and forest area (ha) of each main tree species, for the same regions as the damage data.  "
                "Grey = values we already have (please check).  Yellow: the latest values with their inventory year, and – if available – "
                "an earlier inventory (ideally around 2000–2010) so we can follow change over 2000–2025.  "
                "Area = area of stands where the species is present or dominant – say which in Comment.")
    hs["A2"].font = B
    hs["A2"].alignment = Alignment(wrap_text=True, vertical="top")
    hs.merge_cells("A2:N2")
    hs.row_dimensions[2].height = 62
    hcols = ["Region code", "Region", "Tree species", "We have: growing stock (m3)", "We have: area (ha)",
             "Latest: growing stock (m3)", "Latest: area (ha)", "Latest: inventory year",
             "Earlier: growing stock (m3)", "Earlier: area (ha)", "Earlier: inventory year", "Source", "Comment", "host_key"]
    header(hs, 4, hcols, [10, 24, 26, 14, 12, 14, 12, 11, 14, 12, 11, 22, 34, 10])
    hs.column_dimensions["N"].hidden = True
    dv_n = DataValidation(type="decimal", operator="greaterThanOrEqual", formula1="0", allow_blank=True,
                          error="Please enter a number ≥ 0 (leave blank if no data).", showErrorMessage=True)
    dv_y = DataValidation(type="whole", operator="between", formula1="1990", formula2="2026", allow_blank=True,
                          error="Enter a year, e.g. 2021.", showErrorMessage=True)
    hs.add_data_validation(dv_n)
    hs.add_data_validation(dv_y)
    host_units = HOST_UNITS.get(c) or (units + [(c, "National total")])
    if c not in HOST_UNITS and c not in SUBNAT:
        pass
    elif c in SUBNAT and c not in HOST_UNITS:
        host_units = units
    if c in SUBNAT and c != "CZ":   # CZ host data are NUTS 3 kraje; elsewhere add damage units lacking host data
        have_ = {u for u, _ in host_units}
        host_units = host_units + [(u, nm) for u, nm in units if u not in have_]
    r = 5
    for u, nm in host_units:
        for i, (hk, hname) in enumerate(HOSTS + [("other1", "Other main species (name it in Comment)")]):
            held = HOSTVAL.get((c, u, hk), (None, None))
            vals = [u, nm, hname, held[0], held[1]]
            for j, v in enumerate(vals, 1):
                cell = hs.cell(r, j, v)
                cell.font, cell.border = B, BOX
            for j in (4, 5):
                hs.cell(r, j).fill = GRY
                hs.cell(r, j).number_format = "#,##0"
            for j in range(6, 14):
                cell = hs.cell(r, j)
                cell.fill, cell.border = (OPT if j in (9, 10, 11) else YEL), BOX
                if j in (6, 7, 9, 10):
                    dv_n.add(cell.coordinate)
                    cell.number_format = "#,##0"
                elif j in (8, 11):
                    dv_y.add(cell.coordinate)
            hs.cell(r, 14, hk).font = Font(name=F, size=8, color="808080")
            if i == 0:
                hs.cell(r, 1).font = hs.cell(r, 2).font = BB
                for j in range(1, 14):
                    hs.cell(r, j).border = Border(left=thin, right=thin, bottom=thin, top=Side(style="medium", color="305496"))
            else:
                hs.cell(r, 1).font = hs.cell(r, 2).font = Font(name=F, size=10, color="A6A6A6")
            r += 1
    hs.freeze_panes = "D5"
    hs.auto_filter.ref = f"A4:M{r - 1}"

    # ---------------- Species presence (copied logic from inventory)
    sp = wb.create_sheet("4 Species presence")
    g = d[d.Country == c].copy()
    if c == "SK":
        g = g[g.sp_code != "ips_dup"]
    if c == "CZ":
        g.loc[g.sp_code == "CambXyl_con&BB_con&SM", "sp_code"] = "ips_typ"
    pos = g[g.v > 0].groupby("sp_code").NUTS.agg(set).to_dict()

    def span(ys):
        ys = sorted(set(int(y) for y in ys))
        return f"{ys[0]}" if ys[0] == ys[-1] else f"{ys[0]}–{ys[-1]}"
    span_u = g[g.v > 0].groupby(["sp_code", "NUTS"]).d_year.agg(span).to_dict()
    span_c = g[g.v > 0].groupby("sp_code").d_year.agg(span).to_dict()
    exist = g.groupby("sp_code").NUTS.agg(set).to_dict()
    nzc = d[d.v > 0].groupby("sp_code").Country.nunique()
    SPECIES = sorted([k for k, v in AGENT.items() if v[1] != "group code" and nzc.get(k, 0) >= 3],
                     key=lambda k: (-nzc[k], AGENT[k][0]))
    sp_units = units + ([("FI1D7", "Lappi"), ("FI200", "Åland")] if c == "FI" else [])
    sp["A1"] = f"{COUNTRY[c]} – which species occur where?"
    sp["A1"].font = T
    sp["A2"] = ("Grey cells with years (e.g. 2003–2022) = you already reported damage there in those years (first–last year).  "
                "Yellow cells: how confident are you that the species OCCURRED in that region at any time 2000–2025, even if no damage "
                "was recorded?  0 = absent · 1 = low (possible) · 2 = medium (likely) · 3 = high (confirmed).  Leave blank if you do not know.  "
                "If a species arrived or spread recently, enter the year it was first recorded in your country in 'First recorded (year)' "
                "and the regions/years in Comment.")
    sp["A2"].font = B
    sp["A2"].alignment = Alignment(wrap_text=True, vertical="top")
    sp.merge_cells(start_row=2, start_column=1, end_row=2, end_column=min(3 + len(sp_units), 14))
    sp.row_dimensions[2].height = 72
    fixed = ["Species", "Host tree"] + ([] if sub else ["Reported nationally (years)"])
    nf = len(fixed)
    for j, hname in enumerate(fixed, 1):
        sp.cell(4, j, hname)
        sp.merge_cells(start_row=4, start_column=j, end_row=5, end_column=j)
    for k, (u, nm) in enumerate(sp_units):
        sp.cell(4, nf + 1 + k, u)
        sp.cell(5, nf + 1 + k, nm)
    fr = nf + len(sp_units) + 1          # first-recorded column
    cc = fr + 1                          # comment column
    sp.cell(4, fr, "First recorded (year), if recent")
    sp.merge_cells(start_row=4, start_column=fr, end_row=5, end_column=fr)
    sp.cell(4, cc, "Comment")
    sp.merge_cells(start_row=4, start_column=cc, end_row=5, end_column=cc)
    dv_yr = DataValidation(type="whole", operator="between", formula1="1800", formula2="2026", allow_blank=True,
                           error="Enter a year (e.g. 2017) or leave blank.", showErrorMessage=True)
    sp.add_data_validation(dv_yr)
    for r in (4, 5):
        for j in range(1, cc + 1):
            cell = sp.cell(r, j)
            cell.font, cell.fill, cell.alignment, cell.border = H, HF, CEN, BOX
    sp.row_dimensions[5].height = 45
    dv = DataValidation(type="list", formula1='"0,1,2,3"', allow_blank=True,
                        error="Enter 0, 1, 2 or 3 (or leave blank).", showErrorMessage=True)
    sp.add_data_validation(dv)
    r = 6
    for code in SPECIES:
        sp.cell(r, 1, AGENT[code][0]).font = Font(name=F, size=10, italic=True)
        sp.cell(r, 2, host.loc[code, "HOSTTREE"]).font = B
        if not sub:
            cell = sp.cell(r, 3)
            cell.alignment, cell.font = CEN, BB
            if code in pos:
                cell.value, cell.fill = span_c[code], GRY
                cell.font = Font(name=F, size=9, bold=True)
        for k, (u, _) in enumerate(sp_units):
            cell = sp.cell(r, nf + 1 + k)
            cell.alignment, cell.font = CEN, BB
            if sub and u in pos.get(code, set()):
                cell.value, cell.fill = span_u[(code, u)], GRY
                cell.font = Font(name=F, size=9, bold=True)
            else:
                cell.fill = YEL
                dv.add(cell.coordinate)
        sp.cell(r, cc).fill = YEL
        sp.cell(r, fr).fill = YEL
        sp.cell(r, fr).alignment = CEN
        dv_yr.add(sp.cell(r, fr).coordinate)
        for j in range(1, cc + 1):
            sp.cell(r, j).border = BOX
        r += 1
    r += 1
    sp.cell(r, 1, "Other species that occur (name, regions, confidence):").font = BB
    for _ in range(4):
        r += 1
        for j in range(1, cc + 1):
            sp.cell(r, j).border, sp.cell(r, j).fill = BOX, YEL
    sp.column_dimensions["A"].width = 30
    sp.column_dimensions["B"].width = 15
    if not sub:
        sp.column_dimensions["C"].width = 13
    sp.column_dimensions[get_column_letter(fr)].width = 13
    for k in range(len(sp_units)):
        sp.column_dimensions[get_column_letter(nf + 1 + k)].width = 14
    sp.column_dimensions[get_column_letter(cc)].width = 35
    sp.freeze_panes = sp.cell(6, nf + 1)

    # ---------------- Start here (first sheet)
    st = wb.create_sheet("Start here", 0)
    st.column_dimensions["A"].width = 26
    st.column_dimensions["B"].width = 95
    lines = [(f"EUForDam – data request: {COUNTRY[c]}", None, T), ("", None, B)]
    have = (f"Insect damage by {lvl}, {yrs_held.min()}–{yrs_held.max()}, {len(agents)} agent series."
            if sub else f"Insect damage as national totals, {yrs_held.min()}–{yrs_held.max()}, {len(agents)} agent series.")
    ask_ins = ("Add 2023, 2024, 2025 for the agents and units listed (3 values each)." if sub else
               f"Full series 2000–2025 by {lvl} ({len(units)} units) – this replaces the national series.")

    def ask_ab(mode, yrs, first):
        if mode != "update":
            return "full series 2000–2025 by region"
        return f"update {yrs[0]}–2025" + (f" (optional: 2000–{first - 1})" if first > 2000 else "")
    lines += [
        ("What we already have", have, None),
        ("What we ask", "", BB),
        ("  Sheet 1 – Insect damage", ask_ins, None),
        ("  Sheet 2 – Abiotic damage", f"Wind: {ask_ab(wmode, wy, w_first)}.  Total abiotic: {ask_ab(tmode, ty, t_first)}.", None),
        ("  Sheet 3 – Host availability", "Growing stock (m3) and area (ha) of each main tree species per region: check the grey values we have, give the latest inventory values and, if available, an earlier inventory.", None),
        ("  Sheet 4 – Species presence", "Grey cells show the years you already reported each species. For the other cells, mark how confident you are that the species occurred there in 2000–2025 (0–3); give the first-recorded year for recent arrivals.", None),
        ("", None, B),
        ("How to fill in", "", BB),
        ("  Yellow cells", "Please fill. Light yellow = optional (earlier years we do not have). Grey 'held' = we already have this, no action needed.", None),
        ("  Blank vs 0", "Leave BLANK if there is no data or the agent was not monitored. Enter 0 ONLY if it was monitored and there was no damage.", None),
        ("  Volume and area", "Every agent/region has two lines – Volume (m3) and Area (ha). Fill whichever you have, or both. Other units: say so in Comment.", None),
        ("  Regions", f"Rows use {lvl} codes. If your data use other units, add them in the Comment column.", None),
        ("  Other agents", "Empty rows at the bottom of Sheet 1 – add any important agent we are missing (name, type, host tree, measure, region).", None),
        ("", None, B),
    ]
    q = ([] if sub else ["Can you provide district-level (NUTS) data for 2000–2025 – damage values and host availability? (yes / partly / no – please say which agents and years)"]) + QUESTIONS.get(c, [])
    if q:
        lines.append(("Questions for you", "Please answer in the yellow cell next to each question.", BB))
    r = 1
    for a, b_, f in lines:
        ca = st.cell(r, 1, a)
        ca.font = f or BB
        if b_ is not None:
            cb = st.cell(r, 2, b_)
            cb.font, cb.alignment = B, WRAP
        r += 1
    for qq in q:
        st.cell(r, 1, "Q").font = BB
        st.cell(r, 2, qq).font, st.cell(r, 2).alignment = B, WRAP
        r += 1
        st.cell(r, 1, "  Answer").font = B
        st.cell(r, 2).fill, st.cell(r, 2).border = YEL, BOX
        st.row_dimensions[r].height = 30
        r += 1
    r += 1
    st.cell(r, 1, "Filled in by").font = BB
    st.cell(r, 2).fill, st.cell(r, 2).border = YEL, BOX
    st.cell(r + 1, 1, "Date").font = BB
    st.cell(r + 1, 2).fill, st.cell(r + 1, 2).border = YEL, BOX
    st.sheet_view.showGridLines = False
    for s in wb.worksheets:
        s.sheet_view.zoomScale = 90
    out = os.path.join(OUTDIR, f"EUForDam_DataRequest_{c}.xlsx")
    wb.save(out)
    print(c, os.path.basename(out), "insect rows:", ins.max_row - 4, "abiotic years:", years[0], "-", years[-1])
