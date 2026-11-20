"""Build one expert-facing data-request workbook per country.

Usage: python expert_build.py <build_inventory.py> <EUForDam_REW.xlsx> <old_inventory.xlsx> <out_dir>
Reuses the data definitions in build_inventory.py (everything above its 'styles' section).
"""
import os
import sys

from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.datavalidation import DataValidation

BUILD, REW, OLD, OUTDIR = sys.argv[1:5]
src = open(BUILD, encoding="utf-8").read()
src = src[: src.index("# ---------------------------------------------------------------- styles")]
sys.argv = [BUILD, REW, OLD, "unused.xlsx"]
ns = {}
exec(compile(src, BUILD, "exec"), ns)
d, AGENT, COUNTRY, TARGET, SUBNAT, ABIO = ns["d"], ns["AGENT"], ns["COUNTRY"], ns["TARGET"], ns["SUBNAT"], ns["ABIO"]
UNIT_NAMES, host, unit_of, LEVEL = ns["UNIT_NAMES"], ns["host"], ns["unit_of"], ns["LEVEL"]
os.makedirs(OUTDIR, exist_ok=True)

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
    "PL": ["Can the data be given by NUTS 3 instead of RDLP regions? (optional)",
           "Is a labelled wind / storm series available? We currently use 'processed broken and fallen trees'.",
           "Do data for 2000–2002 exist? (optional)"],
    "SK": ["Which species are in your 'Other bark beetles' column?"],
    "SE": ["Years without outbreak: leave blank if there is no estimate; enter 0 only if damage was assessed and was zero."],
    "CH": ["The file you sent has 2023 bark beetle = 0. Please give the complete 2023 values here."],
    "IT": ["The data cover Friuli Venezia Giulia only. Are data for other regions available? (optional)"],
    "HU": ["Your values are in ha. If volumes (m3) are available, please give m3 and set the Unit column."],
    "LT": ["Your data cover state forests. Please say in the Comment column if values are scaled to all forests."],
}

F = "Arial"
H = Font(name=F, bold=True, color="FFFFFF")
HF = PatternFill("solid", fgColor="305496")
B = Font(name=F, size=10)
BB = Font(name=F, size=10, bold=True)
T = Font(name=F, size=14, bold=True)
YEL = PatternFill("solid", fgColor="FFF2CC")
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


def header(ws, r, cols, widths):
    for j, (h, w) in enumerate(zip(cols, widths), 1):
        c = ws.cell(r, j, h)
        c.font, c.fill, c.alignment, c.border = H, HF, CEN, BOX
        ws.column_dimensions[get_column_letter(j)].width = w
    ws.row_dimensions[r].height = 32


def grid_sheet(ws, title, note, rows, years, req_years, fixed_cols, fixed_w):
    """rows: list of fixed-column tuples. Years not in req_years are greyed 'held'."""
    ws["A1"] = title
    ws["A1"].font = T
    ws["A2"] = note
    ws["A2"].font = B
    ws["A2"].alignment = Alignment(wrap_text=True, vertical="top")
    ws.merge_cells(start_row=2, start_column=1, end_row=2, end_column=min(len(fixed_cols) + len(years) + 1, 14))
    ws.row_dimensions[2].height = 58
    cols = fixed_cols + [str(y) for y in years] + ["Comment"]
    header(ws, 4, cols, fixed_w + [9] * len(years) + [40])
    dv_num = DataValidation(type="decimal", operator="greaterThanOrEqual", formula1="0", allow_blank=True,
                            error="Please enter a number ≥ 0 (leave blank if no data).", showErrorMessage=True)
    dv_unit = DataValidation(type="list", formula1='"m3,ha,other (see comment)"', allow_blank=True)
    ws.add_data_validation(dv_num)
    ws.add_data_validation(dv_unit)
    nf = len(fixed_cols)
    unit_col = fixed_cols.index("Unit") + 1
    r = 5
    prev = None
    for row in rows:
        for j, v in enumerate(row, 1):
            c = ws.cell(r, j, v)
            c.font, c.border, c.alignment = B, BOX, WRAP
        if row[0] == prev:  # repeat label in grey to keep rows readable but quiet
            ws.cell(r, 1).font = Font(name=F, size=10, color="A6A6A6")
        else:
            ws.cell(r, 1).font = BB if row[0] else B
        prev = row[0]
        ucell = ws.cell(r, unit_col)
        ucell.fill = YEL
        dv_unit.add(ucell.coordinate)
        for k, y in enumerate(years):
            cell = ws.cell(r, nf + 1 + k)
            cell.border = BOX
            cell.alignment = CEN
            if y in req_years:
                cell.fill = YEL
                dv_num.add(cell.coordinate)
            else:
                cell.fill, cell.value = GRY, "held"
                cell.font = Font(name=F, size=8, color="808080")
        cm = ws.cell(r, nf + len(years) + 1)
        cm.fill, cm.border = YEL, BOX
        r += 1
    ws.freeze_panes = ws.cell(5, nf + 1)
    ws.auto_filter.ref = f"A4:{get_column_letter(nf + len(years) + 1)}{r - 1}"
    return r


for c in COUNTRY:
    sub = c in SUBNAT
    units = units_for(c)
    lvl = level_for(c)
    agents = agents_for(c)
    yrs_held = d[(d.Country == c) & d.v.notna()].d_year
    wb = Workbook()

    # ---------------- Insect damage
    ins = wb.active
    ins.title = "1 Insect damage"
    if sub:
        years, req = [2023, 2024, 2025], {2023, 2024, 2025}
        note = (f"We hold your data by {lvl} up to 2022. Please add 2023, 2024 and 2025 for each agent and unit below "
                "(yellow cells). Same definition and unit as your earlier data.")
    else:
        years = list(range(2000, 2026))
        req = set(years)
        note = (f"We hold national totals only. Please give the full series 2000–2025 by {lvl} for each agent below. "
                "If a series cannot be split by unit, enter the national total in the row 'National total' and explain in Comment.")
    note += "  BLANK = no data / not monitored.  0 = monitored, no damage.  Volume (m3) preferred; set the Unit column."
    rows = []
    for name, hst, un, code in agents:
        unit_list = units if sub else units + [(c, "National total (only if no split possible)")]
        for u, nm in unit_list:
            rows.append((name, hst, un, u, nm))
    for _ in range(6):
        rows.append(("", "", "", "", ""))
    end = grid_sheet(ins, f"{COUNTRY[c]} – insect damage", note, rows, years, req,
                     ["Agent", "Host tree", "Unit", "Unit code", "Region"], [34, 14, 10, 10, 24])
    ins.cell(end - 6, 1).value = "Other agent (optional): type name →"
    ins.cell(end - 6, 1).font = Font(name=F, size=10, italic=True)
    for r in range(end - 6, end):
        for j in (1, 2, 4, 5):
            ins.cell(r, j).fill = YEL

    # ---------------- Abiotic damage
    ab = wb.create_sheet("2 Abiotic damage")
    wy, wmode = abio_years(c, 0, 1, 3)
    ty, tmode = abio_years(c, 4, 5, 7)
    years = sorted(set(wy) | set(ty)) if (wmode == "full" or tmode == "full") else sorted(set(wy) | set(ty))
    years = list(range(min(years), 2026))
    a_units_w = units_for(c) if (wmode == "update" or not sub) else units_for(c)
    if not sub:
        a_units_w = units + [(c, "National total (only if no split possible)")]
    rows, reqmap = [], []
    for typ, mode, ry in (("Wind (windthrow)", wmode, set(wy)), ("Total abiotic (wind, snow, ice, drought, fire …)", tmode, set(ty))):
        for u, nm in a_units_w:
            rows.append((typ, "m3", u, nm))
            reqmap.append(ry)
    note = ("Wind = windthrow / storm damage only.  Total abiotic = all abiotic causes together (wind, snow, ice, drought, fire, other). "
            "Yellow = please fill, grey = we already hold it.  BLANK = no data, 0 = no damage.  Volume (m3) preferred; set the Unit column.")
    ab["A1"] = f"{COUNTRY[c]} – abiotic damage"
    # grid_sheet uses one req set; build per-row by temporarily greying
    end = grid_sheet(ab, f"{COUNTRY[c]} – abiotic damage", note, rows, years, set(years),
                     ["Damage type", "Unit", "Unit code", "Region"], [34, 10, 10, 24])
    dv_list = ab.data_validations.dataValidation
    num_dv = [v for v in dv_list if v.type == "decimal"][0]
    keep = []
    for i, ry in enumerate(reqmap):
        r = 5 + i
        for k, y in enumerate(years):
            cell = ab.cell(r, 5 + k)
            if y not in ry:
                cell.fill, cell.value = GRY, "held"
                cell.font = Font(name=F, size=8, color="808080")
            else:
                keep.append(cell.coordinate)
    num_dv.sqref = " ".join(keep) if keep else None
    if num_dv.sqref is None:
        ab.data_validations.dataValidation.remove(num_dv)
    else:
        from openpyxl.worksheet.cell_range import MultiCellRange
        num_dv.sqref = MultiCellRange(" ".join(keep))

    # ---------------- Species presence (copied logic from inventory)
    sp = wb.create_sheet("3 Species presence")
    g = d[d.Country == c].copy()
    if c == "SK":
        g = g[g.sp_code != "ips_dup"]
    if c == "CZ":
        g.loc[g.sp_code == "CambXyl_con&BB_con&SM", "sp_code"] = "ips_typ"
    pos = g[g.v > 0].groupby("sp_code").NUTS.agg(set).to_dict()
    exist = g.groupby("sp_code").NUTS.agg(set).to_dict()
    nzc = d[d.v > 0].groupby("sp_code").Country.nunique()
    SPECIES = sorted([k for k, v in AGENT.items() if v[1] != "group code" and nzc.get(k, 0) >= 3],
                     key=lambda k: (-nzc[k], AGENT[k][0]))
    sp_units = units + ([("FI1D7", "Lappi"), ("FI200", "Åland")] if c == "FI" else [])
    sp["A1"] = f"{COUNTRY[c]} – which species occur where?"
    sp["A1"].font = T
    sp["A2"] = ("X = already reported by you (damage > 0).  For every other cell: how confident are you that the species OCCURS "
                "in that region (even if no damage was recorded)?  0 = absent · 1 = low (possible) · 2 = medium (likely) · "
                "3 = high (confirmed).  Leave blank if you do not know.")
    sp["A2"].font = B
    sp["A2"].alignment = Alignment(wrap_text=True, vertical="top")
    sp.merge_cells(start_row=2, start_column=1, end_row=2, end_column=min(3 + len(sp_units), 14))
    sp.row_dimensions[2].height = 45
    fixed = ["Species", "Host tree"] + ([] if sub else ["Reported nationally"])
    nf = len(fixed)
    for j, hname in enumerate(fixed, 1):
        sp.cell(4, j, hname)
        sp.merge_cells(start_row=4, start_column=j, end_row=5, end_column=j)
    for k, (u, nm) in enumerate(sp_units):
        sp.cell(4, nf + 1 + k, u)
        sp.cell(5, nf + 1 + k, nm)
    cc = nf + len(sp_units) + 1
    sp.cell(4, cc, "Comment")
    sp.merge_cells(start_row=4, start_column=cc, end_row=5, end_column=cc)
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
                cell.value, cell.fill = "X", GRY
        for k, (u, _) in enumerate(sp_units):
            cell = sp.cell(r, nf + 1 + k)
            cell.alignment, cell.font = CEN, BB
            if sub and u in pos.get(code, set()):
                cell.value, cell.fill = "X", GRY
            else:
                cell.fill = YEL
                dv.add(cell.coordinate)
        sp.cell(r, cc).fill = YEL
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
        sp.column_dimensions["C"].width = 11
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

    def ask_ab(mode, yrs):
        return f"update {yrs[0]}–2025" if mode == "update" else "full series 2000–2025 by region"
    lines += [
        ("What we already have", have, None),
        ("What we ask", "", BB),
        ("  Sheet 1 – Insect damage", ask_ins, None),
        ("  Sheet 2 – Abiotic damage", f"Wind: {ask_ab(wmode, wy)}.  Total abiotic: {ask_ab(tmode, ty)}.", None),
        ("  Sheet 3 – Species presence", "Mark how confident you are that each species occurs in each region (0–3).", None),
        ("", None, B),
        ("How to fill in", "", BB),
        ("  Yellow cells", "Please fill. Grey cells = we already have this, no action needed.", None),
        ("  Blank vs 0", "Leave BLANK if there is no data or the agent was not monitored. Enter 0 ONLY if it was monitored and there was no damage.", None),
        ("  Units", "Volume (m3) preferred. If you report area (ha) or something else, change the Unit column in that row.", None),
        ("  Regions", f"Rows use {lvl} codes. If your data use other units, add them in the Comment column.", None),
        ("  Other agents", "Empty rows at the bottom of Sheet 1 – add any important agent we are missing.", None),
        ("", None, B),
    ]
    q = QUESTIONS.get(c, [])
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
