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
# REW HOSTTREE values -> host keys; Klim host columns use the same keys (spruce, pine, oak, fir, coniferous, deciduous)
HOSTKEY = {"Picea abies": "spruce", "Pinus sylvestris": "pine", "Quercus spp.": "oak", "Abies alba": "fir",
           "Larix decidua": "larch", "Betula spp.": "birch", "Populus, Salix": "poplar_willow", "Populus, Salix ?": "poplar_willow",
           "Ulmus spp.": "elm", "Aesculus spp.": "aesculus"}
HOSTNAME = {"spruce": "Picea abies (Norway spruce)", "pine": "Pinus sylvestris (Scots pine)", "oak": "Quercus spp. (oaks)",
            "fir": "Abies alba (silver fir)", "larch": "Larix decidua (larch)", "birch": "Betula spp. (birch)",
            "poplar_willow": "Populus, Salix (poplar, willow)", "elm": "Ulmus spp. (elm)", "aesculus": "Aesculus spp. (horse chestnut)",
            "coniferous": "All conifers", "deciduous": "All broadleaves"}
HOST_ORDER = ["spruce", "pine", "fir", "larch", "oak", "birch", "poplar_willow", "elm", "aesculus", "coniferous", "deciduous"]
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
    "AT": ["Your abiotic total comes as several unlabelled rows per region and year. Can you label them by type (wind, snow, other)?"],
    "CZ": ["Your regional rows are NUTS 3 kraje filed under NUTS 2 codes. Can you report by NUTS 3 (kraj) code instead? If yes, add the NUTS 3 code in the Comment column.",
           "Is a wind series (separate from total abiotic) available?"],
    "DE": ["Can Rheinland-Pfalz (DEB) and Saarland (DEC) be reported separately?",
           "Where a state has no data, please leave the cell blank – do not enter 0."],
    "FI": ["Can Lapland (FI1D7) be included?"],
    "PL": ["Is a labelled wind / storm series available? We currently use 'processed broken and fallen trees'."],
    "SK": ["Which species are in your 'Other bark beetles' column?"],
    "SE": ["Years without outbreak: leave blank if there is no estimate; enter 0 only if damage was assessed and was zero."],
    "CH": ["The file you sent has 2023 bark beetle = 0. Please give the complete 2023 values here."],
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


EXPERT_LEVEL = {"AT": "NUTS 2", "CZ": "NUTS 2", "DE": "federal state (NUTS 1)", "FI": "NUTS 3", "PL": "RDLP forest region"}


def level_for(c):
    """Region level in plain words for the expert (no internal notes)."""
    return EXPERT_LEVEL[c] if c in SUBNAT else TARGET[c][0].split(" (")[0]


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




# =====================================================================================
# Every sheet is specific to one country and asks ONLY for what is missing.
# Nothing we already hold is shown (except the X marks in the species sheet, which the
# brief asks for). Rows come only from our data: the agents we hold for the country
# (EUForDam_REW), their host trees, and the country's own region list.
# =====================================================================================
nzc_all = d[d.v > 0].groupby("sp_code").Country.nunique()
SPECIES_ALL = sorted([k for k, v in AGENT.items() if v[1] != "group code" and nzc_all.get(k, 0) >= 3],
                     key=lambda k: (-nzc_all[k], AGENT[k][0]))


def header(ws, r, cols, widths=None):
    for j, h in enumerate(cols, 1):
        c_ = ws.cell(r, j, h)
        c_.font, c_.fill, c_.alignment, c_.border = H, HF, CEN, BOX
        if widths:
            ws.column_dimensions[get_column_letter(j)].width = widths[j - 1]
    ws.row_dimensions[r].height = 32


def title(ws, text, note, span=14):
    ws["A1"] = text
    ws["A1"].font = T
    ws["A2"] = note
    ws["A2"].font = B
    ws["A2"].alignment = Alignment(wrap_text=True, vertical="top")
    ws.merge_cells(start_row=2, start_column=1, end_row=2, end_column=span)
    ws.row_dimensions[2].height = 48


def num_dv(ws):
    v = DataValidation(type="decimal", operator="greaterThanOrEqual", formula1="0", allow_blank=True,
                       error="Please enter a number ≥ 0 (leave blank if no data).", showErrorMessage=True)
    ws.add_data_validation(v)
    return v


def block(ws, r0, fixed_cols, rows, years, dv):
    """Header at r0, then rows. rows: dicts with fixed (list), hidden (list), first (bool). All year cells are inputs."""
    nf, ny = len(fixed_cols), len(years)
    header(ws, r0, fixed_cols + [str(y) for y in years] + ["Comment"] + HIDDEN)
    r = r0 + 1
    for row in rows:
        top = Side(style="medium", color="305496") if row["first"] else thin
        for j, v in enumerate(row["fixed"], 1):
            c_ = ws.cell(r, j, v)
            c_.font = (BB if j == 1 else B) if row["first"] else Font(name=F, size=10, color="A6A6A6" if j == 1 else "000000")
            c_.alignment = WRAP
        for k in range(ny):
            c_ = ws.cell(r, nf + 1 + k)
            c_.fill, c_.alignment = YEL, CEN
            dv.add(c_.coordinate)
        ws.cell(r, nf + ny + 1).fill = YEL
        for j in range(1, nf + ny + 2):
            ws.cell(r, j).border = Border(left=thin, right=thin, bottom=thin, top=top)
        for j, v in enumerate(row["hidden"]):
            ws.cell(r, nf + ny + 2 + j, v).font = Font(name=F, size=8, color="808080")
        r += 1
    return r


def finish(ws, nf, ny, widths):
    for j, w in enumerate(widths, 1):
        ws.column_dimensions[get_column_letter(j)].width = w
    for k in range(ny):
        ws.column_dimensions[get_column_letter(nf + 1 + k)].width = 9
    ws.column_dimensions[get_column_letter(nf + ny + 1)].width = 40
    for j in range(len(HIDDEN)):
        ws.column_dimensions[get_column_letter(nf + ny + 2 + j)].hidden = True


for c in COUNTRY:
    sub = c in SUBNAT
    units = units_for(c)
    lvl = level_for(c)
    agents = agents_for(c)
    wb = Workbook()
    asks = []          # lines for the Start here sheet

    # ---------------- 1 Insect damage: 2023–2025 (has NUTS data) or 2000–2025 by NUTS (national only)
    ins = wb.active
    ins.title = "1 Insect damage"
    years = [2023, 2024, 2025] if sub else list(range(2000, 2026))
    title(ins, f"{COUNTRY[c]} – insect damage, {years[0]}–{years[-1]}",
          f"One block per agent and region ({lvl}). Two lines each: Volume (m3) and Area (ha) – fill whichever you have, or both; "
          "the first line is the unit we use for that agent.  BLANK = no data / not monitored.  0 = monitored, no damage.")
    rows = []
    for name, hst, un, code in agents:
        m = meta.loc[code]
        main = un if un in MEAS else ("ha" if str(m.SP_CATEGORY).startswith("Def") else "m3")
        for u, nm in units:
            for i, mu in enumerate([main, "ha" if main == "m3" else "m3"]):
                rows.append({"fixed": [name, TYPE_OF.get(m.SP_CATEGORY, ""), hst, MEAS[mu], u, nm],
                             "hidden": [code, m.SP_CATEGORY, m.SP_SUBCATEGORY, m.HOSTTREE, m.TREETYPE, main],
                             "first": i == 0})
    end = block(ins, 4, ["Agent", "Type", "Host tree", "Measure", "Unit code", "Region"], rows, years, num_dv(ins))
    finish(ins, 6, len(years), [32, 13, 14, 12, 10, 22])
    ins.freeze_panes = "G5"
    ins.auto_filter.ref = f"A4:{get_column_letter(6 + len(years) + 1)}{end - 1}"
    asks.append(("  Sheet 1 – Insect damage",
                 f"{len(agents)} agents × {len(units)} regions, {years[0]}–{years[-1]}."
                 + ("" if sub else f" By {lvl} – this replaces the national series.")))

    # ---------------- 2 Abiotic damage: one block per damage type, only the years we need
    ab = wb.create_sheet("2 Abiotic damage")
    title(ab, f"{COUNTRY[c]} – abiotic damage",
          "Wind = windthrow / storm damage only.  Total abiotic = all abiotic causes together (wind, snow, ice, drought, fire, other).  "
          "Two lines per region: Volume (m3) and Area (ha) – fill whichever you have, or both.  BLANK = no data, 0 = no damage.")
    dv = num_dv(ab)
    r = 4
    maxy, parts = 0, []
    for typ, code, (yrs, mode) in (("Wind (windthrow)", "ABIO_wind", abio_years(c, 0, 1, 3)),
                                   ("Total abiotic (wind, snow, ice, drought, fire …)", "ABIO_tot", abio_years(c, 4, 5, 7))):
        ab.cell(r, 1, f"{typ.split(' (')[0]} – {yrs[0]}–{yrs[-1]}").font = Font(name=F, size=12, bold=True)
        r += 1
        rows = [{"fixed": [typ, MEAS[mu], u, nm], "hidden": [code, "Abiotic", code, "", "", "m3"], "first": i == 0}
                for u, nm in units for i, mu in enumerate(("m3", "ha"))]
        r = block(ab, r, ["Damage type", "Measure", "Unit code", "Region"], rows, yrs, dv) + 1
        maxy = max(maxy, len(yrs))
        parts.append(f"{typ.split(' (')[0]} {yrs[0]}–{yrs[-1]}")
    finish(ab, 4, maxy, [30, 12, 10, 22])
    ab.freeze_panes = "E4"
    asks.append(("  Sheet 2 – Abiotic damage", " · ".join(parts) + f", by {lvl}."))

    # ---------------- 3 Host availability: host trees of THIS country's agents, only region × host we do not hold
    agent_hosts = {}
    for name, hst, un, code in agents:
        hk = HOSTKEY.get(str(meta.loc[code].HOSTTREE).strip())
        if hk:
            agent_hosts.setdefault(hk, []).append(name)
    host_units = units
    if c == "CZ" and HOST_UNITS.get("CZ"):
        host_units = HOST_UNITS["CZ"]          # CZ host data are by NUTS 3 kraj
    hrows = []
    for u, nm in host_units:
        for hk in HOST_ORDER:
            if hk not in agent_hosts:
                continue
            held = HOSTVAL.get((c, u, hk)) if sub else None
            if held and any(x is not None for x in held):
                continue                         # already held → not asked
            hrows.append((u, nm, hk))
    if hrows:
        hs = wb.create_sheet("3 Host availability")
        title(hs, f"{COUNTRY[c]} – host availability",
              "Growing stock (m3) and forest area (ha) of the host trees of your agents, per region.  Give the most recent inventory "
              "(Inventory A) and, if available, an earlier one (Inventory B, e.g. around 2000–2010) so we can follow change over 2000–2025.  "
              "Area = area where the species is present or dominant – say which in Comment.")
        hcols = ["Region code", "Region", "Host tree", "Host of",
                 "A: growing stock (m3)", "A: area (ha)", "A: inventory year",
                 "B: growing stock (m3)", "B: area (ha)", "B: inventory year", "Source", "Comment", "host_key"]
        header(hs, 4, hcols, [10, 22, 26, 34, 14, 12, 11, 14, 12, 11, 20, 30, 10])
        hs.column_dimensions["M"].hidden = True
        dv_n = num_dv(hs)
        dv_y = DataValidation(type="whole", operator="between", formula1="1990", formula2="2026", allow_blank=True,
                              error="Enter a year, e.g. 2021.", showErrorMessage=True)
        hs.add_data_validation(dv_y)
        r, prev = 5, None
        for u, nm, hk in hrows:
            first = u != prev
            prev = u
            vals = [u, nm, HOSTNAME[hk], ", ".join(agent_hosts[hk])]
            for j, v in enumerate(vals, 1):
                c_ = hs.cell(r, j, v)
                c_.font = (BB if first else Font(name=F, size=10, color="A6A6A6")) if j <= 2 else B
                c_.alignment = WRAP
            hs.cell(r, 4).font = Font(name=F, size=8, color="595959")
            for j in range(5, 13):
                c_ = hs.cell(r, j)
                c_.fill = OPT if j in (8, 9, 10) else YEL
                if j in (5, 6, 8, 9):
                    dv_n.add(c_.coordinate)
                    c_.number_format = "#,##0"
                elif j in (7, 10):
                    dv_y.add(c_.coordinate)
            top = Side(style="medium", color="305496") if first else thin
            for j in range(1, 13):
                hs.cell(r, j).border = Border(left=thin, right=thin, bottom=thin, top=top)
            hs.cell(r, 13, hk).font = Font(name=F, size=8, color="808080")
            r += 1
        hs.freeze_panes = "E5"
        hs.auto_filter.ref = f"A4:L{r - 1}"
        nh = len({hk for _, _, hk in hrows})
        asks.append(("  Sheet 3 – Host availability",
                     f"Growing stock and area of {nh} host tree(s) of your agents, {len({u for u, _, _ in hrows})} regions."))

    # ---------------- 4 Species presence: species whose host tree occurs in this country's data
    sp_n = len(wb.sheetnames) + 1
    sp = wb.create_sheet(f"{sp_n} Species presence")
    g = d[d.Country == c].copy()
    if c == "SK":
        g = g[g.sp_code != "ips_dup"]          # mislabelled 'Other bark beetles'
    if c == "CZ":
        g.loc[g.sp_code == "CambXyl_con&BB_con&SM", "sp_code"] = "ips_typ"
    pos = g[g.v > 0].groupby("sp_code").NUTS.agg(set).to_dict()
    species = [k for k in SPECIES_ALL if HOSTKEY.get(str(meta.loc[k].HOSTTREE).strip()) in agent_hosts]
    title(sp, f"{COUNTRY[c]} – which species occur where?",
          "X = you already reported damage by this species there.  Other cells: how confident are you that the species occurred in that "
          "region in 2000–2025, even without recorded damage?  0 = absent · 1 = low · 2 = medium · 3 = high.  Blank = do not know.  "
          "For a recent arrival, give the year it was first recorded.", span=min(4 + len(units), 14))
    fixed = ["Species", "Host tree"] + ([] if sub else ["Reported nationally"])
    nf = len(fixed)
    for j, h_ in enumerate(fixed, 1):
        sp.cell(4, j, h_)
        sp.merge_cells(start_row=4, start_column=j, end_row=5, end_column=j)
    for k, (u, nm) in enumerate(units):
        sp.cell(4, nf + 1 + k, u)
        sp.cell(5, nf + 1 + k, nm)
    fr, cc = nf + len(units) + 1, nf + len(units) + 2
    sp.cell(4, fr, "First recorded (year), if recent")
    sp.cell(4, cc, "Comment")
    for j in (fr, cc):
        sp.merge_cells(start_row=4, start_column=j, end_row=5, end_column=j)
    for rr in (4, 5):
        for j in range(1, cc + 1):
            c_ = sp.cell(rr, j)
            c_.font, c_.fill, c_.alignment, c_.border = H, HF, CEN, BOX
    sp.row_dimensions[5].height = 45
    dv_c = DataValidation(type="list", formula1='"0,1,2,3"', allow_blank=True, error="Enter 0, 1, 2 or 3 (or leave blank).",
                          showErrorMessage=True)
    dv_yr = DataValidation(type="whole", operator="between", formula1="1800", formula2="2026", allow_blank=True,
                           error="Enter a year (e.g. 2017) or leave blank.", showErrorMessage=True)
    sp.add_data_validation(dv_c)
    sp.add_data_validation(dv_yr)
    r = 6
    for code in species:
        sp.cell(r, 1, AGENT[code][0]).font = Font(name=F, size=10, italic=True)
        sp.cell(r, 2, meta.loc[code].HOSTTREE).font = B
        if not sub:
            c_ = sp.cell(r, 3)
            c_.alignment, c_.font = CEN, BB
            if code in pos:
                c_.value, c_.fill = "X", GRY
        for k, (u, _) in enumerate(units):
            c_ = sp.cell(r, nf + 1 + k)
            c_.alignment, c_.font = CEN, BB
            if sub and u in pos.get(code, set()):
                c_.value, c_.fill = "X", GRY
            else:
                c_.fill = YEL
                dv_c.add(c_.coordinate)
        for j in (fr, cc):
            sp.cell(r, j).fill = YEL
        sp.cell(r, fr).alignment = CEN
        dv_yr.add(sp.cell(r, fr).coordinate)
        for j in range(1, cc + 1):
            sp.cell(r, j).border = BOX
        r += 1
    sp.column_dimensions["A"].width = 30
    sp.column_dimensions["B"].width = 16
    if not sub:
        sp.column_dimensions["C"].width = 11
    for k in range(len(units)):
        sp.column_dimensions[get_column_letter(nf + 1 + k)].width = 14
    sp.column_dimensions[get_column_letter(fr)].width = 13
    sp.column_dimensions[get_column_letter(cc)].width = 35
    sp.freeze_panes = sp.cell(6, nf + 1)
    asks.append((f"  Sheet {sp_n} – Species presence",
                 f"{len(species)} species (hosts of your agents) × {len(units)} regions: confidence 0–3 where not already marked X."))

    # ---------------- Start here
    st = wb.create_sheet("Start here", 0)
    st.column_dimensions["A"].width = 26
    st.column_dimensions["B"].width = 95
    lines = [(f"EUForDam – data request: {COUNTRY[c]}", None, T), ("", None, B), ("What we ask", "", BB)] + \
            [(a_, b_, None) for a_, b_ in asks] + [
        ("", None, B),
        ("How to fill in", "", BB),
        ("  Yellow cells", "Please fill (light yellow = optional).", None),
        ("  Blank vs 0", "Leave BLANK if there is no data or it was not monitored. Enter 0 ONLY if it was monitored and there was no damage.", None),
        ("  Volume and area", "Damage sheets have two lines per agent/region – Volume (m3) and Area (ha). Fill whichever you have, or both.", None),
        ("  Regions", f"Rows use {lvl} codes.", None),
        ("", None, B)]
    q = ([] if sub else ["Can you provide district-level (NUTS) data for 2000–2025 – damage values and host availability? "
                         "(yes / partly / no – please say which agents and years)"]) + QUESTIONS.get(c, [])
    if q:
        lines.append(("Questions for you", "Please answer in the yellow cell under each question.", BB))
    r = 1
    for a_, b_, f_ in lines:
        st.cell(r, 1, a_).font = f_ or BB
        if b_ is not None:
            st.cell(r, 2, b_).font = B
            st.cell(r, 2).alignment = WRAP
        r += 1
    for qq in q:
        st.cell(r, 1, "Q").font = BB
        st.cell(r, 2, qq).font = B
        st.cell(r, 2).alignment = WRAP
        r += 1
        st.cell(r, 1, "  Answer").font = B
        st.cell(r, 2).fill, st.cell(r, 2).border = YEL, BOX
        st.row_dimensions[r].height = 30
        r += 1
    r += 1
    for lab in ("Filled in by", "Date"):
        st.cell(r, 1, lab).font = BB
        st.cell(r, 2).fill, st.cell(r, 2).border = YEL, BOX
        r += 1
    st.sheet_view.showGridLines = False
    for s_ in wb.worksheets:
        s_.sheet_view.zoomScale = 90
    out = os.path.join(OUTDIR, f"EUForDam_DataRequest_{c}.xlsx")
    wb.save(out)
    print(c, " | ".join(f"{n}" for n in wb.sheetnames[1:]), "| insect rows", len(agents) * len(units) * 2,
          "| host rows", len(hrows), "| species", len(species))
