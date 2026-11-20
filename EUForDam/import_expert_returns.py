"""Read filled-in EUForDam_DataRequest_XX.xlsx files and convert them to EUForDam_REW long format.

Usage: python import_expert_returns.py <out.xlsx> <returned_file.xlsx> [more files ...]

Output sheets
  Insects_REW   – one row per agent × region × year, EUForDam_REW columns
                  (value1 = value in the agent's main unit, value2 = the other measure)
                  plus explicit volume_m3 / area_ha columns and the expert's comment.
  Abiotic       – one row per damage type × region × year (volume_m3, area_ha).
  Species       – one row per species × region: years already reported (grey cells) or the expert's 0–3 confidence,
                  plus 'first recorded (year)' and comment.
  Answers       – the answers to the 'Questions for you' block and who filled the file.
NOTE: values are stored exactly as the expert typed them. The country-specific conversion
factors used in EUForDam (e.g. FI ha × 122) are NOT applied here.
"""
import os
import re
import sys

import pandas as pd
from openpyxl import load_workbook

OUT, FILES = sys.argv[1], sys.argv[2:]
MEAS = {"Volume (m3)": "m3", "Area (ha)": "ha"}


def read_grid(ws):
    head = [c.value for c in ws[4]]
    years = [(j, int(h)) for j, h in enumerate(head) if isinstance(h, str) and re.fullmatch(r"\d{4}", h)]
    cidx = head.index("Comment")
    recs = []
    for row in ws.iter_rows(min_row=5, values_only=True):
        if all(v in (None, "") for v in row):
            continue
        base = dict(zip(head[: years[0][0]], row[: years[0][0]]))
        hidden = dict(zip(head[cidx + 1:], row[cidx + 1:]))
        for j, y in years:
            v = row[j]
            if v in (None, "", "held"):
                continue
            try:
                v = float(v)
            except (TypeError, ValueError):
                base.setdefault("_bad", []).append(f"{y}: {v!r}")
                continue
            recs.append({**base, **hidden, "year": y, "value": v, "comment": row[cidx]})
    return pd.DataFrame(recs)


ins_all, ab_all, sp_all, ans_all, problems = [], [], [], [], []
for f in FILES:
    m = re.search(r"DataRequest_([A-Z]{2})", os.path.basename(f))
    country = m.group(1) if m else "??"
    wb = load_workbook(f, data_only=True)

    # ---- insects
    g = read_grid(wb["1 Insect damage"])
    if len(g):
        g["Country"] = country
        g["measure"] = g["Measure"].map(MEAS)
        new = g["sp_code"].eq("NEW")
        g.loc[new, "sp_code"] = "NEW: " + g.loc[new, "Agent"].fillna("?").astype(str)
        g.loc[new, "HOSTTREE"] = g.loc[new, "Host tree"]
        g.loc[new, "SP_CATEGORY"] = "NEW: " + g.loc[new, "Type"].fillna("?").astype(str)
        if g["measure"].isna().any():
            problems.append(f"{country}: {g['measure'].isna().sum()} insect values without a valid Measure")
        g["sp_unit"] = g["sp_unit"].where(g["sp_unit"].isin(["m3", "ha"]), None)
        g.loc[g["sp_unit"].isna(), "sp_unit"] = g.loc[g["sp_unit"].isna(), "SP_CATEGORY"].astype(str).str.contains("Def").map({True: "ha", False: "m3"})
        keys = ["Country", "Unit code", "Region", "year", "sp_code", "Agent", "SP_CATEGORY", "SP_SUBCATEGORY", "HOSTTREE", "TREETYPE", "sp_unit"]
        g[keys] = g[keys].fillna("")
        p = g.pivot_table(index=keys, columns="measure", values="value", aggfunc="first").reset_index()
        cm = g.groupby(keys)["comment"].agg(lambda s: "; ".join(sorted({str(x) for x in s if isinstance(x, str) and x.strip()})))
        p = p.merge(cm.reset_index(), on=keys, how="left")
        for col in ("m3", "ha"):
            if col not in p:
                p[col] = None
        p["value1"] = p.apply(lambda r: r["ha"] if r["sp_unit"] == "ha" else r["m3"], axis=1)
        p["value2"] = p.apply(lambda r: r["m3"] if r["sp_unit"] == "ha" else r["ha"], axis=1)
        p = p.rename(columns={"Unit code": "NUTS", "year": "d_year", "m3": "volume_m3", "ha": "area_ha"})
        ins_all.append(p[["SP_CATEGORY", "SP_SUBCATEGORY", "HOSTTREE", "TREETYPE", "d_year", "Country", "Region", "NUTS",
                          "sp_code", "sp_unit", "value1", "value2", "Agent", "volume_m3", "area_ha", "comment"]])

    # ---- abiotic
    a = read_grid(wb["2 Abiotic damage"])
    if len(a):
        a["Country"] = country
        a["measure"] = a["Measure"].map(MEAS)
        keys = ["Country", "Unit code", "Region", "year", "sp_code", "Damage type"]
        p = a.pivot_table(index=keys, columns="measure", values="value", aggfunc="first").reset_index()
        for col in ("m3", "ha"):
            if col not in p:
                p[col] = None
        ab_all.append(p.rename(columns={"Unit code": "NUTS", "year": "d_year", "sp_code": "abio_code",
                                        "m3": "volume_m3", "ha": "area_ha"}))

    # ---- species presence
    # grey cells hold the years already reported ("2003–2022" / "2015"); yellow cells hold 0–3 confidence
    ws = wb["3 Species presence"]
    codes = [c.value for c in ws[4]]
    names = [c.value for c in ws[5]]
    nat = codes[2] is not None and str(codes[2]).startswith("Reported nationally")
    first_unit = 3 if nat else 2
    fr = next(j for j, v in enumerate(codes) if v and str(v).startswith("First recorded"))
    cm = codes.index("Comment")

    def parse_years(v):
        m = re.fullmatch(r"\s*(\d{4})\s*(?:[–-]\s*(\d{4}))?\s*", str(v))
        return (int(m.group(1)), int(m.group(2) or m.group(1))) if m else None
    for row in ws.iter_rows(min_row=6, values_only=True):
        if not row[0] or str(row[0]).startswith("Other species"):
            continue
        common = {"Country": country, "Species": row[0], "Host": row[1], "first_recorded": row[fr], "comment": row[cm]}
        cols = ([(2, country, "National")] if nat else []) + [(j, codes[j], names[j]) for j in range(first_unit, fr)]
        for j, code_, nm in cols:
            v = row[j]
            if v in (None, ""):
                continue
            yrs = parse_years(v)
            if yrs:
                sp_all.append({**common, "NUTS": code_, "Region": nm, "source": "reported in EUForDam",
                               "reported_from": yrs[0], "reported_to": yrs[1], "confidence": None})
            else:
                sp_all.append({**common, "NUTS": code_, "Region": nm, "source": "expert",
                               "reported_from": None, "reported_to": None, "confidence": v})
        if row[fr] not in (None, "") and not any(r_["Species"] == row[0] and r_["Country"] == country for r_ in sp_all):
            sp_all.append({**common, "NUTS": "", "Region": "", "source": "expert", "reported_from": None,
                           "reported_to": None, "confidence": None})

    # ---- answers
    st = wb["Start here"]
    last_q = None
    for r in range(1, st.max_row + 1):
        a_, b_ = st.cell(r, 1).value, st.cell(r, 2).value
        if a_ == "Q":
            last_q = b_
        elif a_ and str(a_).strip() == "Answer":
            ans_all.append({"Country": country, "Question": last_q, "Answer": b_})
        elif a_ in ("Filled in by", "Date"):
            ans_all.append({"Country": country, "Question": a_, "Answer": b_})

with pd.ExcelWriter(OUT) as xw:
    for name, parts in (("Insects_REW", ins_all), ("Abiotic", ab_all), ("Species", sp_all), ("Answers", ans_all)):
        df = pd.concat(parts, ignore_index=True) if parts and isinstance(parts[0], pd.DataFrame) else pd.DataFrame(parts)
        df.to_excel(xw, sheet_name=name, index=False)
    pd.DataFrame({"problem": problems or ["none"]}).to_excel(xw, sheet_name="Problems", index=False)
print("written", OUT, "| problems:", problems or "none")
