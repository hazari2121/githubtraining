"""Correlation metrics for the EU Forest Damage Database (EU-ForDAM).

Usage:
    python correlation_analysis.py path/to/EUForDam_REW3.xlsx [output_dir]

Follows the preprocessing of Hlasny et al. (2025, Global Change Biology,
doi:10.1111/gcb.70580) where possible:
  * values are log(x + 1) transformed,
  * a time series (region x group) with fewer than 6 non-zero values is
    dropped,
  * the 18 species groups of their Table 1 (3 hierarchical levels) are taken
    from the indicator columns of the spreadsheet.

Zeros are monitored zero damage; a missing value1 means "not recorded".
Rows sharing NUTS x year x agent are summed (this affects only CZ, where the
14 regions (kraje) were mapped to the 8 NUTS2 codes).
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
from scipy import stats
from scipy.cluster.hierarchy import leaves_list, linkage
from scipy.spatial.distance import squareform

MIN_N = 30          # min. overlapping region-years for a pairwise correlation
MIN_NONZERO = 6     # min. non-zero values per series (Hlasny et al. 2025)
MIN_YEARS = 10      # min. years per region for spatial synchrony
KEY = ["Country", "NUTS", "d_year"]

# Hierarchical levels of Hlasny et al. (2025), Table 1. Keys are the
# indicator columns of the spreadsheet (whitespace-stripped).
GROUPS = {
    "Bark borers on conifers": (1, "Borers on gymnosperms"),
    "Bark borers on decidous": (1, "Borers on angiosperms"),
    "Defoliators on conifers": (1, "Defoliators on gymnosperms"),
    "Defoliators on decidous": (1, "Defoliators on angiosperms"),
    "Bark borers on fir": (2, "Borers on Abies alba"),
    "Bark borers on spruce": (2, "Borers on Picea abies"),
    "Bark borers on pine": (2, "Borers on Pinus sylvestris"),
    "Defoliators on spruce": (2, "Defoliators on Picea abies"),
    "Defoliators on pine": (2, "Defoliators on Pinus sylvestris"),
    "Ips typographus": (3, "Ips typographus"),
    "Phaenops cyanea": (3, "Phaenops cyanea"),
    "Tomicus minor and Tomicus piniperda": (3, "Tomicus spp."),
    "Ips acuminatus": (3, "Ips acuminatus"),
    "Pityogenes chalcographus": (3, "Pityogenes chalcographus"),
    "Tortrix viridana": (3, "Tortrix viridana"),
    "Geometridae on oaks": (3, "Geometridae on oaks"),
    "Lymantria dispar": (3, "Lymantria dispar"),
    "Lymantria monacha": (3, "Lymantria monacha"),
}

# Pearson r from Hlasny et al. (2025), Tables 4-6, for side-by-side comparison
PAPER_R = {
    ("Borers on gymnosperms", "Borers on angiosperms"): 0.21,
    ("Borers on gymnosperms", "Defoliators on gymnosperms"): 0.00,
    ("Borers on gymnosperms", "Defoliators on angiosperms"): 0.00,
    ("Borers on angiosperms", "Defoliators on gymnosperms"): 0.02,
    ("Borers on angiosperms", "Defoliators on angiosperms"): 0.04,
    ("Defoliators on gymnosperms", "Defoliators on angiosperms"): 0.20,
    ("Borers on Abies alba", "Borers on Picea abies"): 0.63,
    ("Borers on Abies alba", "Borers on Pinus sylvestris"): 0.73,
    ("Borers on Abies alba", "Defoliators on Picea abies"): 0.04,
    ("Borers on Abies alba", "Defoliators on Pinus sylvestris"): 0.03,
    ("Borers on Picea abies", "Borers on Pinus sylvestris"): 0.88,
    ("Borers on Picea abies", "Defoliators on Picea abies"): 0.00,
    ("Borers on Picea abies", "Defoliators on Pinus sylvestris"): 0.04,
    ("Borers on Pinus sylvestris", "Defoliators on Picea abies"): 0.00,
    ("Borers on Pinus sylvestris", "Defoliators on Pinus sylvestris"): 0.03,
    ("Defoliators on Picea abies", "Defoliators on Pinus sylvestris"): 0.02,
}
_SP = ["Ips typographus", "Phaenops cyanea", "Tomicus spp.", "Ips acuminatus",
       "Pityogenes chalcographus", "Tortrix viridana", "Geometridae on oaks",
       "Lymantria dispar", "Lymantria monacha"]
_T6 = [[0.73], [0.01, 0.15], [0.68, 0.36, 0.01], [0.34, 0.23, 0.20, 0.28],
       [0.07, 0.00, 0.44, 0.17, 0.01], [0.02, 0.00, 0.38, 0.07, 0.15, 0.19],
       [0.03, 0.00, 0.34, 0.06, 0.06, 0.31, 0.61],
       [0.01, 0.01, 0.20, 0.00, 0.39, 0.02, 0.56, 0.20]]
for _i, _row in enumerate(_T6, start=1):
    for _j, _v in enumerate(_row):
        PAPER_R[(_SP[_j], _SP[_i])] = _v

DIVERGING = LinearSegmentedColormap.from_list(
    "bluegrayred", ["#1f5fae", "#2a78d6", "#f0efec", "#e0533f", "#b8321f"])


# ----------------------------------------------------------------- loading
def load(path):
    d = pd.read_excel(path, sheet_name="EUForDam_REW")
    d.columns = [c.strip() for c in d.columns]
    d.attrs["n_raw"], d.attrs["n_missing"] = len(d), int(d["value1"].isna().sum())
    d = d[d["value1"].notna()].copy()
    d["agent"] = d["sp_code"] + " | " + d["HOSTTREE"] + " [" + d["sp_unit"] + "]"
    d["guild"] = np.select(
        [d[["Bark borers on conifers", "Bark borers on decidous"]].eq(1).any(axis=1),
         d[["Defoliators on conifers", "Defoliators on decidous"]].eq(1).any(axis=1)],
        ["borer", "defoliator"], "unassigned")
    return d


def agent_meta(d):
    return (d.groupby("agent")
              .agg(sp_code=("sp_code", "first"), host=("HOSTTREE", "first"),
                   treetype=("TREETYPE", "first"), guild=("guild", "first"),
                   sp_category=("SP_CATEGORY", lambda s: ", ".join(sorted(s.unique()))),
                   unit=("sp_unit", "first"))
              .reset_index())


def agent_long(d):
    return d.groupby(KEY + ["agent"], as_index=False)["value1"].sum()


def group_long(d):
    parts = []
    for col, (level, name) in GROUPS.items():
        g = d[d[col] == 1].groupby(KEY, as_index=False)["value1"].sum()
        parts.append(g.assign(series=name, level=level))
    return pd.concat(parts, ignore_index=True)


def drop_sparse(long, col):
    """Drop region x series time series with < MIN_NONZERO non-zero values."""
    nz = long.groupby(["NUTS", col]).value1.transform(lambda v: (v > 0).sum())
    return long[nz >= MIN_NONZERO]


def wide(long, col):
    return long.pivot_table(index=KEY, columns=col, values="value1", aggfunc="sum")


# ------------------------------------------------------------ transforms
def within_region_anomalies(w):
    """log1p values minus each region's series mean: removes spatial level
    differences so only year-to-year co-fluctuation remains."""
    lg = np.log1p(w)
    return lg - lg.groupby(level="NUTS").transform("mean")


def first_differences(w):
    """Year-to-year change in log1p damage within each region (consecutive
    years only). Removes trends and outbreak persistence."""
    lg = np.log1p(w)
    yrs = lg.index.get_level_values("d_year").to_series(index=lg.index)
    diff = lg.groupby(level="NUTS").diff()
    return diff[yrs.groupby(level="NUTS").diff() == 1]


# --------------------------------------------------------------- metrics
def pairwise(w, method="pearson", min_n=MIN_N):
    cols = w.columns
    r = pd.DataFrame(np.nan, cols, cols)
    p = r.copy()
    n = pd.DataFrame(0, cols, cols)
    f = stats.spearmanr if method == "spearman" else stats.pearsonr
    for i, a in enumerate(cols):
        n.loc[a, a] = w[a].notna().sum()
        r.loc[a, a] = 1.0
        for b in cols[i + 1:]:
            m = w[[a, b]].dropna()
            k = len(m)
            n.loc[a, b] = n.loc[b, a] = k
            if k < min_n or m[a].nunique() < 3 or m[b].nunique() < 3:
                continue
            rr, pp = f(m[a], m[b])
            r.loc[a, b] = r.loc[b, a] = rr
            p.loc[a, b] = p.loc[b, a] = pp
    return r, p, n


def pairwise_europe_totals(w):
    """Pearson r between log1p Europe-wide annual totals. For each pair only
    regions/years where both series are recorded are summed, so the two
    totals cover the same area."""
    cols = w.columns
    r = pd.DataFrame(np.nan, cols, cols)
    p = r.copy()
    n = pd.DataFrame(0, cols, cols)
    for i, a in enumerate(cols):
        r.loc[a, a] = 1.0
        for b in cols[i + 1:]:
            m = w[[a, b]].dropna().groupby(level="d_year").sum()
            n.loc[a, b] = n.loc[b, a] = len(m)
            if len(m) < 10 or m[a].nunique() < 3 or m[b].nunique() < 3:
                continue
            rr, pp = stats.pearsonr(np.log1p(m[a]), np.log1p(m[b]))
            r.loc[a, b] = r.loc[b, a] = rr
            p.loc[a, b] = p.loc[b, a] = pp
    return r, p, n


def lagged(anom, min_n=MIN_N):
    """corr(row series at year t, column series at year t+1), within region."""
    yrs = anom.index.get_level_values("d_year").to_series(index=anom.index)
    nxt = anom.groupby(level="NUTS").shift(-1)
    nxt = nxt[(yrs.groupby(level="NUTS").shift(-1) - yrs) == 1]
    cols = anom.columns
    r = pd.DataFrame(np.nan, cols, cols)
    n = pd.DataFrame(0, cols, cols)
    for a in cols:
        for b in cols:
            m = pd.concat([anom[a], nxt[b]], axis=1, keys=["a", "b"]).dropna()
            n.loc[a, b] = len(m)
            if len(m) >= min_n and m.a.std() > 0 and m.b.std() > 0:
                r.loc[a, b] = stats.pearsonr(m.a, m.b)[0]
    return r, n


def cooccurrence(w, min_n=MIN_N):
    """Jaccard index of damage > 0 among region-years where both are recorded."""
    cols = w.columns
    j = pd.DataFrame(np.nan, cols, cols)
    for a in cols:
        for b in cols:
            m = pd.concat([w[a], w[b]], axis=1, keys=["a", "b"]).dropna()
            if len(m) < min_n:
                continue
            pa, pb = m.a > 0, m.b > 0
            u = (pa | pb).sum()
            j.loc[a, b] = (pa & pb).sum() / u if u else np.nan
    return j


def synchrony(long, col):
    """Spatial synchrony per series: mean pairwise Pearson r between regions'
    log1p time series (regions with >= MIN_YEARS overlapping years)."""
    rows, mats = [], {}
    for name, g in long.groupby(col):
        ts = np.log1p(g.pivot_table(index="d_year", columns="NUTS", values="value1"))
        ts = ts.loc[:, (ts.notna().sum() >= MIN_YEARS) & (ts.std() > 0)]
        if ts.shape[1] < 3:
            continue
        c = ts.corr(min_periods=MIN_YEARS)
        vals = c.values[np.triu_indices_from(c, 1)]
        vals = vals[~np.isnan(vals)]
        rows.append({col: name, "n_regions": ts.shape[1],
                     "n_countries": g.loc[g.NUTS.isin(ts.columns), "Country"].nunique(),
                     "mean_r": vals.mean(), "median_r": np.median(vals),
                     "share_pos": (vals > 0).mean()})
        mats[name] = c
    return pd.DataFrame(rows).sort_values("mean_r", ascending=False), mats


def long_table(r, p, n):
    iu = np.triu_indices(len(r), 1)
    out = pd.DataFrame({"a": r.index[iu[0]], "b": r.columns[iu[1]],
                        "r": r.values[iu], "p": p.values[iu], "n": n.values[iu]})
    out = out[out.r.notna()].copy()
    if len(out):
        out["q_fdr"] = stats.false_discovery_control(out.p.values)  # Benjamini-Hochberg
    return out.sort_values("r", key=np.abs, ascending=False)


# ----------------------------------------------------------------- plots
def cluster_order(r):
    r = r.dropna(how="all").dropna(how="all", axis=1)
    keep = r.index[r.notna().sum() > 1]
    r = r.loc[keep, keep]
    dist = (1 - r.fillna(0)).clip(lower=0).to_numpy(copy=True)
    np.fill_diagonal(dist, 0)
    order = leaves_list(linkage(squareform((dist + dist.T) / 2, checks=False), "average"))
    return list(r.index[order])


def heatmap(r, path, title, order=None, annot=False):
    order = cluster_order(r) if order is None else order
    m = r.loc[order, order]
    k = len(m)
    size = max(5, 0.32 * k + 3) if not annot else max(5, 0.9 * k + 3)
    fig, ax = plt.subplots(figsize=(size, size * 0.85))
    im = ax.imshow(m.values, cmap=DIVERGING, vmin=-1, vmax=1)
    ax.set_xticks(range(k), m.columns, rotation=90, fontsize=7 if not annot else 9)
    ax.set_yticks(range(k), m.index, fontsize=7 if not annot else 9)
    if annot:
        for i in range(k):
            for j in range(k):
                v = m.values[i, j]
                if not np.isnan(v):
                    ax.text(j, i, f"{v:.2f}", ha="center", va="center", fontsize=8,
                            color="white" if abs(v) > 0.6 else "#0b0b0b")
    ax.set_title(title, fontsize=10, loc="left")
    cb = fig.colorbar(im, ax=ax, shrink=0.6)
    cb.set_label("correlation", fontsize=8)
    ax.set_facecolor("#d9d8d4")  # grey = not enough overlap
    fig.tight_layout()
    fig.savefig(path, dpi=200)
    plt.close(fig)


# ------------------------------------------------------------------ main
def group_analysis(gl):
    """All correlation variants for the paper's groups, per hierarchical level."""
    out, levels = [], {}
    for level in (1, 2, 3):
        names = [n for _, (lv, n) in GROUPS.items() if lv == level]
        w = wide(gl[gl.level == level], "series")[names]
        variants = {
            "pooled_log": pairwise(np.log1p(w), "pearson"),
            "europe_totals": pairwise_europe_totals(w),
            "within_region": pairwise(within_region_anomalies(w), "pearson"),
            "first_diff": pairwise(first_differences(w), "pearson"),
        }
        levels[level] = {k: v[0] for k, v in variants.items()}
        for i, a in enumerate(names):
            for b in names[i + 1:]:
                row = {"level": level, "a": a, "b": b,
                       "paper_r": PAPER_R.get((a, b), PAPER_R.get((b, a)))}
                for k, (r, p, n) in variants.items():
                    row[f"{k}_r"], row[f"{k}_p"], row[f"{k}_n"] = r.loc[a, b], p.loc[a, b], n.loc[a, b]
                out.append(row)
    return pd.DataFrame(out), levels


def main(src, out):
    out = Path(out)
    (out / "figures").mkdir(parents=True, exist_ok=True)
    raw = load(src)
    meta = agent_meta(raw)
    al = drop_sparse(agent_long(raw), "agent")
    gl = drop_sparse(group_long(raw), "series")
    w = wide(al, "agent")

    # ------------- coverage
    al_all = agent_long(raw)
    cov = (al_all.assign(recorded=1, nonzero=al_all.value1 > 0)
                 .groupby("agent")
                 .agg(n_region_years=("recorded", "sum"), n_nonzero=("nonzero", "sum"),
                      n_regions=("NUTS", "nunique"), n_countries=("Country", "nunique"),
                      countries=("Country", lambda s: ",".join(sorted(s.unique()))),
                      first_year=("d_year", "min"), last_year=("d_year", "max"),
                      total=("value1", "sum"))
                 .reset_index().merge(meta, on="agent"))
    cov["share_nonzero"] = cov.n_nonzero / cov.n_region_years
    cov["series_kept"] = cov.agent.map(al.groupby("agent").NUTS.nunique()).fillna(0).astype(int)
    cov = cov.sort_values(["guild", "unit", "total"], ascending=[True, True, False])

    totals = (gl.groupby(["level", "series"]).value1.sum().rename("total_after_filter")
                .reset_index())
    totals["total_all_series"] = totals.series.map(group_long(raw).groupby("series").value1.sum())
    totals["n_series"] = totals.series.map(gl.groupby("series").NUTS.nunique())
    totals["n_countries"] = totals.series.map(gl.groupby("series").Country.nunique())

    # ------------- group level (paper's hierarchy)
    grp_pairs, grp_levels = group_analysis(gl)

    # ------------- agent level
    r_pool, p_pool, n_pool = pairwise(np.log1p(w), "pearson")
    r_rank, p_rank, n_rank = pairwise(w, "spearman")
    anom = within_region_anomalies(w)
    r_within, p_within, n_within = pairwise(anom, "pearson")
    r_diff, p_diff, n_diff = pairwise(first_differences(w), "pearson")
    r_lag, n_lag = lagged(anom)
    jac = cooccurrence(w)
    sync, sync_mats = synchrony(al, "agent")
    sync = sync.merge(meta, on="agent")
    gsync, gsync_mats = synchrony(gl, "series")

    # ------------- figures
    titles = {"pooled_log": "pooled region-years, Pearson on log(x+1)",
              "europe_totals": "Europe-wide annual totals, Pearson on log(x+1)",
              "within_region": "within-region anomalies (temporal co-fluctuation)",
              "first_diff": "year-to-year change (detrended)"}
    for level, mats in grp_levels.items():
        for k, r in mats.items():
            heatmap(r, out / f"figures/L{level}_{k}.png",
                    f"Level {level} groups: {titles[k]}", order=list(r.index), annot=True)
    order = cluster_order(r_pool)
    heatmap(r_pool, out / "figures/agents_pooled_log.png",
            f"Agents: Pearson on log(x+1), pooled region-years (n >= {MIN_N})", order)
    heatmap(r_within, out / "figures/agents_within_region.png",
            "Agents: within-region anomalies (temporal co-fluctuation)", order)
    heatmap(r_diff, out / "figures/agents_first_diff.png",
            "Agents: year-to-year change in log damage (detrended)", order)
    heatmap(r_lag, out / "figures/agents_lag1.png",
            "Agents: row in year t vs column in year t+1 (within-region anomalies)", order)
    if "ips_typ | Picea abies [m3]" in sync_mats:
        heatmap(sync_mats["ips_typ | Picea abies [m3]"],
                out / "figures/ips_typographus_region_synchrony.png",
                "Ips typographus: region x region correlation of log damage")

    # ------------- workbook
    sheets = {
        "cleaning_log": "Data cleaning steps and counts.",
        "group_totals": "Totals per Hlasny et al. (2025) group, before and after the >= 6 non-zero filter (compare with the paper's Table 2).",
        "group_pairs": "MAIN TABLE. All pairs within each hierarchical level (1 = guild x host type, 2 = guild x host genus, 3 = species), with the paper's r (Tables 4-6) and four correlation variants: pooled_log, europe_totals, within_region, first_diff. Each has r, p and n.",
        "group_sync": f"Spatial synchrony per group: mean pairwise r between regions' log(x+1) time series (regions with >= {MIN_YEARS} years).",
        "coverage": "Records, non-zero records, regions, countries, years and series kept (>= 6 non-zero) per agent (sp_code x host x unit).",
        "agent_pooled_r": f"Agent x agent Pearson r on log(x+1) across all region-years where both are recorded (n >= {MIN_N}). Mixes spatial and temporal variation.",
        "agent_pooled_n": "Number of overlapping region-years per agent pair (data overlap matrix).",
        "agent_spearman_r": "Same as agent_pooled_r but Spearman rank correlation on raw values (robust to zeros and outliers).",
        "agent_within_r": "Agent x agent Pearson r of log(x+1) minus each region's mean: year-to-year co-fluctuation within regions.",
        "agent_first_diff_r": "Agent x agent Pearson r of year-to-year change in log(x+1) within regions: removes trends and outbreak persistence.",
        "agent_pairs": "Agent pairs in long format: r, p, n and Benjamini-Hochberg q for the pooled, within-region and first-difference variants.",
        "agent_lag1_r": "Row agent in year t vs column agent in year t+1 (within-region anomalies, asymmetric). Inflated by outbreak persistence: compare with agent_within_r before reading it as lead/lag.",
        "agent_cooccurrence": "Jaccard index of damage > 0: share of region-years with damage from either agent in which both had damage.",
        "agent_sync": f"Spatial synchrony per agent (regions with >= {MIN_YEARS} years).",
    }
    readme = pd.DataFrame({"sheet": list(sheets), "description": list(sheets.values())})
    dup = raw.duplicated(["NUTS", "d_year", "agent"], keep=False)
    clean_log = pd.DataFrame({"step": [
        "Raw rows", "Rows with missing value1 dropped (not recorded)",
        "Rows sharing NUTS x year x agent, summed",
        "Region-year x agent observations", "Agent series (region x agent) dropped: < 6 non-zero values",
        "Agents kept", "Regions (NUTS)", "Zeros", "value2", "Units", "Guild assignment"],
        "value": [raw.attrs["n_raw"], raw.attrs["n_missing"],
                  f"{int(dup.sum())} rows (countries: {', '.join(sorted(raw[dup].Country.unique()))}; 14 kraje mapped to 8 NUTS2)",
                  len(al_all),
                  int(al_all.groupby(["NUTS", "agent"]).ngroups - al.groupby(["NUTS", "agent"]).ngroups),
                  w.shape[1], al.NUTS.nunique(),
                  "Treated as monitored zero damage",
                  "Not used",
                  "m3 (borers) and ha (defoliators) never summed together; Pearson on log(x+1) and Spearman are unit-free",
                  "From the indicator columns; SP_CATEGORY labels pri_abi, zei_gri and cep_spp (spruce defoliators) as CambXyl_con"]})

    pairs = []
    for k, (r, p, n) in {"pooled": (r_pool, p_pool, n_pool), "within": (r_within, p_within, n_within),
                         "first_diff": (r_diff, p_diff, n_diff)}.items():
        pairs.append(long_table(r, p, n).set_index(["a", "b"]).add_prefix(f"{k}_"))
    agent_pairs = pd.concat(pairs, axis=1).reset_index().sort_values(
        "pooled_r", key=np.abs, ascending=False)

    with pd.ExcelWriter(out / "EUForDam_correlation_metrics.xlsx") as xw:
        readme.to_excel(xw, sheet_name="README", index=False)
        clean_log.to_excel(xw, sheet_name="cleaning_log", index=False)
        totals.round(0).to_excel(xw, sheet_name="group_totals", index=False)
        grp_pairs.round(4).to_excel(xw, sheet_name="group_pairs", index=False)
        gsync.round(3).to_excel(xw, sheet_name="group_sync", index=False)
        cov.to_excel(xw, sheet_name="coverage", index=False)
        r_pool.loc[order, order].round(3).to_excel(xw, sheet_name="agent_pooled_r")
        n_pool.loc[order, order].to_excel(xw, sheet_name="agent_pooled_n")
        r_rank.loc[order, order].round(3).to_excel(xw, sheet_name="agent_spearman_r")
        r_within.loc[order, order].round(3).to_excel(xw, sheet_name="agent_within_r")
        r_diff.loc[order, order].round(3).to_excel(xw, sheet_name="agent_first_diff_r")
        agent_pairs.round(4).to_excel(xw, sheet_name="agent_pairs", index=False)
        r_lag.loc[order, order].round(3).to_excel(xw, sheet_name="agent_lag1_r")
        jac.loc[order, order].round(3).to_excel(xw, sheet_name="agent_cooccurrence")
        sync.round(3).to_excel(xw, sheet_name="agent_sync", index=False)

    print(clean_log.to_string(index=False))
    return dict(grp_pairs=grp_pairs, totals=totals, gsync=gsync, sync=sync,
                agent_pairs=agent_pairs, cov=cov)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else "output")
