# Project guide: where we are and where things are

Last updated 6 November 2026.

## The main thread right now

**The professor's model** (PROLES methods note "Predicting pest 2 forest damage from pest 1"):
predict pest 2 damage in regions where it is not recorded, from pest 1 (*Ips typographus*),
climate (VPD), host tree and location, with ranges of uncertainty.

First pair: *Ips typographus* (pest 1) to *Pityokteines* spp. (pest 2, fir borers).
30 regions with both recorded (642 region-years, 46 % zero years); 43 regions to predict
(22 of them have fir; the other 21 have no fir, so a prediction there means nothing).

| Step | What | Status |
|---|---|---|
| 1 | Simple (Gaussian) version fitted, as in the note's script | done |
| 2 | Two-step (hurdle) version fitted, the note's Component 6 for data with zeros | done |
| 3 | "Hide a region" test of the simple version, against the note's two baselines | done |
| 4 | "Hide a region" test of the two-step version | running (26 of 30 regions) |
| 5 | One table comparing everything | next |
| 6 | Decide what to try next (the note's own alternatives, Section 8) | after step 5 |

### Results so far

Simple version (fits the zeros badly):

| Model | Score, hide a region | Without Slovenia |
|---|---|---|
| Professor's model, simple version | -0.10 | 0.32 |
| One simple line (note's baseline a) | -0.04 | 0.36 |
| European average (note's baseline b) | -0.03 | -0.02 |

Score: 1 = perfect, 0 = no better than the overall average, below 0 = worse.
Slovenia gets absurd predictions: it has more fir and more *Ips* damage than any region the
model learned from, and the per-region *Ips* line stretches too far.

Two-step version: chains agree (R-hat 1.006), zero years 46 % in the model and in the data.
Any damage at all: more likely with more fir and in bad *Ips* years. How much: more with
more *Ips* (0.36) and more fir (1.43). 151 divergent transitions (about 2.5 %): mention it.

### What is the note's and what we added

| From the note | Added or changed by us, and why |
|---|---|
| Formula, priors, sampler settings | VPD in place of temperature (the 2025 paper used VPD only) |
| Hurdle-Gamma for zeros, ar() dropped | What goes into the "any damage" part (the note does not say) |
| Leave-one-region-out with 2 chains | Hidden region's pest 2 blanked: the note's code let the AR(1) term see it (a leak) |
| Baselines: pooled regression, European mean | A 90 % range for the European mean (the note gives no code) |
| Predictions with 50 % and 90 % ranges, PROLES plots | Regions without the host hidden in the plot; pest 1 rescaled so pest 2 is readable |

### Inputs (checked)

- Damage: EU-ForDAM value1, CZ and DE spruce group counted as *I. typographus* (as in the paper).
- VPD: the paper's ERA5-Land values, averaged 2000-2022 (`data/raw/Data_Klim_Tree_NUTS_24052025.xlsx`,
  `data/raw/Klim_NUTS_STATS.xlsx`); the column names say "2020-2022", a labelling slip.
- Host for *Pityokteines*: share of fir in the forest area (the note's "host tree proportion").
- Positions: NUTS points; Italy = Friuli Venezia Giulia; PL directorate cities; FI forest centres share their NUTS2 point.
- Checked: the model's VPD equals the paper's for every region; all test results come from the current fits.

### Files

| What | Where |
|---|---|
| The model code | `R/pest2_model.R` |
| Run everything for one pair | `bash scripts/pest2_all.sh ips_typ pit_spp fir` |
| Fit summaries, checks, prediction plots | `outputs/pest2/ips_typ_to_pit_spp/` (simple) and `.../hurdle/` (two-step) |
| Test scores | `outputs/pest2/ips_typ_to_pit_spp/loro_scores.csv` |
| Earlier WorldClim run (kept for comparison) | `outputs/pest2/ips_typ_to_pit_spp/worldclim/` |
| Region-by-region tables (hold damage values, not in the public repo) | `data/processed/pest2/` |

## Supporting tables (finished, used by everything)

| Table | What it is | Where |
|---|---|---|
| Input tables | regions (position, VPD, host) and region x year (every insect's damage) | `data/processed/MODEL_input_tables.xlsx`, `outputs/tables/MODEL_regions.csv` |
| Overlap workbook | where every target and predictor species are recorded in the same region and year | `outputs/tables/OVERLAP_target_and_predictor_species.xlsx` |
| Insect list | every insect, its host columns (professor's matrix), overlap with *Ips* | `outputs/tables/MODEL_insects_with_ips_typographus.csv` |

## Side analyses (parked, for background)

The brainstormed equation "target ~ *Ips* x VPD x latitude x longitude", for the 19 insects
that overlap with *Ips* in at least 3 countries. Main lessons: the full equation fits well but
predicts new regions badly; a simple mixed model (with a year effect) predicts best; *Ips*
itself adds real skill mainly for *P. chalcographus*, *Phaenops cyanea* and *Tomicus*.

| Analysis | Where |
|---|---|
| The equation as made, and with host | `outputs/tables/EQUATION_*.xlsx` |
| Mixed model and lasso against the equation | `outputs/tables/PREDICT_better_ways.xlsx`, `outputs/predict_all/better_ways.png` |
| Dredge, step by step, all-subsets tries | `outputs/predict_all/DREDGE_*`, `PREDICT_top5_*`, `BEST_BY_TEST_*`, `HOST_*` |

## Earlier finished work

Correlation pairs, broader categories, predictability across levels, relative damage, comparison
with the 2025 paper, gap-filling: see `reports/summary_notes.html` and `reports/final_graphs.html`.

## Open questions for the professor or Roman

1. The leak in the note's test code (AR(1) sees the hidden region's previous year).
2. Slovenia: drop the per-region *Ips* line, or keep it?
3. 10 of the 30 training regions (8 German, 2 Austrian) report zero *Pityokteines* in every year: real, or not reported?
4. Which pairs after *Ips* to *Pityokteines*?
