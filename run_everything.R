# =============================================================================
# EU-ForDAM: run everything
#
# One script that rebuilds every current result from the raw data:
# checks packages and data, runs the analysis pipeline, and renders the reports.
# After the data are updated, run it again: only the steps whose inputs changed
# are recomputed (the pipeline remembers what is up to date).
#
# HOW TO USE
#   1. Put the project folder somewhere on your computer (download the branch
#      claude/phd-folder-reference-19dxp6 from GitHub as a ZIP and unzip it).
#   2. Put the two raw files into data/raw/ (they are not on GitHub):
#        data/raw/EUForDam_REW3.xlsx
#        data/raw/Appendix_NUTS_Forests_12.xlsx
#   3. Open RStudio, set the working directory to the project folder
#      (Session > Set Working Directory > Choose Directory), open this file
#      and press "Source" (or run: source("run_everything.R")).
#
# First run: about 30-45 minutes (the gap-filling model is tested 10 times).
# Later runs: seconds to minutes, depending on what changed.
#
# Where things end up
#   outputs/figures/final/   the final graphs (sets 1-6, summary table)
#   outputs/figures/         gap-filling figures (GF_CURRENT_*)
#   outputs/tables/          all tables (CSV / XLSX)
#   reports/*.html           the reports (needs Quarto: https://quarto.org)
#   data/processed/          derived datasets (gap-filled data, relative damage)
#
# The pipeline itself is in _targets.R (which step uses which), and the
# functions it calls are in the R/ folder, one file per topic.
# =============================================================================

# ---- 0. Settings -------------------------------------------------------------

# FALSE = build the current results (normal use).
# TRUE  = also rebuild every earlier experiment (gap-filling v1-v5, model
#         comparisons; several hours) and the two history reports.
FULL <- FALSE

# Render the HTML reports at the end (needs Quarto installed)?
RENDER_REPORTS <- TRUE

# ---- 1. Are we in the project folder? ----------------------------------------

if (!file.exists("_targets.R") || !dir.exists("R")) {
  stop("Set the working directory to the project folder first ",
       "(the folder that contains _targets.R and the R/ folder).\n",
       "In RStudio: Session > Set Working Directory > Choose Directory.", call. = FALSE)
}

# ---- 2. Packages ---------------------------------------------------------------

needed <- c("targets", "dplyr", "tidyr", "readr", "readxl", "stringr", "tibble", "purrr",
            "ggplot2", "ggrepel", "gridExtra", "scales", "writexl", "openxlsx", "forcats",
            "rlang", "jsonlite", "knitr", "rmarkdown")
if (FULL) needed <- c(needed, "gllvm", "statmod")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
library(targets)

# ---- 3. Input files --------------------------------------------------------------

inputs <- c(
  "data/raw/EUForDam_REW3.xlsx",                       # damage database (read-only)
  "data/raw/Appendix_NUTS_Forests_12.xlsx",            # forest area and growing stock per region
  "data/lookup/paper_hierarchy_2025.csv",              # groups of the 2025 paper (levels 1-2)
  "data/lookup/range_list_TEST_all_y_as_R.xlsx")       # range list used by the gap-filling
absent <- inputs[!file.exists(inputs)]
if (length(absent) > 0) stop("Missing input file(s):\n  ", paste(absent, collapse = "\n  "), call. = FALSE)
for (d in c("data/processed", "outputs/tables", "outputs/figures/final")) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- 4. What to build ----------------------------------------------------------------
# The end products. The pipeline works out and builds every step they need.

current_results <- c(
  # Checks on the data
  "tot_check_csv", "tot_suspicious_csv", "hs_check_csv",
  # Sets 1-4: proportion of host killed (per year, average), NUTS, total damage
  "final_figs", "hd_avg_csv", "hd_years_csv", "hd_years_sp_csv", "g_years_csv", "g_avg_csv",
  "tot_guild_csv", "tot_species_csv", "tot_year_csv", "avg_guild_csv", "avg_species_csv", "ys_csv",
  # Relative damage at every level of aggregation (species, host tree, host type, guild)
  "rel_csv", "fig_rel_levels",
  # Correlation metrics: all pairs, usable pairs, FDR, robustness, summary table
  "cb_verdict_csv", "cm_final_csv", "cm_robust_csv", "fig_usable_sum",
  # Set 5: usable pairs as time series and scatterplots
  "pairs_ts_csv", "fig_pairs_ts", "fig_pairs_scatter",
  # Set 6: more countries (paper's groups, Europe-wide totals)
  "gp_csv", "fig_more_countries",
  # Questions for the data providers
  "cm_questions_xlsx",
  # Gap-filling (current: v4b + range list; totals with reliable fills only)
  "rng_list_xlsx", "vr_csv", "cur_csv", "fig_cur_europe", "fig_cur_gtot",
  # Comparison with the 2025 paper, table by table
  "ppr_csv", "paper_t6_csv", "p_table2_csv",
  # Read by the correlation-metrics report
  "cb_pred", "cb_q5", "cb_verdict", "p_group_cm", "sp_compare")

# ---- 5. Run the pipeline ---------------------------------------------------------------

t0 <- Sys.time()
if (FULL) {
  tar_make()
} else {
  # (the list is written into the call, because the pipeline runs in a separate R process)
  eval(bquote(tar_make(names = tidyselect::any_of(.(current_results)))))
}
message("Pipeline finished in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " minutes.")

# Stop here if any step failed (the message says which one and why)
errs <- tar_meta(fields = error, complete_only = TRUE)
if (nrow(errs) > 0) {
  print(errs)
  stop("Some steps failed (see above). Fix the error, then run this script again: ",
       "finished steps are not repeated.", call. = FALSE)
}

# ---- 6. Reports ---------------------------------------------------------------------------

reports <- c("final_graphs", "summary_notes", "questions_for_data_providers",
             "comparison_with_paper", "correlation_metrics")
if (FULL) reports <- c(reports, "gapfilling", "report")

quarto <- Sys.which("quarto")
if (RENDER_REPORTS && nzchar(quarto)) {
  for (r in reports) {
    message("Rendering reports/", r, ".qmd")
    status <- system2(quarto, c("render", file.path("reports", paste0(r, ".qmd")), "--quiet"))
    if (status != 0) warning("Report ", r, " did not render (see the message above).", call. = FALSE)
  }
} else if (RENDER_REPORTS) {
  message("Quarto not found, so the HTML reports were not rendered. ",
          "Install it from https://quarto.org, restart RStudio and run this script again ",
          "(the pipeline will not run again, only the reports).")
}

# ---- 7. Summary -----------------------------------------------------------------------------

n_fig <- length(list.files("outputs/figures", pattern = "\\.png$", recursive = TRUE))
n_tab <- length(list.files("outputs/tables", pattern = "\\.(csv|xlsx)$"))
message("\nDone. ", n_fig, " figures in outputs/figures, ", n_tab, " tables in outputs/tables.")
message("Open reports/final_graphs.html for all graphs in one place.")
