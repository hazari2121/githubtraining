# Every insect recorded with I. typographus in at least 3 countries, with and
# without the host tree (R/dredge_ips.R). Each insect is saved as soon as it is
# done (data/processed/host_all/), so the run resumes after an interruption.
#   Rscript scripts/host_all.R
suppressMessages(library(dplyr))
source("R/dredge_ips.R")
ry <- read.csv("data/processed/model_region_year.csv", check.names = FALSE)
reg <- read.csv("outputs/tables/MODEL_regions.csv")
ins <- read.csv("outputs/tables/MODEL_insects_with_ips_typographus.csv")
codes <- ins$code[ins$countries_with_ips >= 3 & !is.na(ins$host_columns)]
dir <- "data/processed/host_all"; dir.create(dir, recursive = TRUE, showWarnings = FALSE)
for (s in codes) {
  f <- file.path(dir, paste0(s, ".csv"))
  if (!file.exists(f)) write.csv(host_compare(ry, reg, ins, s), f, row.names = FALSE)
}
hc <- bind_rows(lapply(file.path(dir, paste0(codes, ".csv")), read.csv))
w <- hc |> select(code, insect, host, regions, region_years, R2_it_alone, with_host, R2_fair, most_chosen_equation, rounds_with_host) |>
  tidyr::pivot_wider(id_cols = c(code, insect, host, regions, region_years, R2_it_alone), names_from = with_host,
                     values_from = c(R2_fair, most_chosen_equation, rounds_with_host)) |>
  transmute(code, insect, host, regions, region_years, R2_it_alone, R2_fair_without_host = R2_fair_FALSE, R2_fair_with_host = R2_fair_TRUE,
            host_gain = R2_fair_TRUE - R2_fair_FALSE, rounds_choosing_host = rounds_with_host_TRUE,
            equation_without_host = most_chosen_equation_FALSE, equation_with_host = most_chosen_equation_TRUE) |>
  arrange(desc(pmax(R2_fair_with_host, R2_fair_without_host)))
write.csv(hc, "outputs/predict_all/HOST_all_insects_detail.csv", row.names = FALSE)
write.csv(w, "outputs/predict_all/HOST_all_insects.csv", row.names = FALSE)
cat("insects:", nrow(w), " no host listed (left out):", paste(ins$code[ins$countries_with_ips >= 3 & is.na(ins$host_columns)], collapse = " "), "\n")
suppressMessages({ source("R/helpers.R"); source("R/figures.R") })
ggplot2::ggsave("outputs/predict_all/host_all_insects.png", plot_host_all(w), width = 12.5, height = 3 + 0.3 * nrow(w), dpi = 200, bg = "white")
