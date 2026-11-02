# Mean annual VPD and temperature per region from WorldClim 2.1 (R/climate.R).
# Run from the project root after tar_make(); downloads about 150 MB once.
#   Rscript scripts/region_climate.R [download folder]
suppressMessages({ library(targets); library(dplyr) })
source("R/climate.R")
a <- commandArgs(trailingOnly = TRUE)
dir <- if (length(a)) a[1] else "_targets/worldclim"
cl <- region_climate(positions_it_fvg(tar_read(region_pos)), worldclim_download(dir))
write.csv(mutate(cl, across(c(vpd, vpd_point), ~ round(.x, 4)), across(c(temp, temp_point), ~ round(.x, 2))),
          region_climate_file, row.names = FALSE)
cat("written", region_climate_file, nrow(cl), "regions\n")
