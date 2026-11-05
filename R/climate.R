# Climate per region: mean annual VPD and air temperature from WorldClim 2.1
# (10-minute grid, 1970-2000 monthly normals; geodata.ucdavis.edu).
#
# The 2025 EU-ForDAM paper used mean VPD 2000-2022 from ERA5-Land (ISIMIP),
# averaged over the forest of each region; that table is not in the shared
# files. Here, from WorldClim:
#   VPD (kPa) per month = saturation vapour pressure - actual vapour pressure,
#     saturation = mean of es(tmax) and es(tmin), es(T) = 0.6108 exp(17.27 T / (T + 237.3))
#     (FAO-56); actual = WorldClim vapr. Annual value = mean of the 12 months.
#   temp (deg C) = WorldClim bio_1, mean annual air temperature.
# Region value = mean of the grid cells within a circle of the region's area
# around its position (R/gapfill_v2.R region_positions); no region boundaries
# are available here.

worldclim_vars <- c("tmin", "tmax", "vapr", "bio")

worldclim_download <- function(dir, vars = worldclim_vars) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (v in vars) {
    z <- file.path(dir, sprintf("wc2.1_10m_%s.zip", v))
    if (!file.exists(z)) utils::download.file(sprintf("https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_10m_%s.zip", v), z, mode = "wb")
    if (!dir.exists(file.path(dir, v))) utils::unzip(z, exdir = file.path(dir, v))
  }
  dir
}

region_climate <- function(pos, dir) {
  rd <- function(v, m) terra::rast(file.path(dir, v, sprintf("wc2.1_10m_%s_%02d.tif", v, m)))
  es <- function(t) 0.6108 * exp(17.27 * t / (t + 237.3))
  vpd <- terra::app(terra::rast(lapply(1:12, function(m) {
    tmax <- rd("tmax", m); tmin <- rd("tmin", m)
    terra::clamp((es(tmax) + es(tmin)) / 2 - rd("vapr", m), lower = 0)
  })), mean)
  tmp <- terra::rast(file.path(dir, "bio", "wc2.1_10m_bio_1.tif"))
  pts <- terra::vect(as.data.frame(pos), geom = c("lon", "lat"), crs = "EPSG:4326")
  circ <- terra::buffer(pts, width = sqrt(pos$area_km2 / pi) * 1000)
  ext <- function(r) terra::extract(r, circ, fun = mean, na.rm = TRUE)[[2]]
  pt <- function(r) terra::extract(r, pts)[[2]]
  out <- list(ext(vpd), ext(tmp), pt(vpd), pt(tmp))
  tibble::tibble(country = pos$country, nuts_id = pos$nuts_id, vpd = out[[1]], temp = out[[2]],
                 vpd_point = out[[3]], temp_point = out[[4]])
}

# The Italian series is one region, Friuli Venezia Giulia (forest appendix
# sheet "Italy_OneDistrict"; DataOrigin_Quality), not all of Italy: use the
# NUTS 2021 point and area of ITH4 for it.
positions_it_fvg <- function(pos) {
  i <- pos$nuts_id == "IT"
  pos$lon[i] <- 13.050; pos$lat[i] <- 46.166; pos$area_km2[i] <- 7770.14; pos$source[i] <- "NUTS 2021 (ITH4, Friuli Venezia Giulia)"
  pos
}

region_climate_file <- "data/external/region_climate.csv"

# ---- Climate of the 2025 EU-ForDAM paper (ERA5-Land) --------------------------------
# data/raw/Data_Klim_Tree_NUTS_24052025.xlsx (sheet DATA_CLEAN: forest and
# climate per region; Polish directorates with the code in NUTS_region, Czech
# regions at NUTS3) and data/raw/Klim_NUTS_STATS.xlsx (climate per region,
# Czech regions at NUTS2). Columns used: mean VPD from the plantecophys
# package (as in the paper), mean air temperature, precipitation; labelled
# "2020-2022_mean" in the files. Both files give identical values for the
# regions they share; CZ01 (Prague) and DE3, DE5, DE6 (Berlin, Bremen,
# Hamburg) have no values.
paper_climate <- function(tree_file = "data/raw/Data_Klim_Tree_NUTS_24052025.xlsx", stats_file = "data/raw/Klim_NUTS_STATS.xlsx") {
  d <- readxl::read_excel(tree_file, "DATA_CLEAN")
  d <- tibble::tibble(nuts_id = dplyr::if_else(d$COUNTRY == "PL", d$NUTS_region, d$NUTS_CODE),
                      vpd = d[["2020-2022_mean_vpd_ecophyspackage_ERALand"]], temp = d[["2020-2022_mean_tas_ERALand"]],
                      precip = d[["2020-2022_mean_pr_ERALand"]])
  s <- readxl::read_excel(stats_file)
  s <- tibble::tibble(nuts_id = dplyr::recode(s$NUTS_CODE, KR0 = "KRO", `DEB;DEC` = "DEB; DEC", ITH4 = "IT"),
                      vpd = s[["2020-2022_mean_vpd_ecophyspackage_ERALand"]], temp = s[["2020-2022_mean_tas_ERALand"]],
                      precip = s[["2020-2022_mean_pr_ERALand"]])
  dplyr::bind_rows(d, dplyr::filter(s, !nuts_id %in% d$nuts_id)) |> dplyr::distinct(nuts_id, .keep_all = TRUE)
}

# Host tree of each insect or group for the model (supervisor's matrix, sheet
# "matrix" of Data_Klim_Tree_NUTS_24052025.xlsx): column name -> forest columns
paper_host_matrix <- function(tree_file = "data/raw/Data_Klim_Tree_NUTS_24052025.xlsx") {
  m <- as.data.frame(readxl::read_excel(tree_file, "matrix", .name_repair = "minimal"))
  rows <- m[[1]]
  purrr::map_dfr(names(m)[-1], function(cn) tibble::tibble(group = cn, host_columns = paste(rows[which(m[[cn]] == 1)], collapse = " + ")))
}
