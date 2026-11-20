#!/usr/bin/env Rscript
# One-time builder for the web explorer's map geometry.
# Usage: Rscript tools/build_map.R <folder with gadm41_*.json files>
#
# Boundaries: GADM 4.1 (https://gadm.org), free for academic / non-commercial use.
# Spatial units follow the damage data:
#   AT, CZ = NUTS 2 (CZ NUTS 2 = groups of kraje); DE = NUTS 1 (DEB; DEC = Rheinland-Pfalz
#   + Saarland); FI = NUTS 3 (2024 codes); IT = Friuli Venezia Giulia; other countries whole.
#   PL = forestry districts (RDLP): no boundary file exists, so each district is drawn as
#   the area closest to its seat city (Voronoi cell clipped to Poland) - APPROXIMATE.
# Output: reference/map/map_geometry.json, projected to ETRS89-LAEA (EPSG:3035), simplified,
# as SVG path strings in a viewBox of width 1000.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(jsonlite)
})
sf_use_s2(FALSE)

src <- commandArgs(trailingOnly = TRUE)[1]
cw <- readr::read_csv("reference/map/map_regions.csv", show_col_types = FALSE,
                      col_types = readr::cols(.default = "c"))
rd <- function(f) st_read(file.path(src, paste0("gadm41_", f, ".json")), quiet = TRUE) |>
  st_make_valid()
crs <- 3035
tol <- 4000   # simplification tolerance in metres

one <- function(g) st_make_valid(st_union(st_make_valid(st_transform(g, crs))))
geoms <- list()
for (i in seq_len(nrow(cw))) {
  r <- cw[i, ]
  if (r$source == "POL_seat") next
  g <- rd(r$source)
  if (!is.na(r$gadm_level)) {
    nm <- strsplit(r$gadm_names, ";")[[1]]
    g <- g[g[[r$gadm_level]] %in% nm, ]
    if (nrow(g) == 0) stop("No GADM match for ", r$nuts)
  }
  geoms[[r$nuts]] <- one(g)
}

# Polish forestry districts: Voronoi cells of the district seats, clipped to Poland
pol <- one(rd("POL_0"))
seats <- cw |> filter(source == "POL_seat") |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> st_transform(crs)
vor <- st_collection_extract(st_voronoi(st_union(seats), envelope = st_as_sfc(st_bbox(pol))))
vor <- st_sf(geometry = vor)
vor$nuts <- seats$nuts[st_nearest_feature(st_centroid(vor), seats)]
pl_cells <- lapply(seq_len(nrow(seats)), function(k)
  st_intersection(vor$geometry[vor$nuts == seats$nuts[k]], pol))
names(pl_cells) <- seats$nuts

units <- st_sf(nuts = c(names(geoms), names(pl_cells)),
               approx = c(rep(FALSE, length(geoms)), rep(TRUE, length(pl_cells))),
               geometry = c(do.call(c, geoms),
                            do.call(c, pl_cells)))

# background countries (grey context), incl. the study countries' full outlines
bg_files <- c("NOR_0", "DNK_0", "NLD_0", "BEL_0", "LUX_0", "FRA_0", "LIE_0", "BLR_0", "UKR_0",
              "MDA_0", "BIH_0", "MNE_0", "ALB_0", "MKD_0", "GRC_0", "XKO_0", "EST_0", "LVA_0",
              "ITA_0", "DEU_0", "AUT_0", "CZE_0", "FIN_0", "POL_0", "BGR_0", "CHE_0", "HRV_0",
              "HUN_0", "LTU_0", "ROU_0", "SRB_0", "SWE_0", "SVN_0", "SVK_0")
bg <- st_sf(id = bg_files, geometry = do.call(c, lapply(bg_files, function(f) one(rd(f)))))

# map window: study area with a margin
bb <- st_bbox(units); m <- 150000
win <- st_as_sfc(st_bbox(c(xmin = bb[["xmin"]] - m, ymin = bb[["ymin"]] - m,
                           xmax = bb[["xmax"]] + m, ymax = bb[["ymax"]] + m), crs = st_crs(crs)))
clip <- function(x) st_intersection(st_make_valid(st_buffer(st_make_valid(st_simplify(x, preserveTopology = TRUE, dTolerance = tol)), 0)), win)
units <- clip(units); bg <- clip(bg)

W <- 1000; wb <- st_bbox(win); sc <- W / (wb[["xmax"]] - wb[["xmin"]])
H <- round((wb[["ymax"]] - wb[["ymin"]]) * sc)
to_path <- function(g) {
  polys <- st_cast(st_sfc(g, crs = crs), "POLYGON")
  paste(vapply(polys, function(p) paste(vapply(p, function(ring) {
    x <- (ring[, 1] - wb[["xmin"]]) * sc; y <- (wb[["ymax"]] - ring[, 2]) * sc
    paste0("M", paste(sprintf("%.0f %.0f", x, y), collapse = "L"), "Z")
  }, ""), collapse = ""), ""), collapse = "")
}
lab <- st_coordinates(st_point_on_surface(units))
out <- list(
  width = W, height = H, crs = "EPSG:3035",
  source = "GADM 4.1 (gadm.org); Polish forestry districts approximated from district seats",
  units = lapply(seq_len(nrow(units)), function(k) list(
    code = units$nuts[k], approx = units$approx[k], d = to_path(units$geometry[k]),
    lx = round((lab[k, 1] - wb[["xmin"]]) * sc, 1), ly = round((wb[["ymax"]] - lab[k, 2]) * sc, 1))),
  background = vapply(seq_len(nrow(bg)), function(k) to_path(bg$geometry[k]), "")
)
writeLines(toJSON(out, auto_unbox = TRUE), "reference/map/map_geometry.json")
cat("Wrote reference/map/map_geometry.json:", nrow(units), "units,",
    round(file.size("reference/map/map_geometry.json") / 1e3), "kB\n")
