# ============================================================
#  FlamMap-Pipeline  |  Schritt 2b: NDVI & NDRE auf 10 cm
#  Hochaufgeloeste Hilfslayer fuer das Setzen von Trainings-
#  punkten in QGIS (native MS-Aufloesung ~9 cm).
#  Auf 10-cm-Grid, das exakt ins 1-m-Referenz-Grid passt
#  (disagg fact=10) -> spaetere Aggregation bleibt deckungsgleich.
#  Voraussetzung: terra
# ------------------------------------------------------------

library(terra)

source("R/00_config.R")
fp_erfordere(root = root)

MS_ORTHO   <- "HEA-DE-NRW-20230911-SCHAVENERHEIDE-MULTISPECTRAL-ORTHO-V01.tif"
MS_BAENDER <- c("blue", "green", "red", "rededge", "nir")

ms_path <- fp_datei(file.path(root, MS_ORTHO))
ms <- rast(ms_path)
names(ms) <- MS_BAENDER
NAflag(ms) <- 0   # NoData-Raender ohne Speicherkopie

# Indizes auf dem NATIVEN Grid rechnen (effizient: nur 2 Ergebnisbaender
# muessen reprojiziert werden, nicht alle 5 Roh-Baender)
ndvi_nat <- (ms$nir - ms$red)     / (ms$nir + ms$red)
ndre_nat <- (ms$nir - ms$rededge) / (ms$nir + ms$rededge)

# 10-cm-Zielgrid: 1-m-Referenz-Grid um Faktor 10 verfeinern.
# So liegt jede 1-m-Zelle exakt ueber 10x10 der feinen Zellen.
ref1m <- rast(fp_datei(file.path(out_dir, "REFERENZ_GRID.tif"),
                       "Erst Schritt 01 ausfuehren."))
ref10 <- disagg(ref1m, fact = 10)   # -> 0.1 m, identisches Alignment

cat("Reprojiziere NDVI/NDRE auf 10 cm (kann etwas dauern) ...\n")
ndvi10 <- project(ndvi_nat, ref10, method = "bilinear"); names(ndvi10) <- "ndvi"
ndre10 <- project(ndre_nat, ref10, method = "bilinear"); names(ndre10) <- "ndre"

writeRaster(ndvi10, file.path(out_dir, "ndvi_10cm.tif"),
            overwrite = TRUE, gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER"))
writeRaster(ndre10, file.path(out_dir, "ndre_10cm.tif"),
            overwrite = TRUE, gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER"))

plot(ndvi10, colNA = "red", main = "NDVI 10 cm (NA = rot)")
