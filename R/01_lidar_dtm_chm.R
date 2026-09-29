# ============================================================
#  FlamMap-Pipeline  |  Schritt 1: DTM, DSM, CHM + Textur
#  MIT Hoehenversatz-Korrektur der Ost-Punktwolke.
#  Voraussetzung: terra, lidR, RCSF
# ------------------------------------------------------------

library(terra)
library(lidR)
library(RCSF)

source("R/00_config.R")
fp_erfordere(root = root)

# Alle Parameter (ZIEL_RES, OST_OFFSET, CSF_*, GROUND_FRACTION)
# stehen zentral in R/00_config.R.
cat("Zielaufloesung:", ZIEL_RES, "m | Ost-Versatz:", OST_OFFSET, "m\n")

las <- list.files(root, pattern="\\.las$", full.names=TRUE, recursive=TRUE, ignore.case=TRUE)
ost_file  <- las[grepl("OST",  basename(las), ignore.case=TRUE) & !grepl("korrigiert", las)]
west_file <- las[grepl("WEST", basename(las), ignore.case=TRUE)]

# ------------------------------------------------------------
# 1) Ost-Punktwolke um den Versatz absenken (einmalig, kachelweise)
# ------------------------------------------------------------
korr_dir <- file.path(root, "OST_korrigiert")
if (length(list.files(korr_dir, pattern="\\.las$")) == 0) {
  cat("Korrigiere Ost-Wolke um", OST_OFFSET, "m ...\n")
  dir.create(korr_dir, showWarnings = FALSE)
  ctg_ost <- readLAScatalog(ost_file)
  opt_chunk_size(ctg_ost)   <- 300
  opt_chunk_buffer(ctg_ost) <- 0
  opt_output_files(ctg_ost) <- file.path(korr_dir, "ost_corr_{XLEFT}_{YBOTTOM}")
  opt_progress(ctg_ost)     <- TRUE
  catalog_apply(ctg_ost, function(chunk, dz){
    las <- readLAS(chunk); if (is.empty(las)) return(NULL)
    las@data$Z <- las@data$Z - dz; las
  }, dz = OST_OFFSET)
} else cat("Korrigierte Ost-Kacheln vorhanden -> ueberspringe Korrektur.\n")

# ------------------------------------------------------------
# 2) Catalog aus korrigiertem Ost + Original West
# ------------------------------------------------------------
ost_corr <- list.files(korr_dir, pattern="\\.las$", full.names=TRUE)
ctg <- readLAScatalog(c(west_file, ost_corr))
crs(ctg) <- ARBEITS_CRS
opt_chunk_size(ctg)   <- 250
opt_chunk_buffer(ctg) <- 20
opt_progress(ctg)     <- TRUE

# ------------------------------------------------------------
# 3) Boden klassifizieren (ausgeduennt) + DTM
# ------------------------------------------------------------
opt_filter(ctg)       <- paste("-keep_random_fraction", GROUND_FRACTION)
opt_output_files(ctg) <- file.path(tempdir(), "cls_{XLEFT}_{YBOTTOM}")
cat("Boden klassifizieren ...\n")
ctg_cls <- classify_ground(ctg, algorithm = csf(class_threshold  = CSF_CLASS_THRESHOLD,
                                             cloth_resolution = CSF_CLOTH_RESOLUTION,
                                             rigidness        = CSF_RIGIDNESS))

cat("DTM ...\n")
dtm <- rasterize_terrain(ctg_cls, res=ZIEL_RES, algorithm=tin())

# ------------------------------------------------------------
# 4) DSM aus voller Wolke + CHM + Textur
# ------------------------------------------------------------
cat("DSM ...\n")
opt_filter(ctg) <- ""; opt_output_files(ctg) <- ""
dsm <- rasterize_canopy(ctg, res=ZIEL_RES, algorithm=p2r())

dsm <- resample(dsm, dtm)
chm <- dsm - dtm; chm[chm<0] <- 0; names(chm) <- "chm"
cat("CHM-Textur ...\n")
chm_sd <- focal(chm, w=3, fun="sd", na.rm=TRUE); names(chm_sd) <- "chm_sd"

# ------------------------------------------------------------
# 5) Topografie + speichern
# ------------------------------------------------------------
slope  <- terrain(dtm, "slope",  unit="degrees")
aspect <- terrain(dtm, "aspect", unit="degrees")
names(dtm) <- "elevation"

writeRaster(dtm,    file.path(out_dir,"elevation.tif"),     overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(slope,  file.path(out_dir,"slope.tif"),         overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(aspect, file.path(out_dir,"aspect.tif"),        overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(chm,    file.path(out_dir,"chm.tif"),           overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(chm_sd, file.path(out_dir,"chm_sd.tif"),        overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(dtm,    file.path(out_dir,"REFERENZ_GRID.tif"), overwrite=TRUE, gdal="COMPRESS=LZW")