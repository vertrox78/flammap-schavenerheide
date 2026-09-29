# ============================================================
#  FlamMap-Pipeline  |  Schritt 2: Multispektral + Indizes
#  Reprojiziert MS aufs Referenz-Grid (Aufloesung folgt
#  automatisch dem Grid aus Schritt 1) + Indizes + Helligkeit.
#  Voraussetzung: terra
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")
fp_erfordere(root = root)

#  Bandbelegung: die Prozessierungsdokumentation des Dienstleisters
#  benennt die Baender nicht, die Reihenfolge wurde anhand der
#  spektralen Signaturen bestimmt (s. Kapitel Methoden).
MS_ORTHO  <- "HEA-DE-NRW-20230911-SCHAVENERHEIDE-MULTISPECTRAL-ORTHO-V01.tif"
MS_BAENDER <- c("blue", "green", "red", "rededge", "nir")

ms_path <- fp_datei(file.path(root, MS_ORTHO))
ms <- rast(ms_path); names(ms) <- MS_BAENDER
NAflag(ms) <- 0

# Referenz-Grid aus Schritt 1 gibt CRS + Aufloesung + Extent vor
ref <- rast(fp_datei(file.path(out_dir, "REFERENZ_GRID.tif"),
                     "Erst Schritt 01 ausfuehren."))

cat("Reprojiziere Multispektral auf das Referenz-Grid (average) ...\n")
ms_ref <- project(ms, ref, method = "average")

nir<-ms_ref$nir; red<-ms_ref$red; green<-ms_ref$green; rededge<-ms_ref$rededge; blue<-ms_ref$blue
ndvi<-(nir-red)/(nir+red);         names(ndvi)<-"ndvi"
ndre<-(nir-rededge)/(nir+rededge); names(ndre)<-"ndre"
ndwi<-(green-nir)/(green+nir);     names(ndwi)<-"ndwi"
brightness <- (blue+green+red);    names(brightness)<-"brightness"   # Schatten-Index

writeRaster(ms_ref,     file.path(out_dir,"ms_bands.tif"),   overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(ndvi,       file.path(out_dir,"ndvi.tif"),       overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(ndre,       file.path(out_dir,"ndre.tif"),       overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(ndwi,       file.path(out_dir,"ndwi.tif"),       overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(brightness, file.path(out_dir,"brightness.tif"),overwrite=TRUE, gdal="COMPRESS=LZW")

plot(c(ndvi,ndre,brightness), main=c("NDVI","NDRE","Helligkeit"))