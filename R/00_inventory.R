# ============================================================
#  FlamMap-Pipeline  |  Schritt 0: Daten-Inventar
#  Sichtet alle Drohnendatensaetze: Format, CRS, Aufloesung,
#  Extent, Baender, Ost/West-Ueberlappung.
#  Ziel: fundierte Merge- und Verarbeitungsstrategie.
# ------------------------------------------------------------

library(terra)
library(lidR)

source("R/00_config.R")
fp_erfordere(root = root)

# Alle relevanten Dateien finden
ras_ext <- "\\.(tif|tiff)$"
las_ext <- "\\.(las|laz)$"
ras_files <- list.files(root, pattern = ras_ext, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
las_files <- list.files(root, pattern = las_ext, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

cat("=========================================================\n")
cat(" RASTER-DATEN (", length(ras_files), "Dateien )\n")
cat("=========================================================\n")
ras_info <- list()
for (f in ras_files) {
  r <- tryCatch(rast(f), error = function(e) NULL)
  if (is.null(r)) { cat("!! konnte nicht gelesen werden:", basename(f), "\n"); next }
  e <- as.vector(ext(r))
  cat("\n", basename(f), "\n", sep = "")
  cat("   CRS       :", crs(r, describe = TRUE)$code, "(", crs(r, describe=TRUE)$name, ")\n")
  cat("   Baender   :", nlyr(r), "\n")
  cat("   Aufloesung:", paste(round(res(r), 3), collapse = " x "), "m\n")
  cat("   Extent    : X", round(e["xmin"]), "-", round(e["xmax"]),
      " Y", round(e["ymin"]), "-", round(e["ymax"]), "\n")
  cat("   Groesse   :", ncol(r), "x", nrow(r), "Zellen\n")
  ras_info[[basename(f)]] <- ext(r)
}

cat("\n=========================================================\n")
cat(" LIDAR-DATEN (", length(las_files), "Dateien )\n")
cat("=========================================================\n")
if (length(las_files) > 0) {
  for (f in las_files) {
    h <- tryCatch(readLASheader(f), error = function(e) NULL)
    if (is.null(h)) { cat("!! konnte nicht gelesen werden:", basename(f), "\n"); next }
    cat("\n", basename(f), "\n", sep = "")
    cat("   CRS       :", tryCatch(st_crs(h)$epsg, error=function(e) "?"), "\n")
    cat("   Punkte    :", h@PHB$`Number of point records`, "\n")
    cat("   Extent    : X", round(h@PHB$`Min X`), "-", round(h@PHB$`Max X`),
        " Y", round(h@PHB$`Min Y`), "-", round(h@PHB$`Max Y`), "\n")
    area <- (h@PHB$`Max X`-h@PHB$`Min X`) * (h@PHB$`Max Y`-h@PHB$`Min Y`)
    if (area > 0) cat("   Dichte    : ~", round(h@PHB$`Number of point records`/area, 1), "Pkt/m2\n")
    cat("   Hinweis   : Punktwolke -> DTM/CHM & Struktur ableitbar (Pipeline A)\n")
  }
} else {
  cat("\nKeine .las/.laz gefunden.\n")
  cat("Falls LiDAR nur als DSM/DTM-GeoTIFF vorliegt, taucht es oben\n")
  cat("unter RASTER-DATEN auf -> dann Pipeline ueber Raster-CHM.\n")
}

# ------------------------------------------------------------
# Ost/West-Ueberlappung der Raster pruefen (grob)
# ------------------------------------------------------------
cat("\n=========================================================\n")
cat(" HINWEISE ZUR MERGE-STRATEGIE\n")
cat("=========================================================\n")
cat("- Gleiches CRS bei allen? Wenn nein: vor dem Mergen project().\n")
cat("- Ueberlappen Ost/West? Dann terra::mosaic() (mittelt Ueberlappung)\n")
cat("  statt merge() (nimmt erstes Raster).\n")
cat("- Multispektral deckt das Gesamtgebiet -> als Vorlage fuer den\n")
cat("  gemeinsamen Extent geeignet.\n")
