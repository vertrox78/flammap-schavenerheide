# ============================================================
#  FlamMap-Pipeline  |  Schritt 2c: Multispektral Loecher fuellen
#  Fuellt NUR kleine Datenluecken (Schatten/Schwarzpixel) in den
#  MS-Baendern, NICHT den grossen Aussenrand. Danach Indizes neu.
#  Voraussetzung: terra
# ------------------------------------------------------------

library(terra)

source("R/00_config.R")

# Reprojizierte 1-m-Baender aus Schritt 2 (oder die 10-cm-Variante anpassen)
ms <- rast(fp_datei(file.path(out_dir, "ms_bands.tif"),
                    "Erst Schritt 02 ausfuehren."))  # blue,green,red,rededge,nir

# ------------------------------------------------------------
# Loecher fuellen: focal-Mittel, NUR auf NA-Zellen (na.policy="only").
# Mehrere Durchlaeufe lassen kleine/mittlere Loecher zuwachsen,
# grosse zusammenhaengende NA-Flaechen (Aussenrand) bleiben NA,
# weil ihr Inneres keine gueltigen Nachbarn hat.
# ------------------------------------------------------------
fill_holes <- function(r, w = 5, iterations = 3) {
  for (i in seq_len(iterations)) {
    na_vorher <- global(r, "isNA")[1, 1]
    r <- focal(r, w = w, fun = "mean", na.policy = "only", na.rm = TRUE)
    na_nachher <- global(r, "isNA")[1, 1]
    cat("  Durchlauf", i, ": NA", na_vorher, "->", na_nachher, "\n")
    if (na_vorher == na_nachher) break   # nichts mehr zu fuellen
  }
  r
}

cat("Fuelle Baender ...\n")
ms_fill <- rast(lapply(1:nlyr(ms), function(b) {
  cat(" Band", names(ms)[b], "\n")
  fill_holes(ms[[b]])
}))
names(ms_fill) <- names(ms)

writeRaster(ms_fill, file.path(out_dir, "ms_bands_filled.tif"),
            overwrite = TRUE, gdal = c("COMPRESS=LZW"))

# ------------------------------------------------------------
# Indizes aus dem gefuellten Multispektral neu berechnen.
#
# ACHTUNG: brightness.tif aus Schritt 02 wird hier NICHT neu
# gerechnet und bleibt damit auf den UNGEFUELLTEN Baendern.
# Ebenso geht in Schritt 03 ms_bands.tif (ungefuellt) in den
# Stapel, nicht ms_bands_filled.tif. Weil die Gueltigkeitsmaske
# in Schritt 03 die Schnittmenge aller Layer bildet, bestimmt
# der ungefuellte Stand die AOI-Grenze -- das Fuellen wirkt also
# nur innerhalb des ohnehin gueltigen Gebiets.
# ------------------------------------------------------------
nir <- ms_fill$nir; red <- ms_fill$red
green <- ms_fill$green; rededge <- ms_fill$rededge

ndvi <- (nir - red)     / (nir + red);     names(ndvi) <- "ndvi"
ndre <- (nir - rededge) / (nir + rededge); names(ndre) <- "ndre"
ndwi <- (green - nir)   / (green + nir);   names(ndwi) <- "ndwi"

writeRaster(ndvi, file.path(out_dir, "ndvi.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
writeRaster(ndre, file.path(out_dir, "ndre.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))
writeRaster(ndwi, file.path(out_dir, "ndwi.tif"), overwrite = TRUE, gdal = c("COMPRESS=LZW"))

plot(ndvi, colNA = "red", main = "NDVI nach Fuellen (NA = rot)")