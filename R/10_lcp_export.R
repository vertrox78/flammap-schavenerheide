# ============================================================
#  FlamMap-Pipeline  |  Schritt 10: Landschafts-Layer exportieren
#  Bringt alle acht LCP-Layer auf EXAKT dasselbe 1-m-Gitter und
#  schreibt sie als ASCII-Grid (.asc) fuer den LCP-Bau in FlamMap.
#  Voraussetzung: terra
#  Eingang : data/{elevation,slope,aspect,fuel_model,canopy_*}.tif
#  Ausgang : data/LCP_layers/<name>.asc (+ .prj)
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")

lcp_dir <- file.path(out_dir, LCP_ORDNER)
dir.create(lcp_dir, showWarnings = FALSE, recursive = TRUE)
cat("Zielordner:", lcp_dir, "\n")

# ------------------------------------------------------------
# 0) Hangneigung und Exposition notfalls aus dem DTM herstellen
#
#    Beide sind rein deterministische Ableitungen des Gelaende-
#    modells (Schritt 01). Fehlen die Einzeldateien -- etwa weil
#    sie zwischenzeitlich aufgeraeumt wurden --, waere sonst der
#    stundenlange LiDAR-Schritt zu wiederholen, obwohl
#    elevation.tif genuegt.
# ------------------------------------------------------------
for (nm in c("slope", "aspect")) {
  ziel <- file.path(out_dir, paste0(nm, ".tif"))
  if (file.exists(ziel)) next
  dtm_pfad <- fp_datei(file.path(out_dir, "elevation.tif"),
                       "Ohne elevation.tif ist Schritt 01 zu wiederholen.")
  cat("Stelle", basename(ziel), "aus elevation.tif her ...\n")
  r <- terrain(rast(dtm_pfad), nm, unit = "degrees")
  names(r) <- nm
  writeRaster(r, ziel, overwrite = TRUE, gdal = "COMPRESS=LZW")
}

# ------------------------------------------------------------
# 1) Referenz-Gitter = canopy_cover (das finale 1-m-Gitter)
# ------------------------------------------------------------
ref <- rast(fp_datei(file.path(out_dir, "canopy_cover.tif"),
                     "Erst Schritt 06 ausfuehren."))

#  Die acht Layer des Landschaftsmodells in FlamMap-Reihenfolge
LAYERS <- c(elevation = "elevation.tif",
            slope     = "slope.tif",
            aspect    = "aspect.tif",
            fuel      = "fuel_model.tif",
            cover     = "canopy_cover.tif",
            height    = "canopy_height.tif",
            cbh       = "canopy_base_height.tif",
            cbd       = "canopy_bulk_density.tif")

fehlt <- LAYERS[!file.exists(file.path(out_dir, LAYERS))]
if (length(fehlt))
  stop("Layer fehlen in ", out_dir, ":\n  ", paste(fehlt, collapse = ", "),
       "\n  Erst die Schritte 01, 06, 07 und 08 ausfuehren.", call. = FALSE)

# ------------------------------------------------------------
# 2) Jeden Layer aufs Referenz-Gitter zwingen + als Integer .asc
#    Fuel Model ist kategorisch -> nearest neighbour.
#    Alle uebrigen sind kontinuierlich -> bilinear.
# ------------------------------------------------------------
NEAR_LAYERS <- c("fuel")

# ------------------------------------------------------------
#  Exposition ist eine ZIRKULAERE Groesse: zwischen 359 und 1 Grad
#  liefert lineare Interpolation 180 Grad, also die Gegenrichtung.
#  Sie wird deshalb ueber Sinus und Kosinus interpoliert.
# ------------------------------------------------------------
aspect_resample <- function(r, ref) {
  cat("  (zirkulaere Interpolation ueber sin/cos)\n")
  rad <- r * pi / 180
  s <- resample(sin(rad), ref, method = "bilinear")
  k <- resample(cos(rad), ref, method = "bilinear")
  a <- atan2(s, k) * 180 / pi
  ifel(a < 0, a + 360, a)
}

for (nm in names(LAYERS)) {
  r <- rast(file.path(out_dir, LAYERS[[nm]]))
  if (!compareGeom(r, ref, stopOnError = FALSE, messages = FALSE)) {
    r <- if (nm == "aspect") {
      aspect_resample(r, ref)
    } else {
      resample(r, ref, method = if (nm %in% NEAR_LAYERS) "near" else "bilinear")
    }
  }
  r <- round(r)                       # FlamMap-Layer als Ganzzahl
  writeRaster(r, file.path(lcp_dir, paste0(nm, ".asc")),
              overwrite = TRUE, NAflag = -9999, datatype = "INT2S")
  cat(sprintf("%-10s -> %s.asc  (Bereich %s)\n", nm, nm,
              paste(round(minmax(r)[, 1], 0), collapse = "-")))
}

cat("\nLCP-Layer liegen in:", lcp_dir, "\n")
cat("Weiter in FlamMap: Landscape -> Generate Landscape File (.lcp),\n")
cat("Brennstoffmodell 89 aus flammap/GA_heath.fmd nachladen.\n")
