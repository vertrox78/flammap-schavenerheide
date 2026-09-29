# ============================================================
#  FlamMap-Pipeline  |  Schritt 3: AOI definieren & zuschneiden
#  Gueltiges Gebiet = Schnittmenge aller Layer (keine NA).
#  Voraussetzung: terra
#  Eingang : data/{elevation,slope,aspect,chm,chm_sd,ndvi,ndre,
#                  ndwi,brightness,ms_bands}.tif
#  Ausgang : data/AOI.gpkg, data/<name>_clip.tif je Layer,
#            data/REFERENZ_GRID_clip.tif
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")

# ------------------------------------------------------------
#  Layer des Stapels.
#
#  ms_bands ist mehrbandig und wird unten in blue/green/red/
#  rededge/nir zerlegt; zusammen mit den uebrigen Eintraegen
#  entstehen genau die Merkmale aus FEATURES plus die Topografie,
#  die spaeter nur ins Landschaftsmodell geht.
#
#  HINWEIS zur Konsistenz: ms_bands.tif stammt aus Schritt 02,
#  waehrend ndvi/ndre/ndwi nach Schritt 02c aus den LUECKEN-
#  GEFUELLTEN Baendern (ms_bands_filled.tif) neu berechnet sind.
#  Die Indizes sind also gefuellt, die Rohbaender nicht. Weil die
#  Gueltigkeitsmaske unten die Schnittmenge ALLER Layer bildet,
#  bestimmt der ungefuellte Stapel die AOI-Grenze -- die gefuellten
#  Indizes vergroessern das Gebiet nicht.
# ------------------------------------------------------------
LAYER <- c("elevation", "slope", "aspect", "chm", "chm_sd",
           "ndvi", "ndre", "ndwi", "brightness", "ms_bands")

files <- file.path(out_dir, paste0(LAYER, ".tif"))
fehlt <- LAYER[!file.exists(files)]
if (length(fehlt))
  stop("Layer fehlen in ", out_dir, ":\n  ",
       paste0(fehlt, ".tif", collapse = ", "),
       "\n  Erst die Schritte 01-02c ausfuehren.", call. = FALSE)
cat("Layer im Stack:", paste0(LAYER, ".tif", collapse = ", "), "\n")

# Extent angleichen (erster Layer = Vorlage)
refr <- rast(files[1])
stk <- rast(lapply(files, function(f) {
  r <- rast(f)
  if (!compareGeom(r, refr, stopOnError = FALSE, messages = FALSE))
    r <- resample(r, refr, "near")
  r
}))

# Gueltigkeits-Maske: nur Zellen, in denen ALLE Layer Werte haben
valid <- app(stk, fun = function(x) as.numeric(sum(is.na(x)) == 0))
valid[valid == 0] <- NA
cat("Gueltige Zellen:", global(valid, "sum", na.rm = TRUE)[1, 1], "von", ncell(valid), "\n")

# AOI-Polygon sichern
aoi <- aggregate(as.polygons(valid))
writeVector(aoi, file.path(out_dir, "AOI.gpkg"), overwrite = TRUE)

# Maskieren + trimmen + je Layer als *_clip schreiben
stk <- trim(mask(stk, valid))
for (i in seq_len(nlyr(stk))) {
  writeRaster(stk[[i]], file.path(out_dir, paste0(names(stk)[i], "_clip.tif")),
              overwrite = TRUE, gdal = "COMPRESS=LZW")
}
writeRaster(stk[[1]], file.path(out_dir, "REFERENZ_GRID_clip.tif"),
            overwrite = TRUE, gdal = "COMPRESS=LZW")

# ------------------------------------------------------------
#  Kontrolle: liegen alle Modellmerkmale zugeschnitten vor?
#  Bricht hier fruehzeitig ab, statt erst in Schritt 04.
# ------------------------------------------------------------
fehlt_feat <- FEATURES[!file.exists(file.path(out_dir, paste0(FEATURES, "_clip.tif")))]
if (length(fehlt_feat)) {
  warning("Diese Modellmerkmale wurden NICHT geschrieben: ",
          paste(fehlt_feat, collapse = ", "), call. = FALSE)
} else {
  cat("Alle", length(FEATURES), "Modellmerkmale liegen als *_clip.tif vor.\n")
}

plot(stk[["ndvi"]], colNA = "red", main = "NDVI nach AOI-Zuschnitt")
