# ============================================================
#  FlamMap-Pipeline  |  Schritt 6: Canopy-Layer aus dem CHM
#  Leitet Canopy Cover (%) und Canopy Height (m) fuer das
#  FlamMap-Grid ab. CBH/CBD folgen als Literaturwerte (Skript 08).
#  Voraussetzung: terra
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")

# FLAMMAP_RES, KRONE_MIN und CHM_MAX stehen zentral in R/00_config.R.
chm <- rast(fp_datei(file.path(out_dir, "chm.tif"),
                     "Erst Schritt 01 und 01b ausfuehren."))  # 25 cm, hoehenkorrigiert

# Ausreisser kappen: Punkte ueber CHM_MAX sind Artefakte (Voegel,
# Rauschpunkte). Hoechste reale Baeume im Gebiet ~22 m -> 30 m als
# grosszuegige, sichere Obergrenze.
n_out <- global(chm > CHM_MAX, "sum", na.rm=TRUE)[1,1]
cat("Gekappte Ausreisser-Zellen (>", CHM_MAX, "m):", n_out, "\n")
chm <- clamp(chm, 0, CHM_MAX)

res_chm <- res(chm)[1]
fact <- round(FLAMMAP_RES / res_chm)         # 1.00 / 0.25 = 4

# ------------------------------------------------------------
# 1) Canopy Cover (%) = Anteil Kronen-Zellen je FlamMap-Zelle
# ------------------------------------------------------------
krone <- chm > KRONE_MIN
cc <- aggregate(krone, fact = fact, fun = "mean", na.rm = TRUE) * 100
cc <- clamp(cc, 0, 100)
names(cc) <- "canopy_cover"

# ------------------------------------------------------------
# 2) Canopy Height (m) = mittlere Hoehe der Kronen-Zellen
#    (offene Flaechen ohne Krone -> 0)
# ------------------------------------------------------------
chm_krone <- ifel(chm > KRONE_MIN, chm, NA)
ch <- aggregate(chm_krone, fact = fact, fun = "mean", na.rm = TRUE)
ch <- subst(ch, NA, 0)
names(ch) <- "canopy_height"

# gemeinsames Grid sicherstellen
ch <- resample(ch, cc, method = "bilinear")

writeRaster(cc, file.path(out_dir, "canopy_cover.tif"),  overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(ch, file.path(out_dir, "canopy_height.tif"), overwrite=TRUE, gdal="COMPRESS=LZW")

# ------------------------------------------------------------
# Kontrolle
# ------------------------------------------------------------
plot(c(cc, ch), main = c("Canopy Cover (%)", "Canopy Height (m)"))