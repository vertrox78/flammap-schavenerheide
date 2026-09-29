# ============================================================
#  FlamMap-Pipeline  |  Schritt 8: Canopy Base Height (CBH) &
#  Canopy Bulk Density (CBD) je Waldtyp (Literaturwerte).
#  Laub bewusst kronenfeuer-hemmend, Nadel realistisch.
#  Voraussetzung: terra
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")

cls <- rast(fp_datei(file.path(out_dir, "vegetation_classified.tif"),
                     "Erst Schritt 05b ausfuehren."))
cc  <- rast(fp_datei(file.path(out_dir, "canopy_cover.tif"),
                     "Erst Schritt 06 ausfuehren."))   # FlamMap-Grid

# Klassifikation aufs FlamMap-Grid bringen.
# ACHTUNG: hier bewusst NEAREST NEIGHBOUR, nicht der Modalwert wie in
# Schritt 07. CBH/CBD sind nur in den beiden Waldklassen von null
# verschieden und werden unten ohnehin mit der Kronenmaske (CC > 0)
# verschnitten, weshalb der Unterschied praktisch nicht durchschlaegt.
# An schmalen Waldraendern koennen Brennstoffmodell- und CBH/CBD-Karte
# dadurch aber zellweise voneinander abweichen.
cls_fm <- resample(cls, cc, method = "near")

# ------------------------------------------------------------
# Literaturwerte je Waldtyp
#  Laubwald (3): feuerresistent -> CBH kuenstlich hoch (10 m),
#                CBD sehr niedrig (0.01) -> praktisch kein Kronenfeuer
#                (LANDFIRE-Konvention, Davies/Populus-Analogie)
#  Nadelwald (4): kronenfeuer-faehig -> niedrige CBH (Leiterbrennstoff),
#                 realistische CBD fuer Kiefer
#  Alle Nicht-Wald-Klassen: 0 (werden bei CC=0 eh ignoriert)
# ------------------------------------------------------------
# Werte stehen zentral in R/00_config.R (CBH_MAP / CBD_MAP).
rcl_cbh <- cbind(as.integer(names(CBH_MAP)), unname(CBH_MAP))   # CBH in m
rcl_cbd <- cbind(as.integer(names(CBD_MAP)), unname(CBD_MAP))   # CBD in kg/m3

cbh <- classify(cls_fm, rcl_cbh); names(cbh) <- "cbh"
cbd <- classify(cls_fm, rcl_cbd); names(cbd) <- "cbd"

# Nur dort behalten, wo tatsaechlich Kronen (CC > 0)
cbh <- ifel(cc > 0, cbh, 0)
cbd <- ifel(cc > 0, cbd, 0)

# ------------------------------------------------------------
# CBD fuer FlamMap-Integer-Export: kg/m3 * 100
#  (0.15 kg/m3 -> 15). CBH bleibt in m.
# ------------------------------------------------------------
cbd_scaled <- cbd * CBD_SKALA; names(cbd_scaled) <- "cbd_x100"

writeRaster(cbh,        file.path(out_dir,"canopy_base_height.tif"), overwrite=TRUE, gdal="COMPRESS=LZW")
writeRaster(cbd_scaled, file.path(out_dir,"canopy_bulk_density.tif"),overwrite=TRUE, gdal="COMPRESS=LZW")

plot(c(cbh, cbd_scaled), main=c("Canopy Base Height (m)","Canopy Bulk Density (kg/m3 x100)"))