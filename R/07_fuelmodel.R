# ============================================================
#  FlamMap-Pipeline  |  Schritt 7: Fuel-Model-Karte (final)
#  Mix-Ansatz: Heide = Custom GA-heath (89), Rest = Standard
#  Scott & Burgan. Aggregiert zugleich aufs FlamMap-Grid
#  (majority = haeufigste Klasse je Zelle).
#  Voraussetzung: terra
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")

cls <- rast(fp_datei(file.path(out_dir, "vegetation_classified.tif"),
                     "Erst Schritt 05b ausfuehren."))
cc  <- rast(fp_datei(file.path(out_dir, "canopy_cover.tif"),
                     "Erst Schritt 06 ausfuehren."))   # FlamMap-Grid

# ------------------------------------------------------------
# 1) Klassen -> Fuel-Model-Codes
#    1 Heide     ->  89  (Custom GA-heath, .FMD)
#    2 Gras      -> 104  (GR4, magere Grasflur)
#    3 Laubwald  -> 186  (TL6)
#    4 Nadelwald -> 165  (TU5)
#    5 Moor/Nass ->  98  (NB8, nicht brennbar)
#    6 Offen     ->  99  (NB9)
# ------------------------------------------------------------
#  Zuordnung steht zentral in R/00_config.R (FUEL_MAP).
rcl <- cbind(as.integer(names(FUEL_MAP)), unname(FUEL_MAP))
fuel_fine <- classify(cls, rcl)

# ------------------------------------------------------------
# 2) Aufs FlamMap-Grid aggregieren -- MAJORITY (haeufigste Klasse)
#    "modal" auf dem feinen Raster, dann aufs cc-Grid ausrichten
# ------------------------------------------------------------
fact <- round(res(cc)[1] / res(fuel_fine)[1])   # richtet sich nach cc-Grid
fuel <- aggregate(fuel_fine, fact = fact, fun = "modal", na.rm = TRUE)
fuel <- resample(fuel, cc, method = "near")     # exakt aufs FlamMap-Grid
names(fuel) <- "fuel_model"

writeRaster(fuel, file.path(out_dir, "fuel_model.tif"),
            overwrite=TRUE, gdal="COMPRESS=LZW", datatype="INT2S")

# ------------------------------------------------------------
# 3) Plausibilitaet: Flaechenanteile
# ------------------------------------------------------------
f <- freq(fuel); f$anteil <- round(f$count/sum(f$count)*100,1)
f$modell <- FUEL_NAMEN[as.character(f$value)]
print(f[,c("modell","value","count","anteil")], row.names=FALSE)

plot(fuel, main = paste0("Fuel Model (FlamMap-Grid, ", FLAMMAP_RES, " m)"))