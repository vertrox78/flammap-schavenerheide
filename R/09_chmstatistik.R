# ============================================================
#  FlamMap-Pipeline  |  Schritt 9: CHM-Statistik je Klasse
#  Misst die reale Vegetationshoehe pro Vegetationsklasse.
#  Dient als (a) Vergleich zu Literatur-Fuel-Models (Diskussion)
#  und (b) Grundlage fuer Canopy-Layer (Wald).
#  Voraussetzung: terra
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")

cls <- rast(fp_datei(file.path(out_dir, "vegetation_classified.tif"),
                     "Erst Schritt 05b ausfuehren."))
# chm_clip.tif entsteht in Schritt 3; falls nicht vorhanden, direkt das
# entspickte chm.tif nehmen -- die Klassenkarte maskiert ohnehin auf das AOI.
chm_file <- file.path(out_dir, "chm_clip.tif")
if (!file.exists(chm_file)) chm_file <- file.path(out_dir, "chm.tif")
cat("CHM-Quelle:", basename(chm_file), "\n")
chm <- rast(chm_file)
if (!compareGeom(chm, cls, stopOnError=FALSE, messages=FALSE))
  chm <- resample(chm, cls, method = "bilinear")

klassen <- KLASSEN   # zentral in R/00_config.R

# Stichprobe (schnell + voellig ausreichend fuer Statistik)
samp <- spatSample(c(cls, chm), size = 1000000, method = "regular",
                   na.rm = TRUE, as.df = TRUE)
names(samp) <- c("class", "chm")

cat("CHM-Hoehenstatistik je Klasse (m), n =", nrow(samp), "Zellen:\n\n")
res <- do.call(rbind, lapply(sort(unique(samp$class)), function(k){
  v <- samp$chm[samp$class == k]
  data.frame(Klasse = klassen[as.character(k)],
             n      = length(v),
             Mittel = round(mean(v), 2),
             Median = round(median(v), 2),
             P95    = round(quantile(v, 0.95), 2),
             SD     = round(sd(v), 2))
}))
print(res, row.names = FALSE)