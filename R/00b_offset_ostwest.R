# ============================================================
#  FlamMap-Pipeline  |  Schritt 0b: Hoehenversatz Ost/West messen
#  Vergleicht die RGB-DSM Ost/West in der Ueberlappungszone.
#  Ergebnis (OST_OFFSET) wird in Schritt 1 zur Korrektur genutzt.
#  Voraussetzung: terra
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")
fp_erfordere(root = root)

DSM_OST  <- "HEA-DE-NRW-20230911-SCHAVENERHEIDEOST-RGB-DSM-V01.tif"
DSM_WEST <- "HEA-DE-NRW-20230911-SCHAVENERHEIDEWEST-RGB-DSM-V01.tif"

dsm_ost  <- rast(fp_datei(file.path(root, DSM_OST)))
dsm_west <- rast(fp_datei(file.path(root, DSM_WEST)))

# Ueberlappungszone: Ost beginnt bei X=335800, West endet bei X=336169.
# Der gewaehlte Ausschnitt liegt sicher innerhalb beider Kacheln.
OVERLAP <- ext(335820, 336150, 5609300, 5610400)
ov <- OVERLAP
o <- crop(dsm_ost, ov)
w <- resample(crop(dsm_west, ov), o, method="bilinear")

diff <- o - w
md <- global(diff, fun=function(x) median(x, na.rm=TRUE))[1,1]
qs <- global(diff, fun=function(x) quantile(x, c(.1,.25,.5,.75,.9), na.rm=TRUE))

cat("=========================================\n")
cat("Median-Versatz Ost-West (DSM):", round(md,3), "m\n")
cat("In R/00_config.R hinterlegt (OST_OFFSET):", OST_OFFSET, "m\n")
cat("Quantile (10/25/50/75/90%):", paste(round(as.numeric(qs),2),collapse=" / "), "\n")
cat("=========================================\n")

plot(diff, main=paste0("DSM-Differenz Ost-West (Median ", round(md,2)," m)"))