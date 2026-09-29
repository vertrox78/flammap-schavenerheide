# ============================================================
#  FlamMap-Pipeline  |  Schritt 1b: CHM entspicken
#  Isolierte Ausreisser via Vergleich zum Nachbar-Median.
#  Nutzt nur native focal-Funktionen (stabil, ohne eigene
#  focal-Funktion, die die Session zum Absturz bringen kann).
#  Voraussetzung: terra
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")
chm <- rast(fp_datei(file.path(out_dir, "chm.tif"),
                     "Erst Schritt 01 ausfuehren."))

#  Ausreisser = deutlich ueber dem 99.9-%-Niveau UND absolut
#  groesser als dieser Sprung. Beide Groessen bestimmen das
#  Ergebnis und stehen daher hier benannt statt im Code vergraben.
DESPIKE_FENSTER  <- 5    # Kantenlaenge des Medianfensters (ungerade)
DESPIKE_MIN_SPRUNG <- 3  # m

# ------------------------------------------------------------
# 1) Nachbar-Median (native "median" statt eigener Funktion)
#    w = ungerade Fenstergroesse; fillvalue haelt Raender stabil
# ------------------------------------------------------------
med_nb <- focal(chm, w = DESPIKE_FENSTER, fun = "median",
                na.rm = TRUE, fillvalue = NA)

# 2) Differenz Zelle - Nachbar-Median
diff <- chm - med_nb

# ------------------------------------------------------------
# 3) Ausreisser-Schwelle datenbasiert aus der Differenz-Verteilung
#    (robust: hohe Perzentile der positiven Abweichungen)
# ------------------------------------------------------------
qs <- global(diff, fun=function(x) quantile(x, c(0.5,0.99,0.999), na.rm=TRUE))
q999 <- as.numeric(qs)[3]
cat("Differenz-Quantile (50/99/99.9%):", paste(round(as.numeric(qs),2),collapse=" / "), "m\n")

schwelle <- max(q999, DESPIKE_MIN_SPRUNG)
spike <- diff > schwelle
n_spike <- global(spike, "sum", na.rm=TRUE)[1,1]
cat("Erkannte Ausreisser-Zellen:", n_spike, "(Schwelle", round(schwelle,1), "m )\n")

# ------------------------------------------------------------
# 4) Nur Spikes durch Nachbar-Median ersetzen
# ------------------------------------------------------------
chm_clean <- ifel(spike, med_nb, chm)
chm_clean <- ifel(is.na(chm_clean), chm, chm_clean)   # Sicherheit

#  ACHTUNG: Dieses Skript UEBERSCHREIBT seine eigene Eingabe.
#  Ein zweiter Lauf entspickt also bereits entspickte Daten.
#  Das ist nahezu idempotent (nach dem ersten Durchgang gibt es
#  kaum noch Zellen ueber der Schwelle), aber nicht exakt --
#  bei Zweifeln Schritt 01 wiederholen.
writeRaster(chm_clean, file.path(out_dir, "chm.tif"),
            overwrite = TRUE, gdal = "COMPRESS=LZW")
plot(chm_clean, main="CHM entspickt (m)")