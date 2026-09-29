# ============================================================
#  FlamMap-Pipeline  |  Schritt 12: Ergebnisstatistik
#  Liest die Multiband-GeoTIFFs (S1..S4_Output.tif) und wertet
#  alle Groessen je Brennstoffmodell aus. Rechnet die imperialen
#  FlamMap-Einheiten nach metrisch um.
#
#  Gruppiert wird nach der BRENNSTOFFMODELLKARTE, nicht nach der
#  Klassifikation: Sie ist exakt die Grundlage, auf der FlamMap
#  gerechnet hat, waehrend die Klassenkarte durch Aggregation und
#  Resampling an den Raendern davon abweicht.
#
#  Voraussetzung: terra
#  Eingang : <flammap_dir>/Output/S*_Output.tif, data/fuel_model.tif
#  Ausgang : Konsolenausgabe + data/flammap_statistik.csv
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")
fp_erfordere(out_root = out_root)

# ------------------------------------------------------------
# 1) Brennstoffkarte laden und aufs FlamMap-Gitter bringen
# ------------------------------------------------------------
cls <- rast(fp_datei(file.path(out_dir, "fuel_model.tif"),
                     "Erst Schritt 07 ausfuehren."))
cat("Gruppierung nach: fuel_model.tif\n")

ref <- rast(fp_datei(file.path(out_root, paste0(SZENARIEN[1], "_Output.tif"))))
cls <- fp_grid_angleichen(cls, ref)
cat("\n")

# ------------------------------------------------------------
# 2) Je Szenario alle Baender auswerten
# ------------------------------------------------------------
erg <- data.frame()
proben <- list()

for (sz in SZENARIEN) {
  f <- file.path(out_root, paste0(sz, "_Output.tif"))
  if (!file.exists(f)) { cat("fehlt:", f, "\n"); next }
  r <- rast(f)
  cat("Verarbeite", sz, "-", nlyr(r), "Baender\n")

  c2 <- fp_grid_angleichen(cls, r, leise = TRUE)
  s  <- fp_stichprobe(c(c2, r), spaltennamen = c("klasse", names(r)))
  proben[[sz]] <- s

  for (bn in names(r)) {
    if (is.null(EINHEITEN[[bn]])) next
    v_all <- s[[bn]] * EINHEITEN[[bn]]$faktor
    for (kl in sort(unique(s$klasse))) {
      v <- v_all[s$klasse == kl]
      v <- v[!is.na(v)]
      if (!length(v)) next
      erg <- rbind(erg, data.frame(
        Szenario = sz,
        Groesse  = EINHEITEN[[bn]]$kurz,
        Einheit  = EINHEITEN[[bn]]$einheit,
        Klasse   = FUEL_NAMEN[as.character(kl)],
        Median   = round(median(v), 2),
        P95      = round(as.numeric(quantile(v, .95)), 2),
        Max      = round(max(v), 2)))
    }
  }
}
stopifnot(length(proben) > 0)

# ------------------------------------------------------------
# 3) Kreuztabellen je Groesse (Medianwerte)
# ------------------------------------------------------------
for (g in unique(erg$Groesse)) {
  e <- erg[erg$Groesse == g, ]
  cat("\n=========================================================\n")
  cat(g, " (Median, ", e$Einheit[1], ")\n", sep = "")
  cat("=========================================================\n")
  tab <- reshape(e[, c("Klasse", "Szenario", "Median")],
                 idvar = "Klasse", timevar = "Szenario", direction = "wide")
  names(tab) <- sub("Median\\.", "", names(tab))
  print(tab, row.names = FALSE)
}

# ------------------------------------------------------------
# 4) Operative Schwellen der Flammenlaenge
#    1,2 m = Grenze Handkraefte | 3,4 m = Kontrolle wirkungslos
# ------------------------------------------------------------
cat("\n=========================================================\n")
cat("FLAECHENANTEIL UEBER FLAMMENLAENGEN-SCHWELLEN (%)\n")
cat("=========================================================\n")
for (sz in names(proben)) {
  s <- proben[[sz]]
  fl <- s[["Flame Length"]] * EINHEITEN[["Flame Length"]]$faktor
  cat("\n", sz, ":\n", sep = "")
  for (kl in sort(unique(s$klasse))) {
    v <- fl[s$klasse == kl]; v <- v[!is.na(v)]
    if (!length(v)) next
    cat(sprintf("  %-18s >%.1f m: %5.1f %%   >%.1f m: %5.1f %%\n",
                FUEL_NAMEN[as.character(kl)],
                FL_SCHWELLE[1], mean(v > FL_SCHWELLE[1]) * 100,
                FL_SCHWELLE[2], mean(v > FL_SCHWELLE[2]) * 100))
  }
}

# ------------------------------------------------------------
# 5) Kronenfeuer-Klassen je Waldtyp (Flaechenanteile)
# ------------------------------------------------------------
cat("\n=========================================================\n")
cat("KRONENFEUER-KLASSEN IM WALD (Flaechenanteil %)\n")
cat("1=Oberflaechenfeuer  2=passiv  3=aktiv\n")
cat("=========================================================\n")
WALD <- c(186, 165)   # TL6 Laubwald, TU5 Nadelwald
for (sz in names(proben)) {
  s <- proben[[sz]]
  cfa <- s[["Crown Fire Activity"]]
  cat("\n", sz, ":\n", sep = "")
  for (kl in WALD) {
    v <- cfa[s$klasse == kl]; v <- v[!is.na(v)]
    if (!length(v)) next
    cat(sprintf("  %-18s K1: %5.1f %%  K2: %5.1f %%  K3: %5.1f %%\n",
                FUEL_NAMEN[as.character(kl)],
                mean(v == 1) * 100, mean(v == 2) * 100, mean(v == 3) * 100))
  }
}

write.csv(erg, file.path(out_dir, "flammap_statistik.csv"), row.names = FALSE)
cat("\nGespeichert: data/flammap_statistik.csv\n")
