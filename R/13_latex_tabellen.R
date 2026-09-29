# ============================================================
#  FlamMap-Pipeline  |  Schritt 13: LaTeX-Tabellen erzeugen
#  Schreibt booktabs-Tabellen der Simulationsergebnisse als
#  .tex-Dateien (UTF-8, LF) zum Einbinden per \input{}.
#  Praeambel benoetigt: \usepackage{booktabs}
#
#  Voraussetzung: terra
#  Eingang : <flammap_dir>/Output/S*_Output.tif, data/fuel_model.tif
#  Ausgang : tex/tab_brandverhalten.tex, tab_schwellen.tex,
#            tab_kronenfeuer.tex
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")
fp_erfordere(out_root = out_root)

#  Kenngroessen der Tabelle tab_brandverhalten (Teilmenge von
#  EINHEITEN, ergaenzt um Spaltenkopf und Nachkommastellen)
TABELLEN <- list(
  "Flame Length"       = list(kopf = "Flammenlänge (m)", dez = 2),
  "Rate of Spread"     = list(kopf = "Ausbreitung (m\\,min$^{-1}$)", dez = 2),
  "Fireline Intensity" = list(kopf = "Intensität (kW\\,m$^{-1}$)", dez = 0)
)

#  NB8 (Moor) und NB9 (Offen) sind nicht brennbar und entfallen
BRENNBAR <- setdiff(FUEL_REIHENFOLGE, c("98", "99"))

# ------------------------------------------------------------
# 1) Raster laden, Gitter angleichen, Stichproben ziehen
# ------------------------------------------------------------
fuel <- rast(fp_datei(file.path(out_dir, "fuel_model.tif"),
                      "Erst Schritt 07 ausfuehren."))
ref  <- rast(fp_datei(file.path(out_root, paste0(SZENARIEN[1], "_Output.tif"))))
fuel <- fp_grid_angleichen(fuel, ref)

proben <- list()
for (sz in SZENARIEN) {
  r <- rast(fp_datei(file.path(out_root, paste0(sz, "_Output.tif"))))
  proben[[sz]] <- fp_stichprobe(c(fuel, r), spaltennamen = c("fuel", names(r)))
}
cat("Stichproben gezogen (je", nrow(proben[[1]]), "Zellen)\n\n")

#  Werte eines Bandes fuer ein Brennstoffmodell herausgreifen
hole <- function(sz, band, fc) {
  s <- proben[[sz]]
  v <- s[[band]][s$fuel == as.numeric(fc)]
  v[!is.na(v)]
}

#  Mediane eines Bandes: Zeilen = Brennstoffmodelle, Spalten = Szenarien
mediane <- function(band, modelle = BRENNBAR) {
  faktor <- EINHEITEN[[band]]$faktor
  m <- t(sapply(modelle, function(fc) sapply(SZENARIEN, function(sz) {
    v <- hole(sz, band, fc) * faktor
    if (!length(v)) NA else median(v)
  })))
  rownames(m) <- FUEL_NAMEN[modelle]
  m
}

# ------------------------------------------------------------
# 2) Brandverhalten: Mediane von Flammenlaenge, Ausbreitung und
#    Intensitaet in einer Tabelle
# ------------------------------------------------------------
werte <- lapply(names(TABELLEN), function(bn) {
  m <- mediane(bn)
  cat(EINHEITEN[[bn]]$kurz, "(Median):\n"); print(round(m, 2)); cat("\n")
  formatC(m, format = "f", digits = TABELLEN[[bn]]$dez)
})
zeilen <- sapply(seq_along(BRENNBAR), function(i)
  paste0(FUEL_NAMEN[BRENNBAR[i]], " & ",
         paste(unlist(lapply(werte, function(w) w[i, ])), collapse = " & ")))

#  Midflame-Wind geht in den Text, nicht in eine Tabelle
cat("MidflameWind (Median, km/h):\n")
print(round(mediane("Midflame Windspeed"), 2)); cat("\n")

fp_tabelle("tab_brandverhalten.tex",
  paste0("Brandverhalten je Brennstoffmodell und Szenario (Mediane): Flammenlänge, ",
         "Ausbreitungsgeschwindigkeit und Feuerlinienintensität."),
  "brandverhalten", "@{}lrrrrrrrrrrrr@{}",
  c(paste0("& ", paste0("\\multicolumn{4}{c}{", sapply(TABELLEN, `[[`, "kopf"), "}",
                        collapse = " & ")),
    "\\cmidrule(lr){2-5}\\cmidrule(lr){6-9}\\cmidrule(lr){10-13}",
    paste0("Brennstoffmodell & ", paste(rep(SZENARIEN, 3), collapse = " & "))),
  zeilen,
  vorspann = c("\\small", "\\setlength{\\tabcolsep}{3.5pt}"),
  fuss = paste0("Die nicht brennbaren Modelle NB8 (Moor) und NB9 (Offenland einschließlich ",
                "der Wege) sind nicht aufgeführt, da FlamMap für sie über alle Szenarien ",
                "hinweg kein Brandverhalten berechnet. Feuerlinienintensität und ",
                "Flammenlänge sind im Modell über eine feste Potenzbeziehung aneinander ",
                "gekoppelt ($I \\propto L^{2{,}17}$); einzig der Nadelwald in S4 weicht davon ",
                "ab, weil die Flammenlänge dort aus dem Kronenfeuermodell stammt ",
                "(Abschnitt~\\ref{sec:kronenfeuer})."))

# ------------------------------------------------------------
# 3) Flaechenanteile ueber den Flammenlaengen-Schwellen
# ------------------------------------------------------------
faktor_fl <- EINHEITEN[["Flame Length"]]$faktor
zeilen <- sapply(BRENNBAR, function(fc) {
  w <- sapply(SZENARIEN, function(sz) {
    v <- hole(sz, "Flame Length", fc) * faktor_fl
    if (!length(v)) c(NA, NA)
    else c(mean(v > FL_SCHWELLE[1]) * 100, mean(v > FL_SCHWELLE[2]) * 100)
  })
  paste0(FUEL_NAMEN[fc], " & ",
         paste(formatC(w[1, ], format = "f", digits = 1), collapse = " & "), " & ",
         paste(formatC(w[2, ], format = "f", digits = 1), collapse = " & "))
})

fp_tabelle("tab_schwellen.tex",
  "Flächenanteil je Brennstoffmodell oberhalb einsatzrelevanter Flammenlängen, in Prozent.",
  "schwellen", "lrrrrrrrr",
  c("Brennstoffmodell & \\multicolumn{4}{c}{$>1{,}2$\\,m} & \\multicolumn{4}{c}{$>3{,}4$\\,m}",
    "\\cmidrule(lr){2-5}\\cmidrule(lr){6-9}",
    " & S1 & S2 & S3 & S4 & S1 & S2 & S3 & S4"),
  zeilen,
  fuss = paste0("Schwellenwerte nach \\textcite{andrewsrothermel1982}: oberhalb von ",
                "1{,}2\\,m (4\\,ft) ist ein direkter Angriff mit Handkräften nicht mehr ",
                "möglich, oberhalb von 3{,}4\\,m (11\\,ft) sind Kontrollversuche an der ",
                "Feuerfront in der Regel wirkungslos."))

# ------------------------------------------------------------
# 4) Kronenfeuerklassen im Wald
# ------------------------------------------------------------
CFA_BEZ <- c("Oberflächenfeuer", "passives Kronenfeuer", "aktives Kronenfeuer")
WALD    <- c("186", "165")   # TL6 Laubwald, TU5 Nadelwald

zeilen <- unlist(lapply(WALD, function(fc) {
  sapply(seq_along(CFA_BEZ), function(kl) {
    a <- sapply(SZENARIEN, function(sz) {
      v <- hole(sz, "Crown Fire Activity", fc)
      if (!length(v)) NA else mean(v == kl) * 100
    })
    paste0(if (kl == 1) FUEL_NAMEN[fc] else "", " & ", CFA_BEZ[kl], " & ",
           paste(formatC(a, format = "f", digits = 1), collapse = " & "))
  })
}))

fp_tabelle("tab_kronenfeuer.tex",
  "Verteilung der Kronenfeuerklassen in den Waldflächen, Flächenanteil in Prozent.",
  "kronenfeuer", "llrrrr",
  "Brennstoffmodell & Klasse & S1 & S2 & S3 & S4",
  zeilen)

cat("\nTabellen liegen in:", tex_dir, "\n")
cat("Einbinden mit \\input{tex/tab_brandverhalten.tex} usw.\n")
cat("Praeambel: \\usepackage{booktabs}\n")
