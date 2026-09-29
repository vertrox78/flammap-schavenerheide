# ============================================================
#  FlamMap-Pipeline  |  Schritt 16: MTT-Barriere-Auswertung
#  Ergaenzende Analyse im Minimum-Travel-Time-Modus (Szenario S3):
#  vergleicht zwei Ankunftszeit-Raster (ohne / mit simulierter
#  Riegelstellung) und quantifiziert die durch die Barriere
#  geschuetzte Flaeche, aufgeschluesselt nach Brennstoffmodell
#  und im Zeitverlauf. Schreibt Tabellen nach tex/ (\input{}) und
#  eine Wirkungskarte nach data/ zur Weiterverarbeitung in
#  14_abbildungen.R.
#
#  Voraussetzung: terra
#  Eingang : <flammap_dir>/Output/S3_MTT_Output_{1,2}.tif,
#            data/fuel_model.tif
#  Ausgang : tex/tab_mtt_barriere.tex, tex/tab_mtt_fuel.tex,
#            data/mtt_barriere_wirkung.tif
# ------------------------------------------------------------

suppressMessages(library(terra))
source("R/00_config.R")
fp_erfordere(out_root = out_root)

#  Vektordaten des MTT-Laufs (nur zur Dokumentation der Eingaben; die
#  Auswertung unten arbeitet ausschliesslich auf den Ankunftszeit-Rastern).
#  Die Riegellinie ist die in QGIS digitalisierte Polylinie (EPSG:4326),
#  die in FlamMap ueber "Open Barriers File" geladen wurde.
ign_shp  <- file.path(flammap_dir, MTT_IGNITION)
barr_shp <- file.path(flammap_dir, MTT_BARRIERE)

f_ohne <- fp_datei(file.path(out_root, "S3_MTT_Output_1.tif"))  # MTT3_1: ohne Barriere
f_mit  <- fp_datei(file.path(out_root, "S3_MTT_Output_2.tif"))  # MTT3_2: mit Riegelstellung

# ------------------------------------------------------------
# 1) Ankunftszeit-Band laden ("MTT Arrival Time", in Minuten)
# ------------------------------------------------------------
a_ohne <- rast(f_ohne)[["MTT Arrival Time"]]
a_mit  <- rast(f_mit)[["MTT Arrival Time"]]
fuel   <- rast(fp_datei(file.path(out_dir, "fuel_model.tif"),
                        "Erst Schritt 07 ausfuehren."))

# Gitter angleichen (FlamMap-Eckenrundung, s. Skript 12/13)
if (!compareGeom(a_ohne, a_mit, stopOnError = FALSE, messages = FALSE))
  a_mit <- resample(a_mit, a_ohne, method = "near")
fuel <- fp_grid_angleichen(fuel, a_ohne)

# ------------------------------------------------------------
# 2) Erreichte Flaeche = Zelle hat eine (endliche) Ankunftszeit
#    (nicht erreichte Zellen sind NA oder < 0)
# ------------------------------------------------------------
erreicht_ohne <- !is.na(a_ohne) & a_ohne >= 0
erreicht_mit  <- !is.na(a_mit)  & a_mit  >= 0

zellflaeche <- prod(res(a_ohne))          # m2 pro Zelle (1x1 = 1)
ha <- function(mask) global(mask, "sum", na.rm = TRUE)[1, 1] * zellflaeche / 10000

flaeche_ohne  <- ha(erreicht_ohne)
flaeche_mit   <- ha(erreicht_mit)
geschuetzt    <- erreicht_ohne & !erreicht_mit   # ohne erreicht, mit nicht
flaeche_gesch <- ha(geschuetzt)

cat("=========================================================\n")
cat("BARRIERE-WIRKUNG (Szenario S3, MTT)\n")
cat("=========================================================\n")
cat(sprintf("Erreichte Flaeche ohne Barriere:   %6.1f ha\n", flaeche_ohne))
cat(sprintf("Erreichte Flaeche mit  Barriere:   %6.1f ha\n", flaeche_mit))
cat(sprintf("Verhinderte (geschuetzte) Flaeche: %6.1f ha  (%.1f %%)\n",
            flaeche_gesch, flaeche_gesch / flaeche_ohne * 100))

# ------------------------------------------------------------
# 3) Flaechenbilanz je Brennstoffmodell
#    Bestand = im Gebiet vorhandene Flaeche des Modells;
#    erreicht ohne/mit = davon vom Feuer erreicht;
#    geschuetzt = Differenz.
# ------------------------------------------------------------
zonal_ha <- function(mask) {
  z <- zonal(mask, fuel, fun = "sum", na.rm = TRUE)
  stats::setNames(z[[2]] * zellflaeche / 10000, as.character(z[[1]]))
}
bestand <- zonal_ha(!is.na(fuel))
b_ohne  <- zonal_ha(erreicht_ohne)
b_mit   <- zonal_ha(erreicht_mit)
b_gesch <- zonal_ha(geschuetzt)

codes <- names(bestand)
z <- data.frame(
  code    = codes,
  klasse  = FUEL_NAMEN[codes],
  bestand = as.numeric(bestand[codes]),
  ohne    = as.numeric(b_ohne[codes]),
  mit     = as.numeric(b_mit[codes]),
  gesch   = as.numeric(b_gesch[codes]),
  stringsAsFactors = FALSE
)
z$anteil    <- z$gesch / flaeche_gesch * 100     # Anteil an der geschuetzten Flaeche
z$pct_ohne  <- z$ohne / z$bestand * 100          # verbrannter Bestandsanteil ohne
z$pct_mit   <- z$mit  / z$bestand * 100          # verbrannter Bestandsanteil mit
z <- z[!is.na(z$klasse) & z$gesch > 0, ]
z <- z[order(-z$gesch), ]

cat("\nFlaechenbilanz nach Brennstoffmodell (ha):\n")
print(data.frame(Klasse = z$klasse, Bestand = round(z$bestand, 2),
                 Ohne = round(z$ohne, 2), Mit = round(z$mit, 2),
                 Geschuetzt = round(z$gesch, 2), Anteil = round(z$anteil, 1),
                 Bestand_ohne_pct = round(z$pct_ohne),
                 Bestand_mit_pct = round(z$pct_mit)),
      row.names = FALSE)

# ------------------------------------------------------------
# 4) Zeitlicher Verlauf: erreichte Flaeche je Intervall
# ------------------------------------------------------------
cat("\nErreichte Flaeche im Zeitverlauf (ha):\n")
cat(sprintf("%8s %12s %12s\n", "Minute", "ohne Barr.", "mit Barr."))
maxt <- ceiling(max(global(a_ohne, "max", na.rm = TRUE)[1, 1],
                    global(a_mit,  "max", na.rm = TRUE)[1, 1], na.rm = TRUE) /
                  MTT_INTERVALL) * MTT_INTERVALL
zeitreihe <- data.frame(t = seq(MTT_INTERVALL, maxt, by = MTT_INTERVALL))
zeitreihe$ohne <- sapply(zeitreihe$t, function(t) ha(erreicht_ohne & a_ohne <= t))
zeitreihe$mit  <- sapply(zeitreihe$t, function(t) ha(erreicht_mit  & a_mit  <= t))
for (i in seq_len(nrow(zeitreihe)))
  cat(sprintf("%8d %12.1f %12.1f\n", zeitreihe$t[i], zeitreihe$ohne[i], zeitreihe$mit[i]))
write.csv(zeitreihe, file.path(out_dir, "mtt_zeitreihe.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 5) Kennzahlen zum Ende des Simulationsfensters
#
#    "Ende des Flaechenwachstums" = Quantil MTT_STILLSTAND_Q der
#    Ankunftszeiten. Die letzten Promille der Zellen sind einzelne
#    Nachzuegler weit hinter der Front; das Quantil beschreibt den
#    Stillstand daher treffender als das Maximum.
# ------------------------------------------------------------
ankunft <- function(x, mask) {
  v <- values(ifel(mask, x, NA))
  v[!is.na(v)]
}
v_ohne <- ankunft(a_ohne, erreicht_ohne)
v_mit  <- ankunft(a_mit,  erreicht_mit)

stillstand_mit  <- as.numeric(quantile(v_mit,  MTT_STILLSTAND_Q))
stillstand_ohne <- as.numeric(quantile(v_ohne, MTT_STILLSTAND_Q))
letzte_mit      <- max(v_mit)
letzte_ohne     <- max(v_ohne)

#  Wachsen die Laeufe am Fensterende noch?
wuchs_ohne <- flaeche_ohne - ha(erreicht_ohne & a_ohne <= (MTT_FENSTER - 10))
wuchs_mit  <- flaeche_mit  - ha(erreicht_mit  & a_mit  <= (MTT_FENSTER - 10))

#  Ein Lauf gilt als am Fensterende noch wachsend, wenn in den
#  letzten zehn Minuten noch nennenswert Flaeche hinzukam. Das ist
#  robuster als ein Vergleich der letzten Zellankunft mit
#  MTT_FENSTER: die Ankunftszeiten erreichen den Fensterrand nur
#  auf Rundung genau.
offen_ohne <- wuchs_ohne > 0.1   # ha
offen_mit  <- wuchs_mit  > 0.1   # ha

#  Vergleichszeitpunkt: Stillstand des Barrierelaufs; waechst auch
#  dieser bis zum Fensterende, die Fenstermitte.
t_ref <- if (offen_mit) MTT_FENSTER / 2 else
  floor(stillstand_mit / MTT_INTERVALL) * MTT_INTERVALL
flaeche_ohne_tref <- ha(erreicht_ohne & a_ohne <= t_ref)
flaeche_mit_tref  <- ha(erreicht_mit  & a_mit  <= t_ref)

cat(sprintf("\nEnde des Flaechenwachstums (Q%.1f%%): ohne %.0f min | mit %.0f min\n",
            MTT_STILLSTAND_Q * 100, stillstand_ohne, stillstand_mit))
cat(sprintf("Letzte Zellankunft: ohne %.1f min | mit %.1f min\n", letzte_ohne, letzte_mit))
cat(sprintf("Zuwachs in den letzten 10 min: ohne %.1f ha | mit %.1f ha\n", wuchs_ohne, wuchs_mit))

# ------------------------------------------------------------
# 5b) Raeumliche Aufteilung an der Laengsachse des Zuendpunkts
#
#     "Westlich" heisst: Zellmittelpunkt links der Ostkoordinate
#     des Zuendpunkts.
# ------------------------------------------------------------
west_kennzahlen <- NULL
if (file.exists(ign_shp)) {
  ign  <- project(vect(ign_shp), crs(a_ohne))
  x_zuend <- terra::crds(ign)[1, 1]
  west <- init(a_ohne, "x") < x_zuend

  w_ohne  <- ha(erreicht_ohne & west)
  w_mit   <- ha(erreicht_mit  & west)
  w_gesch <- ha(geschuetzt    & west)
  v_west  <- ankunft(a_ohne, erreicht_ohne & west)

  west_kennzahlen <- data.frame(
    groesse = c("x_zuendpunkt", "flaeche_west_ohne_ha", "flaeche_west_mit_ha",
                "flaeche_west_geschuetzt_ha", "anteil_west_an_geschuetzt_pct",
                "ankunft_west_mittel_min", "ankunft_west_median_min"),
    wert = round(c(x_zuend, w_ohne, w_mit, w_gesch,
                   w_gesch / flaeche_gesch * 100,
                   mean(v_west), median(v_west)), 2))

  cat("\nWestlich des Zuendpunkts (X <", round(x_zuend, 1), "):\n")
  cat(sprintf("  ohne Barriere      : %6.1f ha\n", w_ohne))
  cat(sprintf("  mit  Barriere      : %6.1f ha\n", w_mit))
  cat(sprintf("  geschuetzt         : %6.1f ha  (%.0f %% der geschuetzten Flaeche)\n",
              w_gesch, w_gesch / flaeche_gesch * 100))
  cat(sprintf("  Ankunftszeit ohne  : Mittel %.0f min | Median %.0f min\n",
              mean(v_west), median(v_west)))

  # ----------------------------------------------------------
  # 5c) Aufteilung entlang der Hauptlaufachse
  #
  #     Fast der gesamte Brand liegt nordoestlich des Zuendpunkts;
  #     eine Quadranten-Aufteilung sagt daher nur, wo der Brand
  #     liegt, nicht wo die Barriere bremst. Stattdessen wird die
  #     geschuetzte Flaeche in ein achsenparalleles System gedreht:
  #     Achse = Richtung vom Zuendpunkt zum Flaechenschwerpunkt des
  #     Laufs ohne Barriere; u = Abstand entlang der Achse,
  #     v = quer dazu (v < 0 links = nordwestliche Flanke).
  #       Spitze : u jenseits der Front des Laufs mit Barriere
  #       Flanken: 0 <= u <= Front mit Barriere
  #       hinten : u < 0 (gegen den Wind hinter dem Zuendpunkt)
  # ----------------------------------------------------------
  y_zuend <- terra::crds(ign)[1, 2]
  dx <- init(a_ohne, "x") - x_zuend
  dy <- init(a_ohne, "y") - y_zuend
  schwer <- function(r, mask) global(ifel(mask, r, NA), "mean", na.rm = TRUE)[1, 1]
  th <- atan2(schwer(dx, erreicht_ohne), schwer(dy, erreicht_ohne))  # Azimut
  u  <- dx * sin(th) + dy * cos(th)
  v  <- dx * cos(th) - dy * sin(th)
  u_front_mit  <- global(ifel(erreicht_mit,  u, NA), "max", na.rm = TRUE)[1, 1]
  u_front_ohne <- global(ifel(erreicht_ohne, u, NA), "max", na.rm = TRUE)[1, 1]

  a_spitze  <- ha(geschuetzt & u > u_front_mit)
  a_hinten  <- ha(geschuetzt & u < 0)
  flanke    <- geschuetzt & u >= 0 & u <= u_front_mit
  a_flanke  <- ha(flanke)
  a_fl_nw   <- ha(flanke & v < 0)
  a_fl_so   <- ha(flanke & v >= 0)

  achse_kennzahlen <- data.frame(
    groesse = c("achse_azimut_grad", "front_ohne_m", "front_mit_m",
                "geschuetzt_spitze_ha", "geschuetzt_flanken_ha",
                "anteil_flanken_pct", "geschuetzt_flanke_nw_ha",
                "geschuetzt_flanke_so_ha", "geschuetzt_hinten_ha",
                "anteil_hinten_pct"),
    wert = round(c((th * 180 / pi) %% 360, u_front_ohne, u_front_mit,
                   a_spitze, a_flanke, a_flanke / flaeche_gesch * 100,
                   a_fl_nw, a_fl_so, a_hinten,
                   a_hinten / flaeche_gesch * 100), 2))
  west_kennzahlen <- rbind(west_kennzahlen, achse_kennzahlen)

  cat(sprintf("\nEntlang der Hauptlaufachse (Azimut %.0f Grad):\n", (th * 180 / pi) %% 360))
  cat(sprintf("  Front vom Zuendpunkt: ohne %.0f m | mit %.0f m\n", u_front_ohne, u_front_mit))
  cat(sprintf("  geschuetzt Spitze   : %6.2f ha  (%.1f %%)\n", a_spitze, a_spitze / flaeche_gesch * 100))
  cat(sprintf("  geschuetzt Flanken  : %6.2f ha  (%.1f %%)  NW %.2f | SO %.2f ha\n",
              a_flanke, a_flanke / flaeche_gesch * 100, a_fl_nw, a_fl_so))
  cat(sprintf("  geschuetzt hinten   : %6.2f ha  (%.1f %%)\n", a_hinten, a_hinten / flaeche_gesch * 100))
} else {
  cat("\nZuendpunkt-Shapefile nicht gefunden -> West/Ost- und Achsenaufteilung uebersprungen.\n")
}

# ------------------------------------------------------------
# 6) LaTeX-Tabellen schreiben
# ------------------------------------------------------------
zeile <- function(label, v1, v2)
  paste0(fp_pad(label, 32), " & ", fp_pad(v1, 17), " & ", v2)

zeilen_haupt <- c(
  zeile("Erreichte Fläche (ha)",
        fp_zahl(flaeche_ohne), fp_zahl(flaeche_mit)),
  zeile(paste0("Fläche nach ", t_ref, "\\,min (ha)"),
        fp_zahl(flaeche_ohne_tref), fp_zahl(flaeche_mit_tref)),
  zeile("Ende des Flächenwachstums (min)",
        if (offen_ohne) "nicht erreicht" else fp_zahl(stillstand_ohne, 0),
        if (offen_mit)  "nicht erreicht" else fp_zahl(stillstand_mit, 0)))

satz_mit <- if (offen_mit) {
  paste0("Beide Läufe wuchsen bis zum Ende des Simulationsfensters weiter (in den letzten ",
         "10\\,min +", fp_zahl(wuchs_ohne), "\\,ha ohne und +", fp_zahl(wuchs_mit),
         "\\,ha mit Barriere). ")
} else {
  paste0("Der Lauf mit Barriere kam nach rund ", fp_zahl(stillstand_mit, 0),
         "\\,min zum Stillstand (letzte Zellankunft ", fp_zahl(letzte_mit), "\\,min), der Lauf ",
         "ohne Barriere wuchs bis zum Ende des Simulationsfensters weiter (+",
         fp_zahl(wuchs_ohne), "\\,ha allein in den letzten 10\\,min). ")
}

fuss_haupt <- paste0(
  "Verhinderte (geschützte) Fläche: ", fp_zahl(flaeche_gesch), "\\,ha (",
  fp_zahl(flaeche_gesch / flaeche_ohne * 100), "\\,\\% der ohne Barriere erreichten ",
  "Fläche). ", satz_mit, "Die angegebene ",
  "geschützte Fläche ist daher an das ", MTT_FENSTER,
  "-min-Fenster gebunden.")

fp_tabelle("tab_mtt_barriere.tex",
  paste0("Wirkung der simulierten Riegelstellung auf die im MTT-Modus erreichte Fläche, ",
         "Szenario S3 (Simulationsfenster ", MTT_FENSTER, "\\,min)."),
  "mtt-barriere", "lrr",
  " & ohne Barriere & mit Barriere",
  zeilen_haupt,
  fuss = fuss_haupt)

zeile_fuel <- function(label, ohne, mit, gesch, anteil)
  paste0(fp_pad(label, 16), " & ", fp_pad(ohne, 7, rechts = TRUE),
         " & ", fp_pad(mit, 7, rechts = TRUE),
         " & ", fp_pad(gesch, 7, rechts = TRUE),
         " & ", fp_pad(anteil, 6, rechts = TRUE))

zeilen_fuel <- c(
  zeile_fuel(z$klasse, fp_zahl(z$ohne, 2), fp_zahl(z$mit, 2),
             fp_zahl(z$gesch, 2), fp_zahl(z$anteil)),
  paste0("\\midrule\n    ",
         zeile_fuel("Summe", fp_zahl(sum(z$ohne), 2), fp_zahl(sum(z$mit), 2),
                    fp_zahl(sum(z$gesch), 2), fp_zahl(sum(z$anteil)))))

#  Praepositionale Form je Brennstoffmodell, damit der Fusstext
#  lesbares Deutsch ergibt ("in der Heide", "im Gras", ...).
DATIV <- c("89" = "in der Heide", "104" = "im Gras",
           "186" = "im Laubwald", "165" = "im Nadelwald",
           "98" = "im Moor", "99" = "im Offenland")

#  Feste Reihenfolge nach aufsteigendem Brennstoffcode -- unabhaengig
#  davon, wie die Tabelle oben nach Flaeche sortiert ist.
z_fuss <- z[order(as.numeric(z$code)), ]
teile  <- sprintf("%s von %.0f auf %.0f\\,\\%%",
                  DATIV[z_fuss$code], z_fuss$pct_ohne, z_fuss$pct_mit)
aufzaehlung <- if (length(teile) > 1) {
  paste0(paste(utils::head(teile, -1), collapse = ", "), " und ", utils::tail(teile, 1))
} else {
  teile
}

fuss_fuel <- paste0(
  "Die Prozentangabe bezeichnet den Anteil des jeweiligen Brennstoffmodells an der ",
  "insgesamt geschützten Fläche. Bezogen auf den im Untersuchungsgebiet vorhandenen ",
  "Bestand sinkt der verbrannte Flächenanteil durch die Riegelstellung ",
  aufzaehlung, ".")

fp_tabelle("tab_mtt_fuel.tex",
  "Durch die Riegelstellung geschützte Fläche je Brennstoffmodell, Szenario S3.",
  "mtt-fuel", "lrrrr",
  c("Brennstoffmodell & \\multicolumn{2}{c}{erreichte Fläche (ha)} & \\multicolumn{2}{c}{geschützt}",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
    " & ohne Barriere & mit Barriere & (ha) & (\\%)"),
  zeilen_fuel,
  fuss = fuss_fuel)

# ------------------------------------------------------------
# 7) Wirkungskarte fuer 14_abbildungen.R / QGIS speichern
#    0 = nie erreicht | 1 = beide erreicht | 2 = nur ohne Barriere
# ------------------------------------------------------------
wirkung <- erreicht_ohne + erreicht_mit          # 0/1/2
wirkung[erreicht_ohne & !erreicht_mit] <- 2      # geschuetzt explizit = 2
wirkung[erreicht_ohne &  erreicht_mit] <- 1
wirkung[!erreicht_ohne] <- 0
names(wirkung) <- "barriere_wirkung"
writeRaster(wirkung, file.path(out_dir, "mtt_barriere_wirkung.tif"),
            overwrite = TRUE, datatype = "INT1U", gdal = "COMPRESS=LZW")

#  Alle Textkennzahlen gesammelt ablegen, damit sich die im
#  Fliesstext genannten Werte ohne Handarbeit abgleichen lassen.
kennzahlen <- data.frame(
  groesse = c("flaeche_ohne_ha", "flaeche_mit_ha", "flaeche_geschuetzt_ha",
              "anteil_geschuetzt_pct", "stillstand_mit_min", "stillstand_ohne_min",
              "letzte_ankunft_mit_min", "letzte_ankunft_ohne_min",
              "zuwachs_ohne_letzte_10min_ha", "zuwachs_mit_letzte_10min_ha", "t_referenz_min",
              "flaeche_ohne_bei_tref_ha", "flaeche_mit_bei_tref_ha"),
  wert = round(c(flaeche_ohne, flaeche_mit, flaeche_gesch,
                 flaeche_gesch / flaeche_ohne * 100,
                 stillstand_mit, stillstand_ohne, letzte_mit, letzte_ohne,
                 wuchs_ohne, wuchs_mit, t_ref, flaeche_ohne_tref, flaeche_mit_tref), 2))
if (!is.null(west_kennzahlen)) kennzahlen <- rbind(kennzahlen, west_kennzahlen)
write.csv(kennzahlen, file.path(out_dir, "mtt_kennzahlen.csv"), row.names = FALSE)
cat("\nTextkennzahlen gespeichert: data/mtt_kennzahlen.csv\n")

cat("Wirkungskarte gespeichert: data/mtt_barriere_wirkung.tif\n")
cat("  0 = nie erreicht, 1 = trotz Barriere erreicht, 2 = durch Barriere geschuetzt\n")
cat("Tabellen liegen in:", tex_dir, "\n")
