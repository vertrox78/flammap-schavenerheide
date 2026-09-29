# ============================================================
#  FlamMap-Pipeline  |  Schritt 14: Abbildungen fuer die Thesis
#  Rendert die Karten (Hoehenmodelle, Klassifikation, Fuel-Model,
#  Vegetationsindizes, Simulationsergebnisse) als PDF nach figures/.
#  Einbinden per \includegraphics[width=\linewidth]{figures/<name>.pdf}.
#
#  Ausgabeformat PDF: Beschriftungen, Achsen und Legenden bleiben
#  Vektorgrafik und damit im Druck scharf. Der cairo-PNG-Treiber glaettet
#  Text unter Windows mit Subpixel-Antialiasing, was farbige Saeume an den
#  Beschriftungen erzeugt; der antialias-Parameter von png() greift dagegen
#  in diesem Build nicht. Die Rasterinhalte selbst werden weiterhin ueber
#  MAXCELL heruntergerechnet, die Dateien bleiben dadurch klein.
#
#  EINHEITLICHKEIT -- alle Karten teilen sich:
#   * dieselbe Bildbreite (FIG_W_IN); da alle Abbildungen im Dokument auf
#     \linewidth skaliert werden, erscheinen Schriften und Linien dadurch
#     ueberall gleich gross,
#   * dieselben Raender, Achsen- und Legendenparameter (THEME),
#   * dieselbe Leiste aus Massstab (fest 500 m) und Nordpfeil,
#   * eine aus dem Kartenformat berechnete Bildhoehe, sodass keine
#     leeren Raender entstehen,
#   * KEINEN eingebrannten Titel -- die Beschriftung liefert die
#     LaTeX-\caption. Nur Teilkarten tragen kurze Panel-Label.
#  Voraussetzung: terra
# ------------------------------------------------------------

suppressMessages(library(terra))
source("R/00_config.R")
fp_erfordere(out_root = out_root)

#  fig_dir, out_root und SZENARIEN stehen in R/00_config.R.
flammap_out <- out_root
szenarien   <- SZENARIEN

# ------------------------------------------------------------
#  Gemeinsames Layout ("Theme")
# ------------------------------------------------------------
#  Bildbreite = Textbreite des Dokuments (A4, Raender 2,9 cm -> 15,2 cm).
#  Fuer eine Darstellung in 17,2 cm Breite FIG_W_IN auf 17.2 / 2.54 setzen,
#  damit die Schriftgroessen exakt bleiben.
FIG_W_IN  <- 15.2 / 2.54   # Bildbreite aller Abbildungen in Zoll
FIG_PS    <- 12            # Punktgroesse
LINE_IN   <- FIG_PS / 72   # Hoehe einer Textzeile in Zoll

# oben 1.4 Zeilen, damit der Titel der Farbskala nicht abgeschnitten wird
MAR_KONT  <- c(2.6, 3.4, 1.4, 4.2)   # unten, links, oben, rechts (Farbskala)
MAR_KAT   <- c(2.6, 3.4, 1.4, 7.5)   # mehr Platz fuer Klassenlegende
CEX_AX    <- 0.65     # Achsenbeschriftung  -> ca. 7,8 pt im Dokument
CEX_LEG   <- 0.75     # Legende             -> ca. 9,0 pt im Dokument
SBAR_M    <- 500      # Massstabsbalken: feste Laenge in Metern
MAXCELL   <- 1.2e6    # Downsampling fuers Rendern grosser Raster

#  Achsenparameter; s skaliert die Schrift bei Mehrfachkarten mit
#  schmaleren Teilkarten.
pax_von <- function(s = 1) list(cex.axis = CEX_AX * s, mgp = c(2, 0.4, 0),
                                tcl = -0.25)
PAX <- pax_von()

# ------------------------------------------------------------
#  Farbpaletten und Klassenschema
# ------------------------------------------------------------
seq_chm   <- hcl.colors(100, "Greens", rev = TRUE)   # Vegetationshoehe
seq_elev  <- hcl.colors(100, "Terrain2")             # Gelaendehoehe
seq_fire  <- hcl.colors(100, "Inferno")              # Feuer-Kenngroessen
div_veg   <- hcl.colors(100, "RdYlGn")               # NDVI/NDRE
seq_water <- hcl.colors(100, "YlGnBu")               # NDWI

#  Klassen-IDs und Brennstoffcodes stammen aus R/00_config.R;
#  Beschriftung und Farbe sind reine Darstellungsentscheidungen
#  und stehen deshalb hier.
klass_id  <- as.integer(names(KLASSEN))
klass_lab <- c("Heide", "Gras", "Laubwald",
               "Nadelwald", "Moor/Nass", "Offenland/Boden")
klass_col <- c("#9E5BA6", "#C9A227", "#4DAF4A",
               "#1B5E3F", "#3B82C4", "#D9C7A3")

fuel_id   <- as.integer(FUEL_REIHENFOLGE)
fuel_lab  <- c("GA-heath (Heide)", "GR4 (Gras)", "TL6 (Laubwald)",
               "TU5 (Nadelwald)", "NB8 (Moor, n. br.)", "NB9 (Offen, n. br.)")
fuel_col  <- c("#9E5BA6", "#C9A227", "#4DAF4A",
               "#1B5E3F", "#9AB6C4", "#CFCFCF")

# ------------------------------------------------------------
#  Bildhoehe aus Kartenformat und Raendern berechnen.
#  nc/nr = Anzahl Spalten/Zeilen bei Mehrfachkarten.
# ------------------------------------------------------------
fig_hoehe_in <- function(r, mar, nc = 1, nr = 1, extra_top_lines = 0) {
  e <- as.vector(ext(r))
  asp <- (e[["ymax"]] - e[["ymin"]]) / (e[["xmax"]] - e[["xmin"]])
  panel_w_in <- FIG_W_IN / nc - (mar[2] + mar[4]) * LINE_IN
  panel_h_in <- panel_w_in * asp
  nr * (panel_h_in + (mar[1] + mar[3] + extra_top_lines) * LINE_IN)
}

fig_auf <- function(datei, hoehe_in) {
  pdf(file.path(fig_dir, datei), width = FIG_W_IN, height = hoehe_in,
      pointsize = FIG_PS, bg = "white", useDingbats = FALSE)
}

# ------------------------------------------------------------
#  Massstab + Nordpfeil, in jeder Karte an derselben relativen
#  Position (unten links, wo das AOI keine Daten hat).
# ------------------------------------------------------------
#  Hinweis: von den Nordpfeil-Typen in terra::north() zeichnet nur
#  type = 1 tatsaechlich einen Pfeil; die uebrigen liefern einen leeren
#  Rahmen. Daher fest auf type = 1.
deko <- function(r, s = 1) {
  e <- as.vector(ext(r))
  bx <- e[["xmax"]] - e[["xmin"]]; by <- e[["ymax"]] - e[["ymin"]]
  x0 <- e[["xmin"]] + 0.02 * bx
  try(sbar(d = SBAR_M, xy = c(x0, e[["ymin"]] + 0.06 * by), type = "bar",
           divs = 2, below = "m", cex = 0.6 * s, ticks = TRUE), silent = TRUE)
  try(north(xy = c(x0 + 0.02 * bx, e[["ymin"]] + 0.20 * by),
            type = 1, cex = 1.0 * s), silent = TRUE)
}

# ------------------------------------------------------------
#  Helfer: kontinuierliche Karte
# ------------------------------------------------------------
karte_kontinuierlich <- function(r, datei, pal, range = NULL, legende = "") {
  fig_auf(datei, fig_hoehe_in(r, MAR_KONT))
  args <- list(r, col = pal, maxcell = MAXCELL, mar = MAR_KONT, pax = PAX,
               plg = list(title = legende, title.cex = CEX_LEG, cex = CEX_LEG))
  if (!is.null(range)) args$range <- range
  do.call(plot, args)
  deko(r)
  dev.off()
  cat("geschrieben:", datei, "\n")
}

# ------------------------------------------------------------
#  Helfer: kategoriale Karte (Klassen/Fuel-Model)
# ------------------------------------------------------------
karte_kategorial <- function(r, datei, ids, labels, cols) {
  r <- as.factor(r)
  present <- levels(r)[[1]]
  vals <- present[[names(present)[1]]]
  vals <- vals[vals %in% ids]
  ord  <- match(vals, ids)
  levels(r) <- data.frame(id = vals, klasse = labels[ord])
  fig_auf(datei, fig_hoehe_in(r, MAR_KAT))
  plot(r, col = cols[ord], maxcell = MAXCELL, mar = MAR_KAT, pax = PAX,
       plg = list(cex = CEX_LEG))
  deko(r)
  dev.off()
  cat("geschrieben:", datei, "\n")
}

pfad <- function(...) file.path(out_dir, ...)
vorhanden <- function(p) if (file.exists(p)) p else { cat("FEHLT:", p, "\n"); NA }

# ------------------------------------------------------------
# 1) Gelaendehoehe (DTM)
# ------------------------------------------------------------
tryCatch({
  p <- vorhanden(pfad("elevation.tif"))
  if (!is.na(p)) karte_kontinuierlich(rast(p), "fig_gelaende.pdf",
      seq_elev, legende = "m ü. NN")
}, error = function(e) cat("Fehler fig_gelaende:", conditionMessage(e), "\n"))

# ------------------------------------------------------------
# 2) Canopy Height Model (CHM)
# ------------------------------------------------------------
tryCatch({
  p <- vorhanden(pfad("chm.tif"))
  if (!is.na(p)) {
    r <- clamp(rast(p), 0, CHM_MAX, values = TRUE)
    karte_kontinuierlich(r, "fig_chm.pdf", seq_chm,
      range = c(0, CHM_MAX), legende = "Höhe [m]")
  }
}, error = function(e) cat("Fehler fig_chm:", conditionMessage(e), "\n"))

# ------------------------------------------------------------
# 3) Vegetationsklassifikation
# ------------------------------------------------------------
tryCatch({
  p <- vorhanden(pfad("vegetation_classified.tif"))
  if (!is.na(p)) karte_kategorial(rast(p), "fig_klassifikation.pdf",
      klass_id, klass_lab, klass_col)
}, error = function(e) cat("Fehler fig_klassifikation:", conditionMessage(e), "\n"))

# ------------------------------------------------------------
# 4) Brennstoffmodellkarte (Fuel Model)
# ------------------------------------------------------------
tryCatch({
  p <- vorhanden(pfad("fuel_model.tif"))
  if (!is.na(p)) karte_kategorial(rast(p), "fig_fuelmodel.pdf",
      fuel_id, fuel_lab, fuel_col)
}, error = function(e) cat("Fehler fig_fuelmodel:", conditionMessage(e), "\n"))

# ------------------------------------------------------------
#  Helfer: Mehrfachkarte (gleiches Gebiet, mehrere Layer nebeneinander)
#
#  Alle Teilkarten teilen sich Palette und Wertespanne und damit EINE
#  gemeinsame Farbskala am rechten Rand. Das ist nicht nur platzsparend
#  (bei drei Teilkarten auf Textbreite bliebe fuer je eigene Legende und
#  Achsen kaum Kartenflaeche uebrig), sondern auch die Voraussetzung
#  dafuer, dass die Teilkarten visuell vergleichbar sind. Auf
#  Koordinatenachsen wird in den Teilkarten verzichtet; die Verortung
#  leisten die Einzelkarten, den Massstabsbezug der Massstabsbalken in
#  der ersten Teilkarte.
# ------------------------------------------------------------
MAR_PANEL <- c(0.4, 0.4, 1.4, 0.4)   # Rand je Teilkarte (ohne Achsen)
LEG_W_IN  <- 0.95                    # Breite der gemeinsamen Farbskala

farbskala <- function(pal, range, titel) {
  par(mar = c(MAR_PANEL[1], 0.4, MAR_PANEL[3], 2.8))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = range, xaxs = "i", yaxs = "i")
  rasterImage(matrix(rev(pal), ncol = 1), 0, range[1], 1, range[2],
              interpolate = FALSE)
  rect(0, range[1], 1, range[2])
  axis(4, cex.axis = CEX_LEG, las = 1, mgp = c(2, 0.4, 0), tcl = -0.25)
  if (nzchar(titel)) mtext(titel, side = 3, line = 0.3, cex = CEX_LEG)
}

#  overlay: optionale Funktion(i), die nach dem Zeichnen der i-ten
#  Teilkarte zusaetzliche Geometrien einzeichnet.
panel_karte <- function(lyr, datei, labels, pal, range,
                        einheit = "", nc = 2, overlay = NULL) {
  n  <- length(lyr)
  nr <- ceiling(n / nc)
  avail_in   <- FIG_W_IN - LEG_W_IN
  panel_w_in <- avail_in / nc - (MAR_PANEL[2] + MAR_PANEL[4]) * LINE_IN
  e   <- as.vector(ext(lyr[[1]]))
  asp <- (e[["ymax"]] - e[["ymin"]]) / (e[["xmax"]] - e[["xmin"]])
  h_in <- nr * (panel_w_in * asp + (MAR_PANEL[1] + MAR_PANEL[3]) * LINE_IN)

  fig_auf(datei, h_in)
  # Teilkarten zeilenweise, rechte Spalte = gemeinsame Farbskala.
  # Ueberzaehlige Zellen bleiben 0 (= leer).
  mat <- t(matrix(seq_len(nr * nc), nc, nr))
  mat[mat > n] <- 0
  layout(cbind(mat, n + 1),
         widths = c(rep(lcm(avail_in / nc * 2.54), nc), lcm(LEG_W_IN * 2.54)))
  s <- if (nc >= 3) 0.7 else 0.85    # Deko an Teilkartenbreite anpassen
  for (i in seq_len(n)) {
    plot(lyr[[i]], col = pal, range = range, maxcell = MAXCELL,
         mar = MAR_PANEL, axes = FALSE, legend = FALSE,
         main = labels[i], cex.main = 0.9)
    if (!is.null(overlay)) overlay(i)
    # Rahmen explizit ueber die Rasterausdehnung; box() zoege ihn nach
    # dem terra-Plot an der falschen Stelle.
    ep <- as.vector(ext(lyr[[i]]))
    rect(ep[["xmin"]], ep[["ymin"]], ep[["xmax"]], ep[["ymax"]], xpd = NA)
    if (i == 1) deko(lyr[[i]], s)   # Massstab/Nordpfeil einmal je Abbildung
  }
  farbskala(pal, range, einheit)
  dev.off()
  cat("geschrieben:", datei, "\n")
}

# ------------------------------------------------------------
# 5) Vegetationsindizes NDVI / NDRE / NDWI
# ------------------------------------------------------------
tryCatch({
  ps <- vapply(c("ndvi_clip.tif", "ndre_clip.tif", "ndwi_clip.tif"),
               function(f) vorhanden(pfad(f)), character(1))
  if (!any(is.na(ps))) {
    # Alle drei sind normierte Differenzindizes auf [-1, 1] und werden
    # daher mit identischer Palette und Spanne dargestellt.
    panel_karte(lapply(ps, rast), "fig_indizes.pdf",
                labels = c("NDVI", "NDRE", "NDWI"),
                pal = div_veg, range = c(-1, 1), nc = 3)
  }
}, error = function(e) cat("Fehler fig_indizes:", conditionMessage(e), "\n"))

# ------------------------------------------------------------
#  Helfer: eine FlamMap-Ausgabegroesse je Szenario als 2x2-Panel
# ------------------------------------------------------------
szenario_panel <- function(band, faktor, datei, pal,
                           cap_quantile = 0.98, einheit = "") {
  lyr <- list()
  for (sz in szenarien) {
    fp <- file.path(flammap_out, paste0(sz, "_Output.tif"))
    if (!file.exists(fp)) { cat("FEHLT:", fp, "\n"); return(invisible()) }
    r <- rast(fp)
    idx <- which(names(r) == band)
    if (!length(idx)) idx <- grep(band, names(r), fixed = TRUE)
    if (!length(idx)) { cat("Band fehlt:", band, "in", sz, "\n"); return(invisible()) }
    x <- r[[idx[1]]] * faktor
    x[x <= 0] <- NA
    lyr[[sz]] <- x
  }
  # Farbskala ueber ALLE Szenarien gemeinsam (Vergleichbarkeit): groesstes
  # der szenarioweisen Perzentile als gemeinsame Obergrenze.
  st   <- rast(lyr)
  caps <- global(st, fun = function(v) quantile(v, cap_quantile, na.rm = TRUE))[, 1]
  cap  <- max(caps, na.rm = TRUE)
  if (!is.finite(cap) || cap <= 0)
    cap <- max(global(st, "max", na.rm = TRUE)[, 1], na.rm = TRUE)
  lyr <- lapply(lyr, function(x) clamp(x, 0, cap, values = TRUE))
  panel_karte(lyr, datei, labels = szenarien, pal = pal,
              range = c(0, cap), einheit = einheit, nc = 2)
}

# ------------------------------------------------------------
# 6) Flammenlaenge je Szenario (ft -> m)
# ------------------------------------------------------------
tryCatch(
  szenario_panel("Flame Length", EINHEITEN[["Flame Length"]]$faktor,
                 "fig_flammenlaenge_szenarien.pdf",
                 seq_fire, einheit = "m"),
  error = function(e) cat("Fehler fig_flammenlaenge:", conditionMessage(e), "\n"))

# ------------------------------------------------------------
# 7) Ausbreitungsgeschwindigkeit je Szenario (chains/h -> m/min)
# ------------------------------------------------------------
tryCatch(
  szenario_panel("Rate of Spread", EINHEITEN[["Rate of Spread"]]$faktor,
                 "fig_ausbreitung_szenarien.pdf",
                 seq_fire, einheit = "m/min"),
  error = function(e) cat("Fehler fig_ausbreitung:", conditionMessage(e), "\n"))

# ------------------------------------------------------------
# 8) MTT-Barriere: Ankunftszeit ohne/mit Riegelstellung (S3)
# ------------------------------------------------------------
tryCatch({
  f_ohne <- file.path(flammap_out, "S3_MTT_Output_1.tif")
  f_mit  <- file.path(flammap_out, "S3_MTT_Output_2.tif")
  if (file.exists(f_ohne) && file.exists(f_mit)) {
    a_ohne <- rast(f_ohne)[["MTT Arrival Time"]]
    a_mit  <- rast(f_mit)[["MTT Arrival Time"]]
    if (!compareGeom(a_ohne, a_mit, stopOnError = FALSE, messages = FALSE))
      a_mit <- resample(a_mit, a_ohne, method = "near")
    a_ohne[a_ohne < 0] <- NA
    a_mit[a_mit < 0]   <- NA
    cap <- max(minmax(a_ohne)[2], minmax(a_mit)[2], na.rm = TRUE)

    # Zuendpunkt und Riegellinie aus FlamMap. Die in QGIS digitalisierte
    # Riegellinie liegt in EPSG:4326, die FlamMap-Ausgabe im Arbeits-CRS
    # (UTM 32N) -- beide Vektoren daher vor dem Plotten umprojizieren.
    ign  <- project(vect(fp_datei(file.path(flammap_dir, MTT_IGNITION))),
                    crs(a_ohne))
    barr <- project(vect(fp_datei(file.path(flammap_dir, MTT_BARRIERE))),
                    crs(a_ohne))

    panel_karte(list(a_ohne, a_mit), "fig_mtt_barriere.pdf",
                labels = c("ohne Riegelstellung", "mit Riegelstellung"),
                pal = seq_fire, range = c(0, cap), einheit = "min", nc = 2,
                overlay = function(i) {
                  # Riegelstellung nur in der zweiten Teilkarte wirksam
                  if (i == 2) plot(barr, col = "cyan", lwd = 2.5, add = TRUE)
                  plot(ign, pch = 8, cex = 1.2, col = "white", add = TRUE)
                })
  } else cat("FEHLT:", f_ohne, "oder", f_mit, "\n")
}, error = function(e) cat("Fehler fig_mtt_barriere:", conditionMessage(e), "\n"))

cat("\nAbbildungen liegen in:", fig_dir, "\n")
