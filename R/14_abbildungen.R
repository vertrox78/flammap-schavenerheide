# ============================================================
#  FlamMap-Pipeline  |  Schritt 14: Abbildungen fuer die Thesis
#  Rendert die Karten (Hoehenmodelle, Klassifikation, Fuel-Model,
#  Vegetationsindizes, Simulationsergebnisse) als PDF nach figures/.
#  Einbinden in Originalgroesse, ohne width-Skalierung (s. unten).
#
#  Ausgabeformat PDF: Beschriftungen, Achsen und Legenden bleiben
#  Vektorgrafik und damit im Druck scharf. Der cairo-PNG-Treiber glaettet
#  Text unter Windows mit Subpixel-Antialiasing, was farbige Saeume an den
#  Beschriftungen erzeugt; der antialias-Parameter von png() greift dagegen
#  in diesem Build nicht. Die Rasterinhalte selbst werden weiterhin ueber
#  MAXCELL heruntergerechnet, die Dateien bleiben dadurch klein.
#
#  EINHEITLICHKEIT -- alle Karten teilen sich:
#   * ihre Enddarstellungsgroesse: Jede Abbildung wird in genau der Breite
#     erzeugt, in der sie im Dokument steht, und dort NICHT skaliert. Die
#     Schriftgroessen in den Karten entsprechen damit direkt den hier
#     gesetzten Punktgroessen,
#   * dieselben Raender, Achsen- und Legendenparameter (THEME),
#   * duenne Rahmen- und Achsenlinien statt des fetten terra-Kastens,
#   * einen schlichten Nordpfeil und einen kleinen Massstab (fest 250 m).
#     Einzelkarten tragen beide in der datenaermsten Kartenecke,
#     Mehrfachkarten einmal fuer alle Teilkarten unter der Farbskala,
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
#  Breite Karten stehen in der Arbeit in 17,2 cm Breite (Satzspiegel
#  15,2 cm + je 1 cm Ueberhang), die beiden Hoehenmodelle untereinander
#  in 14 cm Breite.
FIG_W_IN   <- 17.2 / 2.54  # Breite der breiten Karten in Zoll
FIG_W_HOEHE <- 14 / 2.54   # Breite der Hoehenmodell-Karten (CHM, DTM)
FIG_PS    <- 12            # Punktgroesse
LINE_IN   <- FIG_PS / 72   # Hoehe einer Textzeile in Zoll

# oben 1.4 Zeilen, damit der Titel der Farbskala nicht abgeschnitten wird
MAR_KONT  <- c(2.6, 3.4, 1.4, 4.2)   # unten, links, oben, rechts (Farbskala)
#  Kategoriale Karten: Klassenlegende liegt unten rechts IN der Karte (dort
#  hat das AOI keine Daten), rechts bleibt daher nur ein schmaler Rand.
MAR_KAT   <- c(2.6, 3.4, 1.4, 1.0)
CEX_AX    <- 0.65     # Achsenbeschriftung  -> 7,8 pt
CEX_LEG   <- 0.75     # Farbskalen          -> 9,0 pt
CEX_LEG_KAT <- 0.85   # Klassenlegende      -> 10,2 pt
SBAR_M    <- 250      # Massstabsbalken: feste Laenge in Metern
LWD_RAHMEN <- 0.5     # Linienstaerke von Kartenrahmen und Achsen
MAXCELL   <- 1.2e6    # Downsampling fuers Rendern grosser Raster

#  Achsenparameter. terra zeichnet bei box = FALSE an allen vier Seiten
#  eine Achsenlinie (side = 1:4); mit duenner Linie ergibt das den Rahmen.
#  lab: Seiten mit Koordinatenbeschriftung (1 = unten, 2 = links).
pax_von <- function(lab = 1:2) list(cex.axis = CEX_AX, mgp = c(2, 0.35, 0),
                                    tcl = -0.2, lwd = LWD_RAHMEN,
                                    side = 1:4, tick = lab, lab = lab)
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
#  Laub- und Nadelwald unterscheiden sich deutlich in der Helligkeit,
#  Gras (goldgelb) und Offenland (neutralgrau) im Farbton.
klass_col <- c("#9E5BA6", "#D9A821", "#9ACD5A",
               "#0E4D2A", "#3B82C4", "#BDBDBD")

fuel_id   <- as.integer(FUEL_REIHENFOLGE)
fuel_lab  <- c("GA-heath (Heide)", "GR4 (Gras)", "TL6 (Laubwald)",
               "TU5 (Nadelwald)", "NB8 (Moor, n. br.)", "NB9 (Offen, n. br.)")
fuel_col  <- c("#9E5BA6", "#C9A227", "#4DAF4A",
               "#1B5E3F", "#9AB6C4", "#CFCFCF")

# ------------------------------------------------------------
#  Bildhoehe aus Kartenformat und Raendern berechnen.
#  nc/nr = Anzahl Spalten/Zeilen bei Mehrfachkarten.
# ------------------------------------------------------------
fig_hoehe_in <- function(r, mar, nc = 1, nr = 1, extra_top_lines = 0,
                         breite_in = FIG_W_IN) {
  e <- as.vector(ext(r))
  asp <- (e[["ymax"]] - e[["ymin"]]) / (e[["xmax"]] - e[["xmin"]])
  panel_w_in <- breite_in / nc - (mar[2] + mar[4]) * LINE_IN
  panel_h_in <- panel_w_in * asp
  nr * (panel_h_in + (mar[1] + mar[3] + extra_top_lines) * LINE_IN)
}

fig_auf <- function(datei, hoehe_in, breite_in = FIG_W_IN) {
  pdf(file.path(fig_dir, datei), width = breite_in, height = hoehe_in,
      pointsize = FIG_PS, bg = "white", useDingbats = FALSE)
}

# ------------------------------------------------------------
#  Nordpfeil und Massstab
# ------------------------------------------------------------
#  Beide werden mit Basisgrafik in Kartenkoordinaten gezeichnet: ein
#  schlichter Pfeil mit "N" und ein zweigeteilter Balken. terra::north()
#  und terra::sbar() lieferten zu wuchtige Symbole.
nordpfeil <- function(x, y, h, s = 1) {
  arrows(x, y, x, y + h, length = 0.07 * s, angle = 22, lwd = 1.1 * s)
  text(x, y + h, "N", pos = 3, offset = 0.12, cex = 0.7 * s, font = 2)
}

massstab <- function(x0, y0, laenge, h, s = 1) {
  rect(x0, y0, x0 + laenge / 2, y0 + h, col = "black", border = "black", lwd = 0.5)
  rect(x0 + laenge / 2, y0, x0 + laenge, y0 + h, col = "white", border = "black",
       lwd = 0.5)
  text(c(x0, x0 + laenge), y0, c("0", paste(laenge, "m")), pos = 1,
       offset = 0.18, cex = 0.6 * s)
}

#  Datenaermste Kartenecke: Anteil der Zellen mit Daten je Eckbereich
#  (22 % der Breite und Hoehe), ausgeschlossene Ecken bleiben frei,
#  etwa die unten rechts liegende Klassenlegende.
ecke_waehlen <- function(r, ausschluss = character()) {
  e  <- as.vector(ext(r))
  bx <- e[["xmax"]] - e[["xmin"]]; by <- e[["ymax"]] - e[["ymin"]]
  g  <- aggregate(!is.na(r[[1]]), fact = max(1, floor(ncol(r) / 150)),
                  fun = "mean", na.rm = TRUE)
  wx <- 0.22 * bx; wy <- 0.22 * by
  ecken <- list(
    bottomleft  = ext(e[["xmin"]], e[["xmin"]] + wx, e[["ymin"]], e[["ymin"]] + wy),
    topleft     = ext(e[["xmin"]], e[["xmin"]] + wx, e[["ymax"]] - wy, e[["ymax"]]),
    topright    = ext(e[["xmax"]] - wx, e[["xmax"]], e[["ymax"]] - wy, e[["ymax"]]),
    bottomright = ext(e[["xmax"]] - wx, e[["xmax"]], e[["ymin"]], e[["ymin"]] + wy))
  ecken  <- ecken[setdiff(names(ecken), ausschluss)]
  anteil <- vapply(ecken, function(k) {
    v <- values(crop(g, k), mat = FALSE)
    mean(ifelse(is.na(v), 0, v))
  }, numeric(1))
  names(which.min(anteil))
}

#  Einzelkarte: Massstab und Nordpfeil in der datenaermsten Ecke
deko <- function(r, ausschluss = character()) {
  e  <- as.vector(ext(r))
  bx <- e[["xmax"]] - e[["xmin"]]; by <- e[["ymax"]] - e[["ymin"]]
  ecke  <- ecke_waehlen(r, ausschluss)
  links <- grepl("left", ecke); unten <- grepl("bottom", ecke)
  # rechts mehr Abstand, damit die Beschriftung "250 m" nicht am Rahmen klebt
  x0 <- if (links) e[["xmin"]] + 0.03 * bx else e[["xmax"]] - 0.07 * bx - SBAR_M
  y0 <- if (unten) e[["ymin"]] + 0.05 * by else e[["ymax"]] - 0.17 * by
  massstab(x0, y0, SBAR_M, h = 0.012 * by)
  nordpfeil(x0 + SBAR_M / 2, y0 + 0.045 * by, h = 0.06 * by)
}

#  Mehrfachkarte: Massstab und Nordpfeil einmal fuer alle Teilkarten in
#  einem eigenen Bereich (Normalized Device Coordinates). m_pro_in ist der
#  Kartenmassstab der Teilkarten in Metern je Zoll, damit der Balken dort
#  dieselbe Laenge hat wie in den Karten.
deko_global <- function(fig_ndc, breite_in, hoehe_in, m_pro_in) {
  par(fig = fig_ndc, new = TRUE, mar = c(0, 0, 0, 0))
  plot.new()
  bx <- breite_in * m_pro_in; by <- hoehe_in * m_pro_in
  plot.window(xlim = c(0, bx), ylim = c(0, by), xaxs = "i", yaxs = "i")
  x0 <- (bx - SBAR_M) / 2
  massstab(x0, 0.16 * by, SBAR_M, h = 0.05 * by)
  nordpfeil(bx / 2, 0.36 * by, h = 0.26 * by)
}

# ------------------------------------------------------------
#  Helfer: kontinuierliche Karte
# ------------------------------------------------------------
karte_kontinuierlich <- function(r, datei, pal, range = NULL, legende = "",
                                 breite_in = FIG_W_IN) {
  fig_auf(datei, fig_hoehe_in(r, MAR_KONT, breite_in = breite_in), breite_in)
  args <- list(r, col = pal, maxcell = MAXCELL, mar = MAR_KONT, pax = PAX,
               box = FALSE,
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
  #  terra ignoriert inset bei Positions-Schluesselwoertern; der Abstand
  #  zum Kartenrahmen wird daher per nudge (Karteneinheiten) gesetzt.
  e  <- as.vector(ext(r))
  dx <- 0.01 * (e[["xmax"]] - e[["xmin"]])
  fig_auf(datei, fig_hoehe_in(r, MAR_KAT))
  plot(r, col = cols[ord], maxcell = MAXCELL, mar = MAR_KAT, pax = PAX,
       box = FALSE,
       plg = list(x = "bottomright", nudge = c(-dx, 0), cex = CEX_LEG_KAT,
                  border = "grey35", bty = "n"))
  deko(r, ausschluss = "bottomright")
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
  #  Ellipsoidische Hoehen (rund 46 m ueber NHN); das steht in der Caption,
  #  die Farbskala traegt daher nur die Einheit.
  if (!is.na(p)) karte_kontinuierlich(rast(p), "fig_gelaende.pdf",
      seq_elev, legende = "Höhe [m]", breite_in = FIG_W_HOEHE)
}, error = function(e) cat("Fehler fig_gelaende:", conditionMessage(e), "\n"))

# ------------------------------------------------------------
# 2) Canopy Height Model (CHM)
# ------------------------------------------------------------
tryCatch({
  p <- vorhanden(pfad("chm.tif"))
  if (!is.na(p)) {
    r <- clamp(rast(p), 0, CHM_MAX, values = TRUE)
    karte_kontinuierlich(r, "fig_chm.pdf", seq_chm,
      range = c(0, CHM_MAX), legende = "Höhe [m]", breite_in = FIG_W_HOEHE)
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
#  gemeinsame Farbskala am rechten Rand. Das ist nicht nur platzsparend,
#  sondern auch die Voraussetzung dafuer, dass die Teilkarten visuell
#  vergleichbar sind. Koordinaten stehen nur an den Aussenkanten (unten
#  bzw. links), Massstab und Nordpfeil einmal fuer alle Teilkarten unter
#  der Farbskala.
# ------------------------------------------------------------
MAR_PANEL <- c(1.7, 2.2, 1.3, 0.3)   # Rand je Teilkarte (mit Achsen)
LEG_W_IN  <- 0.8                     # Breite der rechten Spalte
DEKO_H_IN <- 0.85                    # Hoehe des Bereichs fuer Nordpfeil/Massstab

farbskala <- function(pal, range, titel) {
  # Platz fuer deko_global plus Abstand zur unteren Skalenbeschriftung
  unten <- MAR_PANEL[1] + (DEKO_H_IN + 0.25) / LINE_IN
  par(mar = c(unten, 0.4, MAR_PANEL[3], 2.6))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = range, xaxs = "i", yaxs = "i")
  rasterImage(matrix(rev(pal), ncol = 1), 0, range[1], 1, range[2],
              interpolate = FALSE)
  rect(0, range[1], 1, range[2], lwd = LWD_RAHMEN)
  axis(4, cex.axis = CEX_LEG, las = 1, mgp = c(2, 0.4, 0), tcl = -0.25,
       lwd = LWD_RAHMEN)
  if (nzchar(titel)) mtext(titel, side = 3, line = 0.3, cex = CEX_LEG)
}

#  overlay: optionale Funktion(i, r), die nach dem Zeichnen der i-ten
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
  m_pro_in <- NA
  for (i in seq_len(n)) {
    zeile <- ceiling(i / nc); spalte <- (i - 1) %% nc + 1
    # x-Koordinaten unten, wenn darunter keine Teilkarte mehr folgt,
    # y-Koordinaten an der linken Spalte
    lab <- c(if (zeile == nr || i + nc > n) 1, if (spalte == 1) 2)
    plot(lyr[[i]], col = pal, range = range, maxcell = MAXCELL,
         mar = MAR_PANEL, legend = FALSE, box = FALSE,
         pax = pax_von(if (length(lab)) lab else 0),
         main = labels[i], cex.main = 0.9)
    if (i == 1) m_pro_in <- diff(par("usr")[1:2]) / par("pin")[1]
    if (!is.null(overlay)) overlay(i, lyr[[i]])
  }
  farbskala(pal, range, einheit)
  # Nordpfeil und Massstab unter der Farbskala, im Massstab der Teilkarten
  x_ndc <- c((FIG_W_IN - LEG_W_IN) / FIG_W_IN, 1)
  y_ndc <- c(MAR_PANEL[1] * LINE_IN, MAR_PANEL[1] * LINE_IN + DEKO_H_IN) / h_in
  deko_global(c(x_ndc, y_ndc), LEG_W_IN, DEKO_H_IN, m_pro_in)
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
    # daher mit identischer Palette und Spanne dargestellt. Zwei Spalten
    # statt drei: jede Teilkarte wird so deutlich groesser.
    panel_karte(lapply(ps, rast), "fig_indizes.pdf",
                labels = c("NDVI", "NDRE", "NDWI"),
                pal = div_veg, range = c(-1, 1), nc = 2)
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

    #  Zuendpunkt als hellgruener Stern mit schwarzer Kontur: hebt sich
    #  von allen Toenen der Inferno-Palette und vom Weiss ab.
    ign_xy  <- crds(ign)
    ign_col <- "#39FF14"
    stern <- function(cex = 1.5) {
      points(ign_xy, pch = 8, cex = cex, col = "black", lwd = 3.2)
      points(ign_xy, pch = 8, cex = cex, col = ign_col, lwd = 1.6)
    }
    panel_karte(list(a_ohne, a_mit), "fig_mtt_barriere.pdf",
                labels = c("ohne Riegelstellung", "mit Riegelstellung"),
                pal = seq_fire, range = c(0, cap), einheit = "min", nc = 2,
                overlay = function(i, r) {
                  # Riegelstellung nur in der zweiten Teilkarte wirksam
                  if (i == 2) plot(barr, col = "cyan", lwd = 2.5, add = TRUE)
                  stern()
                  if (i == 2) {
                    ecke <- ecke_waehlen(r)
                    legend(ecke, inset = 0.02, bty = "o", bg = "white",
                           box.lwd = LWD_RAHMEN, cex = CEX_LEG,
                           legend = c("Riegelstellung", "Zündpunkt"),
                           col = c("cyan", ign_col), lty = c(1, NA),
                           lwd = c(2.5, NA), pch = c(NA, 8), pt.lwd = 1.6,
                           pt.cex = 1.2)
                  }
                })
  } else cat("FEHLT:", f_ohne, "oder", f_mit, "\n")
}, error = function(e) cat("Fehler fig_mtt_barriere:", conditionMessage(e), "\n"))

cat("\nAbbildungen liegen in:", fig_dir, "\n")
