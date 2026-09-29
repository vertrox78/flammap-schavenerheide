# ============================================================
#  FlamMap-Pipeline  |  Gemeinsame Hilfsfunktionen
#  Kein eigener Pipeline-Schritt. Wird von R/00_config.R
#  eingebunden und steht damit allen Skripten zur Verfuegung.
# ------------------------------------------------------------

# ------------------------------------------------------------
#  Merkmalsstapel fuer die Klassifikation aufbauen
#
#  features  Vektor der Merkmalsnamen (Vorgabe: FEATURES aus
#            00_config.R). Fuer jedes Merkmal wird
#            <out_dir>/<name>_clip.tif erwartet.
# ------------------------------------------------------------
fp_feature_stack <- function(features = FEATURES, verzeichnis = out_dir) {
  dateien <- file.path(verzeichnis, paste0(features, "_clip.tif"))
  fehlt   <- features[!file.exists(dateien)]
  if (length(fehlt))
    stop("Merkmalsraster fehlen in ", verzeichnis, ":\n  ",
         paste0(fehlt, "_clip.tif", collapse = ", "),
         "\n  Erst Schritt 03 (AOI-Zuschnitt) ausfuehren.", call. = FALSE)

  refr <- terra::rast(dateien[1])
  stk  <- terra::rast(lapply(dateien, function(f) {
    r <- terra::rast(f)
    if (!terra::compareGeom(r, refr, stopOnError = FALSE, messages = FALSE))
      r <- terra::resample(r, refr, "near")
    r
  }))
  names(stk) <- features
  stk
}

# ------------------------------------------------------------
#  Gitterversatz zwischen eigener Karte und FlamMap-Ausgabe
#  beheben.
#
#  FlamMap rundet beim LCP-Bau die Eckkoordinate; dadurch liegt
#  die Brennstoffkarte um einen halben Meter versetzt. Bei
#  identischer Zellenzahl entsprechen sich die Zellen 1:1 --
#  dann wird der Extent uebernommen statt zu resampeln, weil
#  Resampling schmale Wege und Moorflaechen verschmieren wuerde.
# ------------------------------------------------------------
fp_grid_angleichen <- function(x, ref, leise = FALSE) {
  if (terra::compareGeom(x, ref, stopOnError = FALSE, messages = FALSE)) return(x)
  if (all(dim(x)[1:2] == dim(ref)[1:2])) {
    if (!leise) cat("Grid-Versatz erkannt -> Extent an FlamMap-Ausgabe angeglichen.\n")
    terra::ext(x) <- terra::ext(ref)
    return(x)
  }
  if (!leise) cat("Abweichende Gittergroesse -> resample(method = \"near\").\n")
  terra::resample(x, ref, "near")
}

# ------------------------------------------------------------
#  Deterministische Stichprobe aus einem Rasterstapel
#  method = "regular" ist ein festes Gitter und damit ohne
#  Zufallszahlen reproduzierbar -- anders als method = "random".
# ------------------------------------------------------------
fp_stichprobe <- function(x, size = STICHPROBE, spaltennamen = NULL) {
  s <- terra::spatSample(x, size = size, method = "regular",
                         na.rm = TRUE, as.df = TRUE)
  if (!is.null(spaltennamen)) names(s) <- spaltennamen
  s
}

# ============================================================
#  LaTeX-Ausgabe
# ============================================================

# ------------------------------------------------------------
#  Dezimalpunkt -> deutsches Dezimalkomma, LaTeX-sicher als {,}
#  Die geschweiften Klammern unterdruecken den Abstand, den
#  LaTeX sonst hinter einem Komma im Mathemodus setzt.
# ------------------------------------------------------------
fp_dez_komma <- function(x) gsub("([0-9])\\.([0-9])", "\\1{,}\\2", x)

#  Zahl mit fester Nachkommastellenzahl und deutschem Komma
fp_zahl <- function(x, dez = 1, breite = NULL) {
  s <- if (is.null(breite)) formatC(x, format = "f", digits = dez)
       else formatC(x, format = "f", digits = dez, width = breite)
  fp_dez_komma(s)
}

# ------------------------------------------------------------
#  Zeichenkette auf eine Spaltenbreite auffuellen.
#
#  sprintf("%-20s", ...) waere naheliegender, misst Feldbreiten
#  aber in BYTES. Bei Umlauten (2 Byte in UTF-8) verrutscht die
#  Ausrichtung dadurch. nchar(type = "chars") zaehlt Zeichen.
#  Die Ausrichtung ist rein kosmetisch -- LaTeX ignoriert
#  Leerraum zwischen den Zellen --, macht die erzeugten .tex-
#  Dateien aber lesbar und diff-freundlich.
# ------------------------------------------------------------
fp_pad <- function(x, breite, rechts = FALSE) {
  x <- as.character(x)
  fehlt <- pmax(0, breite - nchar(x, type = "chars"))
  fuell <- strrep(" ", fehlt)
  if (rechts) paste0(fuell, x) else paste0(x, fuell)
}

# ------------------------------------------------------------
#  booktabs-Tabelle schreiben (UTF-8, LF-Zeilenenden)
#
#  kopf      fertige Kopfzeilen OHNE abschliessendes \\ -- Zeilen,
#            die mit \cmidrule beginnen, bekommen keines angehaengt
#  fuss      optionaler Tabellenfuss in \footnotesize
#  vorspann  optionale Zeilen vor \begin{tabular} (z. B. \small)
# ------------------------------------------------------------
fp_tabelle <- function(datei, titel, label, ausricht, kopf, zeilen,
                       fuss = NULL, vorspann = NULL, verzeichnis = tex_dir) {
  kopf_z <- vapply(kopf, function(k) {
    if (grepl("^\\s*\\\\cmidrule", k)) paste0("    ", k) else paste0("    ", k, " \\\\")
  }, character(1), USE.NAMES = FALSE)

  z <- c("\\begin{table}[htbp]",
         "  \\centering",
         paste0("  \\caption{", titel, "}"),
         paste0("  \\label{tab:", label, "}"),
         if (length(vorspann)) paste0("  ", vorspann),
         paste0("  \\begin{tabular}{", ausricht, "}"),
         "    \\toprule",
         kopf_z,
         "    \\midrule",
         paste0("    ", fp_dez_komma(zeilen), " \\\\"),
         "    \\bottomrule",
         "  \\end{tabular}")
  if (!is.null(fuss))
    z <- c(z, paste0("  \\par\\smallskip\\footnotesize ", fp_dez_komma(fuss)))
  z <- c(z, "\\end{table}")

  #  open = "wb" + enc2utf8() erzwingt LF-Zeilenenden und UTF-8 auch
  #  unter Windows: so erzeugen Laeufe auf verschiedenen Betriebs-
  #  systemen byteweise identische Dateien.
  con <- file(file.path(verzeichnis, datei), open = "wb")
  writeLines(enc2utf8(z), con, sep = "\n", useBytes = TRUE)
  close(con)
  cat("geschrieben:", datei, "\n")
  invisible(file.path(verzeichnis, datei))
}
