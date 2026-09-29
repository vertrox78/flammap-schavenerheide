# ============================================================
#  FlamMap-Pipeline Schavener Heide  |  Gesamtlauf
#
#  Fuehrt die Verarbeitungskette in der richtigen Reihenfolge aus.
#  Aufruf aus dem PROJEKTWURZELVERZEICHNIS:
#
#      Rscript run_all.R              # alle ausfuehrbaren Schritte
#      Rscript run_all.R 12 13 15 16  # nur diese Schritte
#      Rscript run_all.R auswertung   # Gruppe (s. unten)
#
#  Zwischen Schritt 10 und 12 liegen die Simulationslaeufe in
#  FlamMap 6 (Handarbeit, s. README). Schritte, deren Eingaben
#  nicht konfiguriert sind, werden uebersprungen.
# ------------------------------------------------------------

#  Schritte der Kette. "gruppe" steuert die Sammelaufrufe,
#  "braucht" nennt die Voraussetzung ausserhalb des Repos.
SCHRITTE <- list(
  list(id = "00",  datei = "00_inventory.R",      gruppe = "roh",        braucht = "root"),
  list(id = "00b", datei = "00b_offset_ostwest.R", gruppe = "roh",       braucht = "root"),
  list(id = "01",  datei = "01_lidar_dtm_chm.R",  gruppe = "roh",        braucht = "root"),
  list(id = "01b", datei = "01b_chm_despike.R",   gruppe = "roh",        braucht = NA),
  list(id = "02",  datei = "02_multispectral.R",  gruppe = "roh",        braucht = "root"),
  list(id = "02b", datei = "02b_ndvi_10cm.R",     gruppe = "roh",        braucht = "root"),
  list(id = "02c", datei = "02c_fill.R",          gruppe = "roh",        braucht = NA),
  list(id = "03",  datei = "03_aoi_clip.R",       gruppe = "roh",        braucht = NA),
  list(id = "04",  datei = "04_training.R",       gruppe = "modell",     braucht = "qgis_dir"),
  list(id = "05",  datei = "05_random_forest.R",  gruppe = "modell",     braucht = NA),
  list(id = "05b", datei = "05b_predict_50cm.R",  gruppe = "modell",     braucht = NA),
  list(id = "06",  datei = "06_canopy.R",         gruppe = "brennstoff", braucht = NA),
  list(id = "07",  datei = "07_fuelmodel.R",      gruppe = "brennstoff", braucht = NA),
  list(id = "08",  datei = "08_cbh_cbd.R",        gruppe = "brennstoff", braucht = NA),
  list(id = "09",  datei = "09_chmstatistik.R",   gruppe = "brennstoff", braucht = NA),
  list(id = "10",  datei = "10_lcp_export.R",     gruppe = "brennstoff", braucht = NA),
  list(id = "11",  datei = "11_dwd_wetter.R",     gruppe = "wetter",     braucht = "dwd_dir"),
  list(id = "12",  datei = "12_statistik.R",      gruppe = "auswertung", braucht = "out_root"),
  list(id = "13",  datei = "13_latex_tabellen.R", gruppe = "auswertung", braucht = "out_root"),
  list(id = "14",  datei = "14_abbildungen.R",    gruppe = "auswertung", braucht = "out_root"),
  list(id = "15",  datei = "15_kennzahlen.R",     gruppe = "auswertung", braucht = NA),
  list(id = "16",  datei = "16_mtt_barriere.R",   gruppe = "auswertung", braucht = "out_root")
)

args <- commandArgs(trailingOnly = TRUE)

SKRIPTE <- "R"
if (!file.exists(file.path(SKRIPTE, "00_config.R")))
  stop("R/00_config.R nicht gefunden. Bitte aus dem Projektwurzelverzeichnis aufrufen.",
       call. = FALSE)

#  Konfiguration einmal laden, um zu wissen, welche Pfade gesetzt sind
source(file.path(SKRIPTE, "00_config.R"))

gruppen <- unique(vapply(SCHRITTE, function(s) s$gruppe, character(1)))
if (length(args) == 0) {
  auswahl <- SCHRITTE
} else if (all(args %in% gruppen)) {
  auswahl <- Filter(function(s) s$gruppe %in% args, SCHRITTE)
} else {
  auswahl <- Filter(function(s) s$id %in% args, SCHRITTE)
  unbekannt <- setdiff(args, vapply(SCHRITTE, function(s) s$id, character(1)))
  if (length(unbekannt))
    stop("Unbekannte Schritte: ", paste(unbekannt, collapse = ", "),
         "\nVerfuegbar: ", paste(vapply(SCHRITTE, function(s) s$id, character(1)),
                                 collapse = ", "),
         "\nGruppen: ", paste(gruppen, collapse = ", "), call. = FALSE)
}
if (!length(auswahl)) stop("Keine Schritte ausgewaehlt.", call. = FALSE)

verfuegbar <- function(name) {
  if (is.na(name)) return(TRUE)
  p <- get(name, envir = globalenv())
  length(p) == 1 && !is.na(p) && nzchar(p) && (dir.exists(p) || file.exists(p))
}

cat("============================================================\n")
cat(" FlamMap-Pipeline Schavener Heide\n")
cat(" Projektwurzel:", proj_dir, "\n")
cat(" Skriptordner :", SKRIPTE, "\n")
cat(" Geplante Schritte:",
    paste(vapply(auswahl, function(s) s$id, character(1)), collapse = ", "), "\n")
cat("============================================================\n\n")

uebersprungen <- character(0)
for (s in auswahl) {
  if (!verfuegbar(s$braucht)) {
    cat("--- Schritt", s$id, "uebersprungen: Pfad '", s$braucht,
        "' nicht konfiguriert oder nicht vorhanden.\n", sep = "")
    uebersprungen <- c(uebersprungen, s$id)
    next
  }
  cat("\n============================================================\n")
  cat(" Schritt", s$id, "--", s$datei, "\n")
  cat("============================================================\n")
  t0 <- Sys.time()
  source(file.path(SKRIPTE, s$datei), echo = FALSE)
  cat(sprintf("\n[Schritt %s fertig in %.1f s]\n", s$id,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

cat("\n============================================================\n")
cat(" Gesamtlauf beendet.\n")
if (length(uebersprungen))
  cat(" Uebersprungen (Pfade fehlen):", paste(uebersprungen, collapse = ", "), "\n")
cat("============================================================\n")
