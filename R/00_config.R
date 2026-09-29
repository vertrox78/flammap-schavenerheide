# ============================================================
#  FlamMap-Pipeline Schavener Heide
#  Zentrale Konfiguration -- kein eigener Pipeline-Schritt.
#  Wird von allen anderen Skripten per source() eingebunden.
#
#  KEINE maschinenspezifischen Pfade in dieser Datei!
#  Die Eingabeverzeichnisse werden in dieser Reihenfolge
#  aufgeloest:
#    1. Umgebungsvariable (FLAMMAP_ROOT, FLAMMAP_DIR, ...)
#    2. config.local.R im Projektwurzelverzeichnis
#    3. Fehlermeldung mit Anleitung (erst beim Zugriff)
#  Vorlage: config.local.R.example kopieren und anpassen.
# ------------------------------------------------------------

# ------------------------------------------------------------
#  1) Projektwurzel bestimmen (funktioniert bei source(),
#     Rscript und interaktiver Session mit wd = Projektwurzel)
# ------------------------------------------------------------
.fp_current_file <- function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile)) return(ofile)
  args <- commandArgs(trailingOnly = FALSE)
  hit  <- grep("^--file=", args)
  if (length(hit)) return(sub("^--file=", "", args[hit[1]]))
  NULL
}

.fp_cfg <- .fp_current_file()

skript_dir <- if (!is.null(.fp_cfg)) {
  normalizePath(dirname(.fp_cfg), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(file.path(getwd(), "R"), winslash = "/", mustWork = FALSE)
}
proj_dir <- normalizePath(file.path(skript_dir, ".."), winslash = "/", mustWork = FALSE)

# ------------------------------------------------------------
#  2) Lokale Pfadkonfiguration einlesen (nicht versioniert)
# ------------------------------------------------------------
.fp_local <- file.path(proj_dir, "config.local.R")
if (file.exists(.fp_local)) source(.fp_local)

.fp_pfad <- function(env_var, local_var) {
  wert <- Sys.getenv(env_var, unset = NA_character_)
  if (is.na(wert) || !nzchar(wert)) {
    wert <- if (exists(local_var, envir = globalenv())) {
      get(local_var, envir = globalenv())
    } else NA_character_
  }
  if (is.na(wert) || !nzchar(wert)) return(NA_character_)
  normalizePath(wert, winslash = "/", mustWork = FALSE)
}

# ------------------------------------------------------------
#  3) Eingabeverzeichnisse (ausserhalb des Repos)
# ------------------------------------------------------------
#  root        Rohdaten der Drohnenbefliegung (LAS + Orthomosaike)
#  qgis_dir    in QGIS digitalisierte Trainingspolygone
#  flammap_dir FlamMap-Arbeitsverzeichnis (Output, Szenarien, Vektoren)
#  dwd_dir     entpackte DWD-Tageswerte (produkt_klima_tag_*.txt)
#
#  Nicht jedes Skript braucht alle vier. Die Pruefung erfolgt daher
#  nicht hier, sondern erst beim Zugriff ueber fp_erfordere() --
#  so laeuft z. B. die Auswertung (Schritte 12-16) auch auf einem
#  Rechner, auf dem die 200 GB Rohdaten gar nicht liegen.
root        <- .fp_pfad("FLAMMAP_ROOT", "cfg_root")
qgis_dir    <- .fp_pfad("FLAMMAP_QGIS", "cfg_qgis_dir")
flammap_dir <- .fp_pfad("FLAMMAP_DIR",  "cfg_flammap_dir")
dwd_dir     <- .fp_pfad("FLAMMAP_DWD",  "cfg_dwd_dir")

# Unterverzeichnisse des FlamMap-Arbeitsverzeichnisses
out_root <- if (!is.na(flammap_dir)) file.path(flammap_dir, "Output") else NA_character_

# ------------------------------------------------------------
#  4) Ausgabeverzeichnisse
#
#  Vorgabe ist data/ im Repo. Weil die Zwischenergebnisse zusammen
#  mehrere Gigabyte gross werden (und deshalb in .gitignore stehen),
#  laesst sich das Verzeichnis auf eine andere Platte verlegen:
#  Umgebungsvariable FLAMMAP_DATA setzen oder cfg_out_dir in
#  config.local.R definieren.
# ------------------------------------------------------------
out_dir <- .fp_pfad("FLAMMAP_DATA", "cfg_out_dir")
if (is.na(out_dir)) out_dir <- file.path(proj_dir, "data")
tex_dir <- file.path(proj_dir, "tex")       # LaTeX-Tabellen zum \input{}
fig_dir <- file.path(proj_dir, "figures")   # Abbildungen als PDF
for (.d in c(out_dir, tex_dir, fig_dir)) dir.create(.d, showWarnings = FALSE, recursive = TRUE)

#  Pflichtpfade eines Skripts pruefen. Aufruf am Skriptanfang, z. B.
#  fp_erfordere(flammap_dir = flammap_dir)
fp_erfordere <- function(...) {
  args <- list(...)
  for (nm in names(args)) {
    p <- args[[nm]]
    if (length(p) != 1 || is.na(p) || !nzchar(p))
      stop(sprintf(paste0(
        "Der Pfad '%s' ist nicht konfiguriert.\n",
        "  Entweder die passende Umgebungsvariable setzen oder\n",
        "  config.local.R anlegen (Vorlage: config.local.R.example)."), nm),
        call. = FALSE)
    if (!dir.exists(p) && !file.exists(p))
      stop(sprintf("Konfigurierter Pfad '%s' existiert nicht:\n  %s", nm, p),
           call. = FALSE)
  }
  invisible(TRUE)
}

#  Einzelne Eingabedatei pruefen und Pfad zurueckgeben
fp_datei <- function(pfad, hinweis = NULL) {
  if (!file.exists(pfad))
    stop("Eingabedatei fehlt: ", pfad,
         if (!is.null(hinweis)) paste0("\n  ", hinweis) else "", call. = FALSE)
  pfad
}

# ============================================================
#  ZENTRALE PARAMETER DER VERARBEITUNGSKETTE
#  Alle Groessen, die Ergebnisse beeinflussen, stehen hier --
#  nicht verstreut ueber die Einzelskripte.
# ============================================================

# --- Koordinatenreferenzsystem ------------------------------
ARBEITS_CRS <- "EPSG:32632"   # WGS 84 / UTM Zone 32N

# --- Aufloesungen -------------------------------------------
ZIEL_RES    <- 0.25   # m, Aufloesung von DTM/DSM/CHM und Merkmalen
PREDICT_RES <- 0.50   # m, Aufloesung der flaechigen Modellanwendung
FLAMMAP_RES <- 1.00   # m, Zielgitter des Landschaftsmodells

# --- Schritt 0b/1: Hoehenversatz der Ost-Punktwolke ---------
OST_OFFSET <- 2.72     # m, Median aus 00b_offset_ostwest.R

# --- Schritt 1: Bodenfilter (Cloth Simulation Filter) -------
CSF_CLASS_THRESHOLD  <- 0.10
CSF_CLOTH_RESOLUTION <- 0.25
CSF_RIGIDNESS        <- 1
GROUND_FRACTION      <- 0.15   # Ausduennung fuer die Bodenklassifikation

# --- Schritt 6: Kronen-Ableitung aus dem CHM ----------------
KRONE_MIN <- 2    # m, ab dieser Hoehe gilt eine CHM-Zelle als Krone
CHM_MAX   <- 30   # m, Kappung von Ausreissern (hoechste reale Baeume ~22 m)

# --- Schritt 4/5: Random Forest -----------------------------
SEED         <- 42
TRAIN_ANTEIL <- 0.7    # Anteil Trainingspixel, Rest = Holdout
NUM_TREES    <- 500

#  Merkmale des Klassifikationsmodells (explizit, alphabetisch).
#  Topografie (elevation/slope/aspect) fehlt bewusst: sie beschreibt
#  nicht die Vegetation und wuerde das Modell auf eine Gelaende-
#  "Abkuerzung" trainieren. Sie geht erst im Landschaftsmodell
#  (Schritt 10) wieder ein.
FEATURES <- c("blue", "brightness", "chm", "chm_sd", "green",
              "ndre", "ndvi", "ndwi", "nir", "red", "rededge")

#  Vegetationsklassen der Trainingspolygone
KLASSEN <- c("1" = "Heide", "2" = "Gras", "3" = "Laubwald",
             "4" = "Nadelwald", "5" = "Moor/Nass", "6" = "Offen/Boden")

#  Attributspalte der Trainingspolygone mit der Klassen-ID
TRAIN_GPKG    <- "trainingspolygone1.gpkg"
TRAIN_LAYER   <- "trainigsdaten"   # Schreibweise wie im Original-GeoPackage
TRAIN_ID_COL  <- "lu_id"

# --- Schritt 7/8: Brennstoffmodelle -------------------------
#  Klasse -> Fuel-Model-Code (Scott & Burgan; 89 = Custom GA-heath)
FUEL_MAP   <- c("1" = 89, "2" = 104, "3" = 186, "4" = 165, "5" = 98, "6" = 99)
FUEL_NAMEN <- c("89" = "Heide (GA-heath)", "104" = "Gras (GR4)",
                "186" = "Laubwald (TL6)",  "165" = "Nadelwald (TU5)",
                "98"  = "Moor (NB8)",      "99"  = "Offen (NB9)")
FUEL_REIHENFOLGE <- c("89", "104", "186", "165", "98", "99")

#  Canopy Base Height (m) und Canopy Bulk Density (kg/m3) je Klasse
CBH_MAP   <- c("1" = 0, "2" = 0, "3" = 10,   "4" = 2,    "5" = 0, "6" = 0)
CBD_MAP   <- c("1" = 0, "2" = 0, "3" = 0.01, "4" = 0.15, "5" = 0, "6" = 0)
CBD_SKALA <- 100   # FlamMap-Integer-Export: kg/m3 * 100

# --- Schritt 10: LCP-Export ---------------------------------
#  Zielordner unterhalb von out_dir
LCP_ORDNER <- Sys.getenv("FLAMMAP_LCP_DIR", unset = "LCP_layers")

#  Custom-Brennstoffmodell 89 (GA-heath nach Ascoli et al. 2015)
FMD_DATEI <- file.path(proj_dir, "flammap", "GA_heath.fmd")

# --- Schritt 11: Feuerwetter --------------------------------
FEUERSAISON_MONATE <- c(3, 4, 8, 9)   # Maerz, April, August, September
TROCKEN_MM         <- 1               # Tagesniederschlag < 1 mm = Trockentag
DWD_STATIONEN <- c("1327" = "Temperatur/Feuchte/Niederschlag (Tallage)",
                   "3591" = "Kontrolle Hoehenlage",
                   "3623" = "Wind")

# --- Schritt 12/13/16: Auswertung der FlamMap-Ausgaben ------
SZENARIEN   <- c("S1", "S2", "S3", "S4")
STICHPROBE  <- 400000        # Zellen je Szenario (regular = deterministisch)
FL_SCHWELLE <- c(1.2, 3.4)   # m, Schwellen nach Andrews & Rothermel (1982)

#  Umrechnung der imperialen FlamMap-Ausgaben ins metrische System
EINHEITEN <- list(
  "Flame Length"          = list(kurz = "Flammenlaenge", faktor = 0.3048,  einheit = "m"),
  "Rate of Spread"        = list(kurz = "Ausbreitung",   faktor = 0.33528, einheit = "m/min"),
  "Fireline Intensity"    = list(kurz = "Intensitaet",   faktor = 1,       einheit = "kW/m"),
  "Heat/Unit Area"        = list(kurz = "Waerme",        faktor = 11.357,  einheit = "kJ/m2"),
  "Crown Fire Activity"   = list(kurz = "Kronenfeuer",   faktor = 1,       einheit = "Klasse"),
  "Midflame Windspeed"    = list(kurz = "MidflameWind",  faktor = 1.60934, einheit = "km/h"),
  "Crown Fraction Burned" = list(kurz = "Kronenanteil",  faktor = 1,       einheit = "Anteil")
)

#  Ergaenzender MTT-Lauf (Szenario S3)
MTT_FENSTER      <- 240     # min, Simulationsfenster
MTT_STILLSTAND_Q <- 0.999   # Quantil der Ankunftszeit = Ende des Flaechenwachstums
MTT_INTERVALL    <- 15      # min, Schrittweite der Zeitreihe
MTT_IGNITION     <- "IgnitionS3_1.shp"   # Zuendpunkt (EPSG:4326)
MTT_BARRIERE     <- "barriere_s3_5.shp"  # Riegellinie (EPSG:4326)

# ------------------------------------------------------------
#  5) Gemeinsame Hilfsfunktionen nachladen
# ------------------------------------------------------------
source(file.path(skript_dir, "00_helpers.R"))

rm(.fp_cfg, .fp_local, .d)
