# ============================================================
#  FlamMap-Pipeline  |  Schritt 11: Feuerwetter-Perzentile
#  Liest die heruntergeladenen DWD-Tageswerte
#  (produkt_klima_tag_*.txt) direkt ein -- ohne rdwd. Berechnet
#  je Station die Perzentile und die Totbrennstoff-Feuchte.
#
#  Quelle: DWD Climate Data Center, Tageswerte Klima (kl),
#  Stationen 1327, 3591, 3623, "historical" + "recent".
#  "recent" wird taeglich neu erzeugt; ein spaeterer Download
#  liefert daher leicht abweichende Perzentile. Verwendeter Stand:
#  historical bis 31.12.2025, recent bis 20./21.07.2026.
#
#  Voraussetzung: nur Basis-R
#  Eingang : <dwd_dir>/produkt_klima_tag_*.txt
#  Ausgang : Konsolenausgabe + data/feuerwetter_perzentile.csv
# ------------------------------------------------------------

source("R/00_config.R")
fp_erfordere(dwd_dir = dwd_dir)

# ------------------------------------------------------------
# 1) Alle produkt_klima_tag-Dateien einlesen und zusammenfuehren
# ------------------------------------------------------------
dateien <- list.files(dwd_dir, pattern = "^produkt_klima_tag.*\\.txt$",
                      full.names = TRUE, recursive = TRUE)
#  Sortierung festnageln: list.files() ordnet nach der Locale des
#  Systems. Unten wird nach (Station, Datum) dedupliziert und dabei
#  der ERSTE Treffer behalten -- ohne feste Reihenfolge haengt das
#  Ergebnis also von der Spracheinstellung ab.
dateien <- dateien[order(basename(dateien), method = "radix")]
cat("Gefundene Dateien:", length(dateien), "\n")
for (f in dateien) cat("  ", basename(f), "\n")
stopifnot(length(dateien) > 0)

lese <- function(f) {
  d <- read.table(f, sep = ";", header = TRUE, dec = ".",
                  strip.white = TRUE, stringsAsFactors = FALSE,
                  fill = TRUE, comment.char = "")
  names(d) <- trimws(toupper(names(d)))
  d
}
liste <- lapply(dateien, lese)

noetig <- c("STATIONS_ID", "MESS_DATUM", "FM", "RSK", "TXK", "UPM")
fehlt  <- setdiff(noetig, names(liste[[1]]))
if (length(fehlt)) stop("Fehlende Spalten: ", paste(fehlt, collapse = ", "))

raw <- do.call(rbind, lapply(liste, function(d) d[, noetig]))
cat("Zeilen roh:", nrow(raw), "\n")

# ------------------------------------------------------------
# 2) Aufbereiten: Fehlwerte (-999) raus, Duplikate entfernen
# ------------------------------------------------------------
num <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
dat <- data.frame(
  id      = num(raw$STATIONS_ID),
  datum   = as.Date(trimws(as.character(raw$MESS_DATUM)), "%Y%m%d"),
  wind_ms = num(raw$FM),     # Tagesmittel der Windgeschwindigkeit
  nied    = num(raw$RSK),    # Tagessumme Niederschlag
  tmax    = num(raw$TXK),    # Tagesmaximum der Lufttemperatur
  rh      = num(raw$UPM)     # Tagesmittel der relativen Feuchte
)
# -999 nur in den Messspalten ersetzen (nicht in id/datum)
for (sp in c("wind_ms", "nied", "tmax", "rh"))
  dat[[sp]][!is.na(dat[[sp]]) & dat[[sp]] <= -999] <- NA

cat("Zeilen nach Umwandlung:", nrow(dat), "\n")
cat("NA je Spalte:\n"); print(colSums(is.na(dat)))

# Wind- und Feuchtedaten stammen von VERSCHIEDENEN Stationen
# -> getrennt auswerten, sonst faellt eine Station durch den Filter.
datw <- dat[!is.na(dat$wind_ms) & !is.na(dat$datum), c("id", "datum", "wind_ms", "nied")]
WIND_DA <- nrow(datw) > 100

pflicht <- c("id", "datum", "nied", "tmax", "rh")
dat <- dat[stats::complete.cases(dat[, pflicht]), ]
cat("Zeilen nach Fehlwert-Filter:", nrow(dat), "\n")
#  Historische und aktuelle Dateien derselben Station ueberlappen.
#  Behalten wird der erste Treffer, also (nach der Sortierung oben)
#  der aus der historischen Reihe.
dat  <- dat[!duplicated(dat[, c("id", "datum")]), ]
datw <- datw[!duplicated(datw[, c("id", "datum")]), ]
cat("Zeilen nach Duplikat-Filter:", nrow(dat), "\n")
cat("Stationen im Datensatz:", paste(sort(unique(dat$id)), collapse = ", "), "\n")
stopifnot(nrow(dat) > 0)

# ------------------------------------------------------------
# 3) Totbrennstoff-Feuchte, 1-h-Klasse (Simard 1968)
#    Eingang in Grad Celsius und Prozent, Rechnung in Fahrenheit.
# ------------------------------------------------------------
emc <- function(T_c, RH) {
  Tf <- T_c * 9 / 5 + 32
  ifelse(RH < 10, 0.03229 + 0.281073 * RH - 0.000578 * RH * Tf,
         ifelse(RH < 50, 2.22749 + 0.160107 * RH - 0.014784 * Tf,
                21.0606 + 0.005565 * RH^2 - 0.00035 * RH * Tf - 0.483199 * RH))
}

# ------------------------------------------------------------
# 4) Je Station: Feuersaison-Trockentage + Perzentile
#
#    Bei Temperatur zaehlt das OBERE Ende der Verteilung (P95/P97),
#    bei Feuchte und Brennstofffeuchte das UNTERE -- dort ist das
#    Extrem der trockene Fall. Die Spalten sind daher bewusst mit
#    unterschiedlichen Quantilen belegt.
# ------------------------------------------------------------
p <- function(x, q) round(as.numeric(quantile(x, q, na.rm = TRUE)), 1)
erg <- data.frame()

for (sid in sort(unique(dat$id))) {
  d <- dat[dat$id == sid, ]
  d$monat <- as.integer(format(d$datum, "%m"))
  s <- d[d$monat %in% FEUERSAISON_MONATE & d$nied < TROCKEN_MM, ]
  s$fm1 <- emc(s$tmax, s$rh)

  cat("\n=========================================================\n")
  cat("Station", sid, "|", format(min(d$datum)), "-", format(max(d$datum)),
      "|", round(as.numeric(diff(range(d$datum))) / 365.25, 1), "Jahre\n")
  if (!is.na(DWD_STATIONEN[as.character(sid)]))
    cat("Rolle:", DWD_STATIONEN[as.character(sid)], "\n")
  cat("Feuersaison-Trockentage:", nrow(s), "\n")
  cat("=========================================================\n")
  cat(sprintf("%-20s %8s %8s %8s\n", "Kenngroesse", "Median", "P95", "P97"))
  cat(sprintf("%-20s %8.1f %8.1f %8.1f\n", "Tmax (C)",
              p(s$tmax, .5), p(s$tmax, .95), p(s$tmax, .97)))
  cat(sprintf("%-20s %8.1f %8.1f %8.1f\n", "rel.Feuchte (%)",
              p(s$rh, .5), p(s$rh, .05), p(s$rh, .03)))
  cat(sprintf("%-20s %8.1f %8.1f %8.1f\n", "1-h dead FMC(%)",
              p(s$fm1, .5), p(s$fm1, .05), p(s$fm1, .03)))

  erg <- rbind(erg, data.frame(
    Station = sid, Groesse = c("Tmax_C", "rel_Feuchte_pct", "FMC_1h_pct"),
    Median  = c(p(s$tmax, .5),  p(s$rh, .5),  p(s$fm1, .5)),
    P95     = c(p(s$tmax, .95), p(s$rh, .05), p(s$fm1, .05)),
    P97     = c(p(s$tmax, .97), p(s$rh, .03), p(s$fm1, .03)),
    Tage    = nrow(s),
    Von     = format(min(d$datum)), Bis = format(max(d$datum))))
}

# ------------------------------------------------------------
# 5) Wind separat (eigene Station, Feuersaison-Trockentage)
# ------------------------------------------------------------
if (WIND_DA) {
  datw$monat <- as.integer(format(datw$datum, "%m"))
  for (sid in sort(unique(datw$id))) {
    w <- datw[datw$id == sid & datw$monat %in% FEUERSAISON_MONATE, ]
    w <- w[is.na(w$nied) | w$nied < TROCKEN_MM, ]
    if (nrow(w) < 100) next
    cat("\n=========================================================\n")
    cat("WIND | Station", sid, "|", format(min(w$datum)), "-", format(max(w$datum)),
        "|", nrow(w), "Tage\n")
    cat("=========================================================\n")
    cat(sprintf("%-20s %8s %8s %8s\n", "Kenngroesse", "Median", "P95", "P97"))
    cat(sprintf("%-20s %8.1f %8.1f %8.1f\n", "Wind (m/s)",
                p(w$wind_ms, .5), p(w$wind_ms, .95), p(w$wind_ms, .97)))
    cat(sprintf("%-20s %8.1f %8.1f %8.1f\n", "Wind (mph, FlamMap)",
                p(w$wind_ms * 2.237, .5), p(w$wind_ms * 2.237, .95),
                p(w$wind_ms * 2.237, .97)))

    erg <- rbind(erg, data.frame(
      Station = sid, Groesse = c("Wind_ms", "Wind_mph"),
      Median  = c(p(w$wind_ms, .5),  p(w$wind_ms * 2.237, .5)),
      P95     = c(p(w$wind_ms, .95), p(w$wind_ms * 2.237, .95)),
      P97     = c(p(w$wind_ms, .97), p(w$wind_ms * 2.237, .97)),
      Tage    = nrow(w),
      Von     = format(min(w$datum)), Bis = format(max(w$datum))))
  }
} else cat("\nKeine Winddaten gefunden.\n")

write.csv(erg, file.path(out_dir, "feuerwetter_perzentile.csv"), row.names = FALSE)
cat("\nGespeichert: data/feuerwetter_perzentile.csv\n")
cat("10-h/100-h FMC ~ 1-h +2 / +4 Prozentpunkte.\n")
cat("Live FMC bleibt literaturbasiert (Davies-Schwellen).\n")
