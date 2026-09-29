# ============================================================
#  FlamMap-Pipeline  |  Schritt 5b: Modell flaechig anwenden (50 cm)
#  Erzeugt die Klassenkarte, auf der alle Folgeschritte aufbauen.
#
#  Das Modell bleibt das auf 25 cm TRAINIERTE (Trennschaerfe
#  bleibt erhalten), nur die ANWENDUNG laeuft auf dem groeberen
#  Gitter, nachdem sie in Trainingsaufloesung wiederholt an
#  Speichergrenzen scheiterte. Da die Klassenkarte anschliessend
#  ohnehin per Modalwert aufs 1-m-Gitter aggregiert wird
#  (Schritt 07), ist das ein Kompromiss ohne nennenswerten
#  Qualitaetsverlust.
#
#  Voraussetzung: terra, ranger
#  Eingang : data/rf_model.rds (Schritt 05),
#            data/<FEATURES>_clip.tif (Schritt 03)
#  Ausgang : data/vegetation_classified.tif  (50 cm)
# ------------------------------------------------------------

library(terra)
library(ranger)
source("R/00_config.R")

# Trainiertes Modell laden (aus Skript 05 gespeichert)
rf <- readRDS(fp_datei(file.path(out_dir, "rf_model.rds"),
                       "Erst Schritt 05 ausfuehren."))

# --- Merkmalsstapel (25 cm) ---------------------------------
stk <- fp_feature_stack()

#  Gegenprobe: Der Stapel muss genau die Merkmale enthalten, auf
#  denen das Modell trainiert wurde. Ohne diese Pruefung faellt
#  eine Abweichung erst tief in predict() auf -- oder gar nicht.
modell_merkmale <- rf$forest$independent.variable.names
if (!setequal(names(stk), modell_merkmale))
  stop("Merkmalsstapel passt nicht zum Modell.\n",
       "  Modell : ", paste(sort(modell_merkmale), collapse = ", "), "\n",
       "  Stapel : ", paste(sort(names(stk)), collapse = ", "), call. = FALSE)
cat("Merkmale stimmen mit dem Modell ueberein (", nlyr(stk), ").\n", sep = "")

# --- Auf PREDICT_RES aggregieren ----------------------------
#  Kontinuierliche Merkmale -> Mittelwert.
fact <- round(PREDICT_RES / res(stk)[1])   # 0.50 / 0.25 = 2
cat("Aggregiere Merkmalsstapel um Faktor", fact, "auf", PREDICT_RES, "m ...\n")
stk_grob <- aggregate(stk, fact = fact, fun = "mean", na.rm = TRUE)

# --- terra::predict mit sauberem Wrapper --------------------
#  Der Wrapper gibt NUR die Vorhersagen als Integer zurueck, damit
#  terra sie direkt schreiben kann (sonst Label-Umwandlungsfehler).
#  num.threads = 1 haelt die Anwendung reproduzierbar und
#  speicherschonend; die Vorhersage eines fertigen Waldes ist
#  ohnehin deterministisch.
cat("Wende Modell an (terra::predict + Wrapper) ...\n")
predfun <- function(model, data, ...) {
  p <- predict(model, data, num.threads = 1)$predictions
  as.integer(as.character(p))
}
ziel <- file.path(out_dir, "vegetation_classified.tif")
pr <- predict(stk_grob, rf, fun = predfun,
              na.rm = TRUE,
              filename = ziel,
              overwrite = TRUE,
              wopt = list(datatype = "INT1U", gdal = "COMPRESS=LZW"))

cat("Geschrieben:", basename(ziel), "| Aufloesung:",
    paste(res(pr), collapse = " x "), "m\n")
plot(pr, main = paste0("Vegetationsklassifikation (Anwendung ", PREDICT_RES, " m)"))
