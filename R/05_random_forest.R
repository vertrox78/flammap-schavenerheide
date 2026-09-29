# ============================================================
#  FlamMap-Pipeline  |  Schritt 5: Random-Forest-Klassifikation
#  Trainiert das Modell auf den 25-cm-Merkmalen, bewertet es auf
#  einem zurueckgehaltenen Drittel und speichert es.
#
#  Die flaechige Anwendung leistet Schritt 05b (auf 50 cm). Die
#  optionale 25-cm-Anwendung unten ist per Vorgabe abgeschaltet
#  (speicherintensiv) und schreibt nach
#  vegetation_classified_25cm.tif.
#
#  Voraussetzung: terra, ranger
#  Eingang : data/training_table.rds (Schritt 04)
#  Ausgang : data/rf_model.rds
#            optional data/vegetation_classified_25cm.tif
# ------------------------------------------------------------

library(terra)
library(ranger)
source("R/00_config.R")

#  25-cm-Anwendung zusaetzlich rechnen? (langsam, speicherhungrig)
ANWENDUNG_25CM <- FALSE

# ------------------------------------------------------------
#  Training
# ------------------------------------------------------------
df <- readRDS(fp_datei(file.path(out_dir, "training_table.rds"),
                       "Erst Schritt 04 ausfuehren."))
df$ID <- NULL
df$class_id <- factor(df$class_id)

#  Spaltenreihenfolge festnageln: Bei der Formel class_id ~ .
#  bestimmt die Reihenfolge, welche Merkmale ranger an jedem Knoten
#  aus dem Zufallsstrom zieht. Eine andere Reihenfolge ergaebe bei
#  gleichem Seed einen ANDEREN Wald.
stopifnot(all(FEATURES %in% names(df)))
df <- df[, c(FEATURES, "class_id")]

# ------------------------------------------------------------
#  Geschichteter 70/30-Split.
#
#  set.seed() steht unmittelbar vor dem ersten Zufallszugriff.
#  Danach zieht ranger() seinen eigenen Startwert aus demselben
#  Strom -- Reihenfolge und Anzahl der Zufallszugriffe zwischen
#  set.seed() und ranger() duerfen daher nicht veraendert werden,
#  sonst entsteht ein anderer Wald. Skript 15 stellt genau diesen
#  Split wieder her, um die Kennzahlen der Thesis nachzurechnen.
#
#  Die Aufteilung erfolgt auf PIXEL-Ebene, nicht auf Polygon-Ebene:
#  Pixel desselben Polygons liegen also in Trainings- und Testteil.
#  Die Guetemasse sind dadurch optimistisch; die Arbeit diskutiert
#  das unter dem Stichwort raeumliche Autokorrelation.
# ------------------------------------------------------------
set.seed(SEED)
idx <- unlist(tapply(seq_len(nrow(df)), df$class_id, function(i)
  sample(i, floor(TRAIN_ANTEIL * length(i)))))
train <- df[idx, ]; test <- df[-idx, ]

#  Klassengewichte gegen die sehr unterschiedlichen Polygonflaechen
w <- 1 / table(train$class_id); w <- w / sum(w)
rf <- ranger(class_id ~ ., data = train, num.trees = NUM_TREES,
             importance = "impurity",
             class.weights = as.numeric(w[levels(train$class_id)]))
cat("OOB-Fehler:", round(rf$prediction.error * 100, 2), "%\n\n")

# ------------------------------------------------------------
#  Bewertung auf dem zurueckgehaltenen Drittel
# ------------------------------------------------------------
pred <- predict(rf, test)$predictions
cm <- table(Referenz = test$class_id, Vorhersage = pred)
cat("Confusion Matrix:\n"); print(cm)
cat("\nOverall Accuracy:", round(sum(diag(cm)) / sum(cm) * 100, 2), "%\n")
acc <- data.frame(Klasse   = KLASSEN[levels(test$class_id)],
                  Producer = round(diag(cm) / rowSums(cm) * 100, 1),
                  User     = round(diag(cm) / colSums(cm) * 100, 1))
print(acc, row.names = FALSE)
cat("\nMerkmalswichtigkeit:\n"); print(sort(rf$variable.importance, decreasing = TRUE))

saveRDS(rf, file.path(out_dir, "rf_model.rds"))
cat("\nGeschrieben: data/rf_model.rds\n")

# ------------------------------------------------------------
#  Optional: flaechige Anwendung auf 25 cm
#  Eigene Kachelschleife -- verarbeitet Blockreihen einzeln und
#  schreibt direkt raus. Kein RAM-Ueberlauf, kein verschachteltes
#  predict (das brachte die Session zum Absturz).
# ------------------------------------------------------------
if (!ANWENDUNG_25CM) {
  cat("\n25-cm-Anwendung uebersprungen (ANWENDUNG_25CM = FALSE).\n")
  cat("Die flaechige Klassenkarte erzeugt Schritt 05b auf 50 cm.\n")
} else {
  ziel <- file.path(out_dir, "vegetation_classified_25cm.tif")
  unlink(ziel)

  stk <- fp_feature_stack()
  cat("\nWende Modell flaechig an (25 cm, Kachelschleife) ...\n")
  featnames <- names(stk)
  out_ras <- rast(stk, nlyrs = 1); names(out_ras) <- "class_id"

  readStart(stk)                       # Datenquellen zum Lesen oeffnen
  b <- writeStart(out_ras, ziel, overwrite = TRUE,
                  datatype = "INT1U", gdal = "COMPRESS=LZW")
  for (i in seq_len(b$n)) {
    v <- readValues(stk, row = b$row[i], nrows = b$nrows[i], dataframe = TRUE)
    names(v) <- featnames
    res <- rep(NA_integer_, nrow(v))
    ok  <- stats::complete.cases(v)
    if (any(ok)) {
      p <- predict(rf, v[ok, , drop = FALSE])$predictions
      res[ok] <- as.integer(as.character(p))
    }
    writeValues(out_ras, res, b$row[i], b$nrows[i])
    cat("  Block", i, "von", b$n, "\r")
  }
  out_ras <- writeStop(out_ras)
  readStop(stk)                        # Datenquellen wieder schliessen

  cat("\nGeschrieben:", basename(ziel), "\n")
  plot(rast(ziel), main = "Vegetationsklassifikation (25 cm)")
}
