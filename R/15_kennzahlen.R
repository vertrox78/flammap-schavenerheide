# ============================================================
#  FlamMap-Pipeline  |  Schritt 15: Kennzahlen fuer die Thesis
#  Extrahiert die im Text und Anhang berichteten Zahlen aus dem
#  trainierten RF-Modell, der Trainingstabelle, den Trainings-
#  polygonen und der klassifizierten Karte:
#    - OOB, Konfusionsmatrix (30 % Holdout), OA, Kappa, PA/UA
#    - Merkmalswichtigkeit (impurity)
#    - Trainingspixel und -polygone je Klasse
#    - Flaechenanteile der sechs Klassen
#
#  Der 70/30-Split wird mit set.seed(SEED) exakt wie in
#  05_random_forest.R reproduziert. Das Skript PRUEFT diese
#  Reproduktion und bricht ab, wenn sie nicht gelingt -- sonst
#  wuerden stillschweigend Kennzahlen zu einem anderen Split
#  ausgegeben als zu dem, auf dem das Modell trainiert wurde.
#
#  Voraussetzung: terra, ranger
#  Eingang : data/{training_table.rds,rf_model.rds,
#            vegetation_classified.tif}, <qgis_dir>/<TRAIN_GPKG>
#  Ausgang : Konsolenausgabe
# ------------------------------------------------------------

suppressMessages({library(terra); library(ranger)})
source("R/00_config.R")

rf <- readRDS(fp_datei(file.path(out_dir, "rf_model.rds"),
                       "Erst Schritt 05 ausfuehren."))
df <- readRDS(fp_datei(file.path(out_dir, "training_table.rds"),
                       "Erst Schritt 04 ausfuehren."))
df$ID <- NULL
df$class_id <- factor(df$class_id)
df <- df[, c(FEATURES, "class_id")]        # Reihenfolge wie in Schritt 05

# --- Split exakt reproduzieren (wie 05_random_forest.R) ------
set.seed(SEED)
idx <- unlist(tapply(seq_len(nrow(df)), df$class_id, function(i)
  sample(i, floor(TRAIN_ANTEIL * length(i)))))
train <- df[idx, ]; test <- df[-idx, ]

#  Gegenprobe: Der hier erzeugte Trainingsteil muss genau so gross
#  sein wie der, auf dem das gespeicherte Modell trainiert wurde.
if (!identical(as.integer(rf$num.samples), nrow(train)))
  stop("Der Split laesst sich nicht reproduzieren.\n",
       "  Modell wurde auf ", rf$num.samples, " Pixeln trainiert,\n",
       "  hier entstanden ", nrow(train), " Pixel.\n",
       "  Moegliche Ursachen: andere Trainingstabelle, geaendertes SEED\n",
       "  oder eine R-Version mit anderem Zufallszahlengenerator.",
       call. = FALSE)
cat("Split reproduziert:", nrow(train), "Trainings- /", nrow(test), "Testpixel\n\n")

cat("### MODELL ###\n")
cat("num.trees:", rf$num.trees, "| mtry:", rf$mtry,
    "| Merkmale:", length(rf$variable.importance), "\n")
cat("OOB-Fehler:", round(rf$prediction.error * 100, 2), "%\n\n")

cat("### TRAININGSPIXEL je Klasse (gesamt / Train / Test) ###\n")
print(data.frame(Klasse = KLASSEN[levels(df$class_id)],
                 Gesamt = as.integer(table(df$class_id)),
                 Train  = as.integer(table(train$class_id)),
                 Test   = as.integer(table(test$class_id))), row.names = FALSE)
cat("Summe gesamt:", nrow(df), "\n\n")

# --- Konfusionsmatrix auf dem Testsplit ---------------------
pred <- predict(rf, test)$predictions
cm <- table(Referenz = test$class_id, Vorhersage = pred)
cat("### KONFUSIONSMATRIX (30 % Holdout) ###\n"); print(cm)
oa <- sum(diag(cm)) / sum(cm); N <- sum(cm)
pe <- sum(rowSums(cm) * colSums(cm)) / N^2
cat("\nOverall Accuracy:", round(oa * 100, 2), "% | Kappa:",
    round((oa - pe) / (1 - pe), 4), "\n\n")

cat("### PRODUCER / USER ACCURACY ###\n")
print(data.frame(Klasse   = KLASSEN[levels(test$class_id)],
                 Producer = round(diag(cm) / rowSums(cm) * 100, 1),
                 User     = round(diag(cm) / colSums(cm) * 100, 1)), row.names = FALSE)

cat("\n### MERKMALSWICHTIGKEIT (impurity) ###\n")
print(round(sort(rf$variable.importance, decreasing = TRUE)))

# --- Trainingspolygone je Klasse ----------------------------
cat("\n### TRAININGSPOLYGONE je Klasse ###\n")
if (!is.na(qgis_dir) && file.exists(file.path(qgis_dir, TRAIN_GPKG))) {
  gpkg <- file.path(qgis_dir, TRAIN_GPKG)
  vorhandene_layer <- tryCatch(vector_layers(gpkg), error = function(e) character(0))
  v <- if (TRAIN_LAYER %in% vorhandene_layer) vect(gpkg, layer = TRAIN_LAYER) else vect(gpkg)
  feld <- intersect(c(TRAIN_ID_COL, "class_id", "id"), names(v))
  if (length(feld)) print(table(v[[feld[1]]][, 1]))
} else {
  cat("GeoPackage nicht verfuegbar (qgis_dir nicht konfiguriert) -- uebersprungen.\n")
}

# --- Flaechenanteile der sechs Klassen ----------------------
cat("\n### FLAECHENANTEILE (vegetation_classified.tif) ###\n")
cl <- rast(fp_datei(file.path(out_dir, "vegetation_classified.tif"),
                    "Erst Schritt 05b ausfuehren."))
fr <- freq(cl)
fr$ha     <- round(fr$count * prod(res(cl)) / 1e4, 2)
fr$anteil <- round(fr$count / sum(fr$count) * 100, 1)
fr$Klasse <- KLASSEN[as.character(fr$value)]
print(fr[, c("Klasse", "value", "count", "ha", "anteil")], row.names = FALSE)
cat("Zellgroesse (m):", round(res(cl)[1], 3), "\n")
