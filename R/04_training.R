# ============================================================
#  FlamMap-Pipeline  |  Schritt 4: Trainingsdaten extrahieren
#  Stapelt die *_clip-Merkmalsraster (Liste FEATURES aus
#  00_config.R) und zieht unter den QGIS-Polygonen Pixelwerte
#  und Klassen-ID.
#
#  Topografie (elevation/slope/aspect) fehlt im Merkmalssatz
#  bewusst: Hoehe, Neigung und Exposition beschreiben nicht die
#  Vegetation, das Modell wuerde sonst eine Gelaende-"Abkuerzung"
#  lernen. Sie gehen erst im Landschaftsmodell (Schritt 10) ein.
#
#  Voraussetzung: terra
#  Eingang : data/<FEATURES>_clip.tif (Schritt 03),
#            <qgis_dir>/trainingspolygone1.gpkg
#  Ausgang : data/training_table.rds, data/training_table.csv
# ------------------------------------------------------------

library(terra)
source("R/00_config.R")
fp_erfordere(qgis_dir = qgis_dir)

# --- Trainingspolygone --------------------------------------
#  Das GeoPackage enthaelt mehrere Layer; ohne Angabe waehlt
#  terra::vect() stillschweigend den ersten und meldet das nur
#  beilaeufig. Der Layer wird daher explizit benannt
#  (TRAIN_LAYER in 00_config.R).
gpkg <- fp_datei(file.path(qgis_dir, TRAIN_GPKG))
vorhandene_layer <- tryCatch(vector_layers(gpkg), error = function(e) character(0))
if (TRAIN_LAYER %in% vorhandene_layer) {
  trn <- vect(gpkg, layer = TRAIN_LAYER)
} else {
  cat("Hinweis: Layer", TRAIN_LAYER, "nicht gefunden.",
      "Vorhanden:", paste(vorhandene_layer, collapse = ", "), "\n")
  trn <- vect(gpkg)
}

att <- as.data.frame(trn)
stopifnot(TRAIN_ID_COL %in% names(att))
ids <- att[[TRAIN_ID_COL]]

gueltig <- ids %in% as.integer(names(KLASSEN))
if (any(!gueltig))
  cat("Verworfen (ungueltige ID):",
      paste(sort(unique(ids[!gueltig])), collapse = ", "), "\n")
trn <- trn[gueltig, ]; ids <- ids[gueltig]
cat("Polygone je Klasse:\n"); print(table(ids))

# --- Merkmalsstapel -----------------------------------------
#  Explizit ueber FEATURES, unabhaengig vom Verzeichnisinhalt.
stk <- fp_feature_stack()
cat("\nMerkmale fuers Modell (", nlyr(stk), "):\n", sep = ""); print(names(stk))

# --- CRS-Abgleich -------------------------------------------
if (!same.crs(trn, stk)) trn <- project(trn, stk)

# --- Pixelwerte extrahieren ---------------------------------
df <- extract(stk, trn, touches = TRUE, ID = TRUE)
df$class_id <- ids[df$ID]
df <- df[stats::complete.cases(df), ]
df$class_id <- as.factor(df$class_id)

cat("\nTrainingspixel je Klasse:\n"); print(table(df$class_id))
cat("Gesamt:", nrow(df), "Pixel,", nlyr(stk), "Merkmale\n")

saveRDS(df, file.path(out_dir, "training_table.rds"))
write.csv(df, file.path(out_dir, "training_table.csv"), row.names = FALSE)
cat("Geschrieben: data/training_table.rds und data/training_table.csv\n")
