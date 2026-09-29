# Waldbrandsimulation Schavener Heide

R-Code zur Bachelorarbeit: von Drohnen-LiDAR und Multispektraldaten über
eine Random-Forest-Vegetationsklassifikation zu einem Brennstoffmodell,
einem FlamMap-6-Landschaftsmodell und der Auswertung von vier
Feuerwetterszenarien.

```
R/          Verarbeitungsschritte 00–16, Konfiguration (00_config.R)
run_all.R   Gesamtlauf
flammap/    Brennstoffmodell (.fmd), Szenarien (.fms), Zündpunkt/Barriere
data/       Brennstoff- und Klassenkarte
tex/        erzeugte Tabellen
figures/    erzeugte Abbildungen
```

Die Eingangsdaten (Befliegung, Trainingspolygone, FlamMap-Ausgaben) sind
nicht enthalten.

## Nutzung

```sh
Rscript -e 'install.packages(c("terra", "lidR", "ranger", "RCSF"))'
cp config.local.R.example config.local.R   # Pfade eintragen
Rscript run_all.R                          # alle Schritte
Rscript run_all.R auswertung               # nur 12–16
```

Gruppen: `roh` (00–03), `modell` (04–05b), `brennstoff` (06–10),
`wetter` (11), `auswertung` (12–16). Schritte ohne konfigurierte
Eingaben werden übersprungen.

Zwischen Schritt 10 und 12 wird in FlamMap 6 aus `data/LCP_layers/*.asc`
die Landschaftsdatei gebaut, `flammap/GA_heath.fmd` geladen und je
Szenario ein Lauf gerechnet (`S1_Output.tif` … `S4_Output.tif`), dazu
zwei MTT-Läufe für S3 ohne/mit Barriere (`S3_MTT_Output_1/2.tif`).

## Software

R 4.5.2 · terra 1.9.27 · lidR 4.3.2 · ranger 0.17.0 · RCSF 1.0.2 ·
FlamMap 6 · QGIS 3.40.5

## Lizenz

Code: [PolyForm Noncommercial 1.0.0](LICENSE) – Nutzung für Forschung,
Lehre und private Zwecke frei, kommerzielle Nutzung nicht gestattet.
DWD-Daten: GeoNutzV.
