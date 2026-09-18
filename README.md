# FeHBI Paper 1 – Balance

R-Pipeline zur Frage, ob gemessenes Estradiol (E2, pmol/L) und Progesteron
(P4, nmol/L) sowie deren Interaktion mit statischer (SLB/COP) und dynamischer
(mSEBT) posturaler Kontrolle bei freizeitsportlich aktiven Frauen zusammen-
hängen. Prädiktoren sind die gemessenen Konzentrationen, nicht Zyklusphasen.

---

## Starten

```r
source("99_run_all.R")
```

`99_run_all.R` ist die einzige Datei, die die Reihenfolge kennt. Sie sucht das
Skriptverzeichnis selbst, führt alle zehn Skripte der Reihe nach aus und
schreibt ein vollständiges Protokoll nach
`<data_dir>/Statistics/Models/99_protokoll_<datum>.txt`.

Einzelne Skripte lassen sich auch für sich starten: jedes lädt fehlende
Voraussetzungen über `source()` nach.

---

## Pipeline

| # | Skript | Zweck | Outputs |
|---|---|---|---|
| 1 | `00_setup.R` | Plotdesign: `theme_fehbi()`, Farbkonstanten | – |
| 2 | `10_load_data.R` | Einlesen, Trial-Ausschlüsse, Aggregation; setzt `data_dir` | – |
| 3 | `20_prepare.R` | Mundlak-Zerlegung, Designvariablen, Skalierung; erzeugt `OUTCOMES` | – |
| 4 | `30_descriptives.R` | Deskriptive Abbildungen, Ausreisser, Varianzbudget | `30_*` |
| 5 | `40_diagnostics.R` | ICC, Expositionsvarianz, Zyklusdeskription | `40_*` |
| 6 | `50_missingness.R` | fehlende Werte, Dropouts beim Adjustieren | `50_*` |
| 7 | `60_primary.R` | **Primäranalyse**, Hauptabbildung und Haupttabelle | `60_*` |
| 8 | `70_assumptions.R` | Modellkette mit LRT, Annahmenprüfung | `70_*` |
| 9 | `80_specification_curve.R` | Robustheit über Spezifikationen | `80_*` |
| 10 | `90_report.R` | PDF-Zusammenfassung | `90_*` |

Die Nummer im Dateinamen einer Ausgabe nennt immer das Skript, das sie
geschrieben hat. Alle Ausgaben landen in `<data_dir>/Statistics/Models/`,
die Abbildungen aus `10_load_data.R` zusätzlich in `<data_dir>/Statistics/Plots/`.

---

## Datenpfad

Die Daten liegen nicht im Repository. `10_load_data.R` löst den Ordner in
dieser Reihenfolge auf, der erste Treffer gewinnt:

1. Umgebungsvariable `FEHBI_DATA`
2. `pfade_lokal.R` neben den Skripten (steht in `.gitignore`, wird nie geteilt)
3. die Standardliste `DATA_DIR_KANDIDATEN` im Skript

Für einen Lauf auf einem fremden Rechner genügt eine Datei `pfade_lokal.R`
mit einer Zeile:

```r
FEHBI_DATA <- "P:/mein/pfad/zu/den/daten"
```

**Ebenfalls nötig:** `FeHBI_trial_exclusions.csv` im Skriptordner. Fehlt sie,
bricht `10_load_data.R` ab (`EXCL_REQUIRED <- TRUE`) – ein nicht angewendeter
Ausschluss ist genauso schwerwiegend wie ein falscher.

---

## Benötigte Pakete

```r
install.packages(c("tidyverse", "janitor", "lme4", "lmerTest",
                   "broom.mixed", "patchwork"))
```

Optional, jeweils sauber abgefangen, wenn nicht vorhanden:

| Paket | wofür | ohne das Paket |
|---|---|---|
| `pbkrtest` | Kenward-Roger-Korrektur in `60_primary.R` | Sensitivitätszeile B entfällt, Meldung im Protokoll |
| `performance` | `check_model()` in `70_assumptions.R` | Diagnostik-PDF entfällt |
| `ggrepel` | Ausreisserbeschriftung in `30_descriptives.R` | Beschriftung ohne Ausweichlogik |

---

## Konventionen

- R-Kommentare und Konsolenausgaben sind deutsch (Arbeitsebene).
- Alle Achsen, Titel und Legenden der Abbildungen sind englisch
  (publikationsfertig) und folgen `theme_fehbi()` aus `00_setup.R`.
- Farbkonvention: `FEHBI_BLUE` = Progesteron (P4), `FEHBI_ORANGE` = Estradiol (E2).
  Die Farbe steht für das Hormon, nicht für die Ebene; within und between
  werden über Form und Linienart unterschieden.
- Alle Dateien sind UTF-8. `99_run_all.R` prüft die Kodierung am Inhalt, nicht
  am Dateinamen.
- Kein Skript enthält `rm(list = ls())`. `99_run_all.R` überwacht das über die
  Ankerobjekte `data_dir`, `df_slb`, `df_msebt`, `df_slb_mean`,
  `df_msebt_mean`, `OUTCOMES` und bricht ab, wenn eines verschwindet.
