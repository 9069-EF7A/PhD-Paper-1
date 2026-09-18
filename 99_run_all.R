# =============================================================================
# 99_run_all.R  |  FeHBI Paper 1 - Balance  |  Orchestrierung
# =============================================================================
# Diese Datei ist die einzige Stelle, an der die Reihenfolge der Pipeline steht.
# Wer etwas einfuegt, verschiebt oder umbenennt, aendert NUR die Tabelle
# PIPELINE weiter unten - sonst nichts.
#
# REIHENFOLGE UND ZWECK
#   00_setup.R                Plotdesign: theme_fehbi(), FEHBI_BLUE/ORANGE/INK.
#                             Muss vor jeder Abbildung geladen sein, sonst
#                             laufen die Plots im ggplot-Standard.
#   10_load_data.R            Einlesen, Trial-Ausschluesse, Aggregation.
#                             Setzt data_dir - ohne das ist kein Protokollpfad
#                             bestimmbar, deshalb Sonderbehandlung.
#   20_prepare.R              Mundlak-Zerlegung, Designvariablen, Skalierung.
#                             Erzeugt OUTCOMES.
#   30_descriptives.R         Deskriptive Abbildungen, Ausreisserkontrolle,
#                             Roh- gegen Log-Verteilung der COP-Masse.
#                             Erzeugt nur Dateien, keine Objekte.
#   40_diagnostics.R          alles, was VOR dem Modellieren geklaert sein muss
#   50_missingness.R          fehlende Werte
#   60_primary.R              PRIMAERANALYSE, loopt selbst ueber alle Outcomes
#   70_assumptions.R          Modellaufbau + Annahmenpruefung
#   80_specification_curve.R  Robustheit, keine Modellwahl
#   90_report.R               PDF-Zusammenfassung
#
# DESIGNENTSCHEIDUNGEN DER PIPELINE
#
#   cycle_phase ist KEINE Kovariable  Sie ist Elternknoten der Exposition. Eine
#                                     Adjustierung entfernt genau die Hormon-
#                                     varianz, um die es geht: kein Schutz vor
#                                     Verzerrung, aber Praezisionsverlust und
#                                     moegliche Bias-Verstaerkung. Sie erscheint
#                                     nur deskriptiv in 40_diagnostics.R
#                                     (Abschnitt C und D).
#
#   Kovariablenwahl aus dem DAG       Nicht aus dem Fit. Change-in-Estimate
#                                     (>10 %) kann Confounder und Mediator nicht
#                                     unterscheiden und taugt deshalb nicht als
#                                     Entscheidungsregel. Die Tabelle laeuft als
#                                     Robustheitsanzeige in
#                                     80_specification_curve.R mit.
#
#   Keine Selektion nach AICc         Informationskriterien schaetzen den
#                                     Vorhersagefehler, nicht die Guete eines
#                                     Kausalschaetzers. Bei Mixed Models ist
#                                     zudem unklar, welches n einzusetzen ist.
#                                     AIC und BIC werden berichtet, aber nur
#                                     innerhalb derselben Stichprobe verglichen.
#
#   Genau eine Interaktion            Trainingsvolumen x Hormon_within. Fuer
#                                     Hormon x BMI und Hormon x Hueftkraft gibt
#                                     es keine theoretische Grundlage.
#
#   Hueftkraft nicht adjustiert       Zeitkonstant, also strukturell kein
#                                     Confounder des Within-Effekts. Sie fehlt
#                                     bei 17 von 59 Frauen und haette ein
#                                     Drittel der Stichprobe gekostet, ohne
#                                     etwas gegen Verzerrung auszurichten.
#                                     Bleibt deskriptiv in 40_diagnostics.R,
#                                     Abschnitt F.
#
#   Primaergroesse ist der            Ein Gesamt-z-Koeffizient mischt within und
#   Within-Koeffizient                between. Er steht nur als Referenzzeile
#                                     in 60_primary.R.
#
#   Keine Konventionsschwelle         Cohens 0.2 wird nicht verwendet. Berichtet
#                                     werden Schaetzer und KI in Outcome-SD plus
#                                     die Rueckrechnung auf die Messskala
#                                     (mm / mm2 / % Beinlaenge).
#
#   Konfidenzintervalle               t-Quantile mit Satterthwaite-df. Wald-KI
#                                     +/- 1.96*SE sind bei 17-30 Personen
#                                     antikonservativ.
#
# STRUKTUR DES MODELLS
#   standing_leg + (1 | session_id)   Zwei Beine teilen sich eine Messgelegen-
#                                     heit; ohne diese Ebene sind die SE zu klein
#   first_measure                     M1 ist immer die Mensphase von Zyklus 1
#   log vor der Zerlegung             P4 spannt zwei Groessenordnungen
#   Hausman-Kontrast                  Sagen within und between dasselbe?
#   Expositionsvarianz                Wie viel zyklisches Signal ist ueberhaupt da
# =============================================================================


# =============================================================================
# DIE PIPELINE
# -----------------------------------------------------------------------------
# Die einzige Stelle, an der die Reihenfolge steht. Wird eine Datei umbenannt,
# eingefuegt oder verschoben, aendert sich NUR diese Tabelle.
# =============================================================================
PIPELINE <- c(
  "00_setup.R",
  "10_load_data.R",
  "20_prepare.R",
  "30_descriptives.R",
  "40_diagnostics.R",
  "50_missingness.R",
  "60_primary.R",
  "70_assumptions.R",
  "80_specification_curve.R",
  "90_report.R"
)

# Das Skript, das data_dir setzt. Alles davor laeuft noch ohne Protokolldatei,
# weil ohne data_dir nicht bekannt ist, wohin sie geschrieben werden soll.
SKRIPT_MIT_DATA_DIR <- "10_load_data.R"

# Objekte, die nach ihrem ersten Auftauchen bis zum Ende bestehen bleiben
# MUESSEN. Verschwindet eines, hat ein Skript den Workspace geleert - fast
# immer ein rm(list = ls()) am Skriptanfang. Ohne diese Pruefung laeuft die
# Pipeline scheinbar weiter und bricht erst viel spaeter an einer Stelle ab,
# die mit der Ursache nichts zu tun hat.
ANKER <- c("data_dir", "df_slb", "df_msebt", "df_slb_mean", "df_msebt_mean",
           "OUTCOMES")


# =============================================================================
# ARBEITSVERZEICHNIS
# -----------------------------------------------------------------------------
# Alle Skripte sprechen sich gegenseitig mit relativen Dateinamen an. Steht das
# Arbeitsverzeichnis falsch, kommt nur "kann Verbindung nicht oeffnen" - eine
# Meldung, die nicht verraet, WO R gesucht hat. Deshalb sucht dieser Block den
# Ordner selbst. Reihenfolge:
#   1. Das aktuelle Arbeitsverzeichnis. Im RStudio-Projekt oder im geklonten
#      Repository ist das der Normalfall, dann wird nichts geaendert.
#   2. Ein Unterordner - typisch, wenn ein ZIP entpackt wurde.
#   3. Die Eintraege aus CODE_DIR unten.
#
# WENN DER ORDNER UMZIEHT: nur CODE_DIR anpassen, sonst nichts.
# Backslashes gehen in R nicht - entweder "/" oder "\\" schreiben.
# =============================================================================
CODE_DIR <- c(
  "C:/Users/frds/OneDrive - ZHAW/PhD/Paper 1 Balance/R Codes",
  file.path(Sys.getenv("USERPROFILE"),
            "OneDrive - ZHAW/PhD/Paper 1 Balance/R Codes"),
  file.path(Sys.getenv("OneDrive"), "PhD/Paper 1 Balance/R Codes")
)

vollstaendig <- function(d) all(file.exists(file.path(d, PIPELINE)))

if (!vollstaendig(".")) {
  kandidaten <- c(list.dirs(recursive = TRUE, full.names = TRUE), CODE_DIR)
  kandidaten <- kandidaten[nzchar(kandidaten)]
  treffer    <- kandidaten[vapply(kandidaten, vollstaendig, logical(1))]

  if (length(treffer)) {
    setwd(treffer[1])
    cat("Arbeitsverzeichnis gesetzt auf:\n  ", getwd(), "\n", sep = "")
  } else {
    cat("\n=============================================================\n")
    cat("ABBRUCH: Skripte nicht gefunden\n")
    cat("=============================================================\n")
    cat("Arbeitsverzeichnis (getwd):\n  ", getwd(), "\n\n", sep = "")
    cat("Dort fehlen:\n")
    for (f in PIPELINE[!file.exists(PIPELINE)]) cat("  -", f, "\n")
    gefunden <- list.files(pattern = "\\.R$")
    cat("\nGefundene R-Dateien in diesem Ordner:\n")
    if (length(gefunden)) for (f in gefunden) cat("  ", f, "\n") else
      cat("   (keine)\n")
    cat("\nGeprueft wurden zusaetzlich:\n")
    for (d in CODE_DIR) cat("  ", d, if (dir.exists(d)) "" else
      "   (Ordner existiert nicht)", "\n", sep = "")
    cat("\nLoesung: CODE_DIR oben in diesem Skript auf den richtigen Ordner\n")
    cat("setzen - oder in RStudio Session > Set Working Directory >\n")
    cat("To Source File Location, waehrend 99_run_all.R offen ist.\n")
    cat("=============================================================\n")
    stop("Arbeitsverzeichnis stimmt nicht - siehe Meldung oben.", call. = FALSE)
  }
}


# =============================================================================
# PROTOKOLL
# -----------------------------------------------------------------------------
# Die Konsole ist fluechtig - nach einem Durchlauf ist die Scrollback-Historie
# weg oder abgeschnitten. Deshalb wird ALLES zusaetzlich in eine Textdatei
# geschrieben. split = TRUE heisst: Ausgabe erscheint weiterhin in der Konsole
# UND landet in der Datei.
#
# Zusaetzlich werden message() und warning() in normale Ausgabe umgewandelt.
# Sonst waeren sie in stderr und wuerden im Protokoll fehlen - also genau die
# Zeilen mit [!] und [-], die im Zweifel am wichtigsten sind.
#
# WARUM EINE FUNKTION UND NICHT AUF OBERSTER EBENE:
#   on.exit() wirkt nur innerhalb einer Funktion. Auf oberster Ebene gehoert der
#   Ausdruck zur eigenen Zeile und feuert sofort danach - der sink() waere also
#   schon wieder geschlossen, bevor das zweite Skript ueberhaupt beginnt, und
#   die Protokolldatei bliebe leer.
#
# WARUM DIE OBJEKTE TROTZDEM IM WORKSPACE LANDEN:
#   source(..., local = FALSE) legt alles in .GlobalEnv ab, unabhaengig davon,
#   aus welcher Funktion heraus es aufgerufen wird.
# =============================================================================

pipeline_lauf <- function() {

  sink_start <- sink.number()   # Stand vor dem Lauf, zum sauberen Abwickeln

  # Kopien im Funktionsrahmen. Ein rm(list = ls()) in einem der Skripte leert
  # .GlobalEnv und wuerde sonst auch PIPELINE und ANKER mitnehmen - dann
  # scheitert ausgerechnet die Pruefung, die den Vorfall melden soll, mit
  # "object 'ANKER' not found". Hier unten sind sie ausser Reichweite.
  pipeline    <- PIPELINE
  anker_namen <- ANKER
  skript_data <- SKRIPT_MIT_DATA_DIR

  # --- Zeichenkodierung je Datei bestimmen ---------------------------------
  # Die Kodierung wird nicht am Dateinamen festgemacht, sondern am Inhalt:
  # enthaelt die Datei nur gueltige UTF-8-Sequenzen, wird sie als UTF-8
  # gelesen, sonst mit der Systemkodierung. Unter deutschem Windows ist das
  # cp1252 - eine in UTF-8 gespeicherte Datei mit Umlauten wuerde dort ohne
  # diese Pruefung falsch ausgelegt.
  kodierung <- function(datei) {
    zeilen <- tryCatch(readLines(datei, warn = FALSE), error = function(e) "")
    if (all(validUTF8(zeilen))) "UTF-8" else ""
  }

  # --- Ankerpruefung --------------------------------------------------------
  vorhandene_anker <- function() anker_namen[vapply(anker_namen, exists,
                                                    logical(1),
                                                    envir = .GlobalEnv)]

  pruefe_anker <- function(datei, vorher) {
    weg <- setdiff(vorher, vorhandene_anker())
    if (length(weg))
      stop(datei, " hat den Workspace geleert. Verschwunden sind:\n  ",
           paste(weg, collapse = ", "),
           "\n\nFast immer steht am Anfang des Skripts ein rm(list = ls()).",
           "\nIn einer Pipeline darf das nicht stehen - die Zeile loeschen.",
           call. = FALSE)
  }

  # --- ein Skript ausfuehren ------------------------------------------------
  run <- function(datei) {
    cat("\n\n>>>>> ", datei, " <<<<<\n")
    vorher <- vorhandene_anker()
    withCallingHandlers(
      source(datei, local = FALSE, encoding = kodierung(datei)),
      message = function(m) {
        cat(conditionMessage(m)); invokeRestart("muffleMessage")
      },
      warning = function(w) {
        cat("WARNUNG: ", conditionMessage(w), "\n", sep = "")
        invokeRestart("muffleWarning")
      })
    pruefe_anker(datei, vorher)
  }

  # --- 1) alles bis einschliesslich 10_load_data.R --------------------------
  # Ausgabe zwischenspeichern, weil data_dir erst dort entsteht.
  bis_daten <- seq_len(match(skript_data, pipeline))
  vor_datei <- tempfile(pattern = "fehbi_start_", fileext = ".txt")
  vor_con   <- file(vor_datei, open = "wt")
  sink(vor_con, split = TRUE)
  geladen <- tryCatch({
    for (f in pipeline[bis_daten]) run(f)
    TRUE
  }, error = function(e) {
    cat("\nFEHLER vor dem Protokollstart:\n  ", conditionMessage(e), "\n",
        sep = "")
    FALSE
  })
  while (sink.number() > sink_start) sink()
  close(vor_con)

  if (!geladen) {
    cat(readLines(vor_datei), sep = "\n")
    unlink(vor_datei)
    stop("Abbruch vor dem Protokollstart - siehe Meldung oben.", call. = FALSE)
  }
  if (!exists("data_dir", envir = .GlobalEnv)) {
    cat(readLines(vor_datei), sep = "\n")
    unlink(vor_datei)
    stop(skript_data, " hat data_dir nicht gesetzt - ohne data_dir ",
         "ist kein Protokollpfad bestimmbar.", call. = FALSE)
  }

  # --- 2) Protokoll oeffnen -------------------------------------------------
  log_dir <- file.path(get("data_dir", envir = .GlobalEnv),
                       "Statistics", "Models")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(log_dir,
                        paste0("99_protokoll_",
                               format(Sys.time(), "%Y-%m-%d_%H%M"), ".txt"))
  log_con <- file(log_file, open = "wt")
  sink(log_con, split = TRUE)
  on.exit({
    while (sink.number() > sink_start) sink()
    try(close(log_con), silent = TRUE)
    cat("\nProtokoll geschrieben:\n  ", log_file, "\n", sep = "")
  }, add = TRUE)

  cat("=============================================================\n")
  cat("FeHBI Paper 1 - Pipeline-Protokoll\n")
  cat("Gestartet:", format(Sys.time(), "%d.%m.%Y %H:%M:%S"), "\n")
  cat("R:", R.version.string, "\n")
  cat("Arbeitsverzeichnis:", getwd(), "\n")
  cat("Skripte:", length(pipeline), "\n")
  cat("=============================================================\n")

  # --- 3) Ausgabe des ersten Teils nachtragen -------------------------------
  # Der erste Teil lief mit split = TRUE, stand also live schon auf dem
  # Bildschirm. Er wird jetzt nur noch in die DATEI nachgetragen - dafuer wird
  # die Umleitung kurz geschlossen und direkt in die Verbindung geschrieben.
  # Ohne das stuenden die ersten Skripte zweimal in der Konsole.
  while (sink.number() > sink_start) sink()
  writeLines(readLines(vor_datei), log_con)
  sink(log_con, split = TRUE)
  unlink(vor_datei)

  # --- 4) restliche Kette ---------------------------------------------------
  for (f in pipeline[-bis_daten]) run(f)

  cat("\n\n========================================================\n")
  if (exists("OUTCOMES", envir = .GlobalEnv)) {
    OUT <- get("OUTCOMES", envir = .GlobalEnv)
    cat("Pipeline durchgelaufen fuer", nrow(OUT), "Outcomes:\n")
    print(OUT[, intersect(c("var", "label", "task"), names(OUT))],
          row.names = FALSE)
  }
  cat("\nBeendet:", format(Sys.time(), "%d.%m.%Y %H:%M:%S"), "\n")
  cat("\nOutputs in", log_dir, "\n")
  cat("\nHauptergebnisse\n",
      "  60_forest_ALL.png         Hauptabbildung: within vs. between\n",
      "  60_primary_summary.csv    Haupttabelle\n",
      "  60_effects_original_scale.csv  Effekte in mm / mm2 / %BL\n",
      "  60_reach_vs_sdd.csv       Reach gegen die publizierte SDD\n",
      "  60_interaction.csv        E2 x P4 je Ebene\n",
      "  70_lrt_ALL.csv            alle Likelihood-Ratio-Tests\n",
      "  70_residual_skew_ALL.csv  Entscheidungsgrundlage roh vs. log\n",
      "  80_stability_ALL.csv      Robustheit ueber alle Spezifikationen\n",
      "  40_exposure_summary.csv   Within-Anteil der Hormonvarianz\n",
      "  90_Zusammenfassung_*.pdf  Ergebnisbericht mit Erklaerungen\n")

  invisible(log_file)
}

protokoll <- pipeline_lauf()
cat("\nVollstaendiges Protokoll dieses Laufs:\n  ", protokoll, "\n", sep = "")
