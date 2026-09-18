# ==============================================================================
# 10_load_data.R  |  FeHBI Paper 1 - Balance  |  Pipeline-Schritt 2 von 10
# Daten einlesen und aufbereiten
#
# Datenquellen:
#   FeHBI_SLB_long.csv   – SLB Balance + Hormone + Baseline (aus build_longtable.m)
#   FeHBI_mSEBT_long.csv – mSEBT Balance + Hormone + Baseline
#
# Outcomes:
#   SLB:   COP_PathLength  – Gesamtpfadlaenge [mm], Mittelwert ueber Trial
#          COP_EllipseArea – 95%-Konfidenzellipse [mm²], Einzelwert pro Trial
#   mSEBT: reach_anterior_pct       – Anterior Reach [% Beinlaenge]
#          reach_posterolateral_pct – Posterolateral Reach [% Beinlaenge]
#          reach_posteromedial_pct  – Posteromedial Reach [% Beinlaenge]
#          leglength_mm             – Beinlaenge [mm] (Normierungsbasis)
#
# Was dieses Script erstellt:
#   df_slb       – SLB Rohdaten (alle Trials, eine Zeile pro Trial)
#   df_msebt     – mSEBT Rohdaten (alle Trials x Richtungen)
#   df_slb_mean  – SLB gemittelt: Mittelwert aus 2 Trials pro Person x Phase x Standbein
#   df_msebt_mean– mSEBT gemittelt: Mittelwert aus 2 Trials pro Person x Phase x Standbein
#                  (alle drei Reach-Richtungen als Spalten reach_ant/pl/pm)
#
# Phase-Kodierung (aus MATLAB sessionToPhase):
#   21 = Menstrual
#   22 = Late follicular
#   23 = Ovulatory
#   24 = Luteal
#
# Aggregationslogik (_mean Datensaetze):
#   Zwei Trials pro Person x Phase x Standbein sind Messwiederholungen
#   desselben Konstrukts. Ihr Mittelwert ist der stabilste Schaetzer und
#   vermeidet einen unnötigen dritten Nesting-Level in den Mixed Models.
# ==============================================================================

library(tidyverse)
library(janitor)

# Weisser Hintergrund fuer alle Plots
theme_set(
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.background = element_rect(fill = "white", color = NA)
    )
)

# ==============================================================================
# PFADE
# ------------------------------------------------------------------------------
# data_dir – die Messdaten auf dem Laufwerk S:
# code_dir – der Ordner, in dem DIESE Skripte und die Ausschlussliste liegen.
#
# WARUM code_dir ueberhaupt noetig ist: ein relativer Dateiname wie
# "FeHBI_trial_exclusions.csv" wird gegen getwd() aufgeloest. Nach einem
# Neustart von RStudio steht getwd() auf C:/Users/<name>/Documents, und dann
# findet R die Liste nicht - ohne dass irgendetwas offensichtlich kaputtgeht.
# Mit code_dir ist das Skript unabhaengig davon, wo das Arbeitsverzeichnis
# gerade steht.
#
# WENN DER ORDNER UMZIEHT: nur den ersten Eintrag anpassen. Die weiteren sind
# Rueckfallebenen fuer einen anderen Rechner oder Benutzernamen.
# ==============================================================================

# --- Code-Ordner zuerst ------------------------------------------------------
# Muss vor dem Datenordner stehen, weil die Suche nach pfade_lokal.R ihn braucht.
CODE_DIR_KANDIDATEN <- c(
  getwd(),   # im RStudio-Projekt bzw. im geklonten Repository - hat Vorrang
  "C:/Users/frds/OneDrive - ZHAW/PhD/Paper 1 Balance/R Codes",
  file.path(Sys.getenv("USERPROFILE"), "OneDrive - ZHAW/PhD/Paper 1 Balance/R Codes"),
  file.path(Sys.getenv("OneDrive"), "PhD/Paper 1 Balance/R Codes")
)
code_dir <- local({
  k <- CODE_DIR_KANDIDATEN[nzchar(CODE_DIR_KANDIDATEN)]
  k <- k[dir.exists(k)]
  if (!length(k)) getwd() else normalizePath(k[1], winslash = "/")
})
cat("Code-Ordner:", code_dir, "\n")

# --- Datenordner -------------------------------------------------------------
# WARUM NICHT EINFACH DER FESTE PFAD:
#   Sobald die Skripte in einem Git-Repository liegen, klont sie jemand anders
#   und hat kein Laufwerk S:. Ein fest geschriebener Pfad zwingt ihn, genau
#   diese Zeile zu aendern - und dann kollidiert bei jedem Pull eure jeweilige
#   Fassung derselben Datei. Der Pfad wird deshalb aufgeloest, nicht gesetzt.
#
# REIHENFOLGE, der erste Treffer gewinnt:
#   1. Umgebungsvariable FEHBI_DATA
#   2. pfade_lokal.R neben den Skripten (steht in .gitignore, wird nie geteilt)
#   3. die Standardliste unten
#
# Ohne Umgebungsvariable und ohne pfade_lokal.R greift Punkt 3.
DATA_DIR_KANDIDATEN <- c(
  "S:/appl/Beweglab_FeHBI/Studentische Arbeiten/frds"
)

# Bewusst eine benannte Funktion und kein local(): return() innerhalb von
# local() bricht mit "no function to return from" ab, weil local() keinen
# Funktionsrahmen erzeugt.
finde_data_dir <- function() {
  aus_env <- Sys.getenv("FEHBI_DATA")
  if (nzchar(aus_env) && dir.exists(aus_env)) {
    cat("Datenordner aus der Umgebungsvariable FEHBI_DATA\n")
    return(normalizePath(aus_env, winslash = "/"))
  }

  for (p in unique(file.path(c(code_dir, getwd()), "pfade_lokal.R"))) {
    if (file.exists(p)) {
      e <- new.env()
      sys.source(p, envir = e)
      if (!is.null(e$FEHBI_DATA) && dir.exists(e$FEHBI_DATA)) {
        cat("Datenordner aus", p, "\n")
        return(normalizePath(e$FEHBI_DATA, winslash = "/"))
      }
    }
  }

  k <- DATA_DIR_KANDIDATEN[dir.exists(DATA_DIR_KANDIDATEN)]
  if (length(k)) return(normalizePath(k[1], winslash = "/"))

  stop("Der Datenordner wurde nicht gefunden.\n",
       "  Gesucht wurde:\n    ",
       paste(DATA_DIR_KANDIDATEN, collapse = "\n    "), "\n",
       "  Loesung, eine davon:\n",
       "    a) Datei pfade_lokal.R neben die Skripte legen, Inhalt eine Zeile:\n",
       "         FEHBI_DATA <- \"P:/mein/pfad/zu/den/daten\"\n",
       "    b) Umgebungsvariable FEHBI_DATA setzen\n",
       "  pfade_lokal.R steht in .gitignore und wird nicht mitversioniert.",
       call. = FALSE)
}

data_dir <- finde_data_dir()
cat("Datenordner:", data_dir, "\n")

plot_dir <- file.path(data_dir, "Statistics/Plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# FARBPALETTEN
# ==============================================================================

my_colors_base <- c(
  "#E8A838", "#5B8DB8", "#4AA889", "#D4B840", "#3D7ABF",
  "#C9663A", "#8B6BB1", "#5C5C5C", "#7BAF3E", "#D4826A",
  "#4E9B8F", "#B85C8A", "#6E8B3D", "#C47DB5", "#4A7FA5",
  "#D97B4A", "#6B9E6B", "#A05C5C", "#7B6EA8", "#3A8FA0",
  "#C4A03A", "#8A3A5C", "#5A7A3A", "#A07B3A", "#3A5AA0",
  "#C43A8A", "#7A3AC4", "#3AC47A", "#C47A3A", "#5A3AC4",
  "#A0522D", "#2E8B57", "#8B0000", "#4682B4", "#DAA520",
  "#9370DB", "#20B2AA", "#FF6347", "#6495ED", "#8FBC8F"
)

# Auf 100 Farben interpolieren, damit scale_color_manual(record_id) immer
# genug Werte hat (aktuell 65 Probandinnen; 100 gibt Reserve).
my_colors <- grDevices::colorRampPalette(my_colors_base)(100)

phase_colors <- c(
  "menstrual"       = "#E8A838",
  "late_follicular" = "#4AA889",
  "ovulatory"       = "#3D7ABF",
  "luteal"          = "#C9663A"
)

# ==============================================================================
# 1) DATEN EINLESEN
#    Beide CSVs wurden von MATLAB (build_longtable.m) mit Komma als
#    Trennzeichen und Punkt als Dezimalzeichen exportiert.
#    NaN ist MATLAB-Fehlerwert -> wird als NA behandelt.
# ==============================================================================

cat("\n-- Daten einlesen ---------------------------------------------------\n")

raw_slb <- read_csv(
  file.path(data_dir, "FeHBI_SLB_long.csv"),
  na             = c("", "NA", "NaN", "#ZAHL!"),
  locale         = locale(decimal_mark = "."),
  show_col_types = FALSE
)

raw_msebt <- read_csv(
  file.path(data_dir, "FeHBI_mSEBT_long.csv"),
  na             = c("", "NA", "NaN", "#ZAHL!"),
  locale         = locale(decimal_mark = "."),
  show_col_types = FALSE
)

cat(sprintf("  SLB Rohdaten:   %d Zeilen x %d Spalten\n",
            nrow(raw_slb), ncol(raw_slb)))
cat(sprintf("  mSEBT Rohdaten: %d Zeilen x %d Spalten\n",
            nrow(raw_msebt), ncol(raw_msebt)))


# ==============================================================================
# 2) GEMEINSAME AUFBEREITUNG (SLB und mSEBT)
#
# - Spaltennamen vereinheitlichen via clean_names()
#   Nach clean_names(): COP_PathLength    -> cop_path_length
#                       COP_EllipseArea   -> cop_ellipse_area
#                       side_kinResults   -> side_kin_results   (mSEBT)
#                       side_kinetResults -> side_kinet_results (SLB)
#                       hip_abd_l/r kommen ohne Suffix an
# - Standbein via coalesce(side_kin_results, side_kinet_results):
#   SLB nutzt side_kinet_results, mSEBT nutzt side_kin_results
# - Zyklusphase als geordneten Faktor kodieren
# - Sport type kategorisieren (Stop-and-Go vs. nicht)
# - Standbeinspezifische Huefte-Abduktionskraft berechnen
#   hip_abd_stand = hip_abd_l oder hip_abd_r je nach Standbein [N]
# ==============================================================================

# Stop-and-Go Sportarten (Regex case-insensitive)
stop_and_go_pattern <- paste0(
  "(?i)(fussball|handball|basketball|volleyball|beachvolleyball|",
  "unihockey|korbball|faustball|rugby|wasserball|tennis|",
  "badminton|squash|eishockey|kickbox|boxen)"
)

prep_balance <- function(raw) {
  raw %>%
    # --- Spaltennamen bereinigen ---
    # Ergebnis u.a.: cop_path_length, cop_ellipse_area,
    #                reach_anterior_pct, reach_posterolateral_pct,
    #                reach_posteromedial_pct, leglength_mm
    clean_names() %>%
    
    # --- sports_min absichern ---------------------------------------------
  # Seit der Korrektur in build_longtable.m kommt sports_min bereits als
  # saubere Zahl an (Notationsvarianten wie "3x60min" oder "1.5 h" werden
  # dort aufgeloest). parse_number() ist deshalb nur noch eine Rueckfall-
  # ebene fuer den Fall, dass die Spalte doch als Text ankommt.
  # ACHTUNG, falls jemals wieder Freitext durchrutscht: parse_number("3x60")
  # liefert 3, nicht 180 - also stillschweigend falsch. Die Aufloesung
  # gehoert nach MATLAB, nicht hierher.
  mutate(sports_min = readr::parse_number(as.character(sports_min))) %>%
    
    # --- Standbein vereinheitlichen ---
    # MATLAB outerjoin erzeugt zwei side-Spalten mit unterschiedlichen Suffixen:
    #   side_kinResults   -> nach clean_names(): side_kin_results
    #   side_kinetResults -> nach clean_names(): side_kinet_results
    # SLB:   side kommt aus kinetResults (kinResults hat keine SLB-Zeilen mehr)
    # mSEBT: side kommt aus kinResults   (kinetResults hat keine mSEBT-Zeilen)
    # coalesce() nimmt den ersten nicht-NA Wert -> robust fuer beide Tasks
    mutate(
      side_raw = dplyr::coalesce(
        if ("side"               %in% names(.)) side               else NA_character_,
        if ("side_kin_results"   %in% names(.)) side_kin_results   else NA_character_,
        if ("side_kinet_results" %in% names(.)) side_kinet_results else NA_character_
      ),
      standing_leg = case_when(
        side_raw == "L" ~ "left",
        side_raw == "R" ~ "right",
        TRUE            ~ NA_character_
      )
    ) %>%
    
    # --- Zyklusphase als geordneter Faktor ---
    # phase kommt aus MATLAB als 2-stellige Zahl (21-24)
    mutate(
      cycle_phase = factor(
        case_when(
          phase == 21 ~ "menstrual",
          phase == 22 ~ "late_follicular",
          phase == 23 ~ "ovulatory",
          phase == 24 ~ "luteal"
        ),
        levels = c("menstrual", "late_follicular", "ovulatory", "luteal")
      )
    ) %>%
    
    # --- Sport type ---
    # 1 = Stop-and-Go Sportart, 0 = nicht
    mutate(
      sport_stop_go = as.integer(
        grepl(stop_and_go_pattern, sports_types, perl = TRUE)
      )
    ) %>%
    
    # --- Standbeinspezifische Huefte-Abduktionskraft ---
    # hip_abd_l/r: Mittelwert aus 2 Wdh. [N], berechnet in build_longtable.m
    # hip_abd_stand: Wert des Standbeins [N]
    mutate(
      hip_abd_stand = case_when(
        standing_leg == "left"  ~ hip_abd_l,
        standing_leg == "right" ~ hip_abd_r,
        TRUE                    ~ NA_real_
      )
    )
}

df_slb   <- prep_balance(raw_slb)
df_msebt <- prep_balance(raw_msebt)

# =============================================================================
# AUSSCHLÜSSE
# -----------------------------------------------------------------------------
# WAS ER TUT: liest FeHBI_trial_exclusions.csv und entfernt die dort
#        gelisteten Einzeltrials aus df_slb / df_msebt. Die Rohdateien bleiben
#        unangetastet; ausgeschlossen wird ausschliesslich ueber diese Liste.
#        Damit ist jede Entscheidung nachvollziehbar, umkehrbar und
#        zitierfaehig - und der Ausschluss steht nicht verstreut im Code.
#
# WARUM HIER UND NIRGENDWO SONST:
#   Vorher geht nicht - standing_leg entsteht erst in prep_balance().
#   Nachher geht nicht - das summarise() in Abschnitt 3 mittelt ueber die
#   Trials, danach ist der schlechte Versuch im Mittelwert verbacken.
#
# DREI SICHERUNGEN, alle brechen ab statt still weiterzulaufen:
#   1. Datei nicht gefunden          -> stop() mit Angabe, wo gesucht wurde
#   2. Eine Zeile trifft keinen Trial -> stop() mit der betroffenen Zeile
#   3. Zahl der entfernten Zeilen weicht von der Trefferkontrolle ab -> stop()
#
#   Sicherung 1 bricht bewusst ab und meldet nicht nur. Ein NICHT angewendeter
#   Ausschluss ist genauso schwerwiegend wie ein falscher - er muss also
#   genauso laut scheitern, statt still auf ungereinigten Daten weiterzurechnen.
#
#   Wer bewusst einmal ohne Bereinigung rechnen will (z. B. um den Einfluss
#   der Ausschluesse zu zeigen), setzt EXCL_REQUIRED <- FALSE.
# =============================================================================

EXCL_REQUIRED <- TRUE   # FALSE nur fuer einen bewussten Lauf ohne Bereinigung

EXCL_FILE <- local({
  muster <- "^FeHBI_trial_exclusions\\.csv$"
  # Reihenfolge: Code-Ordner, Arbeitsverzeichnis, dann Unterordner von beiden.
  kand <- c(
    list.files(code_dir, pattern = muster, full.names = TRUE),
    list.files(".",      pattern = muster, full.names = TRUE),
    list.files(code_dir, pattern = muster, recursive = TRUE, full.names = TRUE),
    list.files(".",      pattern = muster, recursive = TRUE, full.names = TRUE)
  )
  # ERST normalisieren, DANN entdoppeln. Sonst gelten "./x.csv" und
  # "C:/.../x.csv" als zwei Treffer, obwohl es dieselbe Datei ist - und die
  # Warnung unten meldet eine Mehrdeutigkeit, die es gar nicht gibt.
  kand <- kand[file.exists(kand)]
  kand <- unique(normalizePath(kand, winslash = "/", mustWork = FALSE))
  
  if (!length(kand)) {
    aehnlich <- unique(c(list.files(code_dir, pattern = "(?i)exclusion",
                                    recursive = TRUE, full.names = TRUE),
                         list.files(".", pattern = "(?i)exclusion",
                                    recursive = TRUE, full.names = TRUE)))
    csvs <- list.files(code_dir, pattern = "(?i)\\.csv$")
    txt <- paste0(
      "FeHBI_trial_exclusions.csv wurde nicht gefunden.\n",
      "  Gesucht in code_dir:        ", code_dir, "\n",
      "  und im Arbeitsverzeichnis:  ", getwd(), "\n",
      if (length(aehnlich))
        paste0("  Aehnlich benannte Dateien:\n    ",
               paste(aehnlich, collapse = "\n    "), "\n",
               "  -> Endung pruefen. Windows blendet '.txt' im Explorer aus,\n",
               "     eine aus Notepad gespeicherte Datei heisst oft in\n",
               "     Wahrheit FeHBI_trial_exclusions.csv.txt\n")
      else "",
      "  CSV-Dateien im Code-Ordner:\n    ",
      if (length(csvs)) paste(csvs, collapse = "\n    ") else "(keine)", "\n",
      "  Loesung: die Liste in den Code-Ordner legen, oder oben CODE_DIR_",
      "KANDIDATEN anpassen.")
    if (isTRUE(EXCL_REQUIRED)) stop(txt, call. = FALSE)
    message(txt)
    NA_character_
  } else {
    if (length(kand) > 1)
      warning("Mehrere Ausschlusslisten gefunden, verwendet wird die erste:\n  ",
              paste(normalizePath(kand, winslash = "/"), collapse = "\n  "),
              call. = FALSE)
    kand[1]
  }
})

apply_exclusions <- function(d, test_label) {
  if (is.na(EXCL_FILE)) {
    message("  [!] KEIN Trial ausgeschlossen (EXCL_REQUIRED = FALSE).")
    return(d)
  }
  # Bewusst OHNE fileEncoding: Excel schreibt unter deutschem Windows cp1252,
  # eine erzwungene UTF-8-Auslegung wuerde Umlaute im Grundtext abschneiden.
  # Die Schluesselspalten sind ohnehin reines ASCII; grund wird nur gedruckt.
  ex <- utils::read.csv(EXCL_FILE, stringsAsFactors = FALSE)
  
  soll <- c("test", "record_id", "phase", "standing_leg", "rep", "grund")
  fehlt_sp <- setdiff(soll, names(ex))
  if (length(fehlt_sp))
    stop("In ", basename(EXCL_FILE), " fehlen die Spalten: ",
         paste(fehlt_sp, collapse = ", "),
         "\nGefunden wurden: ", paste(names(ex), collapse = ", "), call. = FALSE)
  
  # --- Ist die Datei ueberhaupt heil? ---------------------------------------
  # Wird die Liste in Excel geoeffnet und gespeichert, kann eine Zeile, deren
  # Begruendung ein Komma enthaelt, komplett in EIN Feld geraten:
  #     "SLB,sub075,23,left,1,""Ganze Session ...,...,SF"
  # read.csv liest das klaglos: test enthaelt dann die ganze Zeile, record_id
  # und phase sind NA. Der Filter ex$test == test_label wirft solche Zeilen
  # anschliessend LAUTLOS weg - der Ausschluss findet nicht statt, und man
  # merkt es erst Schritte spaeter an einer Zeilenzahl.
  # Deshalb: jede Zeile muss ein bekanntes Testlabel tragen und darf in den
  # Schluesselspalten kein NA haben.
  gueltig <- c("SLB", "mSEBT")
  krumm   <- !ex$test %in% gueltig |
    is.na(ex$record_id) | is.na(ex$phase) |
    is.na(ex$standing_leg) | is.na(ex$rep)
  if (any(krumm))
    stop(basename(EXCL_FILE), ": ", sum(krumm), " Zeile(n) sind nicht ",
         "auswertbar. Erwartet wird in Spalte 'test' genau 'SLB' oder ",
         "'mSEBT', und in record_id / phase / standing_leg / rep darf nichts ",
         "fehlen.\nBetroffen (gekuerzt):\n",
         paste(sprintf("  Zeile %d: %s", which(krumm),
                       substr(ex$test[krumm], 1, 60)), collapse = "\n"),
         "\n\nFast immer ist das ein Excel-Artefakt: eine Begruendung mit ",
         "Komma wird beim Speichern neu eingepackt und die ganze Zeile landet ",
         "in einem Feld. Abhilfe: KEINE Kommas in der Spalte 'grund' ",
         "verwenden, dann muss nichts in Anfuehrungszeichen stehen. Die Datei ",
         "am besten in einem Texteditor pflegen, nicht in Excel.",
         call. = FALSE)
  
  ex <- ex[ex$test == test_label, , drop = FALSE]
  if (!nrow(ex)) {
    cat(sprintf("  %s: keine Ausschluesse in der Liste.\n", test_label))
    return(d)
  }
  
  key <- c("record_id", "phase", "standing_leg", "rep")
  fehlt <- setdiff(key, names(d))
  if (length(fehlt))
    stop("apply_exclusions: Spalte(n) ", paste(fehlt, collapse = ", "),
         " fehlen in ", test_label, ". Ohne sie ist kein Trial identifizierbar.",
         call. = FALSE)
  
  # Typen angleichen - read.csv liefert phase/rep als integer, die Rohdaten
  # koennen numeric sein; ein Join ueber verschiedene Typen matcht sonst nicht.
  ex$phase <- as.numeric(ex$phase); ex$rep <- as.numeric(ex$rep)
  d$phase  <- as.numeric(d$phase);  d$rep  <- as.numeric(d$rep)
  ex$standing_leg <- as.character(ex$standing_leg)
  d$standing_leg  <- as.character(d$standing_leg)
  
  # Kontrolle: trifft jede Ausschlusszeile auch wirklich etwas?
  treffer <- ex %>%
    dplyr::rowwise() %>%
    dplyr::mutate(n_zeilen = sum(d$record_id == record_id & d$phase == phase &
                                   d$standing_leg == standing_leg & d$rep == rep)) %>%
    dplyr::ungroup()
  leer <- treffer %>% dplyr::filter(n_zeilen == 0)
  if (nrow(leer))
    stop("apply_exclusions (", test_label, "): ", nrow(leer),
         " Eintrag/Eintraege der Ausschlussliste passen auf keinen Trial:\n",
         paste(sprintf("  %s Phase %s %s Versuch %s", leer$record_id, leer$phase,
                       leer$standing_leg, leer$rep), collapse = "\n"),
         "\nBitte ", basename(EXCL_FILE), " pruefen.", call. = FALSE)
  
  vor <- nrow(d)
  d <- dplyr::anti_join(d, ex[, key], by = key)
  entfernt <- vor - nrow(d)
  
  # mSEBT hat DREI Zeilen je Trial (eine pro Reichrichtung) - dort ist die Zahl
  # der entfernten Zeilen deshalb dreimal so gross wie die Zahl der Trials.
  erwartet <- sum(treffer$n_zeilen)
  if (entfernt != erwartet)
    stop("apply_exclusions (", test_label, "): ", entfernt, " Zeilen entfernt, ",
         "erwartet waren ", erwartet, ". Der Join hat nicht getan, was die ",
         "Kontrolle angekuendigt hat.", call. = FALSE)
  
  cat(sprintf("  %s: %d Trial(s) laut Liste ausgeschlossen -> %d von %d Zeilen entfernt\n",
              test_label, nrow(ex), entfernt, vor))
  print(as.data.frame(treffer[, c("record_id", "phase", "standing_leg", "rep",
                                  "n_zeilen", "grund")]), row.names = FALSE)
  d
}

cat("\n-- Ausgeschlossene Einzeltrials --------------------------------------\n")
if (!is.na(EXCL_FILE)) cat("  Liste:", EXCL_FILE, "\n")
df_slb   <- apply_exclusions(df_slb,   "SLB")
df_msebt <- apply_exclusions(df_msebt, "mSEBT")

cat(sprintf("  SLB aufbereitet:   %d Zeilen\n", nrow(df_slb)))
cat(sprintf("  mSEBT aufbereitet: %d Zeilen\n", nrow(df_msebt)))


# ==============================================================================
# 3) AGGREGIERTE DATENSAETZE
#
#    Mittelwert aus 2 Trials pro Person x Phase x Standbein
#    (mSEBT: die drei Reach-Richtungen liegen als Spalten vor, nicht als Zeilen)
#
#    SLB-Outcomes (nach clean_names):
#      cop_path_length  – Pfadlaenge [mm]: Mittelwert der Trial-Gesamtpfade
#      cop_ellipse_area – Ellipsenflaeche [mm²]: Mittelwert der Trial-Werte
#
#    mSEBT-Outcomes:
#      reach_anterior_pct       – Anterior Reach [% BL]: Mittelwert
#      reach_posterolateral_pct – Posterolateral Reach [% BL]: Mittelwert
#      reach_posteromedial_pct  – Posteromedial Reach [% BL]: Mittelwert
#      leglength_mm             – Beinlaenge [mm]: first() (konstant pro Person)
#
#    Kovariablen:
#      conc_estr, conc_prog, bmi_lab – Mittelwert (Einzelmessung pro Phase,
#                                       Mittelwert ueber 2 Standbeine)
#      sports_min, sport_stop_go,
#      hip_abd_l/r, hip_abd_stand    – first() (konstant pro Person/Standbein)
#
#    HINWEIS zu den Zeilenzahlen: wird EIN Versuch einer Zelle ausgeschlossen,
#    bleibt die Zelle bestehen - sie wird dann nur ueber einen statt ueber zwei
#    Trials gemittelt. Die Zeilenzahl von df_*_mean aendert sich dadurch NICHT.
#    Sie sinkt erst, wenn eine Zelle komplett wegfaellt (wie bei sub075).
# ==============================================================================

# --- SLB ---
df_slb_mean <- df_slb %>%
  group_by(record_id, phase, cycle_phase, standing_leg) %>%
  summarise(
    # COP Outcomes (primaer)
    # Mittelwert aus 2 Trials (jeder Trial-Wert bereits ueber gesamte Standphase)
    cop_path_length  = mean(cop_path_length,  na.rm = TRUE),  # [mm]
    cop_ellipse_area = mean(cop_ellipse_area, na.rm = TRUE),  # [mm²]
    # Hormone (Einzelmessung pro Phase; Mittelwert falls 2 Standbeine)
    conc_estr        = mean(conc_estr, na.rm = TRUE),         # [pmol/L]
    conc_prog        = mean(conc_prog, na.rm = TRUE),         # [nmol/L]
    bmi_lab          = mean(bmi_lab,   na.rm = TRUE),         # [kg/m²]
    # Baseline M0 (konstant pro Person bzw. pro Standbein)
    sports_min       = first(sports_min),                     # [min/Woche]
    sport_stop_go    = first(sport_stop_go),                  # [0/1]
    sports_types     = first(sports_types),
    hip_abd_l        = first(hip_abd_l),                      # [N]
    hip_abd_r        = first(hip_abd_r),                      # [N]
    hip_abd_stand    = first(hip_abd_stand),                  # [N]
    .groups = "drop"
  ) %>%
  # df_slb_mean: nach summarise(...), vor mutate(log_estr = ...)
  mutate(across(
    c(cop_path_length, cop_ellipse_area, conc_estr, conc_prog, bmi_lab),
    ~ ifelse(is.nan(.x), NA_real_, .x)
  )) %>%
  # Log-Transformationen fuer rechtschiefe COP-Variablen
  # (Verteilungen und Residuenschiefe siehe 30_descriptives.R bzw.
  #  70_assumptions.R - USE_LOG_COP steht auf "both")
  mutate(
    log_estr         = log(conc_estr),
    log_prog         = log(conc_prog),
    log_cop_path     = log(cop_path_length),
    log_cop_ellipse  = log(cop_ellipse_area)
  )

# --- mSEBT ---
df_msebt_mean <- df_msebt %>%
  group_by(record_id, phase, cycle_phase, standing_leg) %>%
  summarise(
    # Reach Outcomes (primaer)
    # Mittelwert aus 2 Trials (Einzelwert pro Trial aus calcYBalance_FeHBI)
    reach_ant        = mean(reach_anterior_pct,       na.rm = TRUE),  # [% BL]
    reach_pl         = mean(reach_posterolateral_pct, na.rm = TRUE),  # [% BL]
    reach_pm         = mean(reach_posteromedial_pct,  na.rm = TRUE),  # [% BL]
    leglength_mm     = first(leglength_mm),                           # [mm]
    # Hormone
    conc_estr        = mean(conc_estr, na.rm = TRUE),                 # [pmol/L]
    conc_prog        = mean(conc_prog, na.rm = TRUE),                 # [nmol/L]
    bmi_lab          = mean(bmi_lab,   na.rm = TRUE),                 # [kg/m²]
    # Baseline M0
    sports_min       = first(sports_min),                             # [min/Woche]
    sport_stop_go    = first(sport_stop_go),                          # [0/1]
    sports_types     = first(sports_types),
    hip_abd_l        = first(hip_abd_l),                              # [N]
    hip_abd_r        = first(hip_abd_r),                              # [N]
    hip_abd_stand    = first(hip_abd_stand),                          # [N]
    .groups = "drop"
  ) %>%
  # df_msebt_mean: nach summarise(...), vor mutate(log_estr = ...)
  mutate(across(
    c(reach_ant, reach_pl, reach_pm, conc_estr, conc_prog, bmi_lab),
    ~ ifelse(is.nan(.x), NA_real_, .x)
  )) %>%
  mutate(
    log_estr = log(conc_estr),
    log_prog = log(conc_prog)
  )


# ==============================================================================
# 4) KONTROLLAUSGABEN
# ==============================================================================

cat("\n-- Datenstruktur ----------------------------------------------------\n")
cat(sprintf("  SLB Rohdaten:    %d Zeilen (%d Personen)\n",
            nrow(df_slb), n_distinct(df_slb$record_id)))
cat(sprintf("  SLB gemittelt:   %d Zeilen (%d Personen x 4 Phasen x 2 Beine)\n",
            nrow(df_slb_mean), n_distinct(df_slb_mean$record_id)))
cat(sprintf("  mSEBT Rohdaten:  %d Zeilen (%d Personen)\n",
            nrow(df_msebt), n_distinct(df_msebt$record_id)))
cat(sprintf("  mSEBT gemittelt: %d Zeilen\n", nrow(df_msebt_mean)))

cat("\n-- Phasenverteilung SLB (gemittelt) ---------------------------------\n")
df_slb_mean %>% count(cycle_phase) %>% print()

cat("\n-- Fehlende Werte SLB (gemittelt) -----------------------------------\n")
df_slb_mean %>%
  summarise(
    n_total       = n(),
    miss_cop_path = sum(is.na(cop_path_length)),
    miss_cop_ell  = sum(is.na(cop_ellipse_area)),
    miss_e2       = sum(is.na(conc_estr)),
    miss_p4       = sum(is.na(conc_prog)),
    miss_hip_abd  = sum(is.na(hip_abd_stand))
  ) %>%
  print()

cat("\n-- Fehlende Werte mSEBT (gemittelt) ---------------------------------\n")
df_msebt_mean %>%
  summarise(
    n_total    = n(),
    miss_ant   = sum(is.na(reach_ant)),
    miss_pl    = sum(is.na(reach_pl)),
    miss_pm    = sum(is.na(reach_pm)),
    miss_e2    = sum(is.na(conc_estr)),
    miss_p4    = sum(is.na(conc_prog))
  ) %>%
  print()

cat("\n✓ Daten geladen\n")