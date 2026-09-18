# =============================================================================
# 20_prepare.R   |  FeHBI Paper 1 - Balance  |  Pipeline-Schritt 3 von 10
# -----------------------------------------------------------------------------
# ZWECK  Einmalige Aufbereitung für alle folgenden Skripte. Ändert nichts an
#        10_load_data.R, sondern ergänzt Spalten.
#
# LEGT AN
#   (1) MUNDLAK-ZERLEGUNG der Hormone (auf Log-Skala)
#         *_b  Personenmittel                -> Between
#         *_w  Abweichung vom Personenmittel -> Within  = ZIELGROESSE
#       Warum überhaupt: Der Random Intercept modelliert die Abhängigkeits-
#       struktur, trennt die Ebenen aber NICHT - der Koeffizient bleibt ein
#       Kompromiss aus beiden. Nimmt man das Personenmittel als eigenen Term
#       auf, ist der Koeffizient auf der Abweichung EXAKT der Fixed-Effects-
#       (Within-)Schätzer. Das ist ein Theorem, keine Näherung, und hängt
#       weder vom ICC noch von der Zahl der Messungen ab.
#       Zweiter Grund: Das RE-Modell setzt Cov(u_i, x_ij) = 0 voraus - genau
#       das verletzt jede Between-Konfundierung. Das Personenmittel absorbiert
#       diese Korrelation.
#
#       Log VOR der Zerlegung: P4 spannt über den Zyklus rund zwei
#       Grössenordnungen; das arithmetische Personenmittel wäre vom
#       Lutealwert dominiert. Auf Log-Skala heisst "within" = prozentuale
#       Abweichung vom eigenen geometrischen Mittel. Das ist eine Wahl der
#       Funktionsform, keine Verteilungsannahme.
#
#   (2) ZWEI SKALIERUNGEN, beide werden gebraucht
#         *_w / *_b          gemeinsame Gesamt-SD -> within und between direkt
#                            MITEINANDER vergleichbar (Hausman, Übersichtsplot)
#         *_w_own / *_b_own  je eigene SD -> jeder Koeffizient für sich
#                            interpretierbar ("pro typische Zyklusschwankung")
#       Primär wird die gemeinsame Skala berichtet, die zweite als Zusatzspalte.
#
#   (3) DESIGNVARIABLEN der Messstruktur
#         standing_leg  Standbein - es gibt ZWEI Zeilen pro Person x Phase.
#                       Deine Spalte, unverändert; es wird KEINE Kopie unter
#                       einem neuen Namen angelegt.
#         session_id    Person x Zyklus x Phase = eine Messgelegenheit;
#                       die zwei Beine teilen sie sich. Ohne diese Ebene
#                       sind die Standardfehler zu klein.
#         first_measure M1 ist laut Protokoll immer die Mensphase von Zyklus 1
#                       -> Lerneffekt und menstruelles Fenster sind konfundiert
#         cycle_nr      Zyklus 1 vs. 2
#
# INPUT   df_slb_mean / df_msebt_mean aus 10_load_data.R
# OUTPUT  dieselben Objekte, ergänzt; plus Konsolen-Diagnostik
# =============================================================================
library(dplyr)

if (!exists("df_slb_mean")) source("10_load_data.R", encoding = "UTF-8")

# --- Globale Einstellungen für die gesamte Pipeline -------------------------
# COP-Outcomes: roh, logarithmiert, oder BEIDE.
#   TRUE   -> nur log_cop_path / log_cop_ellipse
#   FALSE  -> nur cop_path_length / cop_ellipse_area
#   "both" -> beide Varianten laufen als eigene Outcomes durch die Pipeline.
#             Das ist die ehrliche Variante: die Wahl fällt dann anhand der
#             BEDINGTEN Residuen in 70_assumptions.R, nicht vorab.
USE_LOG_COP         <- "both"
STANDARDIZE_OUTCOME <- TRUE   # Outcome z -> Effekte in Outcome-SD vergleichbar

# --- KEINE KONVENTIONELLE RELEVANZSCHWELLE ----------------------------------
# Es wird bewusst KEIN ROPE nach Cohen (0.2) verwendet.
#
# Cohens 0.2 ist eine Konvention aus der Verhaltensforschung; Cohen selbst hat
# die Grenzen als willkuerlich bezeichnet. Fuer Balance und Sexualhormone gibt
# es keine Arbeit, die sagt: ab hier ist es relevant. Eine erfundene Linie
# erzeugt Pseudopraezision - in diesen Daten lagen zwei Intervalle bei
# 0.2077 und 0.2080 und wuerden dadurch als "nicht aequivalent" klassifiziert,
# waehrend praktisch identische Intervalle bei 0.1990 als "aequivalent"
# gegolten haetten. Diese Unterscheidung ist eine Aussage ueber die Linie,
# nicht ueber die Daten.
#
# STATTDESSEN werden zwei Dinge berichtet, die ohne Konvention auskommen:
#
#   1) Schaetzer und Konfidenzintervall in Outcome-SD - unveraendert.
#
#   2) Die RUECKRECHNUNG auf die Messskala (Funktion effekt_original unten).
#      Aus "0.083 Outcome-SD" wird "+4.2 % COP-Pfadlaenge, 95 % KI -2.1 bis
#      +10.8 %". Das braucht keine Schwelle und ist nicht angreifbar - der
#      Leser entscheidet selbst, ob ihn eine Veraenderung dieser Groesse
#      interessiert.
#
#   3) NUR bei den Reach-Richtungen zusaetzlich der Vergleich mit der
#      publizierten kleinsten erkennbaren Veraenderung (SDD). Die ist eine
#      GEMESSENE Groesse aus einer Reliabilitaetsstudie, keine Konvention -
#      deshalb bleibt sie drin. Fuer die COP-Masse existiert nichts
#      Vergleichbares, dort wird auch nichts behauptet.

# MDC nur für die Reach-Richtungen. Dort gibt es eine direkt publizierte
# kleinste erkennbare Veränderung ZWISCHEN Sessions, gemessen am SEBT, an
# gesunden jungen Freizeitsportlerinnen und -sportlern - also an einer
# Population, die zu FeHBI passt:
#   SDD 6.13-8.15 % Beinlänge über die Reichrichtungen
#     Munro & Herrington 2010, Phys Ther Sport, DOI 10.1016/j.ptsp.2010.07.002
#   SDC 5.0-7.2 % für den Composite Score
#     van Lieshout et al. 2016, Int J Sports Phys Ther
# Daraus die zusammengefasste Spanne 5.0 bis 8.2 %BL. Es wird eine SPANNE
# berichtet, kein einzelner Wert - die Literatur gibt keinen einzelnen her.
#
# Für die COP-Masse gibt es KEINE verwendbare MDC. Die vorliegenden Arbeiten
# berichten den Standardmessfehler (SEM), nicht die MDC; eine Umrechnung
# (MDC95 = 2.77 x SEM) ergäbe für die Pfadlänge rund 53 % relativ und beruht
# zudem auf einem Protokoll, das nicht dem hiesigen Einbeinstand entspricht.
# Deshalb bleibt ROPE_MDC für COP bewusst NA, statt eine Zahl zu erfinden.
MDC_RANGE <- list(reach_ant = c(5.0, 8.2),
                  reach_pl  = c(5.0, 8.2),
                  reach_pm  = c(5.0, 8.2))

# Gibt die messfehlerbasierte Relevanzschwelle als Spanne in Outcome-SD zurück,
# oder NA, wenn die Literatur für dieses Outcome nichts hergibt.
rope_mdc <- function(oc, x) {
  if (is.null(MDC_RANGE[[oc]])) return(c(NA_real_, NA_real_))
  x <- x[is.finite(x)]
  if (length(x) < 3) return(c(NA_real_, NA_real_))
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(c(NA_real_, NA_real_))
  round(MDC_RANGE[[oc]] / s, 3)
}

# =============================================================================
# RUECKRECHNUNG AUF DIE MESSSKALA
# -----------------------------------------------------------------------------
# Das Herzstueck der Berichterstattung ohne Schwelle. Ein Koeffizient von
# 0.083 Outcome-SD sagt niemandem etwas. Auf der Originalskala schon.
#
# Zwei Faelle:
#
#   LOG-OUTCOME (log_cop_path, log_cop_ellipse). Der Koeffizient b steht in
#   SD des LOGARITHMIERTEN Outcomes. b * sd(log_y) ist die Veraenderung in
#   Log-Einheiten, exp() davon der multiplikative Faktor. Angegeben wird die
#   prozentuale Veraenderung: 100 * (exp(b * sd(log_y)) - 1).
#   Lesart: "pro einer Within-SD Hormon aendert sich die Pfadlaenge um X %".
#
#   ROH-OUTCOME (reach_ant/pl/pm, cop_path_length, cop_ellipse_area). Hier ist
#   b * sd(y) direkt die Veraenderung in der Originaleinheit - % Beinlaenge
#   bei den Reach-Massen, mm bzw. mm2 bei den rohen COP-Massen.
#
# Zurueckgegeben wird immer auch die Einheit, damit die Tabellen sich selbst
# erklaeren und niemand eine Prozentangabe fuer Millimeter haelt.
# =============================================================================
effekt_original <- function(oc, b, lo, hi) {
  x <- base_of(oc)[[oc]]
  x <- x[is.finite(x)]
  s <- if (length(x) >= 3) stats::sd(x) else NA_real_
  if (!is.finite(s) || s == 0)
    return(list(wert = NA_real_, lo = NA_real_, hi = NA_real_, einheit = NA_character_))

  if (grepl("^log_", oc)) {
    f <- function(k) round(100 * (exp(k * s) - 1), 2)
    list(wert = f(b), lo = f(lo), hi = f(hi), einheit = "%")
  } else {
    einheit <- if (grepl("^reach_", oc)) "%BL" else
               if (oc == "cop_ellipse_area") "mm2" else "mm"
    f <- function(k) round(k * s, 2)
    list(wert = f(b), lo = f(lo), hi = f(hi), einheit = einheit)
  }
}

# Nachweisgrenze des Assays (Lower Limit of Quantification), falls bekannt.
# Hintergrund: log(0) ist -Inf, und ein EINZIGER Nullwert macht über sd()
# die komplette Spalte zu NaN. lmer meldet dann "0 (non-NA) cases".
#   NA          -> Werte <= 0 werden auf NA gesetzt (Zeile fällt aus)
#   Zahl        -> es wird LOD/2 eingesetzt
# Bitte nur eintragen, was im Laborbericht steht - nicht schätzen.
LOD_ESTR <- NA_real_          # Estradiol, pmol/L
LOD_PROG <- NA_real_          # Progesteron, nmol/L
RE <- "(1 | record_id) + (1 | session_id)"

# --- Outcome-Register --------------------------------------------------------
# Fünf Outcomes laut Protokoll: 2 x SLB (COP) und 3 x mSEBT (Reach).
# Bei USE_LOG_COP == "both" sind es sieben, weil die COP-Masse doppelt laufen.
# label = ENGLISCH, erscheint direkt in den Abbildungen.
cop_raw <- data.frame(var   = c("cop_path_length", "cop_ellipse_area"),
                      label = c("COP path length", "COP ellipse area"),
                      stringsAsFactors = FALSE)
cop_log <- data.frame(var   = c("log_cop_path", "log_cop_ellipse"),
                      label = c("COP path length (log)", "COP ellipse area (log)"),
                      stringsAsFactors = FALSE)
# Achtung: "else" darf auf oberster Ebene nicht auf einer neuen Zeile stehen -
# deshalb die geklammerte Form.
cop_vars <- if (identical(USE_LOG_COP, "both")) {
  rbind(cop_raw, cop_log)
} else if (isTRUE(USE_LOG_COP)) {
  cop_log
} else {
  cop_raw
}

OUTCOMES <- rbind(
  cbind(cop_vars, task = "SLB", stringsAsFactors = FALSE),
  data.frame(var   = c("reach_ant", "reach_pl", "reach_pm"),
             label = c("mSEBT anterior", "mSEBT posterolateral",
                       "mSEBT posteromedial"),
             task  = "mSEBT", stringsAsFactors = FALSE))
cat("Outcomes in dieser Pipeline (", nrow(OUTCOMES), "):\n", sep = "")
print(OUTCOMES, row.names = FALSE)

base_of <- function(oc) if (OUTCOMES$task[OUTCOMES$var == oc] == "SLB")
  df_slb_mean else df_msebt_mean

# =============================================================================
# EINHEITLICHES DESIGN FUER ALLE ABBILDUNGEN DER PIPELINE
# -----------------------------------------------------------------------------
# Zentral hier, damit 30/40/50 nichts Eigenes definieren. Regeln:
#   - ALLE Achsen, Titel und Legenden sind ENGLISCH (publikationsfertig)
#   - R-Kommentare und Konsolenausgaben bleiben deutsch (Arbeitsebene)
#   - die Plots rufen KEIN theme_*() mehr auf; theme_set() unten gilt global.
#     Nur echte Ausnahmen (z. B. Legende oben) stehen noch im Plot selbst.
#
# FARBKONVENTION - übernommen aus deinem 00_setup.R:
#   FEHBI_BLUE   = Progesteron (P4)
#   FEHBI_ORANGE = Estradiol (E2)
# Wichtig: Farbe steht damit für das HORMON, nicht für die Ebene. Within und
# between werden deshalb über Form und Linienart unterschieden - sonst hätte
# Blau in den Modellplots eine andere Bedeutung als in deinen Deskriptivplots.
# =============================================================================
library(ggplot2)

# 99_run_all.R laedt 00_setup.R bereits als ersten Schritt. Wird dieses Skript
# einzeln ausgefuehrt, ist theme_fehbi() noch nicht da - dann wird nachgeladen.
if (!exists("theme_fehbi")) {
  if (!file.exists("00_setup.R"))
    stop("00_setup.R fehlt im Arbeitsverzeichnis - ohne sie ist kein ",
         "einheitliches Design möglich.")
  source("00_setup.R", encoding = "UTF-8")
}
stopifnot(is.function(theme_fehbi), exists("FEHBI_BLUE"), exists("FEHBI_ORANGE"))

# 10_load_data.R setzt per theme_set() einen weissen Hintergrund. theme_set()
# ERSETZT das Theme, es ergänzt nicht - der weisse Hintergrund wird hier also
# wieder angehängt. Sonst exportieren die Modellplots mit transparenter
# Fläche, was in Word und in PDFs unsauber aussieht.
FEHBI_BG <- theme(
  plot.background  = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  strip.background = element_rect(fill = "white", colour = NA))
theme_set(theme_fehbi() + FEHBI_BG)

FIG_DPI <- 300    # Auflösung aller Abbildungen; viele Journals verlangen 300

# Semantische Farben, alle aus deiner Palette (my_colors_base bzw. theme_fehbi)
FEHBI_COL <- c(p4      = FEHBI_BLUE,
               e2      = FEHBI_ORANGE,
               ink     = FEHBI_INK,
               neutral = "grey45",
               accent  = "#4AA889")   # Grün aus my_colors_base

# Englische Beschriftungen, an EINER Stelle gepflegt
LAB_WITHIN  <- "Within-person (cyclic)"
LAB_BETWEEN <- "Between-person"
LAB_EFFECT  <- "Effect (outcome SD per 1 SD hormone)"
HORMONE_LAB <- c(P4 = "Progesterone (P4)", E2 = "Estradiol (E2)")
HORMONE_COL <- setNames(c(FEHBI_BLUE, FEHBI_ORANGE),
                        c(HORMONE_LAB[["P4"]], HORMONE_LAB[["E2"]]))
outcome_label <- function(v) {
  hit <- OUTCOMES$label[match(v, OUTCOMES$var)]
  ifelse(is.na(hit), v, hit)
}

# z-Standardisierung, die an einem einzelnen Inf/NaN nicht die ganze Spalte
# verliert - scale() würde in dem Fall NUR NaN zurückgeben.
zsafe <- function(x) {
  x <- ifelse(is.finite(x), x, NA_real_)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  as.numeric((x - mean(x, na.rm = TRUE)) / s)
}

# Gesamtmittel einer Personenkonstante, korrekt auf PERSONENebene gebildet.
# Ueber alle Zeilen zu mitteln wuerde Frauen mit mehr gueltigen Messungen
# staerker gewichten. Da der Wert je Person konstant ist, genuegt die jeweils
# erste Zeile pro record_id.
gm_person <- function(x, id) {
  k <- !duplicated(id)
  v <- x[k]
  v <- v[is.finite(v)]
  if (!length(v)) return(NA_real_)
  mean(v)
}

# --- Spaltennamen, die je nach Rohdaten anders heissen können ---------------
pick_col <- function(d, candidates, what) {
  hit <- candidates[candidates %in% names(d)]
  if (length(hit) == 0) {
    message("  [!] Keine Spalte für '", what, "' gefunden (gesucht: ",
            paste(candidates, collapse = ", "), ")")
    return(NA_character_)
  }
  hit[1]
}
# standing_leg ist der Name in 10_load_data.R (Werte "left"/"right").
LEG_CAND   <- c("standing_leg", "stand_leg", "leg", "stance_leg", "side",
                "limb", "bein", "standbein")
# Diese Spalte existiert in df_*_mean derzeit NICHT - siehe Block unten.
CYCLE_CAND <- c("cycle_nr", "cycle", "cycle_number", "cycle_num", "zyklus",
                "zyklus_nr", "redcap_repeat_instance")
DATE_CAND  <- c("date", "measurement_date", "visit_date", "test_date", "datum")

add_prep <- function(d, label) {
  cat("\n-- 10_prepare:", label, "--\n")
  leg_var <- pick_col(d, LEG_CAND, "Standbein")
  cyc_var <- pick_col(d, CYCLE_CAND, "Zyklusnummer")
  dat_var <- pick_col(d, DATE_CAND, "Messdatum")

  d <- d %>%
    mutate(across(c(conc_estr, conc_prog, bmi_lab, sports_min, hip_abd_stand),
                  as.numeric))

  # --- Nicht-positive Hormonwerte abfangen, BEVOR logarithmiert wird ---------
  for (v in c("conc_estr", "conc_prog")) {
    lod <- if (v == "conc_estr") LOD_ESTR else LOD_PROG
    bad <- !is.na(d[[v]]) & d[[v]] <= 0
    if (any(bad)) {
      if (is.na(lod)) {
        d[[v]][bad] <- NA_real_
        message(sprintf(paste0("  [!] %s: %d Wert(e) <= 0 -> auf NA gesetzt. ",
                               "log() ist dort nicht definiert. Wenn die ",
                               "Nachweisgrenze bekannt ist, oben LOD_%s setzen."),
                        v, sum(bad), toupper(sub("conc_", "", v))))
      } else {
        d[[v]][bad] <- lod / 2
        message(sprintf("  [i] %s: %d Wert(e) <= 0 -> LOD/2 = %.4g eingesetzt.",
                        v, sum(bad), lod / 2))
      }
    }
  }

  # --- cycle_phase: NICHT auf erwartete Labels zwingen ------------------------
  # Die Vorversion hat feste Labelnamen vorgegeben. Heissen die Stufen in den
  # Daten anders, wird dabei ALLES zu NA - stillschweigend, und alle späteren
  # Phasenblocke laufen auf leere Daten. Deshalb wird hier nur UMSORTIERT,
  # wenn die erwarteten Labels tatsächlich vorkommen, sonst bleibt alles wie
  # es ist. Treatment-Kontraste statt ordered(): .L/.Q/.C sind nicht
  # interpretierbar. cycle_phase wird ohnehin nur deskriptiv gebraucht.
  PHASE_ORDER <- c("menstrual", "late_follicular", "ovulatory", "luteal")
  obs_lvl <- unique(stats::na.omit(as.character(d$cycle_phase)))
  d$cycle_phase <- if (all(obs_lvl %in% PHASE_ORDER)) {
    factor(as.character(d$cycle_phase), levels = intersect(PHASE_ORDER, obs_lvl))
  } else {
    message("  [i] cycle_phase-Stufen weichen von der Erwartung ab und werden ",
            "unverändert übernommen: ", paste(obs_lvl, collapse = ", "))
    droplevels(factor(as.character(d$cycle_phase)))
  }
  cat("   cycle_phase:", paste(levels(d$cycle_phase), collapse = " | "),
      sprintf("(%d fehlend)\n", sum(is.na(d$cycle_phase))))

  # Standbein: die Spalte heisst in 10_load_data.R standing_leg. Es wird KEINE
  # Kopie unter neuem Namen angelegt - der Term im Modell heisst genauso wie
  # die Spalte in deinen Daten.
  if (!is.na(leg_var)) d[[leg_var]] <- droplevels(factor(d[[leg_var]]))

  # --- Zyklusnummer aus dem Phasencode ableiten -------------------------------
  # Der Phasencode ist zweistellig: ERSTE Ziffer = Zyklus, ZWEITE = Fenster.
  #   11-14 = Zyklus 1 (M1-M4),  21-24 = Zyklus 2 (M5-M8)
  # Damit braucht es keine eigene Zyklusspalte. Und weil 10_load_data.R
  # ohnehin nach `phase` gruppiert, wurden Zyklus 1 und 2 NIE zusammen-
  # gemittelt - der Verdacht von vorhin ist damit ausgeräumt.
  if ("phase" %in% names(d) && is.numeric(d$phase) &&
      all(d$phase >= 10, na.rm = TRUE)) {
    d$phase_idx <- d$phase %% 10
    d$cycle_nr  <- droplevels(factor(d$phase %/% 10))
  } else if (!is.na(cyc_var)) {
    d$phase_idx <- NA_real_
    d$cycle_nr  <- droplevels(factor(d[[cyc_var]]))
  } else {
    d$phase_idx <- NA_real_
    d$cycle_nr  <- factor(rep("1", nrow(d)))
  }
  has_cycle <- nlevels(d$cycle_nr) > 1

  # cycle_phase aus der zweiten Ziffer nachtragen, falls 10_load_data.R nur
  # die Codes 21-24 kennt und die Zyklus-1-Zeilen deshalb NA haben. Die
  # Zuordnung Ziffer -> Label wird NICHT hartkodiert, sondern aus den bereits
  # vorhandenen Paaren gelernt.
  if (any(is.na(d$cycle_phase)) && !all(is.na(d$phase_idx))) {
    lut <- d %>% filter(!is.na(cycle_phase), !is.na(phase_idx)) %>%
      distinct(phase_idx, cycle_phase)
    if (nrow(lut) && !anyDuplicated(lut$phase_idx)) {
      fehlt <- is.na(d$cycle_phase)
      d$cycle_phase[fehlt] <- lut$cycle_phase[match(d$phase_idx[fehlt],
                                                    lut$phase_idx)]
      cat(sprintf("   cycle_phase für %d Zeilen aus der Phasenziffer ergänzt\n",
                  sum(fehlt & !is.na(d$cycle_phase))))
    }
  }

  # session_id = eine Messgelegenheit (Person x Zyklus x Phase). Die zwei Beine
  # teilen sie sich. Diese Ebene bleibt auch ohne Zyklusnummer korrekt.
  d$session_id <- interaction(d$record_id, d$cycle_nr, d$cycle_phase, drop = TRUE)

  # first_measure ist OHNE Zyklusnummer nicht identifizierbar: "menstrual"
  # würde sonst JEDE Mensmessung markieren, nicht nur die allererste. Dann
  # lieber NA und aus dem Design nehmen, als eine falsche Variable adjustieren.
  # M1 ist der niedrigste Phasencode überhaupt (11): Zyklus 1, erstes Fenster.
  if (has_cycle && !all(is.na(d$phase_idx))) {
    d$first_measure <- as.integer(d$phase == min(d$phase, na.rm = TRUE))
  } else if (has_cycle) {
    d$first_measure <- as.integer(d$cycle_nr == levels(d$cycle_nr)[1] &
                                    d$cycle_phase == levels(d$cycle_phase)[1])
  } else {
    d$first_measure <- NA_integer_
    message("  [!] Nur ein Zyklus in den Daten -> cycle_nr UND first_measure ",
            "fallen aus dem Design. Beides greift automatisch, sobald die ",
            "Phasencodes 11-14 dazukommen.")
  }
  has_first <- has_cycle && length(unique(stats::na.omit(d$first_measure))) > 1

  if (!is.na(dat_var)) {
    d <- d %>% group_by(record_id) %>%
      mutate(session_idx = as.numeric(rank(.data[[dat_var]], ties.method = "min"))) %>%
      ungroup() %>% mutate(session_idx_c = session_idx - mean(session_idx, na.rm = TRUE))
  } else {
    d$session_idx <- NA_real_; d$session_idx_c <- NA_real_
    message("  [i] Kein Datum -> session_idx entfällt. first_measure steht trotzdem,",
            " weil M1 laut Protokoll immer die Mensphase von Zyklus 1 ist.")
  }

  # --- Mundlak auf Log-Skala -------------------------------------------------
  # fmean/fsd ignorieren nicht nur NA, sondern auch Inf und NaN. Sonst kann ein
  # einzelner unendlicher Wert über sd() eine ganze Spalte vernichten.
  fmean <- function(x) { x <- x[is.finite(x)]; if (!length(x)) NA_real_ else mean(x) }
  fsd   <- function(x) { x <- x[is.finite(x)]; if (length(x) < 2) NA_real_ else sd(x) }

  d <- d %>%
    # log_estr / log_prog werden hier NEU berechnet und überschreiben die
    # Versionen aus 10_load_data.R - dort steckte für conc_prog == 0 noch ein
    # -Inf drin. Namen und Bedeutung bleiben identisch.
    mutate(log_estr = log(conc_estr), log_prog = log(conc_prog),
           log_estr = ifelse(is.finite(log_estr), log_estr, NA_real_),
           log_prog = ifelse(is.finite(log_prog), log_prog, NA_real_)) %>%
    group_by(record_id) %>%
    mutate(estr_b_raw = fmean(log_estr), estr_w_raw = log_estr - estr_b_raw,
           prog_b_raw = fmean(log_prog), prog_w_raw = log_prog - prog_b_raw,
           bmi_b_raw  = fmean(bmi_lab), bmi_w_raw = bmi_lab - bmi_b_raw) %>%
    ungroup()

  cat(sprintf("   Hormone verwendbar: E2 %d | P4 %d von %d Zeilen\n",
              sum(!is.na(d$log_estr)), sum(!is.na(d$log_prog)), nrow(d)))

  # BMI wurde pro Session gemessen, ist über 2-3 Monate aber praktisch
  # konstant. Hier wird geprüft, ob die Within-Streuung überhaupt zählt.
  sd_bmi_w <- sd(d$bmi_w_raw, na.rm = TRUE)
  cat(sprintf("   BMI: SD innerhalb Person %.3f | zwischen %.3f -> %s\n",
              sd_bmi_w, sd(d$bmi_b_raw, na.rm = TRUE),
              if (isTRUE(sd_bmi_w < 0.3)) "als zeitkonstant behandeln"
              else "Within-Anteil prüfen"))

  # --- Skalierung: gemeinsame Skala (primär) + eigene SD (sekundär) --------
  sdE <- fsd(d$log_estr); sdP <- fsd(d$log_prog)
  cat(sprintf("   SD(log E2) = %.3f | SD(log P4) = %.3f\n", sdE, sdP))
  if (!is.finite(sdE) || !is.finite(sdP))
    stop("SD der log-Hormone ist nicht endlich (", label, "). Damit würden ",
         "estr_w/prog_w komplett NaN und jedes Modell meldete '0 (non-NA) ",
         "cases'. Bitte conc_estr / conc_prog auf Nullwerte und Ausreisser ",
         "prüfen.")
  own <- function(x) as.numeric(x / fsd(x))
  d <- d %>%
    mutate(
      estr_w = estr_w_raw / sdE,
      prog_w = prog_w_raw / sdP,
      # Die Personenmittel werden am Gesamtmittel ZENTRIERT. Ohne Zentrierung
      # ist estr_b der rohe Personenmittelwert auf Log-Skala, also eine Zahl
      # weit weg von null. Sobald ExP_b = estr_b * prog_b im Modell steht, ist
      # der Koeffizient von prog_b definitionsgemaess der Effekt BEI estr_b = 0
      # - eine Extrapolation weit ausserhalb der Daten, und ExP_b ist fast
      # perfekt kollinear mit beiden Haupteffekten. Genau das hat im Lauf vom
      # 21.08. zu Between-Koeffizienten von 3 bis 4 SD mit riesigen Intervallen
      # gefuehrt.
      # Die Zentrierung veraendert weder den Modellfit noch einen einzigen LRT
      # noch die Within-Koeffizienten - der Spaltenraum ist derselbe. Sie macht
      # nur die Between-Haupteffekte wieder lesbar: Effekt des einen Hormons
      # beim DURCHSCHNITTLICHEN Spiegel des anderen.
      estr_b = (estr_b_raw - gm_person(estr_b_raw, record_id)) / sdE,
      prog_b = (prog_b_raw - gm_person(prog_b_raw, record_id)) / sdP,
      estr_w_own = own(estr_w_raw), estr_b_own = own(estr_b_raw),
      prog_w_own = own(prog_w_raw), prog_b_own = own(prog_b_raw),
      # Interaktion JE EBENE, nicht als zerlegtes Produkt. Der Unterschied ist
      # nicht kosmetisch: Die Within-Komponente von log(E2)*log(P4) ist nicht
      # gleich estr_w * prog_w. Gebraucht wird die zweite Form - sie ist die
      # Frage "aendert sich der P4-Slope einer Frau, wenn ihr E2 hoch ist".
      # Kovariablendokument v3, Abschnitt 11.
      ExP_w = estr_w * prog_w, ExP_b = estr_b * prog_b,
      z_bmi_b      = zsafe(bmi_b_raw),
      z_sports_min = zsafe(sports_min),
      # z_hip_abd bleibt berechnet, wird aber in KEINEM Modell mehr verwendet -
      # nur noch deskriptiv in 40_diagnostics, Abschnitt F.
      z_hip_abd    = zsafe(hip_abd_stand)
    )

  cat(sprintf("   Zeilen %d | Personen %d | Sessions %d | Beine %s | Zyklen %s\n",
              nrow(d), n_distinct(d$record_id), n_distinct(d$session_id),
              if (is.na(leg_var)) "-" else paste(levels(d[[leg_var]]), collapse = "/"),
              paste(levels(d$cycle_nr), collapse = "/")))

  # Harte Kontrolle: eine komplett leere Schlüsselspalte darf nicht erst in
  # lmer als "0 (non-NA) cases" auffallen.
  for (v in c("estr_w", "estr_b", "prog_w", "prog_b", "ExP_w", "ExP_b")) {
    n_ok <- sum(is.finite(d[[v]]))
    if (n_ok == 0)
      stop("Spalte ", v, " ist in ", label, " vollständig NA/NaN. ",
           "Ursache liegt in conc_estr / conc_prog.")
    if (n_ok < 0.5 * nrow(d))
      message(sprintf("  [!] %s: nur %d von %d Zeilen verwendbar.", v, n_ok, nrow(d)))
  }

  # Designterme, die in DIESEN Daten überhaupt schätzbar sind. Die Namen sind
  # die ECHTEN Spaltennamen aus 10_load_data.R, keine Umbenennungen.
  dt <- character(0)
  if (!is.na(leg_var) && nlevels(d[[leg_var]]) > 1) dt <- c(dt, leg_var)
  if (has_first)                                    dt <- c(dt, "first_measure")
  if (has_cycle)                                    dt <- c(dt, "cycle_nr")
  attr(d, "design_terms") <- dt
  d
}

df_slb_mean   <- add_prep(df_slb_mean,   "SLB")
df_msebt_mean <- add_prep(df_msebt_mean, "mSEBT")

# --- Design zentral festlegen, damit 20/30/40/50 nichts hartkodieren ---------
DESIGN_TERMS <- intersect(attr(df_slb_mean,   "design_terms"),
                          attr(df_msebt_mean, "design_terms"))
DESIGN <- if (length(DESIGN_TERMS)) paste(DESIGN_TERMS, collapse = " + ") else "1"
cat("\nDesignterme:", DESIGN, "\n")
if (!"cycle_nr" %in% DESIGN_TERMS)
  cat("  -> ohne Zyklusnummer: Zyklus 1 und 2 sind in diesen Daten nicht",
      "unterscheidbar.\n     Siehe Prüfung unten.\n")

# =============================================================================
# PRUEFUNG: werden in df_*_mean zwei Zyklen still zusammengemittelt?
# -----------------------------------------------------------------------------
# 10_load_data.R bildet df_*_mean mit group_by(record_id, phase, cycle_phase,
# standing_leg). Sind in den Rohdaten BEIDE Zyklen mit denselben Phasencodes
# 21-24 abgelegt, dann landen Zyklus 1 und Zyklus 2 in derselben Gruppe und
# werden gemittelt. Das halbiert genau die Within-Variation, um die es geht,
# ohne Fehlermeldung. Laut Protokoll gibt es 2 Trials pro Session:
#   n == 2  -> ein Zyklus pro Phasencode, alles in Ordnung
#   n == 4  -> zwei Zyklen liegen übereinander -> Zyklus muss schon in
#             10_load_data.R mitgeführt
#              werden (z. B. aus der Session-ID / redcap_event_name / Datum)
# Erwartung beim aktuellen Datenstand (nur Zyklus 2): durchgehend n == 2.
# Erscheint hier eine 4, liegt Zyklus 1 doch schon im File und wird gerade
# weggemittelt - dann bitte melden.
# =============================================================================
for (nm in c("df_slb", "df_msebt")) {
  if (!exists(nm)) next
  dl <- get(nm)
  if (!all(c("record_id", "phase", "standing_leg") %in% names(dl))) next
  tt <- dl %>% count(record_id, phase, standing_leg) %>% count(n, name = "gruppen")
  cat("\nTrials je Person x Phase x Bein in", nm, ":\n")
  print(as.data.frame(tt), row.names = FALSE)
  if (any(tt$n > 2))
    cat("  [!] Gruppen mit mehr als 2 Trials. Das ist der Fingerabdruck von zwei\n",
        "      übereinandergelegten Zyklen. Bitte in 10_load_data.R eine\n",
        "      Zyklusspalte mitführen und in group_by() aufnehmen.\n")
}

cat("\n10_prepare fertig. Neue Spalten: estr_w/_b, prog_w/_b (+ _own),",
    "ExP_w/_b, leg, session_id, first_measure, cycle_nr\n")
cat("Globale Einstellungen: USE_LOG_COP =", USE_LOG_COP,
    "| STANDARDIZE_OUTCOME =", STANDARDIZE_OUTCOME,
    "| Relevanzschwelle: keine (Rueckrechnung auf Messskala)\n")
