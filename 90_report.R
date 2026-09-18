# =============================================================================
# 90_report.R  |  FeHBI Paper 1  |  Pipeline-Schritt 10 von 10
# -----------------------------------------------------------------------------
# ZWECK  Eine PDF-Zusammenfassung der wichtigsten Ergebnisse, mit Erklärung
#        dazu, was die Zahlen bedeuten. Gedacht zum Ausdrucken und Mitnehmen
#        ins Meeting - nicht als Ersatz für die CSV-Dateien.
#
# ROBUST  Liest ausschliesslich die geschriebenen CSV-Dateien, nicht die
#         Objekte im Speicher. Damit läuft es auch, wenn nur ein Teil der
#         Pipeline gelaufen ist: fehlende Abschnitte werden übersprungen und
#         am Ende aufgelistet, statt einen Fehler zu werfen.
#
# SPRACHE Fliesstext deutsch mit Umlauten (Arbeitsdokument), Abbildungen
#         englisch - das sind die Paper-Abbildungen.
#         WICHTIG: Diese Datei ist UTF-8. In RStudio unter
#         File > Reopen with Encoding > UTF-8 öffnen, falls die Umlaute
#         zerschossen aussehen. cairo_pdf() gibt sie korrekt aus; das
#         Ersatzgerät pdf() nutzt ISOLatin1 und kann sie ebenfalls.
#         In einer reinen C/POSIX-Locale (nackter Server, cron) kann R
#         keine Umlaute darstellen; dort vorher Sys.setlocale() auf eine
#         UTF-8-Locale setzen. Unter Windows/RStudio ist das kein Thema.
#
# OUTPUT  90_Zusammenfassung_<datum>.pdf
# =============================================================================
library(ggplot2)

out_dir <- file.path(data_dir, "Statistics", "Models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(exists("FEHBI_COL"), exists("OUTCOMES"))

pdf_file <- file.path(out_dir, paste0("90_Zusammenfassung_",
                                      format(Sys.time(), "%Y-%m-%d"), ".pdf"))

# --- Hilfsfunktionen ---------------------------------------------------------
lies <- function(datei) {
  p <- file.path(out_dir, datei)
  if (!file.exists(p)) return(NULL)
  tryCatch(read.csv(p, stringsAsFactors = FALSE), error = function(e) NULL)
}
fehlt <- character(0)

INK <- "#1F2933"; MUT <- "#5C6874"; ACC <- unname(FEHBI_COL[["p4"]])
WARN <- unname(FEHBI_COL[["e2"]])

# Layoutkonstanten. WRAP_W bricht zu breite Tabellen um; die Schriftgrösse
# wird NICHT geschätzt, sondern zur Laufzeit gemessen (siehe mono_fit unten).
# Genau diese Schätzung war zweimal falsch und hat Spalten abgeschnitten.
MONO_SIZE <- 8.5
MONO_MIN  <- 4.0
# WRAP_W war zu klein: es hat Tabellen umgebrochen, die mono_fit problemlos
# durch Verkleinern untergebracht hätte - und beim Umbruch geht der Schlüssel
# (Outcome, Hormon) im zweiten Block verloren. Jetzt greift zuerst die
# gemessene Schriftverkleinerung; der Umbruch ist nur noch das Notventil für
# Tabellen, die selbst bei Minimalschrift nicht passen.
WRAP_W    <- 95

# Misst die breiteste Zeile eines Monospace-Blocks im aktuellen Viewport und
# gibt die Schriftgrösse zurück, bei der sie sicher hineinpasst. Muss INNERHALB
# des Seiten-Viewports aufgerufen werden, weil "npc" sich darauf bezieht.
mono_fit <- function(zeilen, start = MONO_SIZE) {
  zeilen <- zeilen[!is.na(zeilen)]
  if (!length(zeilen)) return(start)
  laengste <- zeilen[which.max(nchar(zeilen))]
  grid::pushViewport(grid::viewport(
    gp = grid::gpar(fontsize = start, fontfamily = "mono")))
  w <- try(grid::convertWidth(grid::stringWidth(laengste), "npc",
                              valueOnly = TRUE), silent = TRUE)
  grid::popViewport()
  if (inherits(w, "try-error") || !is.finite(w) || w <= 0) return(start)
  if (w <= 0.97) return(start)
  max(MONO_MIN, floor(start * 0.97 / w * 10) / 10)
}

# Eine Textseite mit automatischem Seitenumbruch. `bloecke` ist eine Liste aus:
#   list(typ = "h",    text = "Überschrift")
#   list(typ = "p",    text = "Fliesstext, wird umbrochen")
#   list(typ = "mono", text = <character vector, schon formatiert>)
#   list(typ = "warn", text = "Hinweis, farbig")
seite <- function(titel, bloecke, fuss = NULL) {
  vp <- grid::viewport(x = 0.05, y = 0.05, width = 0.90, height = 0.90,
                       just = c("left", "bottom"))
  y <- 1
  offen <- FALSE

  schliessen <- function() {
    if (offen) {
      if (!is.null(fuss))
        grid::grid.text(fuss, x = 0, y = 0.005, just = c("left", "bottom"),
                        gp = grid::gpar(fontsize = 7, col = MUT))
      grid::popViewport()
      offen <<- FALSE
    }
  }
  # Reicht der Platz nicht mehr, wird umgebrochen statt abgeschnitten.
  neue_seite <- function(t) {
    schliessen()
    grid::grid.newpage(); grid::pushViewport(vp)
    offen <<- TRUE; y <<- 1
    grid::grid.text(t, x = 0, y = y, just = c("left", "top"),
                    gp = grid::gpar(fontsize = 15, col = INK, fontface = 2))
    y <<- y - 0.045
  }
  zeile <- function(txt, cex, col, face = 1, fam = "sans", dy = 0.028) {
    if (y < 0.05) neue_seite(paste(titel, "(Fortsetzung)"))
    grid::grid.text(txt, x = 0, y = y, just = c("left", "top"),
                    gp = grid::gpar(fontsize = cex, col = col,
                                    fontface = face, fontfamily = fam))
    y <<- y - dy
  }

  neue_seite(titel)
  for (b in bloecke) {
    if (b$typ == "h") {
      y <- y - 0.012
      zeile(b$text, 11, ACC, face = 2, dy = 0.030)
    } else if (b$typ == "p") {
      for (l in strwrap(b$text, width = 105)) zeile(l, 9, INK, dy = 0.021)
    } else if (b$typ == "warn") {
      for (l in strwrap(b$text, width = 105)) zeile(l, 9, WARN, face = 2, dy = 0.021)
    } else if (b$typ == "mono") {
      fs <- mono_fit(b$text)
      dy <- 0.0021 * fs + 0.002
      # Tabellen nicht mitten durchtrennen: passt der Block nicht mehr auf die
      # Seite, würde die Kopfzeile allein zurückbleiben. Passt er auf eine
      # frische Seite, wird dort begonnen.
      noetig <- length(b$text) * dy
      if (y - noetig < 0.05 && noetig < 0.88)
        neue_seite(paste(titel, "(Fortsetzung)"))
      for (l in b$text) zeile(l, fs, INK, fam = "mono", dy = dy)
    }
    y <- y - 0.010
  }
  schliessen()
}

# Data Frame als Monospace-Block. options(width) ist der entscheidende Teil:
# print.data.frame bricht damit zu breite Tabellen in gestapelte Blöcke um,
# statt sie am Seitenrand verschwinden zu lassen. Zusätzlich wird hart
# gekürzt, falls eine einzelne Zelle die Zeile trotzdem sprengt.
# Kürzt bekannte, sehr lange Spaltennamen NUR für die Anzeige im Report.
# Die CSV-Dateien behalten ihre vollen Namen.
kompakt <- function(df) {
  map <- c(n_obs = "n", n_id = "ids", var_person = "v_pers",
           var_session = "v_sess", var_rest = "v_rest", ICC_person = "ICC",
           E2_sd_within = "E2_w", E2_sd_between = "E2_b",
           E2_anteil_within = "E2_%w", P4_sd_within = "P4_w",
           P4_sd_between = "P4_b", P4_anteil_within = "P4_%w",
           schiefe_residuen = "schiefe", n_modelle = "n_mod",
           anteil_positiv = "%pos",
           anteil_signif_deskriptiv = "%signif", median_est = "median",
           mdc_min = "SDD_min", mdc_max = "SDD_max", standing_leg = "bein",
           orig_wert = "Effekt", orig_lo = "orig_lo", orig_hi = "orig_hi",
           einheit = "Einh.", unter_sdd = "<SDD")
  hit <- names(df) %in% names(map)
  names(df)[hit] <- unname(map[names(df)[hit]])
  df
}

als_text <- function(df, ziffern = 3, breite = WRAP_W) {
  if (!is.null(df) && is.data.frame(df)) df <- kompakt(df)
  if (is.null(df) || !nrow(df)) return("(keine Daten)")
  alt <- getOption("width")
  on.exit(options(width = alt), add = TRUE)
  options(width = breite)
  zeilen <- capture.output(print(as.data.frame(df), row.names = FALSE,
                                 digits = ziffern))
  ifelse(nchar(zeilen) > breite + 2, paste0(substr(zeilen, 1, breite), ">"),
         zeilen)
}

# =============================================================================
# Achtung: "else" darf auf oberster Ebene nicht auf einer neuen Zeile stehen -
# R hat das if bereits abgeschlossen. Deshalb die geklammerte Form.
if (capabilities("cairo")) {
  cairo_pdf(pdf_file, width = 8.27, height = 11.69, onefile = TRUE)
} else {
  pdf(pdf_file, width = 8.27, height = 11.69, onefile = TRUE,
      encoding = "ISOLatin1")
}

# -----------------------------------------------------------------------------
# 1  Deckblatt
# -----------------------------------------------------------------------------
seite("FeHBI Paper 1 - Balance und Sexualhormone", list(
  list(typ = "p", text = paste0("Automatisch erzeugte Zusammenfassung, ",
       format(Sys.time(), "%d.%m.%Y %H:%M"), ".")),
  list(typ = "h", text = "Fragestellung"),
  list(typ = "p", text = paste(
    "Hängen die gemessenen Serumkonzentrationen von Estradiol (pmol/L) und",
    "Progesteron (nmol/L) mit der statischen und dynamischen Balance zusammen?",
    "Prädiktor ist die Konzentration, nicht das Phasenlabel. Geschätzt wird der",
    "Totaleffekt der INNERHALB einer Person auftretenden Hormonvariation.")),
  list(typ = "h", text = "Primärmodell"),
  list(typ = "mono", text = c(
    paste("outcome_z ~ prog_w + estr_w + prog_b + estr_b +", DESIGN),
    paste("            +", RE))),
  list(typ = "p", text = paste(
    "prog_w / estr_w sind die Abweichungen vom eigenen geometrischen Mittel -",
    "die Zielgrösse. Durch die Aufnahme der Personenmittel (prog_b / estr_b)",
    "ist dieser Koeffizient exakt der Fixed-Effects-Schätzer und gegen",
    "Konfundierung zwischen Personen immun.")),
  list(typ = "h", text = "Outcomes in diesem Durchlauf"),
  list(typ = "mono", text = als_text(OUTCOMES[, c("var", "label", "task")])),
  list(typ = "h", text = "Einstellungen"),
  list(typ = "mono", text = c(
    paste("USE_LOG_COP        =", USE_LOG_COP),
    paste("STANDARDIZE_OUTCOME=", STANDARDIZE_OUTCOME),
    paste("Relevanzschwelle   = keine (Rueckrechnung auf Messskala)"),
    paste("Designterme        =", DESIGN)))
), fuss = pdf_file)

# -----------------------------------------------------------------------------
# 2  Kann die Frage beantwortet werden?
# -----------------------------------------------------------------------------
expo <- lies("40_exposure_summary.csv")
icc  <- lies("40_icc.csv")
if (is.null(expo)) fehlt <- c(fehlt, "40_exposure_summary.csv")
if (is.null(icc))  fehlt <- c(fehlt, "40_icc.csv")

bl <- list(list(typ = "p", text = paste(
  "Diese beiden Tabellen entscheiden, wie viel alles Weitere wert ist. Sie",
  "gehören in die Stichprobenbeschreibung.")))

if (!is.null(expo)) {
  aw <- suppressWarnings(mean(c(expo$E2_anteil_within, expo$P4_anteil_within),
                              na.rm = TRUE))
  bl <- c(bl, list(
    list(typ = "h", text = "Within-Anteil der Hormonvarianz"),
    list(typ = "mono", text = als_text(expo)),
    list(typ = "p", text = paste(
      "Anteil der Hormonvarianz, der INNERHALB der Personen liegt. Über etwa",
      "0.5 heisst: viel zyklisches Signal vorhanden, ein Nullbefund ist eine",
      "Aussage. Unter etwa 0.3 heisst: die Within-Analyse ist strukturell",
      "schwach, und ein Nullbefund wäre vor allem ein Power-Problem.")),
    list(typ = if (is.finite(aw) && aw >= 0.5) "p" else "warn",
         text = if (is.finite(aw) && aw >= 0.5)
           sprintf(paste("BEFUND: mittlerer Within-Anteil %.2f - die Exposition",
             "variierte innerhalb der Personen deutlich stärker als zwischen",
             "ihnen. Die Voraussetzung für die Within-Analyse ist erfüllt."), aw)
         else sprintf(paste("ACHTUNG: mittlerer Within-Anteil nur %.2f. Ein",
             "Nullbefund ist unter diesen Bedingungen schwach interpretierbar."), aw)),
    list(typ = "p", text = paste(
      "Die SD-Werte stehen auf der Log-Skala. exp(SD) gibt den Faktor pro",
      "Standardabweichung: exp(2.0) = 7.4 bedeutet, dass Progesteron innerhalb",
      "derselben Frau um das Siebenfache schwankt."))))
}

if (!is.null(icc)) {
  bl <- c(bl, list(
    list(typ = "h", text = "Varianzzerlegung des Outcomes"),
    list(typ = "mono", text = als_text(icc)),
    list(typ = "p", text = paste(
      "ICC_person: Anteil der Outcome-Varianz zwischen Personen. Hoch heisst,",
      "die Balance ist stark personengebunden. var_session grösser null belegt,",
      "dass die zwei Standbeine einer Messgelegenheit korreliert sind - die",
      "Rechtfertigung für den zweiten Zufallseffekt. var_rest ist unerklärte",
      "Streuung: ein hoher Wert dämpft jeden Effekt zusätzlich."))))
}
seite("1  Kann die Frage überhaupt beantwortet werden?", bl)

# -----------------------------------------------------------------------------
# 3  Hauptergebnis: Abbildung
# -----------------------------------------------------------------------------
wb    <- lies("60_primary_summary.csv")
skala <- lies("60_effects_original_scale.csv")
sdd   <- lies("60_reach_vs_sdd.csv")
if (is.null(wb)) fehlt <- c(fehlt, "60_primary_summary.csv")

if (!is.null(wb)) {
  pd <- wb
  pd$outcome <- factor(pd$outcome, levels = rev(OUTCOMES$var),
                       labels = rev(OUTCOMES$label))
  pd$ebene <- factor(pd$ebene, levels = c("within", "between"),
                     labels = c(LAB_WITHIN, LAB_BETWEEN))
  pd$hormon <- factor(unname(HORMONE_LAB[pd$hormon]),
                      levels = unname(HORMONE_LAB[c("P4", "E2")]))
  p <- ggplot(pd, aes(x = estimate, y = outcome,
                      colour = hormon, shape = ebene, linetype = ebene)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y",
                  position = position_dodge(width = 0.6), width = 0.22) +
    geom_point(position = position_dodge(width = 0.6), size = 2.4,
               fill = "white") +
    facet_wrap(~ hormon) +
    scale_colour_manual(values = HORMONE_COL, guide = "none") +
    scale_shape_manual(values = c(16, 21)) +
    scale_linetype_manual(values = c("solid", "22")) +
    labs(title = "Within- versus between-person hormone effects on balance",
         subtitle = paste0("Outcomes z-standardised; bars = 95% CI; ",
                           "no relevance threshold imposed"),
         x = LAB_EFFECT, y = NULL, shape = NULL, linetype = NULL)
  print(p)
}

# -----------------------------------------------------------------------------
# 4  Hauptergebnis: Zahlen und Lesart
# -----------------------------------------------------------------------------
bl <- list()
if (!is.null(wb)) {
  w <- wb[wb$ebene == "within", ]
  kurz <- data.frame(outcome = w$outcome, hormon = w$hormon,
                     est = round(w$estimate, 3),
                     KI = sprintf("[%.3f, %.3f]", w$conf.low, w$conf.high),
                     p = signif(w$p.value, 2))
  bl <- c(bl, list(
    list(typ = "h", text = "Within-Effekte (die Zielgrösse)"),
    list(typ = "mono", text = als_text(kurz)),
    list(typ = "p", text = paste(
      "Der Schätzer steht in Outcome-Standardabweichungen pro einer",
      "Standardabweichung Hormonabweichung innerhalb derselben Frau.")),
    list(typ = "p", text = sprintf(paste(
      "Die Punktschätzer liegen zwischen %.3f und %.3f, die Intervalle sind",
      "%.3f bis %.3f breit. Es wird bewusst KEINE Relevanzschwelle angelegt:",
      "Für Balance und Sexualhormone existiert keine belegte Grenze, ab der",
      "ein Effekt bedeutsam wäre. Cohens 0.2 ist eine Konvention, die er",
      "selbst als willkürlich bezeichnet hat - eine Klassifikation dagegen",
      "sagt mehr über die Linie als über die Daten."),
      min(w$estimate, na.rm = TRUE), max(w$estimate, na.rm = TRUE),
      min(w$conf.high - w$conf.low, na.rm = TRUE),
      max(w$conf.high - w$conf.low, na.rm = TRUE)))))
}
if (!is.null(skala)) bl <- c(bl, list(
  list(typ = "h", text = "Dieselben Effekte auf der Messskala"),
  list(typ = "mono", text = als_text(skala)),
  list(typ = "p", text = paste(
    "Das ist die Zahl, die ohne Konvention auskommt. Effekt/orig_lo/orig_hi",
    "geben die Veränderung pro EINER Within-Standardabweichung des Hormons in",
    "der Originaleinheit an - bei den log-Outcomes in Prozent, sonst in mm,",
    "mm2 oder Prozent Beinlänge. Damit entscheidet die Leserin selbst, ob eine",
    "Veränderung dieser Grösse relevant ist."))))
if (!is.null(sdd)) bl <- c(bl, list(
  list(typ = "h", text = "Reach: Vergleich mit der publizierten SDD"),
  list(typ = "mono", text = als_text(sdd)),
  list(typ = "p", text = paste(
    "Der einzige Schwellenvergleich, der belegt ist. Die kleinste zwischen",
    "zwei Sessions erkennbare Veränderung des mSEBT liegt bei 5.0 bis 8.2",
    "Prozent Beinlänge (Munro & Herrington 2010; van Lieshout 2016) - eine",
    "GEMESSENE Grösse aus einer Reliabilitätsstudie, keine Konvention.",
    "<SDD = TRUE heisst: das gesamte Intervall liegt unterhalb dessen, was",
    "am Instrument überhaupt unterscheidbar wäre. Für die COP-Masse gibt die",
    "Literatur nichts Vergleichbares her, dort wird auch nichts behauptet."))))
# -----------------------------------------------------------------------------
# 5  Modellprüfung
# -----------------------------------------------------------------------------
lrt  <- lies("70_lrt_ALL.csv")
skew <- lies("70_residual_skew_ALL.csv")
haus <- lies("60_hausman.csv")
bl <- list()

if (!is.null(lrt)) {
  prim <- lrt[grepl("PRIMAERTEST", lrt$was), c("outcome", "df", "chisq", "p")]
  bl <- c(bl, list(
    list(typ = "h", text = "Primärtest: bringen die Within-Hormone etwas?"),
    list(typ = "mono", text = als_text(prim)),
    list(typ = "p", text = paste(
      "Likelihood-Ratio-Test M2 gegen M3: Modell mit Designtermen gegen",
      "Modell mit zusätzlich prog_w und estr_w. Dieser eine Test ist die",
      "konfirmatorische Aussage, alles Weitere ist Beschreibung."))))
}
if (!is.null(haus)) bl <- c(bl, list(
  list(typ = "h", text = "Within gegen between (Hausman-Kontrast)"),
  list(typ = "mono", text = als_text(haus)),
  list(typ = "p", text = paste(
    "Eine deutliche Abweichung heisst: die beiden Ebenen sagen NICHT dasselbe.",
    "Ein klassisches Random-Intercept-Modell hätte einen Mischwert geliefert,",
    "der weder das eine noch das andere ist. Das ist die empirische",
    "Rechtfertigung für die Mundlak-Zerlegung."))))
if (!is.null(skew)) {
  auff <- skew[abs(skew$schiefe_residuen) > 1, ]
  bl <- c(bl, list(
    list(typ = "h", text = "Verteilung der bedingten Residuen"),
    list(typ = "mono", text = als_text(skew)),
    list(typ = "p", text = paste(
      "Massgeblich ist die Schiefe der BEDINGTEN Residuen, nicht die",
      "Randverteilung des Rohoutcomes. Unter 0.5 im Betrag unauffällig,",
      "über 1 deutlich schief."))))
  if (nrow(auff))
    bl <- c(bl, list(list(typ = "warn", text = paste0(
      "ACHTUNG: |Schiefe| > 1 bei ", paste(auff$outcome, collapse = ", "),
      ". Bei einem Outcome ohne sinnvolle Transformation ist das fast immer ",
      "ein Ausreisserproblem. Siehe 70_extreme_residuals_ALL.csv - dort stehen ",
      "die auffälligsten Messungen mit record_id, Phase und Standbein, um sie ",
      "in den Rohdaten nachzuschlagen."))))
}
seite("3  Hält das Modell?", bl)

# -----------------------------------------------------------------------------
# 6  Robustheit
# -----------------------------------------------------------------------------
stab <- lies("80_stability_ALL.csv")
bl <- list()
if (!is.null(stab)) {
  bl <- c(bl, list(
    list(typ = "mono", text = als_text(
      stab[, intersect(c("outcome", "term", "n_modelle", "anteil_positiv",
                         "median_est", "spannweite"),
                       names(stab))])),
    list(typ = "p", text = paste(
      "anteil_positiv nahe 0.5 bei grosser Spannweite: kein robuster",
      "gerichteter Effekt. Nahe 0.95 bei kaum signifikanten Modellen:",
      "konsistente Richtung ohne Power - eine andere Geschichte.")),
    list(typ = "p", text = sprintf(paste(
      "Eine grosse Spannweite (hier %.3f) hiesse: die Modellwahl",
      "allein kann ein 'relevantes' Ergebnis erzeugen. Das gehört in die",
      "Diskussion."), max(stab$spannweite, na.rm = TRUE))),
    list(typ = "p", text = paste(
      "WICHTIG: anteil_signif ist DESKRIPTION, keine Inferenz. Die",
      "Spezifikationen laufen auf denselben Daten und sind hochgradig",
      "abhängig voneinander."))))
} else fehlt <- c(fehlt, "80_stability_ALL.csv")
seite("4  Wie stabil ist das Ergebnis?", bl)

# -----------------------------------------------------------------------------
# 7  Fallen, Limitationen, fehlende Dateien
# -----------------------------------------------------------------------------
lim <- c(
  paste("Zyklus:", if (length(grep("cycle_nr", DESIGN)))
    "beide Zyklen im Modell." else
    "nur ein Zyklus in den Daten - vier Messgelegenheiten pro Person statt acht."),
  "Tageszeit der Messung wurde nicht erhoben (nur die der Blutentnahme).",
  "Akute Belastung 24-48 h vor der Session wurde nicht erhoben.",
  "Zyklussymptome wurden nicht erhoben.",
  "Hüftkraft fehlt bei einem Teil der Frauen und ist zeitkonstant -",
  "  sie steht deshalb in keinem Modell, nur deskriptiv in 40_diagnostics.")

seite("5  Fallen und Limitationen", list(
  list(typ = "h", text = "Zwei Fallen beim Lesen"),
  list(typ = "p", text = paste(
    "Erstens: die Koeffizienten von Standbein, BMI und Trainingsvolumen NICHT",
    "als eigene Effekte interpretieren. Ein Modell, das für einen Effekt",
    "korrekt spezifiziert ist, ist es für die übrigen Koeffizienten in der",
    "Regel nicht (Table-2-Fallacy).")),
  list(typ = "p", text = paste(
    "Zweitens: die Between-Koeffizienten sind deskriptiv. Sie beruhen auf der",
    "Zahl der Personen, nicht der Messungen, und sind durch alles konfundiert,",
    "was Frauen dauerhaft unterscheidet.")),
  list(typ = "h", text = "Limitationen dieses Durchlaufs"),
  list(typ = "mono", text = lim),
  list(typ = "h", text = "Nicht gefundene Dateien"),
  list(typ = "mono", text = if (length(fehlt)) fehlt else "keine - alles vollständig."),
  list(typ = "h", text = "Vollständige Ergebnisse"),
  list(typ = "mono", text = c(
    "60_primary_ALL.csv           alle Koeffizienten aller Modelle",
    "60_forest_ALL.png            Hauptabbildung",
    "70_coef_table_ALL.csv        Modellkette M1-M9",
    "70_extreme_residuals_ALL.csv auffällige Einzelmessungen",
    "80_specs_ALL.csv             alle Spezifikationen",
    "99_protokoll_<datum>.txt     vollständiges Konsolenprotokoll"))
), fuss = "Erzeugt von 90_report.R")

dev.off()
cat("\nPDF-Zusammenfassung geschrieben:\n  ", pdf_file, "\n")
if (length(fehlt))
  cat("Hinweis: diese Dateien fehlten und wurden übersprungen:\n  ",
      paste(fehlt, collapse = ", "), "\n")
