# =============================================================================
# 60_primary.R  |  FeHBI Paper 1  |  Pipeline-Schritt 7 von 10
# -----------------------------------------------------------------------------
# ZWECK  DIE PRIMAERANALYSE. Loopt selbst über alle 5 Outcomes.
#        Liefert die Hauptabbildung und die Haupttabelle des Papers.
#
# PRIMAERMODELL  (pro Outcome, konfirmatorisch)
#   outcome_z ~ prog_w + estr_w + prog_b + estr_b
#               + standing_leg + first_measure + cycle_nr
#               + (1 | record_id) + (1 | session_id)
#
#   Zielgrösse ist prog_w / estr_w: der Within-Person-Effekt. Durch die
#   Aufnahme der Personenmittel (prog_b / estr_b) ist dieser Koeffizient
#   exakt der Fixed-Effects-Schätzer und gegen Between-Konfundierung immun.
#   Die Between-Koeffizienten fallen dabei ohne Zusatzaufwand an; sie sind
#   deskriptiv und gehören ins Supplementary, nicht in die Hauptaussage
#   (n = Anzahl Personen, eingeschränkter BMI-Bereich, konfundiert).
#
#   Kovariablenwahl kommt aus dem literaturbasierten DAG, nicht aus dem Fit.
#   Genau das erlaubt es, dieses Modell trotz der bereits gelaufenen
#   Explorationen als präspezifiziert zu bezeichnen.
#
# SEKUNDAER
#   + z_sports_min + z_bmi_b (zeitkonstant, Between-Ebene)
#   + Moderator Trainingsvolumen x Hormon_within (einzige theoretisch
#     begründete Interaktion; Athletinnen zeigten keinen Zykluseffekt auf
#     die Balance, Nicht-Athletinnen schon)
#
# NICHT IM MODELL
#   - cycle_phase in jeder Form (Begründung: 40_diagnostics.R, Abschnitt D)
#   - Interaktionen Hormon x BMI / Hormon x Hüftkraft (keine Grundlage)
#   - der Gesamt-z-Koeffizient als Primärgrösse. Er mischt within und
#     between und steht deshalb nur als eine Referenzzeile in der Tabelle.
#
# ZWEI STICHPROBEN
#   d_core  Outcome + beide Hormone            -> volle Fallzahl
#   d_adj   zusätzlich BMI und Sport          -> kleinere Fallzahl
#   n_obs / n_id werden pro Modell ausgewiesen.
#
# OUTPUT  60_primary_ALL.csv, 60_primary_summary.csv, 60_hausman.csv,
#         60_effects_original_scale.csv, 60_reach_vs_sdd.csv,
#         60_interaction.csv, 60_interaction_slopes.csv, 60_forest_ALL.png,
#         60_interaction_slopes_P4.png, 60_interaction_slopes_E2.png,
#         60_sensitivity_session_level.csv
# =============================================================================
library(dplyr)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(ggplot2)

# Robuste Prüfung: exists(..., where = <tibble>) ist unzuverlässig und kann
# TRUE liefern, obwohl die Spalte fehlt. Dann läuft 20_prepare.R nicht, und
# später bricht ein lm() mit einer irreführenden Kontrast-Meldung ab.
# || kurzschliesst: fehlt das Objekt ganz (Skript einzeln gestartet), wird
# names() gar nicht erst ausgewertet und 20_prepare.R uebernimmt.
if (!exists("df_slb_mean") ||
    !all(c("estr_w", "prog_w") %in% names(df_slb_mean)))
  source("20_prepare.R", encoding = "UTF-8")
stopifnot(all(c("estr_w", "prog_w", "estr_b", "prog_b") %in% names(df_slb_mean)),
          all(c("estr_w", "prog_w", "estr_b", "prog_b") %in% names(df_msebt_mean)))
out_dir <- file.path(data_dir, "Statistics", "Models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Theme, Farben und die englischen Labels kommen zentral aus 20_prepare.R.
stopifnot(exists("FEHBI_COL"), exists("HORMONE_COL"), exists("LAB_WITHIN"))

# DESIGN kommt aus 20_prepare.R und enthält nur Terme, die in den Daten
# überhaupt schätzbar sind (ohne Zyklusspalte fallen cycle_nr und
# first_measure automatisch heraus).
if (!exists("DESIGN")) DESIGN <- "standing_leg"
# Die Interaktion E2 x P4 ist Teil der Fragestellung und steht so im
# Kovariablendokument v3, Abschnitt 11. Wichtig dort: Die Within-Komponente
# eines Produkts ist NICHT das Produkt der Within-Komponenten. Deshalb wird das
# Produkt je Ebene separat gebildet - ExP_w = estr_w * prog_w und
# ExP_b = estr_b * prog_b, beide in 20_prepare.R erzeugt. ExP_b muss mit ins
# Modell, sonst gilt die Mundlak-Eigenschaft fuer ExP_w nicht mehr.
CORE_WB  <- "prog_w + estr_w + ExP_w + prog_b + estr_b + ExP_b"
# Hüftkraft ist raus: zeitkonstant, kann den Within-Effekt strukturell nicht
# konfundieren, fehlt aber bei 17 von 59 Frauen. Sie hätte ein Drittel der
# Stichprobe gekostet, ohne etwas gegen Verzerrung auszurichten. Sie bleibt
# deskriptiv in 40_diagnostics (Abschnitt F) auf ihrer eigenen Fallzahl.
COVAR_B  <- "z_bmi_b + z_sports_min"

fitm <- function(rhs, dat)
  suppressWarnings(lmer(as.formula(paste("outcome ~", rhs, "+", RE)), dat, REML = FALSE))

grab <- function(mod, lab, d, oc) {
  broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(outcome = oc, model = lab, term, estimate, std.error,
              statistic, p.value, conf.low, conf.high,
              n_obs = nrow(d), n_id = n_distinct(d$record_id),
              singular = isSingular(mod))
}

# Jedes Modell einzeln absichern. Vorher riss ein scheiterndes SEKUNDAER-
# modell (z. B. weil hip_abd_stand fast nur NA ist und d_adj leer bleibt) das
# ganze Outcome mit - und am Ende war all_coef leer, ohne dass klar war warum.
fit_try <- function(rhs, dat, lab, oc) {
  if (nrow(dat) < 10) {
    message("  [-] ", oc, " / ", lab, ": nur ", nrow(dat),
            " Zeilen - Modell übersprungen.")
    return(NULL)
  }
  tryCatch(fitm(rhs, dat), error = function(e) {
    message("  [-] ", oc, " / ", lab, ": ", conditionMessage(e)); NULL })
}

analyze <- function(oc) {
  base <- base_of(oc)
  mk <- function(d) { d$outcome <- if (STANDARDIZE_OUTCOME)
    zsafe(d[[oc]]) else d[[oc]]; d }

  d_core <- base %>% filter(!is.na(.data[[oc]]), !is.na(conc_estr), !is.na(conc_prog)) %>% mk()
  d_adj  <- base %>% filter(!is.na(.data[[oc]]), !is.na(conc_estr), !is.na(conc_prog),
                            !is.na(bmi_lab), !is.na(sports_min)) %>% mk()
  cat(sprintf("   d_core: %d Zeilen / %d Personen | d_adj: %d / %d\n",
              nrow(d_core), n_distinct(d_core$record_id),
              nrow(d_adj),  n_distinct(d_adj$record_id)))
  if (nrow(d_adj) == 0)
    message("   [i] d_adj ist leer - prüfe bmi_lab / sports_min ",
            "auf fehlende Werte. Die Primäranalyse läuft trotzdem.")

  m_prim <- fit_try(paste(CORE_WB, "+", DESIGN), d_core, "PRIMAER", oc)
  if (is.null(m_prim)) return(NULL)          # ohne Primärmodell kein Ergebnis

  m_adj  <- fit_try(paste(CORE_WB, "+", DESIGN, "+", COVAR_B), d_adj,
                    "sek_+Kovariablen", oc)
  m_modP <- fit_try(paste(CORE_WB, "+", DESIGN, "+", COVAR_B,
                          "+ z_sports_min:prog_w"), d_adj, "Moderator_P4w", oc)
  m_modE <- fit_try(paste(CORE_WB, "+", DESIGN, "+", COVAR_B,
                          "+ z_sports_min:estr_w"), d_adj, "Moderator_E2w", oc)
  # Referenz: der klassische Gesamt-Koeffizient (Mischung aus within+between)
  d_core$z_prog_tot <- zsafe(d_core$log_prog)
  d_core$z_estr_tot <- zsafe(d_core$log_estr)
  m_mix  <- fit_try(paste("z_prog_tot + z_estr_tot +", DESIGN), d_core,
                    "REF_Gesamt_Mischung", oc)

  parts <- list(list(m_prim, "PRIMAER_within_between",  d_core),
                list(m_adj,  "sek_+Kovariablen",        d_adj),
                list(m_modP, "sek_Moderator_Sport:P4w", d_adj),
                list(m_modE, "sek_Moderator_Sport:E2w", d_adj),
                list(m_mix,  "REF_Gesamt_Mischung",     d_core))
  parts <- Filter(function(p) !is.null(p[[1]]), parts)

  list(coef = bind_rows(lapply(parts, function(p)
         grab(p[[1]], p[[2]], p[[3]], oc))),
       prim = m_prim)
}

res <- lapply(OUTCOMES$var, function(oc) {
  cat(sprintf("-- %s --\n", oc))
  tryCatch(analyze(oc), error = function(e) {
    message("  FEHLER bei ", oc, ": ", conditionMessage(e)); NULL })
})
names(res) <- OUTCOMES$var
res <- res[!sapply(res, is.null)]

if (length(res) == 0)
  stop("Kein einziges Outcome konnte geschätzt werden. Die Ursache steht in ",
       "den [-] / FEHLER-Zeilen oberhalb dieser Meldung - bitte diese schicken.")

all_coef <- bind_rows(lapply(res, function(x) x$coef))
stopifnot("model" %in% names(all_coef))
write.csv(all_coef, file.path(out_dir, "60_primary_ALL.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Zusammenfassung: Within und Between je Hormon und Outcome
# -----------------------------------------------------------------------------
wb <- all_coef %>%
  filter(model == "PRIMAER_within_between", term %in% c("prog_w", "prog_b",
                                                        "estr_w", "estr_b")) %>%
  mutate(hormon = ifelse(grepl("^prog", term), "P4", "E2"),
         ebene  = ifelse(grepl("_w$", term), "within", "between")) %>%
  select(outcome, hormon, ebene, estimate, conf.low, conf.high, p.value,
         n_obs, n_id)
cat("\n========== PRIMAERERGEBNIS: WITHIN vs BETWEEN ==========\n")
print(as.data.frame(wb), row.names = FALSE, digits = 3)
write.csv(wb, file.path(out_dir, "60_primary_summary.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Interaktion E2 x P4 - bewusst SEPARAT
# -----------------------------------------------------------------------------
# Warum nicht in der Tabelle oben: Der Koeffizient von ExP_w steht auf einer
# anderen Skala. Er ist die Aenderung des P4-Slopes pro einer SD estr_w, also
# ein Effekt zweiter Ordnung und laesst sich nicht auf dieselbe Weise auf
# die Messskala zurueckrechnen wie ein Haupteffekt. Deshalb wird die
# Interaktion mit Schaetzer und KI berichtet und sonst nicht weiter verrechnet.
inter <- all_coef %>%
  filter(model == "PRIMAER_within_between", term %in% c("ExP_w", "ExP_b")) %>%
  mutate(ebene = ifelse(term == "ExP_w", "within", "between")) %>%
  select(outcome, ebene, estimate, std.error, conf.low, conf.high, p.value,
         n_obs, n_id)
cat("\n========== INTERAKTION E2 x P4 (je Ebene) ==========\n")
if (nrow(inter)) {
  print(as.data.frame(inter), row.names = FALSE, digits = 3)
  cat("\nLesart: ExP_w ist die Aenderung des Progesteron-Slopes pro einer\n",
      "Standardabweichung estr_w. Ein Effekt zweiter Ordnung - die\n",
      "Relevanzschwelle der Haupteffekte gilt hier NICHT unbesehen.\n", sep = "")
  write.csv(inter, file.path(out_dir, "60_interaction.csv"), row.names = FALSE)
} else {
  cat("[!] Kein Interaktionsterm geschaetzt - ExP_w/ExP_b fehlen im Modell\n",
      "    oder waren nicht schaetzbar (siehe 20_prepare.R).\n", sep = "")
}

# -----------------------------------------------------------------------------
# Die Interaktion als Bild: Simple Slopes
# -----------------------------------------------------------------------------
# Ein Koeffizient von -0.039 mit einem Intervall von -0.182 bis +0.104 sagt
# beim Vortragen wenig. Dieselbe Aussage als Bild: vorhergesagtes Outcome
# gegen die Abweichung des einen Hormons, getrennt fuer drei Niveaus des
# anderen. Parallele Linien heissen keine Interaktion, ein Faecher heisst
# Interaktion. Das ist ohne Erklaerung lesbar.
#
# BEIDE BLICKRICHTUNGEN werden gezeichnet. Der Koeffizient ist derselbe -
# ExP_w = estr_w * prog_w, und ein Produkt ist kommutativ. Nach prog_w
# sortiert ist die Steigung b_P4 + b_int * estr_w, nach estr_w sortiert
# b_E2 + b_int * prog_w. Zwei Saetze, ein Koeffizient.
#
# Das Bild unterscheidet sich trotzdem, und zwar in allem AUSSER dem
# Faecherwinkel:
#   - die Grundneigung ist einmal b_P4 und einmal b_E2,
#   - die vertikalen Abstaende der Linien kommen vom Haupteffekt des
#     jeweils anderen Hormons, die Rollen tauschen also,
#   - der Schnittpunkt liegt bei -b_E2/b_int beziehungsweise -b_P4/b_int.
# Welche Richtung man zeigt, ist eine Darstellungsentscheidung. Genau
# deshalb liegen beide vor, statt dass eine stillschweigend gewaehlt wird.
#
# Gerechnet wird aus dem PRIMAERmodell, nicht aus den Rohdaten:
#   - das Fokushormon laeuft ueber sein beobachtetes 2.5- bis 97.5-Prozent-
#     Intervall, damit nicht ueber die Daten hinaus extrapoliert wird,
#   - der Moderator steht fest auf dem Mittelwert und auf plus/minus einer
#     Within-Standardabweichung,
#   - ExP_w MUSS auf dem Raster als Produkt der beiden mitgefuehrt werden.
#     Laesst man es auf seinem Mittelwert stehen, sind die drei Linien per
#     Konstruktion parallel - die Abbildung wuerde dann immer zeigen, was
#     sie eigentlich pruefen soll,
#   - alle uebrigen Terme (Between-Komponenten, Standbein, Designvariablen)
#     stehen auf ihrem Spaltenmittel der Designmatrix. Das ist die
#     durchschnittliche Teilnehmerin; sie verschiebt alle drei Linien
#     gemeinsam und aendert an ihrer Neigung nichts.
#
# Beide Achsen sind in Within-Standardabweichungen des jeweiligen Hormons.
# Damit hat die Abbildung dieselbe Skala wie der Forest Plot, unabhaengig
# davon, wie prog_w und estr_w in 20_prepare.R skaliert wurden.
# -----------------------------------------------------------------------------
slope_raster <- function(fe, V, X, mf, oc, fokus = "prog", n_punkte = 60) {
  mod    <- if (fokus == "prog") "estr" else "prog"
  f_w    <- paste0(fokus, "_w")
  m_w    <- paste0(mod,   "_w")
  noetig <- c(f_w, m_w, "ExP_w")
  if (!all(noetig %in% names(fe)) || !all(noetig %in% colnames(X))) return(NULL)

  f_sd <- stats::sd(mf[[f_w]], na.rm = TRUE)
  m_sd <- stats::sd(mf[[m_w]], na.rm = TRUE)
  if (!is.finite(f_sd) || !is.finite(m_sd) || f_sd == 0 || m_sd == 0) return(NULL)
  m_mit <- mean(mf[[m_w]], na.rm = TRUE)
  f_seq <- seq(stats::quantile(mf[[f_w]], 0.025, na.rm = TRUE),
               stats::quantile(mf[[f_w]], 0.975, na.rm = TRUE),
               length.out = n_punkte)

  basis <- colMeans(X)
  do.call(rbind, lapply(c(-1, 0, 1), function(k) {
    mv <- m_mit + k * m_sd
    xx <- matrix(rep(basis, each = n_punkte), nrow = n_punkte,
                 dimnames = list(NULL, names(basis)))
    xx[, f_w]     <- f_seq
    xx[, m_w]     <- mv
    xx[, "ExP_w"] <- mv * f_seq          # Produkt, nicht Mittelwert
    xx  <- xx[, names(fe), drop = FALSE] # Spaltenfolge wie fixef
    fit <- as.vector(xx %*% fe)
    se  <- sqrt(rowSums((xx %*% V) * xx))  # Var(x'b) = x' V x, zeilenweise
    data.frame(outcome = oc, fokus = fokus, x_sd = f_seq / f_sd, stufe = k,
               fit = fit, lo = fit - 1.96 * se, hi = fit + 1.96 * se,
               stringsAsFactors = FALSE)
  }))
}

# Heller, mittlerer und dunkler Ton der Hormonfarbe, die in der ganzen
# Pipeline gilt: Orange fuer Estradiol, Blau fuer Progesteron. Sequenziell,
# weil das Moderatorniveau geordnet ist - drei Signalfarben waeren falsch.
MOD_COL <- list(estr = c("#F0B694", "#C9663A", "#5E2A12"),
                prog = c("#A9C6E3", "#3D7ABF", "#1B3E63"))
HORM_NAME <- c(prog = "Progesterone", estr = "Estradiol")
# Bewusst ein einfacher Bindestrich und kein typografisches Minus: das
# Zeichen fehlt in manchen Schriftschnitten und wuerde beim Zeichnen eine
# Warnung samt leerem Kaestchen erzeugen.
STUFEN_LAB <- c("-1 SD", "her own mean", "+1 SD")

slopes <- bind_rows(lapply(c("prog", "estr"), function(fk)
  bind_rows(lapply(names(res), function(oc)
    tryCatch(slope_raster(lme4::fixef(res[[oc]]$prim),
                          as.matrix(vcov(res[[oc]]$prim)),
                          model.matrix(res[[oc]]$prim),
                          res[[oc]]$prim@frame, oc, fokus = fk),
             error = function(e) {
               message("  [-] Simple Slopes ", oc, " / ", fk, ": ",
                       conditionMessage(e))
               NULL })))))

if (nrow(slopes) == 0) {
  cat("[!] Keine Simple-Slopes-Abbildung - ExP_w war in keinem Modell vorhanden.\n")
} else {
  oc_lab <- function(x) factor(x, levels = OUTCOMES$var, labels = OUTCOMES$label)

  # Vier COP-Varianten oben, drei Reichweiten unten - dieselbe Gruppierung
  # wie in den Deskriptivabbildungen. Stimmt die Aufteilung nicht, sagt es
  # die Meldung, statt dass die Zeilen still falsch laufen.
  n_cop   <- sum(grepl("cop", OUTCOMES$var, ignore.case = TRUE))
  spalten <- max(n_cop, nrow(OUTCOMES) - n_cop, 1)
  if (n_cop != spalten)
    message("  [i] ", n_cop, " COP- und ", nrow(OUTCOMES) - n_cop,
            " Reach-Outcomes: die Zeilen des Rasters trennen die beiden ",
            "Aufgaben nicht sauber.")

  # Der Interaktionskoeffizient gehoert in die Abbildung: die Linien zeigen
  # die Richtung, die Zahl zeigt, wie genau sie geschaetzt ist. Sie ist in
  # beiden Blickrichtungen dieselbe - das ist der Punkt.
  ann <- inter %>% filter(ebene == "within") %>%
    transmute(outcome = oc_lab(outcome),
              txt = sprintf("E2 \u00d7 P4:  %+.3f  (%+.3f to %+.3f)",
                            estimate, conf.low, conf.high))

  slopes_bild <- function(fk) {
    mod <- if (fk == "prog") "estr" else "prog"
    d <- slopes %>% filter(fokus == fk) %>%
      mutate(outcome = oc_lab(outcome),
             stufe = factor(stufe, levels = c(-1, 0, 1), labels = STUFEN_LAB))
    ggplot(d, aes(x_sd, fit, colour = stufe, fill = stufe)) +
      geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
      geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.10, colour = NA) +
      geom_line(linewidth = 0.9) +
      geom_text(data = ann, aes(x = -Inf, y = Inf, label = txt),
                inherit.aes = FALSE, hjust = -0.04, vjust = 1.5,
                size = 2.8, colour = "grey35") +
      facet_wrap(~ outcome, ncol = spalten) +
      scale_colour_manual(values = MOD_COL[[mod]]) +
      scale_fill_manual(values = MOD_COL[[mod]]) +
      labs(title = sprintf("Does the %s effect depend on %s?",
                           tolower(HORM_NAME[[fk]]), tolower(HORM_NAME[[mod]])),
           subtitle = paste0(
             "Predicted outcome against a woman's own ", tolower(HORM_NAME[[fk]]),
             " deviation, at three ", tolower(HORM_NAME[[mod]]), " levels.\n",
             "Parallel lines mean no interaction. The interaction estimate is ",
             "the same in both directions; only the picture differs."),
           x = paste0(HORM_NAME[[fk]], ", within-person deviation (SD)"),
           y = if (STANDARDIZE_OUTCOME) "Predicted outcome (SD)" else
               "Predicted outcome",
           colour = paste0(HORM_NAME[[mod]], ", within-person:"),
           fill   = paste0(HORM_NAME[[mod]], ", within-person:")) +
      theme(legend.position = "bottom")
  }

  for (fk in unique(slopes$fokus)) {
    pp <- slopes_bild(fk)
    datei <- sprintf("60_interaction_slopes_%s.png",
                     ifelse(fk == "prog", "P4", "E2"))
    print(pp)
    ggsave(file.path(out_dir, datei), pp, width = 11.5, height = 6.0,
           dpi = FIG_DPI)
    cat("Simple-Slopes-Abbildung:", datei, "\n")
  }
  write.csv(slopes, file.path(out_dir, "60_interaction_slopes.csv"),
            row.names = FALSE)
}

# -----------------------------------------------------------------------------
# Hausman-artiger Kontrast: sagen die beiden Ebenen dasselbe?
# -----------------------------------------------------------------------------
hausman <- bind_rows(lapply(names(res), function(oc) {
  m <- res[[oc]]$prim; fe <- fixef(m); V <- as.matrix(vcov(m))
  bind_rows(lapply(c("prog", "estr"), function(h) {
    w <- paste0(h, "_w"); b <- paste0(h, "_b")
    if (!all(c(w, b) %in% names(fe))) return(NULL)
    dd <- unname(fe[b] - fe[w]); s <- sqrt(V[b, b] + V[w, w] - 2 * V[b, w])
    data.frame(outcome = oc, hormon = ifelse(h == "prog", "P4", "E2"),
               within = round(unname(fe[w]), 3), between = round(unname(fe[b]), 3),
               diff = round(dd, 3), se_diff = round(s, 3),
               z = round(dd / s, 2), p = signif(2 * pnorm(-abs(dd / s)), 3))
  }))
}))
cat("\n========== HAUSMAN-KONTRAST (between - within) ==========\n")
print(hausman, row.names = FALSE)
cat("Deutliche Abweichung -> die Ebenen sagen NICHT dasselbe, und der\n",
    "klassische Random-Intercept-Koeffizient (Zeile REF_Gesamt_Mischung)\n",
    "wäre weder das eine noch das andere.\n")
write.csv(hausman, file.path(out_dir, "60_hausman.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# Äquivalenz / Präzision statt reiner p-Werte
# -----------------------------------------------------------------------------
# HAUPTBERICHT: die Within-Effekte auf der MESSSKALA
# -----------------------------------------------------------------------------
# Keine Konventionsschwelle mehr (siehe 20_prepare.R, Kopfkommentar). Was hier
# steht, kommt ohne willkuerliche Linie aus: Schaetzer und Intervall in
# Outcome-SD, daneben dasselbe in Millimetern, Quadratmillimetern oder Prozent
# Beinlaenge. Der Leser entscheidet selbst, ob ihn eine Veraenderung dieser
# Groesse interessiert.
skala <- wb %>% filter(ebene == "within") %>%
  rowwise() %>%
  mutate(o = list(effekt_original(outcome, estimate, conf.low, conf.high)),
         orig_wert = o$wert, orig_lo = o$lo, orig_hi = o$hi,
         einheit = o$einheit,
         KI_breite = round(conf.high - conf.low, 3)) %>%
  ungroup() %>%
  select(outcome, hormon, estimate, conf.low, conf.high, KI_breite,
         orig_wert, orig_lo, orig_hi, einheit)
cat("\n========== WITHIN-EFFEKTE AUF DER MESSSKALA ==========\n")
print(as.data.frame(skala), row.names = FALSE, digits = 3)
cat("\nLesart: orig_wert ist die Veraenderung pro EINER Within-Standardab-\n",
    "weichung des Hormons. Bei den log-Outcomes in Prozent, bei den uebrigen\n",
    "in der Originaleinheit (mm, mm2, % Beinlaenge).\n", sep = "")
write.csv(skala, file.path(out_dir, "60_effects_original_scale.csv"),
          row.names = FALSE)

# -----------------------------------------------------------------------------
# EINZIGER Schwellenvergleich, der belegt ist: Reach gegen die publizierte SDD
# -----------------------------------------------------------------------------
# Die kleinste erkennbare Veraenderung des mSEBT zwischen Sessions ist eine
# GEMESSENE Groesse aus einer Reliabilitaetsstudie (Munro & Herrington 2010;
# van Lieshout 2016), keine Konvention. Deshalb steht sie hier - und nur fuer
# die drei Reach-Richtungen. Fuer die COP-Masse gibt es nichts Vergleichbares,
# dort wird auch nichts behauptet.
mdc_tab <- bind_rows(lapply(OUTCOMES$var, function(oc) {
  r <- rope_mdc(oc, base_of(oc)[[oc]])
  data.frame(outcome = oc, mdc_min = r[1], mdc_max = r[2], row.names = NULL)
})) %>% filter(!is.na(mdc_min))

if (nrow(mdc_tab)) {
  sdd <- wb %>% filter(ebene == "within") %>%
    inner_join(mdc_tab, by = "outcome") %>%
    mutate(unter_sdd = conf.low > -mdc_min & conf.high < mdc_min) %>%
    select(outcome, hormon, estimate, conf.low, conf.high,
           mdc_min, mdc_max, unter_sdd)
  cat("\n========== REACH: VERGLEICH MIT DER PUBLIZIERTEN SDD ==========\n")
  print(as.data.frame(sdd), row.names = FALSE, digits = 3)
  cat("\nunter_sdd = TRUE heisst: Das gesamte Konfidenzintervall liegt unter\n",
      "der kleinsten Veraenderung, die zwischen zwei Sessions ueberhaupt vom\n",
      "Messfehler unterscheidbar ist (5.0-8.2 % Beinlaenge; strengere Grenze\n",
      "verwendet). Das ist eine messtheoretische Aussage, keine klinische.\n",
      sep = "")
  write.csv(sdd, file.path(out_dir, "60_reach_vs_sdd.csv"), row.names = FALSE)
}

# -----------------------------------------------------------------------------
# Hauptabbildung
# -----------------------------------------------------------------------------
plot_df <- wb %>%
  mutate(outcome = factor(outcome, levels = rev(OUTCOMES$var),
                          labels = rev(OUTCOMES$label)),
         ebene  = factor(ebene, levels = c("within", "between"),
                         labels = c(LAB_WITHIN, LAB_BETWEEN)),
         hormon = factor(unname(HORMONE_LAB[hormon]),
                         levels = unname(HORMONE_LAB[c("P4", "E2")])))

# Farbe = Hormon (blau P4, orange E2, wie in deinen Deskriptivplots).
# Form und Linienart = Ebene. So bleibt die Farbbedeutung im ganzen Paper
# dieselbe, und die Legende erklärt nur noch within vs. between.
p <- ggplot(plot_df, aes(x = estimate, y = outcome,
                         colour = hormon, shape = ebene, linetype = ebene)) +
  # Kein schattiertes Schwellenband mehr - es gab eine Grenze vor, die es
  # fuer Balance und Hormone nicht gibt. Nur die Nulllinie bleibt.
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y",
                position = position_dodge(width = 0.6), width = 0.22,
                linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.6), size = 2.6, fill = "white") +
  facet_wrap(~ hormon) +
  scale_colour_manual(values = HORMONE_COL, guide = "none") +
  scale_shape_manual(values = c(16, 21)) +
  scale_linetype_manual(values = c("solid", "22")) +
  labs(title = "Within- versus between-person hormone effects on balance",
       subtitle = paste0("Primary model; outcomes z-standardised. ",
                         "Points = estimates, bars = 95% CI. ",
                         "No relevance threshold is imposed."),
       x = LAB_EFFECT, y = NULL, shape = NULL, linetype = NULL)
print(p)
ggsave(file.path(out_dir, "60_forest_ALL.png"), p,
       width = 11, height = 5.5, dpi = FIG_DPI)

# -----------------------------------------------------------------------------
# Sensitivitaet: traegt die Session-Ebene die Within-Schaetzer ueberhaupt?
# -----------------------------------------------------------------------------
# Anlass ist ein Widerspruch, der im Vortrag kommen wird. Beide Hormonwerte
# aendern sich nur von Session zu Session. Ein Within-Effekt kann deshalb nur
# Varianz erklaeren, die auf der Session-Ebene liegt. Bei der Ellipsenflaeche
# liegen dort aber nur rund ein bis zwei Prozent der Gesamtvarianz - und genau
# dieses Outcome hat als einziges ein Intervall, das die Null ausschliesst.
# Rechnet man es nach, muesste Progesteron fast die gesamte Session-Varianz
# dieses Outcomes erklaeren. Das ist unplausibel, also stimmt eine der beiden
# Zahlen nicht.
#
# Der wahrscheinlichste Mechanismus ist kein Fehler im Schaetzer, sondern im
# Standardfehler: Faellt die Session-Varianz auf null (singulaerer Fit), so
# behandelt das Modell die beiden Beine derselben Session als unabhaengige
# Beobachtungen. Ein Praediktor, der nur auf Session-Ebene variiert, bekommt
# damit doppelt so viele unabhaengige Faelle zugesprochen, wie er hat, und sein
# Standardfehler wird zu klein. Genau dieses Muster - fehlende Varianzkomponente
# auf der Ebene des Praediktors, erhoehte Typ-I-Fehlerrate - haben Barr et al.
# 2013 (J Mem Lang 68:255-278) fuer weggelassene Random-Effekte gezeigt.
#
# Drei Pruefungen, alle auf denselben Modellen, keine neuen Daten:
#   A  Varianzkomponenten und Singularitaet direkt aus dem Primaermodell.
#   B  Kenward-Roger: korrigiert Standardfehler und Freiheitsgrade dafuer, dass
#      die Varianzkomponenten geschaetzt und nicht bekannt sind (Kenward &
#      Roger 1997, Biometrics 53:983-997). Verlangt REML, deshalb ein Refit.
#   C  Refit auf SESSION-MITTELWERTEN, eine Zeile je Session. Damit gibt es
#      keine Beinebene mehr, die faelschlich als unabhaengig zaehlen koennte.
#      Das ist die konservative Referenz.
#
# Bewusst NICHT dabei: ein parametrischer Bootstrap (bootMer). Er simuliert aus
# den GESCHAETZTEN Varianzkomponenten. Ist die Session-Varianz zu tief
# geschaetzt, reproduziert der Bootstrap denselben zu kleinen Standardfehler -
# er kann dieses Problem konstruktionsbedingt nicht finden.
# -----------------------------------------------------------------------------
WB_TERME <- c("prog_w", "estr_w", "ExP_w")

var_anteile <- function(m) {
  v <- as.data.frame(lme4::VarCorr(m))
  v <- v[is.na(v$var2), c("grp", "vcov")]
  setNames(v$vcov / sum(v$vcov), v$grp)
}

zeilen_von <- function(m, oc, methode, n_obs) {
  s <- summary(m)$coefficients
  t <- intersect(WB_TERME, rownames(s))
  if (!length(t)) return(NULL)
  est <- s[t, "Estimate"]; se <- s[t, "Std. Error"]
  data.frame(outcome = oc, methode = methode, term = t,
             estimate = est, std.error = se,
             conf.low = est - 1.96 * se, conf.high = est + 1.96 * se,
             n_obs = n_obs, singular = lme4::isSingular(m), row.names = NULL)
}

sens_eines <- function(oc) {
  m  <- res[[oc]]$prim
  fr <- m@frame
  if (!all(c("record_id", "session_id") %in% names(fr))) return(NULL)

  # --- A  Varianzkomponenten ------------------------------------------------
  va  <- var_anteile(m)
  a_s <- unname(va[grep("session", names(va))])[1]
  if (!length(a_s) || is.na(a_s)) a_s <- NA_real_

  # Wie viel Outcome-Varianz erklaert der Hormonterm selbst? b^2 * var(x)/var(y).
  # So gerechnet ist es unabhaengig davon, ob die Hormone standardisiert in das
  # Modell gehen oder nicht.
  s <- summary(m)$coefficients
  erkl <- sapply(WB_TERME, function(tm)
    if (tm %in% rownames(s))
      s[tm, "Estimate"]^2 * stats::var(fr[[tm]]) / stats::var(fr$outcome)
    else NA_real_)

  # --- B  Kenward-Roger ------------------------------------------------------
  kr <- tryCatch({
    # Bewusst kein update(): dessen Aufruf verweist auf Objekte aus analyze(),
    # die hier nicht mehr existieren. formula(m) plus m@frame ist selbsttragend.
    m_re <- suppressWarnings(lmer(stats::formula(m), data = fr, REML = TRUE))
    ct   <- coef(summary(m_re, ddf = "Kenward-Roger"))
    t    <- intersect(WB_TERME, rownames(ct))
    data.frame(outcome = oc, methode = "B_Kenward_Roger", term = t,
               estimate = ct[t, "Estimate"], std.error = ct[t, "Std. Error"],
               conf.low  = ct[t, "Estimate"] - 1.96 * ct[t, "Std. Error"],
               conf.high = ct[t, "Estimate"] + 1.96 * ct[t, "Std. Error"],
               n_obs = nrow(fr), singular = lme4::isSingular(m_re),
               row.names = NULL)
  }, error = function(e) {
    message("  [-] ", oc, ": Kenward-Roger nicht verfuegbar (",
            conditionMessage(e), ") - Paket pbkrtest installieren.")
    NULL })

  # --- C  Refit auf Session-Mittelwerten -------------------------------------
  # Die Hormonterme sind je Session konstant; der Mittelwert aendert daran
  # nichts und macht die Zeile nur robust gegen Rundung.
  vorhanden <- intersect(c("prog_w", "estr_w", "ExP_w",
                           "prog_b", "estr_b", "ExP_b"), names(fr))
  konst <- fr %>% group_by(session_id) %>%
    summarise(across(all_of(vorhanden), ~ stats::sd(.x)), .groups = "drop")
  # Sessions mit nur einer Zeile liefern NA; max() ueber lauter NA gibt -Inf
  # samt Warnung, deshalb unterdrueckt und unten auf is.finite geprueft.
  abw <- suppressWarnings(max(unlist(konst[, vorhanden]), na.rm = TRUE))
  if (is.finite(abw) && abw > 1e-8)
    message("  [i] ", oc, ": Hormonwerte variieren innerhalb einer Session ",
            "(max. SD ", signif(abw, 3), "). Erwartet ist 0 - bitte pruefen.")

  agg <- fr %>% group_by(record_id, session_id) %>%
    summarise(outcome = mean(outcome),
              across(all_of(vorhanden), ~ mean(.x)),
              n_zeilen = dplyr::n(), .groups = "drop")
  # Ist eine Session nur mit einem Bein vertreten, verschiebt das ihren
  # Mittelwert um die halbe Beindifferenz. Das wird ausgewiesen, nicht
  # stillschweigend hingenommen.
  einbeinig <- sum(agg$n_zeilen < max(agg$n_zeilen))
  if (einbeinig)
    message("  [i] ", oc, ": ", einbeinig, " von ", nrow(agg),
            " Sessions unvollstaendig (weniger Zeilen als das Maximum).")

  m_sess <- tryCatch(suppressWarnings(lmer(
    as.formula(paste("outcome ~", paste(vorhanden, collapse = " + "),
                     "+ (1 | record_id)")), agg, REML = TRUE)),
    error = function(e) NULL)

  bind_rows(
    zeilen_von(m, oc, "A_Primaermodell", nrow(fr)),
    kr,
    if (!is.null(m_sess)) zeilen_von(m_sess, oc, "C_Session_Mittel", nrow(agg))
  ) %>%
    mutate(anteil_session = a_s,
           erklaerte_varianz = unname(erkl[term]),
           # Wie viel der gesamten Session-Varianz muesste dieser eine Term
           # tragen? Werte nahe oder ueber 1 sind das Warnsignal.
           anteil_der_session_varianz = erklaerte_varianz / anteil_session)
}

sens <- bind_rows(lapply(names(res), function(oc)
  tryCatch(sens_eines(oc), error = function(e) {
    message("  [-] Sensitivitaet ", oc, ": ", conditionMessage(e)); NULL })))

if (nrow(sens)) {
  cat("\n========== SENSITIVITAET: SESSION-EBENE ==========\n")
  print(as.data.frame(sens %>% select(outcome, methode, term, estimate,
                                      std.error, conf.low, conf.high, singular)),
        row.names = FALSE, digits = 3)
  cat("\nVarianzbilanz je Outcome (aus dem Primaermodell):\n")
  print(as.data.frame(sens %>% filter(methode == "A_Primaermodell") %>%
    select(outcome, term, anteil_session, erklaerte_varianz,
           anteil_der_session_varianz)), row.names = FALSE, digits = 3)
  cat("\nLesart\n",
      "  anteil_session              Anteil der Outcome-Varianz auf Session-Ebene.\n",
      "  erklaerte_varianz           Anteil, den dieser Hormonterm erklaert.\n",
      "  anteil_der_session_varianz  Verhaeltnis der beiden. Ein Wert nahe 1\n",
      "                              heisst: der Term muesste praktisch die\n",
      "                              gesamte Schwankung zwischen den Sessions\n",
      "                              erklaeren. Das ist kein Beweis gegen den\n",
      "                              Effekt, aber ein Grund, ihn zurueckhaltend\n",
      "                              zu berichten.\n",
      "  Weitet sich das Intervall von A ueber B nach C deutlich, lag es am\n",
      "  Standardfehler und nicht am Schaetzer. C ist dann die Zahl, die\n",
      "  berichtet wird.\n", sep = "")
  write.csv(sens, file.path(out_dir, "60_sensitivity_session_level.csv"),
            row.names = FALSE)
}

cat(sprintf("\nDone. Outputs in %s\n", out_dir))
