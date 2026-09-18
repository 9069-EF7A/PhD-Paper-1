# =============================================================================
# 40_diagnostics.R   |  FeHBI Paper 1 - Balance  |  Pipeline-Schritt 5 von 10
# -----------------------------------------------------------------------------
# ZWECK  Alles, was VOR dem Modellieren geklärt sein muss. Diese Zahlen
#        entscheiden, ob und wie die Hormonmodelle überhaupt aussagekräftig
#        sein können - und sie gehören in die Stichprobenbeschreibung.
#
# ENTHAELT
#   A  Varianzzerlegung des Outcomes (ICC) je Outcome
#   B  Within-Person-Varianz der EXPOSITION - die eigentliche Power-Frage
#   C  cycle_phase: variiert die Balance überhaupt über den Zyklus?
#      (der EINZIGE verbleibende Einsatz von cycle_phase - deskriptiv,
#       als eigenständiges Modell, NICHT als Kovariable)
#   D  Preis einer Adjustierung auf cycle_phase, falls jemand danach fragt
#   E  Lutealphasen-Deskription (P4 > 16 nmol/L) - Kenngrösse, kein Kriterium
#   F  Bivariate Roh-Assoziationen, jede Variable auf ihrer maximalen Stichprobe
#
# GRENZEN DIESES SKRIPTS
#   - Change-in-Estimate ist hier KEINE Entscheidungsregel. Die Regel kann
#     Confounder und Mediator nicht unterscheiden; die Kovariablenwahl kommt
#     aus dem DAG. Die Tabelle dient nur als Robustheitsanzeige
#     (siehe 80_specification_curve.R).
#   - cycle_phase wird nirgends adjustiert, sie ist Elternknoten der
#     Exposition. Abschnitt C und D beschreiben sie, mehr nicht.
#   - Konfidenzintervalle sind t-Quantile mit Satterthwaite-df, keine
#     Wald-KI +/- 1.96*SE.
#
# INPUT   10_load_data.R -> 20_prepare.R
# OUTPUT  40_icc.csv, 40_exposure_summary.csv, 40_exposure_variance.csv,
#         40_phase_omnibus.csv,
#         40_phase_cost.csv, 40_luteal_check.csv, 40_bivariate_<outcome>.csv
# =============================================================================
library(dplyr)
library(lme4)
library(lmerTest)

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

prep_out <- function(d, oc) {
  d <- d[!is.na(d[[oc]]), ]
  d$outcome <- if (STANDARDIZE_OUTCOME) zsafe(d[[oc]]) else d[[oc]]
  d
}
fitm <- function(rhs, dat)
  suppressWarnings(lmer(as.formula(paste("outcome ~", rhs, "+", RE)), dat, REML = FALSE))

# -----------------------------------------------------------------------------
# A  Varianzzerlegung: wie viel Outcome-Varianz liegt zwischen Personen?
# -----------------------------------------------------------------------------
cat("\n========== A) VARIANZZERLEGUNG (leeres Modell) ==========\n")
icc_tab <- bind_rows(lapply(OUTCOMES$var, function(oc) {
  d <- prep_out(base_of(oc), oc)
  m <- fitm("1", d)
  vc <- as.data.frame(VarCorr(m))
  v_id <- sum(vc$vcov[vc$grp == "record_id"])
  v_se <- sum(vc$vcov[vc$grp == "session_id"])
  v_e  <- vc$vcov[vc$grp == "Residual"]
  data.frame(outcome = oc, n_obs = nrow(d), n_id = n_distinct(d$record_id),
             var_person = round(v_id, 3), var_session = round(v_se, 3),
             var_rest = round(v_e, 3),
             ICC_person = round(v_id / (v_id + v_se + v_e), 3))
}))
print(icc_tab, row.names = FALSE)
cat("\nHinweis: var_session > 0 belegt, dass die zwei Beine einer Messung\n",
    "korreliert sind. Ohne die Ebene (1 | session_id) waeren die\n",
    "Standardfehler aller Modelle zu klein.\n")
write.csv(icc_tab, file.path(out_dir, "40_icc.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# B  Expositionsvarianz - entscheidet über die Aussagekraft eines Nullbefunds
# -----------------------------------------------------------------------------
cat("\n========== B) WITHIN-VARIANZ DER EXPOSITION ==========\n")
expo <- bind_rows(lapply(c("SLB", "mSEBT"), function(tk) {
  d <- if (tk == "SLB") df_slb_mean else df_msebt_mean
  d <- d[!is.na(d$log_estr) & !is.na(d$log_prog), ]
  data.frame(task = tk, n_id = n_distinct(d$record_id),
             E2_sd_within = round(sd(d$estr_w_raw, na.rm = TRUE), 3),
             E2_sd_between = round(sd(d$estr_b_raw, na.rm = TRUE), 3),
             E2_anteil_within = round(var(d$estr_w_raw, na.rm = TRUE) /
                                        (var(d$estr_w_raw, na.rm = TRUE) +
                                           var(d$estr_b_raw, na.rm = TRUE)), 2),
             P4_sd_within = round(sd(d$prog_w_raw, na.rm = TRUE), 3),
             P4_sd_between = round(sd(d$prog_b_raw, na.rm = TRUE), 3),
             P4_anteil_within = round(var(d$prog_w_raw, na.rm = TRUE) /
                                        (var(d$prog_w_raw, na.rm = TRUE) +
                                           var(d$prog_b_raw, na.rm = TRUE)), 2))
}))
print(expo, row.names = FALSE)
cat("\nLesehilfe: Anteil within hoch (> ~.5) -> viel zyklisches Signal,\n",
    "ein Nullbefund ist aussagekräftig. Anteil niedrig -> die Within-Analyse\n",
    "ist strukturell schwach, und man weiss warum.\n")

# Pro Person: wer trägt überhaupt Variation bei? (flache P4-Verläufe =
# anovulatorische Zyklen -> korrekt gemessen, aber kaum informativ)
per_person <- bind_rows(lapply(c("SLB", "mSEBT"), function(tk) {
  d <- if (tk == "SLB") df_slb_mean else df_msebt_mean
  d %>% filter(!is.na(log_prog)) %>% group_by(record_id) %>%
    summarise(task = tk, n_sessions = n_distinct(session_id),
              sd_lE2 = sd(log_estr, na.rm = TRUE), sd_lP4 = sd(log_prog, na.rm = TRUE),
              p4_max = max(conc_prog, na.rm = TRUE), .groups = "drop")
}))
cat("\nVerteilung der personenspezifischen SD (log-Skala):\n")
print(summary(per_person[, c("sd_lE2", "sd_lP4")]))
write.csv(expo, file.path(out_dir, "40_exposure_summary.csv"), row.names = FALSE)
write.csv(per_person, file.path(out_dir, "40_exposure_variance.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# C  cycle_phase - deskriptiv. Der einzige verbleibende Einsatz.
# -----------------------------------------------------------------------------
cat("\n========== C) VARIIERT DAS OUTCOME UEBER DEN ZYKLUS? ==========\n")
phase_tab <- bind_rows(lapply(OUTCOMES$var, function(oc) {
  d <- prep_out(base_of(oc), oc); d <- d[!is.na(d$cycle_phase), ]
  d$cycle_phase <- droplevels(d$cycle_phase)
  if (nlevels(d$cycle_phase) < 2) {
    message("  [!] ", oc, ": cycle_phase hat < 2 Stufen (",
            nrow(d), " Zeilen) - Abschnitt C übersprungen.")
    return(NULL)
  }
  a <- anova(fitm(DESIGN, d),
             fitm(paste(DESIGN, "+ cycle_phase"), d))
  data.frame(outcome = oc, n_obs = nrow(d), n_id = n_distinct(d$record_id),
             chisq = round(a$Chisq[2], 2), df = a$Df[2],
             p = signif(a$`Pr(>Chisq)`[2], 3))
}))
print(phase_tab, row.names = FALSE)
cat("\nLesehilfe:\n",
    " kein Phaseneffekt + viel Hormonvariation über die Phasen\n",
    "   -> ein Hormoneffekt auf dieses Outcome ist a priori unwahrscheinlich\n",
    " Phaseneffekt vorhanden, aber Hormone erklären ihn nicht\n",
    "   -> etwas anderes Zyklisches (Symptome, Schlaf, Wasserhaushalt)\n")
write.csv(phase_tab, file.path(out_dir, "40_phase_omnibus.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# D  Was würde eine Adjustierung auf cycle_phase kosten?
#    (Begründung, warum sie in KEINEM Hormonmodell steht)
# -----------------------------------------------------------------------------
cat("\n========== D) PREIS EINER PHASEN-ADJUSTIERUNG ==========\n")
cost <- bind_rows(lapply(c("SLB", "mSEBT"), function(tk) {
  d <- if (tk == "SLB") df_slb_mean else df_msebt_mean
  # filter() statt d[...]: bei einer fehlenden Spalte gibt d$xxx stillschweigend
  # NULL, der Filter wird logical(0) und übrig bleiben NULL Zeilen - der
  # Folgefehler lautet dann "Kontraste ... 2 oder mehr Stufen" und zeigt in die
  # völlig falsche Richtung. filter() bricht stattdessen mit Klartext ab.
  n0 <- nrow(d)
  d <- d %>% filter(!is.na(cycle_phase), !is.na(estr_w), !is.na(prog_w))
  d$cycle_phase <- droplevels(d$cycle_phase)
  cat(sprintf("   %s: %d von %d Zeilen verwendbar\n", tk, nrow(d), n0))
  # Schutz gegen den Fall "Faktor mit weniger als 2 Stufen": lm() bricht sonst
  # mit einer Kontrast-Fehlermeldung ab, die nicht verrät, woran es liegt.
  if (nrow(d) < 5 || nlevels(d$cycle_phase) < 2) {
    message("  [!] ", tk, ": cycle_phase hat ", nlevels(d$cycle_phase),
            " Stufe(n) bei ", nrow(d), " Zeilen -> Abschnitt D übersprungen.",
            " Bitte table(cycle_phase, useNA = 'ifany') prüfen.")
    return(NULL)
  }
  bind_rows(lapply(c("estr_w", "prog_w"), function(v) {
    r2 <- summary(lm(as.formula(paste(v, "~ cycle_phase")), d))$r.squared
    data.frame(task = tk, exposition = v, R2_durch_phase = round(r2, 3),
               verbleibend = round(1 - r2, 3),
               SE_inflation = round(1 / sqrt(max(1 - r2, 1e-6)), 2))
  }))
}))
print(cost, row.names = FALSE)
cat("\nR2 = Anteil der Hormonvarianz, den die Phase erklärt - genau dieser\n",
    "Anteil wird durch eine Adjustierung entfernt. cycle_phase ist Elternknoten\n",
    "der Exposition, kein Confounder: die Adjustierung bringt keinen Schutz,\n",
    "kostet aber Präzision und kann bei unbeobachteter Konfundierung die\n",
    "Verzerrung sogar verstärken. Deshalb steht sie in keinem Hormonmodell.\n")
write.csv(cost, file.path(out_dir, "40_phase_cost.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# E  Lutealphase: deskriptive Kenngrösse, KEIN Einschlusskriterium
# -----------------------------------------------------------------------------
cat("\n========== E) LUTEAL-DESKRIPTION (P4 > 16 nmol/L) ==========\n")
lut <- bind_rows(lapply(c("SLB", "mSEBT"), function(tk) {
  d <- if (tk == "SLB") df_slb_mean else df_msebt_mean
  d %>% filter(!is.na(conc_prog), !is.na(cycle_phase)) %>%
    group_by(cycle_phase) %>%
    summarise(task = tk, n = n(), p4_median = round(median(conc_prog), 2),
              anteil_p4_gt16 = round(mean(conc_prog > 16), 2), .groups = "drop")
}))
print(lut, row.names = FALSE)
cat("\nWeil die Analyse mit KONZENTRATIONEN rechnet und nicht mit Phasen-\n",
    "Labels, ist die Schwelle kein Ausschlusskriterium. Ein niedriger Anteil\n",
    "im Lutealfenster heisst: viele Zyklen mit gedämpfter P4-Amplitude,\n",
    "also weniger Within-Signal - ein Power-, kein Validitätsproblem.\n")
write.csv(lut, file.path(out_dir, "40_luteal_check.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# F  Bivariate Roh-Assoziationen, jede Variable auf maximaler Stichprobe
# -----------------------------------------------------------------------------
cat("\n========== F) BIVARIATE ROH-ASSOZIATIONEN ==========\n")
biv_specs <- list(
  c("prog_w", "P4 within"), c("prog_b", "P4 between"),
  c("estr_w", "E2 within"), c("estr_b", "E2 between"),
  c("z_bmi_b", "BMI between"), c("z_sports_min", "Trainingsvolumen"),
  c("z_hip_abd", "Hüftabduktorenkraft")
)
biv_all <- bind_rows(lapply(OUTCOMES$var, function(oc) {
  bind_rows(lapply(biv_specs, function(s) {
    d <- prep_out(base_of(oc), oc); d <- d[!is.na(d[[s[1]]]), ]
    if (nrow(d) < 10) return(NULL)
    d$xx <- d[[s[1]]]
    m  <- fitm("xx", d); sm <- coef(summary(m))
    est <- sm["xx", "Estimate"]; se <- sm["xx", "Std. Error"]
    tq  <- qt(0.975, sm["xx", "df"])
    data.frame(outcome = oc, variable = s[2], estimate = round(est, 3),
               se = round(se, 3), ci_lo = round(est - tq * se, 3),
               ci_hi = round(est + tq * se, 3),
               p = signif(sm["xx", "Pr(>|t|)"], 3),
               n_obs = nrow(d), n_id = n_distinct(d$record_id))
  }))
}))
print(biv_all, row.names = FALSE)
cat("\nAchtung: roh und unadjustiert, nur zur Orientierung. Die konfirmatorische\n",
    "Schätzung steht in 60_primary.R.\n")
write.csv(biv_all, file.path(out_dir, "40_bivariate_ALL.csv"), row.names = FALSE)

cat(sprintf("\nDone. Outputs in %s\n", out_dir))
