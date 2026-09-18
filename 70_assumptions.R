# =============================================================================
# 70_assumptions.R  |  FeHBI Paper 1  |  Pipeline-Schritt 8 von 10
# -----------------------------------------------------------------------------
# ZWECK  Schrittweiser Modellaufbau mit LRT plus vollständige Annahmenprüfung.
#        Das ist die Rechtfertigung des Primärmodells aus 60_primary.R -
#        nicht die Suche nach einem besseren.
#
#        LOOPT SELBST über ALLE Outcomes aus OUTCOMES, wie 60_primary.R. Wer
#        nur ein einzelnes Outcome will, setzt vor dem Aufruf
#        outcome_var <- "reach_ant" - dann läuft nur das.
#
# MODELLKETTE  (d_core = volle Fallzahl)
#   M1  null                       Varianzzerlegung
#   M2  + Design                   standing_leg (+ first_measure, cycle_nr,
#                                  sobald Zyklus 1 in den Daten ist)
#   M2a + prog_w + estr_w          nur die beiden Haupteffekte (Zwischenstufe)
#   M3  + prog_w * estr_w          Within-Hormone inkl. Interaktion, 3 df
#                                                      <<< PRIMAERTEST
#   M4  + prog_b + estr_b + ExP_b  Mundlak = PRIMAERMODELL aus 60_primary.R
#
#   Der Primaertest ist M2 vs M3 mit 3 Freiheitsgraden. Die Fragestellung
#   nennt E2, P4 und deren Interaktion; der Test muss deshalb alle drei
#   Within-Terme umfassen (Kovariablendokument v3, Abschnitt 11). M2 vs M2a
#   (2 df) und M2a vs M3 (1 df) werden zusaetzlich berichtet, damit sichtbar
#   bleibt, welcher Teil den Beitrag traegt.
# (d_adj = zusätzlich Kovariablen vollständig)
#   M6, M7  Between-Kovariablen einzeln (BMI, Trainingsvolumen)
#   M8      beide gemeinsam
#   M9      Moderator Trainingsvolumen x P4_within
#
#   Hüftkraft (z_hip_abd) ist NICHT dabei: zeitkonstant, strukturell kein
#   Confounder des Within-Effekts, fehlt aber bei 17 von 59 Frauen. Sie bleibt
#   deskriptiv in 40_diagnostics.R, Abschnitt F.
#
# NICHT IN DER KETTE
#   - cycle_phase, in keinem Modell. Stünde sie ab M2 in jedem Folgemodell,
#     schätzten alle Hormonkoeffizienten den Effekt "zusätzlich zur Kenntnis
#     der Phase" - also ohne die zyklische Variation, um die es geht.
#   - Interaktionen cycle_phase x Hormon, aus demselben Grund.
#   - AICc: bei Mixed Models ist unklar, welches n einzusetzen ist (Zeilen
#     oder Cluster). AIC und BIC werden berichtet, aber nur INNERHALB
#     derselben Stichprobe verglichen.
#
# GUELTIGKEIT DER LRT  Zwei-Stichproben-Logik d_core / d_adj, wobei die Basis
#   auf d_adj neu gefittet wird - sonst vergleicht der LRT zwei Modelle auf
#   verschiedenen Stichproben. n_obs/n_id werden pro Modell ausgewiesen.
#   Die Diagnostik laeuft am Primaermodell, nicht am vollsten Modell.
#
# OUTPUT  je Outcome: 70_model_stats_<outcome>.csv, 70_coef_table_<outcome>.csv,
#                     70_diagnostics_<outcome>.png, 70_checkmodel_<outcome>.pdf
#         über alle: 70_model_stats_ALL.csv, 70_coef_table_ALL.csv,
#                     70_residual_skew_ALL.csv  <- Entscheidungsgrundlage log
# =============================================================================
library(dplyr)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(ggplot2)
library(patchwork)

# || kurzschliesst: fehlt das Objekt ganz (Skript einzeln gestartet), wird
# names() gar nicht erst ausgewertet und 20_prepare.R uebernimmt.
if (!exists("df_slb_mean") ||
    !all(c("estr_w", "prog_w") %in% names(df_slb_mean)))
  source("20_prepare.R", encoding = "UTF-8")
stopifnot(all(c("estr_w", "prog_w", "estr_b", "prog_b") %in% names(df_slb_mean)),
          all(c("estr_w", "prog_w", "estr_b", "prog_b") %in% names(df_msebt_mean)),
          exists("FEHBI_COL"))
out_dir <- file.path(data_dir, "Statistics", "Models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Welche Outcomes? Standard: alle. Ein vorab gesetztes outcome_var schränkt ein.
OC_LIST <- if (exists("outcome_var") && length(outcome_var) == 1 &&
                 outcome_var %in% OUTCOMES$var) outcome_var else OUTCOMES$var

if (!exists("DESIGN")) DESIGN <- "standing_leg"
fitm <- function(rhs, dat)
  suppressWarnings(lmer(as.formula(paste("outcome ~", rhs, "+", RE)), dat, REML = FALSE))

# -----------------------------------------------------------------------------
# Kennzahlen
# -----------------------------------------------------------------------------
r2_manual <- function(m) {
  vf <- var(as.vector(model.matrix(m) %*% fixef(m)))
  vr <- sum(sapply(VarCorr(m), function(x) as.numeric(x)))
  ve <- sigma(m)^2
  c(R2m = round(vf / (vf + vr + ve), 3), R2c = round((vf + vr) / (vf + vr + ve), 3))
}
model_stats <- function(m, name, samp, oc) {
  r2 <- r2_manual(m)
  vr <- sum(sapply(VarCorr(m), function(x) as.numeric(x)))
  data.frame(outcome = oc, model = name, sample = samp, n_obs = nobs(m),
             n_id = ngrps(m)[["record_id"]],
             AIC = round(AIC(m), 1), BIC = round(BIC(m), 1),
             R2m = r2["R2m"], R2c = r2["R2c"],
             ICC = round(vr / (vr + sigma(m)^2), 3),
             singular = isSingular(m), row.names = NULL)
}
schiefe <- function(x) mean((x - mean(x))^3) / stats::sd(x)^3

# Macht aus einem anova()-Vergleich eine speicherbare Zeile, damit die LRTs
# nicht nur auf der Konsole stehen und nach dem Durchlauf verloren sind.
lrt_row <- function(a, vergleich, was, oc) {
  data.frame(outcome = oc, vergleich = vergleich, was = was,
             df = a$Df[2], chisq = round(a$Chisq[2], 2),
             p = signif(a[["Pr(>Chisq)"]][2], 4),
             AIC_klein = round(a$AIC[1], 1), AIC_gross = round(a$AIC[2], 1),
             row.names = NULL)
}

# =============================================================================
# Ein Outcome komplett durchrechnen
# =============================================================================
run_outcome <- function(oc) {
  cat("\n\n############################################################\n")
  cat("###  OUTCOME:", oc, "-", outcome_label(oc), "\n")
  cat("############################################################\n")

  base_data <- base_of(oc)
  mk <- function(d) { d$outcome <- if (STANDARDIZE_OUTCOME)
    zsafe(d[[oc]]) else d[[oc]]; d }

  d_core <- base_data %>%
    filter(!is.na(.data[[oc]]), !is.na(conc_estr), !is.na(conc_prog)) %>% mk()
  d_adj <- base_data %>%
    filter(!is.na(.data[[oc]]), !is.na(conc_estr), !is.na(conc_prog),
           !is.na(bmi_lab), !is.na(sports_min)) %>% mk()

  cat(sprintf("  d_core: %d Zeilen / %d Personen | d_adj: %d / %d\n",
              nrow(d_core), n_distinct(d_core$record_id),
              nrow(d_adj), n_distinct(d_adj$record_id)))
  if (nrow(d_core) < 20) {
    message("  [-] ", oc, ": zu wenige Zeilen - übersprungen.")
    return(NULL)
  }

  # ---------------------------------------------------------------------------
  # Block A: Design und Hormone (d_core)
  # ---------------------------------------------------------------------------
  cat("\n---------- BLOCK A: DESIGN + HORMONE (d_core) ----------\n")
  # Die Fragestellung nennt E2, P4 UND deren Interaktion. Der Primaertest
  # umfasst deshalb alle drei Within-Terme und hat 3 Freiheitsgrade, nicht 2
  # (Kovariablendokument v3, Abschnitt 11). M2a bleibt als Zwischenstufe
  # erhalten, damit der eigene Beitrag der Interaktion (1 df) sichtbar bleibt.
  M1  <- fitm("1", d_core)
  M2  <- fitm(DESIGN, d_core)
  M2a <- fitm(paste(DESIGN, "+ prog_w + estr_w"), d_core)          # nur Haupteffekte
  M3  <- fitm(paste(DESIGN, "+ prog_w * estr_w"), d_core)          # <<< PRIMAERTEST (3 df)
  M4  <- fitm(paste(DESIGN, "+ prog_w * estr_w",
                            "+ prog_b + estr_b + ExP_b"), d_core)  # PRIMAERMODELL (Mundlak)

  a12  <- anova(M1, M2);   a23 <- anova(M2, M3)
  a2a  <- anova(M2, M2a);  aIx <- anova(M2a, M3)
  a34  <- anova(M3, M4)
  cat("\n-- M1 vs M2 (+ Design) --\n");                              print(a12)
  cat("\n-- M2 vs M3 (+ Within-Hormone inkl. Interaktion, 3 df)",
      "<<< PRIMAERTEST --\n");                                       print(a23)
  cat("\n-- M2 vs M2a (nur Within-Haupteffekte, 2 df) --\n");        print(a2a)
  cat("\n-- M2a vs M3 (eigener Beitrag der Interaktion, 1 df) --\n"); print(aIx)
  cat("\n-- M3 vs M4 (+ Between-Hormone, Mundlak) --\n");            print(a34)
  lrts <- bind_rows(
    lrt_row(a12, "M1 vs M2",  "Design", oc),
    lrt_row(a23, "M2 vs M3",  "Within-Hormone inkl. E2xP4 (PRIMAERTEST)", oc),
    lrt_row(a2a, "M2 vs M2a", "Within-Haupteffekte allein", oc),
    lrt_row(aIx, "M2a vs M3", "Within-Interaktion E2xP4 allein", oc),
    lrt_row(a34, "M3 vs M4",  "Between-Hormone inkl. E2xP4 (Mundlak)", oc))

  # ---------------------------------------------------------------------------
  # Block B: Between-Kovariablen (d_adj), Basis neu gefittet
  # ---------------------------------------------------------------------------
  models  <- list("M1_null" = M1, "M2_design" = M2,
                  "M2a_within_haupteffekte" = M2a, "M3_within_inkl_ixn" = M3,
                  "M4_PRIMAER_mundlak" = M4)
  samples <- rep("core", 5)

  if (nrow(d_adj) >= 20) {
    cat("\n---------- BLOCK B: BETWEEN-KOVARIABLEN (d_adj) ----------\n")
    BASE <- paste(DESIGN, "+ prog_w * estr_w + prog_b + estr_b + ExP_b")
    M4a <- fitm(BASE, d_adj)
    M6  <- fitm(paste(BASE, "+ z_bmi_b"), d_adj)
    M7  <- fitm(paste(BASE, "+ z_sports_min"), d_adj)
    M8  <- fitm(paste(BASE, "+ z_bmi_b + z_sports_min"), d_adj)
    M9  <- fitm(paste(BASE, "+ z_bmi_b + z_sports_min + z_sports_min:prog_w"), d_adj)

    cat("\n(Basis M4a auf d_adj neu gefittet, damit die Vergleiche gültig sind)\n")
    b6 <- anova(M4a, M6); b7 <- anova(M4a, M7)
    b8 <- anova(M4a, M8); b9 <- anova(M8, M9)
    cat("\n-- M4a vs M6 (+ BMI between) --\n");      print(b6)
    cat("\n-- M4a vs M7 (+ Trainingsvolumen) --\n"); print(b7)
    cat("\n-- M4a vs M8 (+ beide) --\n");            print(b8)
    cat("\n-- M8 vs M9 (Moderator Sport x P4_within) --\n"); print(b9)
    lrts <- bind_rows(lrts,
      lrt_row(b6, "M4a vs M6", "+ BMI between", oc),
      lrt_row(b7, "M4a vs M7", "+ Trainingsvolumen", oc),
      lrt_row(b8, "M4a vs M8", "+ beide Kovariablen", oc),
      lrt_row(b9, "M8 vs M9",  "Moderator Sport x P4_within", oc))
    cat("\nErwartung: Diese Terme verschieben den WITHIN-Koeffizienten kaum -\n",
        "sie sind zeitkonstant und können ihn strukturell nicht konfundieren.\n")

    models  <- c(models, list("M6_+bmi_b" = M6, "M7_+sport" = M7,
                              "M8_+beide_kov" = M8, "M9_moderator" = M9))
    samples <- c(samples, rep("adj", 4))
  } else {
    message("  [i] d_adj zu klein - Block B für ", oc, " übersprungen.")
  }

  # ---------------------------------------------------------------------------
  # Tabellen
  # ---------------------------------------------------------------------------
  stats_table <- bind_rows(lapply(seq_along(models), function(i)
    model_stats(models[[i]], names(models)[i], samples[i], oc)))
  cat("\n---------- MODELL-KENNZAHLEN (AIC/BIC nur innerhalb sample!) ----------\n")
  print(stats_table, row.names = FALSE)
  write.csv(stats_table, file.path(out_dir, paste0("70_model_stats_", oc, ".csv")),
            row.names = FALSE)

  coef_table <- bind_rows(lapply(names(models), function(nm) {
    m <- models[[nm]]
    broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
      mutate(outcome = oc, model = nm, n_obs = nobs(m),
             n_id = ngrps(m)[["record_id"]])
  })) %>% select(outcome, model, n_obs, n_id, term, estimate, std.error,
                 statistic, p.value, conf.low, conf.high)
  write.csv(coef_table, file.path(out_dir, paste0("70_coef_table_", oc, ".csv")),
            row.names = FALSE)

  cat("\n-- R2 marginal vs conditional --\n")
  print(stats_table[, c("model", "sample", "R2m", "R2c", "ICC")], row.names = FALSE)
  cat("R2m klein und R2c gross -> individuelle Unterschiede dominieren und\n",
      "lassen sich mit diesen Prädiktoren nicht erklären. Das ist ein\n",
      "legitimes Ergebnis und in der Diskussion mehr wert als jeder p-Wert.\n")

  # ---------------------------------------------------------------------------
  # Annahmenprüfung am PRIMAERMODELL (M4)
  # ---------------------------------------------------------------------------
  prim    <- M4
  diag_df <- data.frame(fitted = fitted(prim), residuals = residuals(prim))
  oc_lab  <- outcome_label(oc)

  # Kennzahl für die log-Entscheidung: Schiefe der BEDINGTEN Residuen.
  # Nicht die Randverteilung des Rohoutcomes - das war der Punkt deines
  # Statistikers. Faustregel: |Schiefe| < 0.5 unauffällig, > 1 deutlich schief.
  sk <- schiefe(diag_df$residuals)
  cat(sprintf("\nSchiefe der bedingten Residuen: %.2f\n", sk))
  if (abs(sk) > 1)
    cat("  [!] |Schiefe| > 1. Bei einem nicht transformierbaren Outcome ist das\n",
        "      fast immer ein Ausreisserproblem, kein Verteilungsproblem.\n")

  # Die extremsten Residuen mit Identifikation - damit ein auffälliger Wert
  # in den Rohdaten nachgeschlagen werden kann statt anonym zu bleiben.
  idcols <- intersect(c("record_id", "phase", "cycle_phase", "standing_leg"),
                      names(d_core))
  extrem <- d_core[idcols]
  extrem$outcome_var  <- oc
  extrem$wert         <- d_core[[oc]]
  extrem$residuum     <- diag_df$residuals
  extrem$resid_z      <- round(diag_df$residuals / stats::sd(diag_df$residuals), 2)
  extrem <- extrem[order(-abs(extrem$resid_z)), ][seq_len(min(8, nrow(extrem))), ]
  cat("\nGrösste standardisierte Residuen:\n")
  print(extrem, row.names = FALSE, digits = 4)

  p_qq <- ggplot(diag_df, aes(sample = residuals)) +
    stat_qq(colour = FEHBI_COL[["p4"]], alpha = 0.6, size = 1) +
    stat_qq_line(colour = FEHBI_COL[["e2"]]) +
    labs(title = "Normal Q-Q plot of conditional residuals", subtitle = oc_lab,
         x = "Theoretical quantiles", y = "Sample quantiles")
  p_rf <- ggplot(diag_df, aes(fitted, residuals)) +
    geom_point(alpha = 0.5, colour = FEHBI_COL[["p4"]], size = 1) +
    geom_hline(yintercept = 0, colour = FEHBI_COL[["e2"]], linetype = "dashed") +
    geom_smooth(se = FALSE, colour = FEHBI_COL[["accent"]], linewidth = 0.8) +
    labs(title = "Residuals versus fitted values", subtitle = oc_lab,
         x = "Fitted values", y = "Residuals")
  dp <- p_qq + p_rf
  print(dp)
  ggsave(file.path(out_dir, paste0("70_diagnostics_", oc, ".png")),
         dp, width = 10, height = 4.2, dpi = FIG_DPI)

  if (requireNamespace("performance", quietly = TRUE)) {
    pdf(file.path(out_dir, paste0("70_checkmodel_", oc, ".pdf")),
        width = 11, height = 8)
    print(performance::check_model(prim)); dev.off()
  }

  write.csv(lrts, file.path(out_dir, paste0("70_lrt_", oc, ".csv")), row.names = FALSE)

  list(stats = stats_table, coefs = coef_table, lrts = lrts, extrem = extrem,
       skew  = data.frame(outcome = oc, label = oc_lab,
                          n_obs = nobs(prim),
                          schiefe_residuen = round(sk, 3),
                          ICC = stats_table$ICC[stats_table$model == "M4_PRIMAER_mundlak"],
                          row.names = NULL))
}

# =============================================================================
# Alle Outcomes durchlaufen
# =============================================================================
res40 <- lapply(OC_LIST, function(oc)
  tryCatch(run_outcome(oc), error = function(e) {
    message("  FEHLER bei ", oc, ": ", conditionMessage(e)); NULL }))
res40 <- res40[!sapply(res40, is.null)]

if (length(res40) == 0)
  stop("70_assumptions: kein Outcome konnte gerechnet werden.")

stats_ALL <- bind_rows(lapply(res40, function(x) x$stats))
coefs_ALL <- bind_rows(lapply(res40, function(x) x$coefs))
skew_ALL  <- bind_rows(lapply(res40, function(x) x$skew))
lrt_ALL   <- bind_rows(lapply(res40, function(x) x$lrts))
extrem_ALL <- bind_rows(lapply(res40, function(x) x$extrem))
write.csv(extrem_ALL, file.path(out_dir, "70_extreme_residuals_ALL.csv"),
          row.names = FALSE)
write.csv(lrt_ALL, file.path(out_dir, "70_lrt_ALL.csv"), row.names = FALSE)
write.csv(stats_ALL, file.path(out_dir, "70_model_stats_ALL.csv"), row.names = FALSE)
write.csv(coefs_ALL, file.path(out_dir, "70_coef_table_ALL.csv"), row.names = FALSE)
write.csv(skew_ALL,  file.path(out_dir, "70_residual_skew_ALL.csv"), row.names = FALSE)

cat("\n\n========== PRIMAERTEST (M2 vs M3) UEBER ALLE OUTCOMES ==========\n")
print(as.data.frame(lrt_ALL[grepl("PRIMAERTEST", lrt_ALL$was), ]), row.names = FALSE)

cat("\n\n========== ANNAHMEN UEBER ALLE OUTCOMES ==========\n")
print(as.data.frame(skew_ALL), row.names = FALSE)
cat("\nLesehilfe zur log-Entscheidung:\n",
    " |Schiefe| < 0.5  unauffällig\n",
    " |Schiefe| > 1    deutlich schief -> die andere Variante prüfen\n",
    " Bei USE_LOG_COP = 'both' stehen cop_path_length und log_cop_path als\n",
    " getrennte Zeilen nebeneinander. Massgeblich sind diese bedingten\n",
    " Residuen, nicht die Randverteilung des Rohoutcomes.\n")
cat(sprintf("\nDone. %d Outcomes gerechnet. Outputs in %s\n",
            length(res40), out_dir))
