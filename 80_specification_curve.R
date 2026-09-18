# =============================================================================
# 80_specification_curve.R  |  FeHBI Paper 1  |  Pipeline-Schritt 9 von 10
# -----------------------------------------------------------------------------
# ZWECK  ROBUSTHEIT, ausdrücklich KEINE Modellwahl. Es wird gezeigt, wie stark
#        der Within-Effekt von vertretbaren Analyseentscheidungen abhängt.
#        Ergebnis ist eine VERTEILUNG, kein Gewinnermodell.
#
# KEINE RANGLISTE
#   Es wird ausdrücklich KEIN "bestes Modell" und keine Top-Liste nach AICc
#   ausgegeben. Informationskriterien schätzen den Vorhersagefehler; sie sagen
#   nichts darüber, ob ein Koeffizient einen Kausaleffekt trifft, und können
#   Modelle bevorzugen, die besser vorhersagen und den fokalen Koeffizienten
#   schlechter schätzen. Bei Mixed Models ist AICc zudem unklar definiert
#   (Zeilen-n oder Cluster-n).
#
#   Zielgrösse ist immer der WITHIN-Koeffizient, nicht ein Gesamt-z-Effekt.
#   Als Interaktion ist nur Trainingsvolumen x Hormon_within im Kandidatensatz;
#   Hormon x BMI und Hormon x Hüftkraft haben keine theoretische Grundlage.
#
# AUFBAU
#   - Jede Spezifikation wird als DAG-KONFORM getaggt oder nicht. Multiverse-
#     Analysen täuschen, wenn gut und schlecht begründete Spezifikationen im
#     selben Topf landen: ein echter Effekt kann darin untergehen, und falsch
#     adjustierte Modelle können Falsch-Positive erzeugen. Nicht DAG-konform
#     ist hier ausschliesslich cycle_phase (Elternknoten der Exposition).
#     Über INCLUDE_PHASE_SPECS steuerbar - Standard FALSE.
#   - Specification Curve statt Histogramm: Schätzer sortiert mit KI, darunter
#     die Belegung der Terme.
#   - isSingular() wird mitgeführt und der Anteil berichtet.
#   - "Anteil signifikant" bleibt, aber als DESKRIPTION: die Modelle sind nicht
#     unabhängig, das ist keine Inferenz.
#
# MISSING  Bewusst Complete Case über alle Kandidaten - alle Spezifikationen
#          müssen auf DERSELBEN Stichprobe laufen, sonst vergleicht man
#          Stichprobenwechsel statt Modellwahl.
#
# LOOPT SELBST über ALLE Outcomes aus OUTCOMES, wie 60_primary.R und
# 70_assumptions.R. Wer nur ein einzelnes Outcome will, setzt vorher
# outcome_var <- "reach_ant".
#
# OUTPUT  je Outcome: 80_specs_<outcome>.csv, 80_stability_<outcome>.csv,
#                     80_speccurve_P4_<outcome>.png, 80_speccurve_E2_<outcome>.png
#         über alle: 80_specs_ALL.csv, 80_stability_ALL.csv
# =============================================================================
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(patchwork)

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

stopifnot(exists("FEHBI_COL"))
INCLUDE_PHASE_SPECS <- FALSE   # TRUE nur, wenn ein Review es verlangt

# Welche Outcomes? Standard: alle. Ein vorab gesetztes outcome_var schränkt ein.
OC_LIST <- if (exists("outcome_var") && length(outcome_var) == 1 &&
                 outcome_var %in% OUTCOMES$var) outcome_var else OUTCOMES$var

CORE <- c("prog_w", "estr_w")   # Zielgrösse, immer im Modell
# Designterme aus 20_prepare.R - nicht hartkodieren, sonst entstehen
# Spezifikationen mit einer Variablen, die es in den Daten gar nicht gibt.
if (!exists("DESIGN_TERMS")) DESIGN_TERMS <- "standing_leg"
optional_terms <- c(DESIGN_TERMS,
                    "prog_b", "estr_b",
                    "z_bmi_b", "z_sports_min",
                    "prog_w:estr_w", "z_sports_min:prog_w", "z_sports_min:estr_w")
NON_DAG <- character(0)
if (INCLUDE_PHASE_SPECS) {
  optional_terms <- c(optional_terms, "cycle_phase"); NON_DAG <- "cycle_phase"
}

# Hierarchieprinzip: Interaktion nur mit beiden Haupteffekten
needs <- list("z_sports_min:prog_w" = "z_sports_min",
              "z_sports_min:estr_w" = "z_sports_min")
valid <- function(tt) all(vapply(names(needs), function(ix)
  !(ix %in% tt) || needs[[ix]] %in% tt, logical(1)))

specs <- Filter(valid, do.call(c, lapply(0:length(optional_terms), function(r)
  combn(optional_terms, r, simplify = FALSE))))
cat(sprintf("Spezifikationen je Outcome: %d | Outcomes: %d\n",
            length(specs), length(OC_LIST)))

est_se <- function(m, term) {
  fe <- fixef(m)
  if (!term %in% names(fe)) return(c(NA_real_, NA_real_))
  c(unname(fe[term]), unname(sqrt(diag(vcov(m)))[term]))
}

# -----------------------------------------------------------------------------
# Stabilitäts-Kennzahlen
# -----------------------------------------------------------------------------
stab <- function(d, e, s, lab, oc) {
  d <- d[!is.na(d[[e]]), ]; if (!nrow(d)) return(NULL)
  tv <- d[[e]] / d[[s]]
  data.frame(outcome = oc, term = lab, n_modelle = nrow(d),
             anteil_positiv = round(mean(d[[e]] > 0), 3),
             anteil_signif_deskriptiv = round(mean(abs(tv) >= 1.96), 3),
             median_est = round(median(d[[e]]), 4),
             min_est = round(min(d[[e]]), 4), max_est = round(max(d[[e]]), 4),
             spannweite = round(diff(range(d[[e]])), 4), row.names = NULL)
}

# -----------------------------------------------------------------------------
# Specification Curve. Farbe = Hormon (blau P4, orange E2, wie überall);
# DAG-Konformität über die Form, damit die Farbbedeutung eindeutig bleibt.
# -----------------------------------------------------------------------------
curve <- function(sp, e, s, lab, col, oc) {
  d <- sp %>% filter(!is.na(.data[[e]])) %>%
    mutate(est = .data[[e]], lo = est - 1.96 * .data[[s]],
           hi = est + 1.96 * .data[[s]],
           dag = factor(ifelse(dag_konform, "DAG-consistent",
                               "not DAG-consistent"),
                        levels = c("DAG-consistent", "not DAG-consistent"))) %>%
    arrange(est) %>% mutate(rank = row_number())
  top <- ggplot(d, aes(rank, est)) +
    geom_linerange(aes(ymin = lo, ymax = hi), colour = "grey75", linewidth = 0.2) +
    geom_point(aes(shape = dag), colour = col, size = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    scale_shape_manual(values = c("DAG-consistent" = 16,
                                  "not DAG-consistent" = 4), drop = FALSE) +
    labs(title = paste0("Specification curve: ", lab),
         subtitle = paste0(outcome_label(oc),
                           " | ", nrow(d), " specifications"),
         x = NULL, y = LAB_EFFECT, shape = NULL) +
    theme(legend.position = "top")
  bot <- d %>% select(rank, starts_with("has_")) %>%
    pivot_longer(-rank, names_to = "term", values_to = "drin") %>%
    filter(drin) %>% mutate(term = sub("^has_", "", term)) %>%
    ggplot(aes(rank, term)) +
    geom_point(size = 0.35, colour = FEHBI_COL[["ink"]]) +
    labs(x = "Specifications, ordered by effect size", y = NULL) +
    theme(axis.text.y = element_text(size = FEHBI_BASE - 3.5))
  top / bot + plot_layout(heights = c(2, 1.4))
}

# =============================================================================
# Ein Outcome durchrechnen
# =============================================================================
run_specs <- function(oc) {
  cat("\n\n############################################################\n")
  cat("###  OUTCOME:", oc, "-", outcome_label(oc), "\n")
  cat("############################################################\n")

  df <- base_of(oc) %>%
    filter(!is.na(.data[[oc]]), !is.na(conc_estr), !is.na(conc_prog),
           !is.na(bmi_lab), !is.na(sports_min), !is.na(cycle_phase))
  df$outcome <- if (STANDARDIZE_OUTCOME) zsafe(df[[oc]]) else df[[oc]]
  cat(sprintf("  %d Zeilen / %d Personen\n", nrow(df), n_distinct(df$record_id)))
  if (nrow(df) < 20) {
    message("  [-] ", oc, ": zu wenige Zeilen - übersprungen.")
    return(NULL)
  }

  res <- vector("list", length(specs))
  for (i in seq_along(specs)) {
    tt  <- specs[[i]]
    rhs <- paste(c(CORE, tt), collapse = " + ")
    m <- tryCatch(suppressWarnings(
      lmer(as.formula(paste("outcome ~", rhs, "+", RE)), df, REML = FALSE)),
      error = function(e) NULL)
    if (is.null(m)) next
    pw <- est_se(m, "prog_w"); ew <- est_se(m, "estr_w")
    row <- data.frame(outcome = oc, model_id = i,
                      spec = if (length(tt) == 0) "(nur Kern)" else paste(tt, collapse = ", "),
                      n_terms = length(tt),
                      dag_konform = !any(tt %in% NON_DAG),
                      AIC = AIC(m), BIC = BIC(m),
                      est_prog_w = pw[1], se_prog_w = pw[2],
                      est_estr_w = ew[1], se_estr_w = ew[2],
                      singular = isSingular(m), row.names = NULL)
    for (x in optional_terms) row[[paste0("has_", make.names(x))]] <- x %in% tt
    res[[i]] <- row
  }
  sp <- bind_rows(res)
  if (!nrow(sp)) { message("  [-] ", oc, ": kein Modell konvergiert."); return(NULL) }

  cat(sprintf("  Gefittet: %d | singulär: %d (%.0f %%)\n",
              nrow(sp), sum(sp$singular), 100 * mean(sp$singular)))
  cat("  AIC/BIC stehen in der CSV, werden aber NICHT zum Ranking benutzt.\n")
  write.csv(sp, file.path(out_dir, paste0("80_specs_", oc, ".csv")),
            row.names = FALSE)

  stab_tab <- bind_rows(stab(sp, "est_prog_w", "se_prog_w", "P4 within", oc),
                        stab(sp, "est_estr_w", "se_estr_w", "E2 within", oc))
  cat("\n---------- STABILITAET (anteil_signif ist DESKRIPTIV) ----------\n")
  print(stab_tab, row.names = FALSE)
  write.csv(stab_tab, file.path(out_dir, paste0("80_stability_", oc, ".csv")),
            row.names = FALSE)

  pc_p4 <- curve(sp, "est_prog_w", "se_prog_w", "within-person progesterone",
                 FEHBI_COL[["p4"]], oc)
  pc_e2 <- curve(sp, "est_estr_w", "se_estr_w", "within-person estradiol",
                 FEHBI_COL[["e2"]], oc)
  ggsave(file.path(out_dir, paste0("80_speccurve_P4_", oc, ".png")),
         pc_p4, width = 11, height = 7, dpi = FIG_DPI)
  ggsave(file.path(out_dir, paste0("80_speccurve_E2_", oc, ".png")),
         pc_e2, width = 11, height = 7, dpi = FIG_DPI)

  list(specs = sp, stab = stab_tab)
}

# =============================================================================
# Alle Outcomes durchlaufen
# =============================================================================
res50 <- lapply(OC_LIST, function(oc)
  tryCatch(run_specs(oc), error = function(e) {
    message("  FEHLER bei ", oc, ": ", conditionMessage(e)); NULL }))
res50 <- res50[!sapply(res50, is.null)]

if (length(res50) == 0)
  stop("80_specification_curve: kein Outcome konnte gerechnet werden.")

specs_ALL <- bind_rows(lapply(res50, function(x) x$specs))
stab_ALL  <- bind_rows(lapply(res50, function(x) x$stab))
write.csv(specs_ALL, file.path(out_dir, "80_specs_ALL.csv"), row.names = FALSE)
write.csv(stab_ALL,  file.path(out_dir, "80_stability_ALL.csv"), row.names = FALSE)

cat("\n\n========== STABILITAET UEBER ALLE OUTCOMES ==========\n")
print(as.data.frame(stab_ALL), row.names = FALSE)
cat("\nLesehilfe:\n",
    " anteil_positiv ~ .5, grosse Spannweite -> kein robuster gerichteter Effekt\n",
    " anteil_positiv ~ .95, kaum signifikant -> konsistente Richtung ohne Power\n",
    " grosse spannweite -> die Modellwahl allein verschiebt den Schaetzer\n",
    "   spuerbar; das gehört in die Diskussion\n")
cat(sprintf("\nDone. %d Outcomes gerechnet. Outputs in %s\n",
            length(res50), out_dir))
