# =============================================================================
# 50_missingness.R  |  FeHBI Paper 1 - Balance  |  Pipeline-Schritt 6 von 10
# -----------------------------------------------------------------------------
# ZWECK
#   Zeigt GENAU, welche Werte wo fehlen - damit du gezielt suchen und ergaenzen
#   kannst, statt Personen pauschal aus den Modellen zu verlieren.
#
#   Hintergrund: Die Modellskripte (60/70/80) nutzen Complete Cases. Fehlt eine
#   PERSONENKONSTANTE Kovariable (sports_min, hip_abd_stand aus M0), faellt die
#   ganze Person aus allen Modellen (17 -> 13). Fehlt dagegen ein Hormonwert in
#   nur einer Phase, geht nur diese eine Zeile verloren.
#
# -----------------------------------------------------------------------------
# WAS DAS SCRIPT AUSGIBT (in <data_dir>/Statistics/Models/)
#   50_missing_cells_ALL.csv   - eine Zeile pro fehlendem Wert:
#                                task, record_id, cycle_phase, standing_leg, variable
#                                (die Liste zum Abarbeiten/Nachtragen)
#   50_missing_by_variable.csv - Anzahl fehlender Zellen je Variable
#   50_dropouts.csv            - Personen, die beim Adjustieren wegfallen,
#                                und WELCHE Kovariable bei ihnen fehlt
#
#   Zusaetzlich Konsole: n-Vergleich "nur Hormone" vs "+ Kovariablen" je Task.
# =============================================================================

library(dplyr)
library(tidyr)

if (!exists("df_slb_mean")) source("10_load_data.R", encoding = "UTF-8")

out_dir <- file.path(data_dir, "Statistics", "Models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Schluesselvariablen je Task
vars_slb   <- c("cop_path_length", "cop_ellipse_area",
                "conc_estr", "conc_prog",
                "bmi_lab", "sports_min", "hip_abd_stand")
vars_msebt <- c("reach_ant", "reach_pl", "reach_pm", "leglength_mm",
                "conc_estr", "conc_prog",
                "bmi_lab", "sports_min", "hip_abd_stand")

# -----------------------------------------------------------------------------
# 1. Liste aller fehlenden Zellen
# -----------------------------------------------------------------------------
missing_cells <- function(df, task, key_vars) {
  present <- intersect(key_vars, names(df))
  id_cols <- intersect(c("record_id", "cycle_phase", "standing_leg"), names(df))
  df %>%
    select(all_of(id_cols), all_of(present)) %>%
    pivot_longer(all_of(present), names_to = "variable", values_to = "value") %>%
    filter(is.na(value)) %>%
    mutate(task = task) %>%
    select(task, all_of(id_cols), variable)
}

miss_all <- bind_rows(
  missing_cells(df_slb_mean,   "SLB",   vars_slb),
  missing_cells(df_msebt_mean, "mSEBT", vars_msebt)
) %>%
  arrange(task, variable, record_id)

cat("\n========== FEHLENDE ZELLEN (Kopf) ==========\n")
print(head(miss_all, 30), row.names = FALSE)
write.csv(miss_all, file.path(out_dir, "50_missing_cells_ALL.csv"),
          row.names = FALSE)

# -----------------------------------------------------------------------------
# 2. Fehlend je Variable
# -----------------------------------------------------------------------------
miss_by_var <- miss_all %>% count(task, variable, name = "n_fehlend")
cat("\n========== FEHLEND JE VARIABLE ==========\n")
print(as.data.frame(miss_by_var), row.names = FALSE)
write.csv(miss_by_var, file.path(out_dir, "50_missing_by_variable.csv"),
          row.names = FALSE)

# -----------------------------------------------------------------------------
# 3. Wer faellt beim Adjustieren weg - und wegen welcher Kovariable?
#    Kern = Outcome + Hormone vollstaendig; Adjustiert = zusaetzlich Kovariablen.
# -----------------------------------------------------------------------------
covars <- c("bmi_lab", "sports_min", "hip_abd_stand")

dropout_report <- function(df, task, outcome_col) {
  core <- df %>%
    filter(!is.na(.data[[outcome_col]]), !is.na(conc_estr), !is.na(conc_prog))
  ids_core <- unique(core$record_id)

  # Person ueberlebt Adjustierung, wenn >= 1 Zeile alle Kovariablen hat
  adj <- core %>%
    filter(!is.na(bmi_lab), !is.na(sports_min), !is.na(hip_abd_stand))
  ids_adj <- unique(adj$record_id)

  dropped <- setdiff(ids_core, ids_adj)

  cat(sprintf("\n[%s | Outcome %s] Personen: nur Hormone = %d  ->  + Kovariablen = %d  (verloren: %d)\n",
              task, outcome_col, length(ids_core), length(ids_adj), length(dropped)))

  if (length(dropped) == 0) return(NULL)

  # Fuer jede verlorene Person: welche Kovariable ist in ALLEN ihren Zeilen NA?
  bind_rows(lapply(dropped, function(id) {
    rows <- core[core$record_id == id, , drop = FALSE]
    fehlend <- covars[sapply(covars, function(v) all(is.na(rows[[v]])))]
    data.frame(task = task, outcome = outcome_col, record_id = id,
               fehlende_kovariable = paste(fehlend, collapse = ", "))
  }))
}

cat("\n========== DROPOUTS BEIM ADJUSTIEREN ==========\n")
dropouts <- bind_rows(
  dropout_report(df_slb_mean,   "SLB",   "cop_path_length"),
  dropout_report(df_slb_mean,   "SLB",   "cop_ellipse_area"),
  dropout_report(df_msebt_mean, "mSEBT", "reach_ant")
)

if (!is.null(dropouts) && nrow(dropouts) > 0) {
  cat("\nVerlorene Personen + fehlende Kovariable:\n")
  print(as.data.frame(dropouts), row.names = FALSE)
  write.csv(dropouts, file.path(out_dir, "50_dropouts.csv"), row.names = FALSE)
} else {
  cat("  Keine Dropouts durch Kovariablen.\n")
}

# -----------------------------------------------------------------------------
# 4. Fallzahl je Filter-Regime (was nutzt welches Script?)
#    "nur Hormone"   = Kernmodelle in 60_primary.R / 70_assumptions.R
#                      -> volle Fallzahl
#    "+ Kovariablen" = adjustierte Modelle in 40_diagnostics.R und
#                      80_specification_curve.R -> kleinere Fallzahl
# -----------------------------------------------------------------------------
regime_n <- function(df, task, oc) {
  core <- df %>% filter(!is.na(.data[[oc]]), !is.na(conc_estr), !is.na(conc_prog))
  adj  <- core %>% filter(!is.na(bmi_lab), !is.na(sports_min), !is.na(hip_abd_stand))
  data.frame(
    task = task, outcome = oc,
    regime = c("nur Hormone (Kern)", "+ Kovariablen (adjustiert)"),
    n_obs  = c(nrow(core), nrow(adj)),
    n_id   = c(n_distinct(core$record_id), n_distinct(adj$record_id))
  )
}

regime_tab <- bind_rows(
  regime_n(df_slb_mean,   "SLB",   "cop_path_length"),
  regime_n(df_slb_mean,   "SLB",   "cop_ellipse_area"),
  regime_n(df_msebt_mean, "mSEBT", "reach_ant"),
  regime_n(df_msebt_mean, "mSEBT", "reach_pl"),
  regime_n(df_msebt_mean, "mSEBT", "reach_pm")
)
cat("\n========== FALLZAHL JE FILTER-REGIME ==========\n")
print(as.data.frame(regime_tab), row.names = FALSE)
write.csv(regime_tab, file.path(out_dir, "50_n_by_regime.csv"), row.names = FALSE)

cat(sprintf("\nDone. Outputs in %s\n", out_dir))
