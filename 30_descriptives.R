# =============================================================================
# 30_descriptives.R  |  FeHBI Paper 1 - Balance  |  Pipeline-Schritt 4 von 10
# Deskriptive Abbildungen fuer die Betreuungsbesprechung
# -----------------------------------------------------------------------------
# VORAUSSETZUNG: 10_load_data.R und 20_prepare.R sind gelaufen.
#   Gebraucht werden: df_slb_mean, df_msebt_mean, OUTCOMES, data_dir,
#                     FIG_DPI, FEHBI_BLUE, FEHBI_ORANGE, FEHBI_INK
#   sowie df_slb / df_msebt auf TRIALEBENE (Bloecke 6 bis 8). Fehlen die,
#   werden diese Bloecke uebersprungen statt abzubrechen.
#   Das Design kommt aus 00_setup.R, das 20_prepare.R per theme_set() setzt.
#   Hier wird KEIN eigenes Theme definiert - sonst laufen die Deskriptivplots
#   und die Modellplots optisch auseinander.
#
# REGEL WIE IN DER GANZEN PIPELINE:
#   Achsen, Titel, Legenden ENGLISCH. Kommentare und Konsole deutsch.
#
# AUSGABE:
#   - PNGs nach data_dir/Statistics/"deskriptive Analyse", Praefix 01b_
#   - 30_slide_numbers.csv: die Aggregate fuer die Folien. NUR Kennzahlen,
#     KEINE Zeilendaten - diese Datei kann bedenkenlos weitergegeben werden.
#   - 30_messfehler.csv, 30_varianzbudget.csv, 30_bein_symmetrie.csv,
#     30_kovariablen_korrelation.csv, 30_outliers.csv, 30_median_check.csv
#
# BLOECKE:
#   0 Median-Check   1 Vollstaendigkeit   2 Exposition   3 Stichprobe
#   4 Outcomes       5 Datenqualitaet     6 Messfehler   7 Varianzbudget
#   8 Bein-Symmetrie 9 Within-Streuung   10 Kovariablen
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
})

# ggrepel ist nur fuer die Ausreisser-Beschriftung noetig. Fehlt es, wird
# geom_text verwendet - das Skript soll daran nicht scheitern.
HAT_REPEL <- requireNamespace("ggrepel", quietly = TRUE)
if (!HAT_REPEL)
  message("  [i] ggrepel fehlt - Ausreisser werden ohne Ausweichlogik beschriftet.")
label_layer <- function(...) {
  if (HAT_REPEL) ggrepel::geom_text_repel(..., max.overlaps = 20, seed = 1)
  else geom_text(..., hjust = -0.2, check_overlap = TRUE)
}

stopifnot(exists("df_slb_mean"), exists("df_msebt_mean"))
# --- Ausgabeordner -----------------------------------------------------------
# Alles Deskriptive geht in einen EIGENEN Ordner, getrennt von den Modell-
# ausgaben (die 40_diagnostics.R nach Statistics/Models schreibt). Deshalb ein
# eigener Name: die Variable out_dir wird von den Modellskripten neu gesetzt,
# DESK_DIR nicht - egal in welcher Reihenfolge die Skripte laufen.
DESK_DIR <- if (exists("data_dir"))
  file.path(data_dir, "Statistics", "deskriptive Analyse") else
    file.path("Statistics", "deskriptive Analyse")
dir.create(DESK_DIR, recursive = TRUE, showWarnings = FALSE)
cat("Ausgabeordner:", normalizePath(DESK_DIR, winslash = "/"), "\n")
if (!exists("FIG_DPI"))  FIG_DPI  <- 300
if (!exists("FEHBI_BLUE"))   FEHBI_BLUE   <- "#3D7ABF"
if (!exists("FEHBI_ORANGE")) FEHBI_ORANGE <- "#C9663A"
if (!exists("FEHBI_INK"))    FEHBI_INK    <- "#1F2933"

P4C <- FEHBI_BLUE; E2C <- FEHBI_ORANGE; INKC <- FEHBI_INK

WINDOW_LAB <- c(menstrual = "Menstrual", late_follicular = "Late follicular",
                ovulatory = "Ovulatory",  luteal = "Luteal")

# Sammelbehaelter fuer die Foliennzahlen
NUM <- list()
add_num <- function(block, kennzahl, wert, einheit = NA_character_) {
  NUM[[length(NUM) + 1]] <<- data.frame(
    block = block, kennzahl = kennzahl,
    wert = if (is.numeric(wert)) round(wert, 4) else as.character(wert),
    einheit = einheit, stringsAsFactors = FALSE)
}

# Jede Abbildung wird zusaetzlich im Speicher behalten, damit am Ende ein
# Sammel-PDF entstehen kann. Die Zielmasse w/h werden mitgespeichert, sonst
# waere im PDF jede Abbildung auf dieselbe Seitenform gestreckt.
PLOTS <- list()

save_fig <- function(p, datei, w, h) {
  # p ist entweder ein ggplot oder - bei den zweizeiligen Rastern - eine
  # gtable. ggsave zeichnet beides, das Sammel-PDF braucht eine Fallunter-
  # scheidung (weiter unten).
  pfad <- file.path(DESK_DIR, datei)
  ggsave(pfad, p, width = w, height = h, dpi = FIG_DPI, bg = "white")
  PLOTS[[datei]] <<- list(p = p, w = w, h = h)
  cat("  geschrieben:", datei, "\n")
}

win_factor <- function(x) factor(WINDOW_LAB[as.character(x)],
                                 levels = unname(WINDOW_LAB))

# =============================================================================
# OUTCOME-REIHENFOLGE UND ZWEIZEILIGES RASTER
# -----------------------------------------------------------------------------
# Alle Abbildungen, die alle fuenf Outcomes zeigen, sollen dieselbe Ordnung
# haben: oben die beiden COP-Masse aus dem Einbeinstand, unten die drei
# Reichweiten aus dem mSEBT. Das sind zwei verschiedene Aufgaben mit zwei
# verschiedenen Einheiten - nebeneinander in einer Reihe gemischt liest man
# sie als eine Gruppe, was sie nicht sind.
#
# facet_wrap fuellt zeilenweise und kann nicht "zwei in der ersten, drei in
# der zweiten Zeile". Der Trick: eine leere Faktorstufe als Platzhalter an
# dritter Position, facet_wrap(ncol = 3, drop = FALSE) - dann sitzen die
# COP-Masse in Zeile eins und die Reichweiten in Zeile zwei. Das leere Panel
# samt Streifen und Achsen wird anschliessend aus der gtable entfernt.
#
# Warum ueber die gtable und nicht mit patchwork oder ggh4x: die beiden
# Pakete koennen das eleganter, aber sie sind keine Abhaengigkeit dieser
# Pipeline. gtable und grid kommen mit ggplot2 beziehungsweise mit R selbst,
# es muss also nichts installiert werden.
# =============================================================================
OUT_COP   <- c("COP path (mm)", "COP ellipse (mm2)")
OUT_SEBT  <- c("Anterior reach (%LL)", "Posterolateral reach (%LL)",
               "Posteromedial reach (%LL)")
OUT_LEER  <- " "                       # Platzhalter, wird nie gezeichnet
OUT_ORDER <- c(OUT_COP, OUT_SEBT)      # fuer Tabellen und einzelne Balken
OUT_RASTER <- c(OUT_COP, OUT_LEER, OUT_SEBT)   # fuer die Facetten

oc_factor <- function(x) factor(as.character(x), levels = OUT_RASTER)

# Das Raster-Facet, damit nicht in jeder Abbildung dieselben drei Argumente
# stehen und eines davon irgendwann abweicht.
facet_raster <- function(scales = "free")
  facet_wrap(~ outcome, scales = scales, ncol = 3, drop = FALSE)

zwei_bloecke <- function(p, leer = 3) {
  # Erwartet einen ggplot mit facet_raster() und outcome als oc_factor().
  # Liefert eine gtable - save_fig und das Sammel-PDF koennen damit umgehen.
  g  <- ggplot2::ggplotGrob(p)
  pn <- g$layout[grepl("^panel", g$layout$name), , drop = FALSE]
  pn <- pn[order(pn$t, pn$l), ]        # zeilenweise, so fuellt facet_wrap
  if (nrow(pn) < leer) return(g)       # weniger Panels als erwartet: unveraendert
  zt <- pn$t[leer]; zl <- pn$l[leer]
  # Panel, Streifen und Achsen des Platzhalters: gleiche Spalte im Band
  # ueber und unter dem Panel, gleiche Zeile in den Nachbarspalten.
  raus <- which((g$layout$l == zl & g$layout$t %in% (zt - 2):(zt + 1)) |
                  (g$layout$t == zt & g$layout$l %in% (zl - 1):(zl + 1)))
  for (i in raus) g$grobs[[i]] <- grid::nullGrob()
  g
}

cat("\n=====================================================\n")
cat("01b  DESKRIPTIVE ABBILDUNGEN\n")
cat("=====================================================\n")

# =============================================================================
# BLOCK 0  Median-Ebenen-Check
# -----------------------------------------------------------------------------
# Offene Frage aus der Besprechungsvorbereitung: In den Dokumenten stehen
# E2-Mediane von 128.5 / 542 / 386.5 / 575, im Deskriptiv-PDF 130 / 542 /
# 382 / 572. Vermutung: die einen sind auf ZEILENebene (jede Frau zaehlt
# zweimal, einmal pro Bein), die anderen auf Person x Fenster. Hier wird das
# entschieden, statt geraten.
# =============================================================================
cat("\n--- 0) Median-Ebenen-Check E2 -----------------------\n")

med_zeile <- df_slb_mean %>%
  group_by(cycle_phase) %>%
  summarise(n_zeilen = sum(is.finite(conc_estr)),
            e2_zeilenebene = median(conc_estr, na.rm = TRUE), .groups = "drop")

med_person <- df_slb_mean %>%
  group_by(record_id, cycle_phase) %>%
  summarise(e2 = mean(conc_estr, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(e2)) %>%
  group_by(cycle_phase) %>%
  summarise(n_personen = n(),
            e2_personenebene = median(e2), .groups = "drop")

med_check <- left_join(med_zeile, med_person, by = "cycle_phase")
print(as.data.frame(med_check), row.names = FALSE)
cat("\n  -> Die Personenebene ist die richtige fuer eine Stichproben-\n",
    "     beschreibung: das Hormon wurde EINMAL pro Session bestimmt.\n",
    "     Auf Zeilenebene zaehlt derselbe Wert zweimal, weil zwei Beine.\n", sep = "")

for (i in seq_len(nrow(med_check))) {
  add_num("0 Median-Check",
          paste0("E2 Median ", med_check$cycle_phase[i], " (Zeilenebene, n=",
                 med_check$n_zeilen[i], ")"), med_check$e2_zeilenebene[i], "pmol/L")
  add_num("0 Median-Check",
          paste0("E2 Median ", med_check$cycle_phase[i], " (Personenebene, n=",
                 med_check$n_personen[i], ")"), med_check$e2_personenebene[i], "pmol/L")
}

# =============================================================================
# BLOCK 1  Datenstand und Vollstaendigkeit
# =============================================================================
cat("\n--- 1) Vollstaendigkeit -----------------------------\n")

vollst <- function(d, task) {
  d %>% group_by(record_id, cycle_phase) %>%
    summarise(n_legs = n_distinct(standing_leg), .groups = "drop") %>%
    mutate(task = task)
}
vd <- bind_rows(vollst(df_slb_mean, "SLB"), vollst(df_msebt_mean, "mSEBT")) %>%
  mutate(window = win_factor(cycle_phase),
         legs = factor(n_legs, levels = c(0, 1, 2),
                       labels = c("none", "one leg", "both legs")))

# Personen nach Anzahl vollstaendiger Fenster sortieren, damit Luecken oben
# zusammenlaufen und nicht ueber die ganze Abbildung verstreut sind
reihen <- vd %>% filter(task == "SLB") %>% group_by(record_id) %>%
  summarise(s = sum(n_legs), .groups = "drop") %>% arrange(s, record_id)
vd$record_id <- factor(vd$record_id, levels = reihen$record_id)

p1 <- ggplot(vd, aes(window, record_id, fill = legs)) +
  geom_tile(colour = "white", linewidth = 0.35) +
  facet_wrap(~ task) +
  scale_fill_manual(values = c("none" = "#D9534F", "one leg" = "#E8B04B",
                               "both legs" = "#C3CDD6"), drop = FALSE) +
  labs(title = "Design completeness",
       subtitle = "One row per participant. Amber = one leg only, white gap = cell absent.",
       x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.y = element_text(size = 5),
        legend.position = "bottom",
        panel.grid = element_blank())
save_fig(p1, "30_completeness.png", 9.5, 8.0)

zellen <- vd %>% filter(task == "SLB") %>% count(legs)
for (i in seq_len(nrow(zellen)))
  add_num("1 Vollstaendigkeit", paste0("SLB Zellen mit ", zellen$legs[i]),
          zellen$n[i], "Zellen")

# --- fehlende Werte je Variable und Fenster ---------------------------------
fehl <- df_slb_mean %>%
  group_by(cycle_phase) %>%
  summarise(`E2` = mean(!is.finite(conc_estr)),
            `P4` = mean(!is.finite(conc_prog)),
            `COP path` = mean(!is.finite(cop_path_length)),
            `BMI` = mean(!is.finite(bmi_lab)),
            `Training volume` = mean(!is.finite(sports_min)), .groups = "drop") %>%
  pivot_longer(-cycle_phase, names_to = "variable", values_to = "anteil") %>%
  mutate(window = win_factor(cycle_phase))

p2 <- ggplot(fehl, aes(window, variable, fill = anteil)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(anteil > 0, sprintf("%.0f%%", 100 * anteil), "")),
            size = 3.1, colour = "white") +
  scale_fill_gradient(low = "#C3CDD6", high = "#B8443F",
                      labels = scales::percent, limits = c(0, NA)) +
  labs(title = "Missing values by variable and cycle window",
       subtitle = "Share of rows without a valid value",
       x = NULL, y = NULL, fill = "Missing") +
  theme(panel.grid = element_blank())
save_fig(p2, "30_missing.png", 8.5, 3.6)

# Trainingsumfang: fehlt er wirklich, oder ist es ein Merge-Problem?
# WICHTIG: erst je Person aggregieren. distinct() ueber bmi_lab wuerde NICHT
# auf eine Zeile pro Person kollabieren, weil bmi_lab in 10_load_data.R je
# Session gemittelt wird und darum zwischen Sessions minimal schwanken kann.
personen <- df_slb_mean %>%
  group_by(record_id) %>%
  summarise(bmi   = mean(bmi_lab,       na.rm = TRUE),
            sport = mean(sports_min,    na.rm = TRUE),
            hip   = mean(hip_abd_stand, na.rm = TRUE),
            stop_go = dplyr::first(sport_stop_go), .groups = "drop")

sport_chk <- personen %>%
  summarise(n_personen  = n(),
            bmi_fehlt   = sum(!is.finite(bmi)),
            sport_fehlt = sum(!is.finite(sport)))
cat("\n  Personenebene: n =", sport_chk$n_personen,
    "| BMI fehlt bei", sport_chk$bmi_fehlt,
    "| Trainingsumfang fehlt bei", sport_chk$sport_fehlt, "\n")
add_num("1 Vollstaendigkeit", "Personen gesamt", sport_chk$n_personen, "Personen")
add_num("1 Vollstaendigkeit", "BMI fehlt", sport_chk$bmi_fehlt, "Personen")
add_num("1 Vollstaendigkeit", "Trainingsumfang fehlt", sport_chk$sport_fehlt, "Personen")

# =============================================================================
# BLOCK 2  Die Exposition - die Existenzbedingung der Studie
# =============================================================================
cat("\n--- 2) Exposition ----------------------------------\n")

horm <- df_slb_mean %>%
  group_by(record_id, cycle_phase) %>%
  summarise(E2 = mean(conc_estr, na.rm = TRUE),
            P4 = mean(conc_prog, na.rm = TRUE), .groups = "drop") %>%
  mutate(window = win_factor(cycle_phase)) %>%
  pivot_longer(c(E2, P4), names_to = "hormon", values_to = "conc") %>%
  filter(is.finite(conc)) %>%
  mutate(hormon = factor(hormon, levels = c("E2", "P4"),
                         labels = c("Estradiol (pmol/L)",
                                    "Progesterone (nmol/L)")))

HC <- setNames(c(E2C, P4C), levels(horm$hormon))

# --- Verteilung je Fenster ---------------------------------------------------
p3 <- ggplot(horm, aes(window, conc, colour = hormon, fill = hormon)) +
  geom_boxplot(alpha = 0.16, outlier.shape = NA, width = 0.6, linewidth = 0.5) +
  geom_jitter(width = 0.16, height = 0, size = 0.9, alpha = 0.45) +
  facet_wrap(~ hormon, scales = "free_y") +
  scale_y_log10() +
  scale_colour_manual(values = HC, guide = "none") +
  scale_fill_manual(values = HC, guide = "none") +
  labs(title = "Hormone concentrations by cycle window",
       subtitle = "One point per participant and window. Log scale.",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_fig(p3, "30_hormones_by_window.png", 9.5, 4.4)

# --- individuelle Verlaeufe: das Bild, das within-Variation zeigt -----------
p4 <- ggplot(horm, aes(window, conc, group = record_id, colour = hormon)) +
  geom_line(alpha = 0.30, linewidth = 0.4) +
  geom_point(size = 0.8, alpha = 0.55) +
  stat_summary(aes(group = 1), fun = median, geom = "line",
               linewidth = 1.3, colour = INKC) +
  facet_wrap(~ hormon, scales = "free_y") +
  scale_y_log10() +
  scale_colour_manual(values = HC, guide = "none") +
  labs(title = "Individual hormone trajectories across the cycle",
       subtitle = "One line per participant, dark line = median. This is the variation the within-person analysis uses.",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_fig(p4, "30_hormone_trajectories.png", 9.5, 4.6)

# --- wo liegt die Expositionsvarianz? ---------------------------------------
zerleg <- df_slb_mean %>%
  mutate(lE2 = ifelse(is.finite(conc_estr) & conc_estr > 0, log(conc_estr), NA_real_),
         lP4 = ifelse(is.finite(conc_prog) & conc_prog > 0, log(conc_prog), NA_real_)) %>%
  group_by(record_id) %>%
  mutate(across(c(lE2, lP4), list(b = ~ mean(.x, na.rm = TRUE)), .names = "{.col}_b")) %>%
  ungroup() %>%
  mutate(lE2_w = lE2 - lE2_b, lP4_w = lP4 - lP4_b)

# Between-Varianz ueber die Personenmittel, jede Frau GENAU EINMAL.
# unique() auf den Mittelwerten waere fehleranfaellig - zwei Frauen koennten
# denselben Mittelwert haben und wuerden dann zu einem verschmelzen.
pm <- zerleg %>% group_by(record_id) %>%
  summarise(b_e2 = dplyr::first(lE2_b), b_p4 = dplyr::first(lP4_b),
            .groups = "drop")

anteil_within <- function(w, b) {
  vw <- stats::var(w, na.rm = TRUE)
  vb <- stats::var(b, na.rm = TRUE)
  if (!is.finite(vw) || !is.finite(vb) || (vw + vb) == 0) return(NA_real_)
  vw / (vw + vb)
}
aw_e2 <- anteil_within(zerleg$lE2_w, pm$b_e2)
aw_p4 <- anteil_within(zerleg$lP4_w, pm$b_p4)

var_df <- data.frame(
  hormon = rep(c("Estradiol", "Progesterone"), each = 2),
  ebene  = factor(rep(c("Within persons", "Between persons"), 2),
                  levels = c("Within persons", "Between persons")),
  anteil = c(aw_e2, 1 - aw_e2, aw_p4, 1 - aw_p4))

p5 <- ggplot(var_df, aes(anteil, hormon, fill = ebene)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = ifelse(anteil > 0.06, sprintf("%.0f%%", 100 * anteil), "")),
            position = position_stack(vjust = 0.5), colour = "white", size = 3.6) +
  scale_fill_manual(values = c("Within persons" = INKC,
                               "Between persons" = "#C3CDD6")) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Where the hormone variation sits",
       subtitle = "The within-person share is the precondition of the design.",
       x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())
save_fig(p5, "30_exposure_variance.png", 8.0, 3.0)

add_num("2 Exposition", "Within-Anteil E2", aw_e2, "Anteil")
add_num("2 Exposition", "Within-Anteil P4", aw_p4, "Anteil")

# --- Lutealverifikation ------------------------------------------------------
lut <- horm %>% filter(window == "Luteal", grepl("Progesterone", hormon)) %>%
  arrange(conc) %>% mutate(rang = row_number(),
                           ueber = conc > 16)
p6 <- ggplot(lut, aes(rang, conc, colour = ueber)) +
  geom_hline(yintercept = 16, linetype = "dashed", colour = INKC) +
  annotate("text", x = 2, y = 16, label = "16 nmol/L", vjust = -0.6,
           hjust = 0, size = 3.2, colour = INKC) +
  geom_point(size = 1.9) +
  scale_colour_manual(values = c("TRUE" = P4C, "FALSE" = "#B8443F"),
                      labels = c("TRUE" = "above", "FALSE" = "below"),
                      name = NULL) +
  labs(title = "Luteal verification",
       subtitle = "Progesterone in the luteal window, one point per participant, sorted.",
       x = "Participants, sorted", y = "Progesterone (nmol/L)") +
  theme(legend.position = "bottom")
save_fig(p6, "30_luteal_check.png", 8.0, 4.0)
add_num("2 Exposition", "Anteil luteal ueber 16 nmol/L", mean(lut$ueber), "Anteil")

# =============================================================================
# BLOCK 3  Die Stichprobe - Personenebene
# =============================================================================
cat("\n--- 3) Stichprobe ----------------------------------\n")

# personen stammt aus Block 1: EINE Zeile pro Frau
pers <- personen %>%
  rename(`BMI (kg/m2)` = bmi, `Training volume (min/week)` = sport,
         `Hip abduction, stance leg (N)` = hip) %>%
  pivot_longer(c(`BMI (kg/m2)`, `Training volume (min/week)`,
                 `Hip abduction, stance leg (N)`),
               names_to = "variable", values_to = "wert") %>%
  filter(is.finite(wert))

p7 <- ggplot(pers, aes(wert)) +
  geom_histogram(bins = 18, fill = "#C3CDD6", colour = "white", linewidth = 0.3) +
  geom_vline(data = pers %>% group_by(variable) %>%
               summarise(m = mean(wert), .groups = "drop"),
             aes(xintercept = m), colour = INKC, linetype = "dashed") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "The sample, at person level",
       subtitle = "One observation per participant. Dashed line = mean.",
       x = NULL, y = "Participants")
save_fig(p7, "30_sample.png", 10.5, 3.4)

for (v in unique(pers$variable)) {
  x <- pers$wert[pers$variable == v]
  add_num("3 Stichprobe", paste0(v, " Mittelwert"), mean(x))
  add_num("3 Stichprobe", paste0(v, " SD"), stats::sd(x))
  add_num("3 Stichprobe", paste0(v, " n"), length(x), "Personen")
}
add_num("3 Stichprobe", "Stop-and-go-Sportart",
        sum(personen$stop_go == 1, na.rm = TRUE), "Personen")

# =============================================================================
# BLOCK 4  Die Outcomes
# =============================================================================
cat("\n--- 4) Outcomes ------------------------------------\n")

# Kennspalten mitschleppen, damit die Ausreisserliste die einzelne MESSUNG
# eindeutig benennt (Bein, Zyklus, Phasencode) und nicht nur die Person.
ID_COLS <- c("record_id", "cycle_phase", "standing_leg", "cycle_nr", "phase")

oc_long <- bind_rows(
  df_slb_mean %>%
    select(dplyr::any_of(ID_COLS), `COP path (mm)` = cop_path_length,
           `COP ellipse (mm2)` = cop_ellipse_area) %>%
    pivot_longer(-dplyr::any_of(ID_COLS), names_to = "outcome", values_to = "wert"),
  df_msebt_mean %>%
    select(dplyr::any_of(ID_COLS), `Anterior reach (%LL)` = reach_ant,
           `Posterolateral reach (%LL)` = reach_pl,
           `Posteromedial reach (%LL)` = reach_pm) %>%
    pivot_longer(-dplyr::any_of(ID_COLS), names_to = "outcome", values_to = "wert")
) %>% filter(is.finite(wert)) %>%
  mutate(window = win_factor(cycle_phase), outcome = oc_factor(outcome))

# --- Verteilungen ------------------------------------------------------------
p8 <- ggplot(oc_long, aes(wert)) +
  geom_histogram(bins = 26, fill = "#C3CDD6", colour = "white", linewidth = 0.25) +
  facet_raster("free") +
  labs(title = "Outcome distributions",
       subtitle = "All measurements. The COP measures are right-skewed, which is why they are also modelled on the log scale.",
       x = NULL, y = "Measurements")
save_fig(zwei_bloecke(p8), "30_outcome_distributions.png", 10.5, 5.4)

# --- die Zwei-Ebenen-Abbildung ----------------------------------------------
# Personenmittel als Punkt, Spannweite innerhalb der Person als Linie. Zeigt
# auf einen Blick, wie viel Streuung zwischen und wie viel innerhalb liegt.
zwei <- oc_long %>% group_by(outcome, record_id) %>%
  summarise(m = mean(wert), lo = min(wert), hi = max(wert),
            n = n(), .groups = "drop") %>%
  filter(n >= 2) %>%
  group_by(outcome) %>% arrange(m) %>% mutate(rang = row_number()) %>% ungroup()

p9 <- ggplot(zwei, aes(rang, m)) +
  geom_linerange(aes(ymin = lo, ymax = hi), colour = "#C3CDD6", linewidth = 0.55) +
  geom_point(size = 0.85, colour = INKC) +
  facet_raster("free") +
  labs(title = "Two levels in one picture",
       subtitle = "Point = participant mean, sorted. Line = that participant's range across windows.",
       x = "Participants, sorted by their own mean", y = NULL)
save_fig(zwei_bloecke(p9), "30_outcome_two_levels.png", 10.5, 5.4)

# --- Interkorrelation der Outcomes ------------------------------------------
korr_dat <- df_msebt_mean %>%
  select(reach_ant, reach_pl, reach_pm) %>%
  rename(`Anterior` = reach_ant, `Posterolateral` = reach_pl,
         `Posteromedial` = reach_pm)
km <- suppressWarnings(stats::cor(korr_dat, use = "pairwise.complete.obs"))
kl <- as.data.frame(as.table(km)); names(kl) <- c("x", "y", "r")

p10 <- ggplot(kl, aes(x, y, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3.6,
            colour = ifelse(abs(kl$r) > 0.6, "white", INKC)) +
  scale_fill_gradient2(low = E2C, mid = "white", high = P4C,
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "How strongly the reach directions agree",
       subtitle = "Pearson correlations. This is the basis for choosing one primary dynamic outcome.",
       x = NULL, y = NULL, fill = "r") +
  theme(panel.grid = element_blank())
save_fig(p10, "30_outcome_correlations.png", 6.4, 4.6)

for (i in seq_len(nrow(kl)))
  if (as.character(kl$x[i]) < as.character(kl$y[i]))
    add_num("4 Outcomes", paste0("r ", kl$x[i], " vs ", kl$y[i]), kl$r[i])

for (o in unique(oc_long$outcome)) {
  x <- oc_long$wert[oc_long$outcome == o]
  add_num("4 Outcomes", paste0(o, " Mittelwert"), mean(x))
  add_num("4 Outcomes", paste0(o, " SD"), stats::sd(x))
}

# =============================================================================
# BLOCK 5  Datenqualitaet
# =============================================================================
cat("\n--- 5) Datenqualitaet ------------------------------\n")

tukey <- oc_long %>% group_by(outcome) %>%
  mutate(q1 = stats::quantile(wert, .25, na.rm = TRUE),
         q3 = stats::quantile(wert, .75, na.rm = TRUE),
         iqr = q3 - q1,
         aus = wert < q1 - 1.5 * iqr | wert > q3 + 1.5 * iqr) %>%
  ungroup()

# ZWEI VARIANTEN DERSELBEN ABBILDUNG
#   30_outliers.png        mit Beschriftung - zeigt, WELCHE Frau hinter einem
#                           Punkt steckt. Unverzichtbar, solange man den Werten
#                           noch nachgeht.
#   30_outliers_plain.png  ohne Beschriftung - fuer Folien und Manuskript.
#                           Bei der COP-Weglaenge liegen so viele Namen
#                           uebereinander, dass sie die Verteilung verdecken,
#                           und dort geht es nur um die Form der Verteilung.
#
# Der Jitter bekommt einen festen seed. Ohne ihn wuerfelt ggplot die
# horizontale Auslenkung bei jedem Zeichnen neu, und die beiden Varianten
# haetten die Punkte an leicht verschiedenen Stellen - nebeneinander gelegt
# sieht das aus wie zwei verschiedene Datensaetze.
#
# KEINE ROTE MARKIERUNG MEHR. Rot liest sich als "fehlerhaft", und genau das
# sind diese Werte nicht: die Sichtung der Rohaufnahmen hat gezeigt, dass es
# echte hohe Werte sind, keine Artefakte (was ausgeschlossen gehoerte, steht
# in FeHBI_trial_exclusions.csv und ist hier laengst raus). Alle Punkte werden
# darum gleich gezeichnet; die Tukey-Grenze bleibt als Zahl in
# 30_outliers.csv erhalten, wo sie hingehoert.
#
# Die Punkte liegen trotzdem in ZWEI Layern. Nur so bekommt die beschriftete
# Variante ihre Namen an die richtige Stelle: ggplot erzeugt den Jitter pro
# Layer neu ueber die Zeilen dieses Layers. Punkt-Layer und Label-Layer teilen
# sich denselben Datenausschnitt (aus == TRUE) und denselben seed und landen
# deshalb exakt uebereinander.
PKT <- list(size = 0.7, alpha = 0.35, colour = "grey50")
JIT <- function() position_jitter(width = 0.15, height = 0, seed = 1)

OUT_SUB <- paste("Box: median, quartiles, whiskers to 1.5 x IQR. Every point is",
                 "one trial of the cleaned data set.\nValues outside the whiskers",
                 "were checked against the raw recordings and kept - they are",
                 "genuine high values, not artefacts.")

p11_basis <- ggplot(tukey, aes(window, wert)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, colour = INKC,
               fill = "#EEF2F5", linewidth = 0.42) +
  geom_point(data = ~ dplyr::filter(.x, !aus), position = JIT(),
             size = PKT$size, alpha = PKT$alpha, colour = PKT$colour) +
  geom_point(data = ~ dplyr::filter(.x, aus), position = JIT(),
             size = PKT$size, alpha = PKT$alpha, colour = PKT$colour) +
  facet_raster("free") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p11 <- p11_basis +
  label_layer(data = ~ dplyr::filter(.x, aus), position = JIT(),
              aes(label = record_id), size = 2.4, colour = INKC) +
  labs(title = "Outcome distributions, values beyond 1.5 x IQR labelled",
       subtitle = OUT_SUB)
save_fig(zwei_bloecke(p11), "30_outliers.png", 10.5, 5.8)

p11b <- p11_basis +
  labs(title = "Outcome distributions", subtitle = OUT_SUB)
save_fig(zwei_bloecke(p11b), "30_outliers_plain.png", 10.5, 5.8)

aus_tab <- tukey %>% filter(aus) %>%
  mutate(richtung = ifelse(wert > q3 + 1.5 * iqr, "hoch", "tief")) %>%
  select(record_id, dplyr::any_of(c("standing_leg", "cycle_nr", "phase")),
         window, outcome, wert, richtung, q1, q3) %>%
  arrange(record_id, outcome, window)
cat("\n  Ausreisser nach Tukey:", nrow(aus_tab), "\n")
print(as.data.frame(head(aus_tab, 25)), row.names = FALSE)
write.csv(aus_tab, file.path(DESK_DIR, "30_outliers.csv"), row.names = FALSE)
add_num("5 Datenqualitaet", "Ausreisser nach Tukey", nrow(aus_tab), "Werte")

# Nullwerte und nicht logarithmierbare Hormonwerte
for (v in c("conc_estr", "conc_prog")) {
  x <- df_slb_mean[[v]]
  add_num("5 Datenqualitaet", paste0(v, " Werte <= 0"),
          sum(is.finite(x) & x <= 0), "Werte")
}

# =============================================================================
# BLOCK 6  Messfehler aus den zwei Versuchen
# -----------------------------------------------------------------------------
# Schliesst die Luecke, die in 20_prepare.R dokumentiert ist: fuer die
# COP-Masse gibt es keine verwendbare MDC in der Literatur, deshalb steht
# dort bewusst NA. Aus zwei Versuchen pro Zelle laesst sie sich schaetzen.
#
#   SEM = SD der Differenzen / sqrt(2)
#   SDD = 2.77 * SEM   (kleinste erkennbare Veraenderung, 95 %)
#
# SKALENWAHL, und das ist kein Detail: die Methode setzt voraus, dass die
# Streuung der Differenzen ueber den Messbereich konstant ist. Bei den
# rechtsschiefen COP-Massen ist sie das nicht - grosse Werte streuen absolut
# staerker. Deshalb werden COP path und ellipse auf der LOG-Skala gerechnet;
# dort ist die Annahme erfuellt, und das Ergebnis ist eine prozentuale SDD.
# Die Reach-Masse sind symmetrisch und bleiben auf der Rohskala in %BL.
# Genau diese Skalenwahl trifft auch das Modell.
#
# Der Bias (Versuch 2 minus Versuch 1) beantwortet nebenbei die Frage nach
# Aufwaermen oder Ermuedung. Das ist der einzige Reihenfolgeeffekt, der sich
# mit Zyklus 2 allein pruefen laesst - ueber die Fenster hinweg sind
# Messreihenfolge und Zyklusphase perfekt kollinear.
# =============================================================================
cat("\n--- 6) Messfehler aus den zwei Versuchen -----------\n")

KEY_T <- c("record_id", "phase", "standing_leg", "rep")
haben_trials <- exists("df_slb") && exists("df_msebt") &&
  all(KEY_T %in% names(df_slb)) && all(KEY_T %in% names(df_msebt))

if (!haben_trials) {
  cat("  [i] df_slb / df_msebt mit Trialspalte nicht vorhanden - Block 6 und 7\n",
      "      werden uebersprungen.\n")
} else {
  
  # mSEBT hat DREI identische Zeilen je Trial (eine pro Reichrichtung) -
  # ohne distinct() zaehlt jeder Versuch dreifach.
  trials <- bind_rows(
    df_slb %>%
      select(dplyr::all_of(KEY_T), `COP path (mm)` = cop_path_length,
             `COP ellipse (mm2)` = cop_ellipse_area) %>% distinct() %>%
      pivot_longer(-dplyr::all_of(KEY_T), names_to = "outcome", values_to = "wert"),
    df_msebt %>%
      select(dplyr::all_of(KEY_T), `Anterior reach (%LL)` = reach_anterior_pct,
             `Posterolateral reach (%LL)` = reach_posterolateral_pct,
             `Posteromedial reach (%LL)` = reach_posteromedial_pct) %>% distinct() %>%
      pivot_longer(-dplyr::all_of(KEY_T), names_to = "outcome", values_to = "wert")
  ) %>%
    filter(is.finite(wert), wert > 0)
  
  # OUT_ORDER, OUT_RASTER und oc_factor stehen im Kopf des Skripts - eine
  # zweite Definition hier waere die Stelle, an der die Reihenfolge irgendwann
  # auseinanderlaeuft.
  trials$outcome <- oc_factor(trials$outcome)
  LOG_OC <- OUT_COP
  
  cat("  Trialebene:", nrow(trials), "Messwerte,",
      dplyr::n_distinct(trials$record_id), "Personen\n")
  
  # Nur Zellen mit GENAU zwei gueltigen Versuchen - eine Zelle mit einem
  # Versuch traegt nichts zum Messfehler bei.
  paare <- trials %>%
    mutate(log_skala = outcome %in% LOG_OC,
           y = ifelse(log_skala, log(wert), wert)) %>%
    group_by(record_id, phase, standing_leg, outcome, log_skala) %>%
    filter(n() == 2) %>%
    arrange(rep, .by_group = TRUE) %>%
    summarise(mittel_roh = mean(wert), mittel = mean(y),
              diff = dplyr::last(y) - dplyr::first(y), .groups = "drop")
  
  mess <- paare %>%
    group_by(outcome, log_skala) %>%
    summarise(n_zellen = n(), bias = mean(diff), sd_diff = stats::sd(diff),
              sem = stats::sd(diff) / sqrt(2),
              sdd = 2.77 * stats::sd(diff) / sqrt(2),
              mittel_roh = mean(mittel_roh), .groups = "drop") %>%
    mutate(
      # Log-Skala: exp(SDD) - 1 ist die prozentuale Veraenderung.
      # Rohskala: SDD durch den Mittelwert.
      sdd_pct  = ifelse(log_skala, 100 * (exp(sdd) - 1), 100 * sdd / mittel_roh),
      bias_pct = ifelse(log_skala, 100 * (exp(bias) - 1), 100 * bias / mittel_roh),
      einheit  = ifelse(log_skala, "log-Einheiten", "Rohskala"))
  
  print(as.data.frame(mess %>%
                        mutate(across(where(is.numeric), ~round(.x, 3))) %>%
                        select(outcome, einheit, n_zellen, bias, sem, sdd, sdd_pct)),
        row.names = FALSE)
  write.csv(mess, file.path(DESK_DIR, "30_messfehler.csv"), row.names = FALSE)
  for (i in seq_len(nrow(mess))) {
    add_num("6 Messfehler", paste0(mess$outcome[i], " SEM"), mess$sem[i])
    add_num("6 Messfehler", paste0(mess$outcome[i], " SDD"), mess$sdd[i])
    add_num("6 Messfehler", paste0(mess$outcome[i], " SDD %"), mess$sdd_pct[i], "%")
    add_num("6 Messfehler", paste0(mess$outcome[i], " Bias %"), mess$bias_pct[i], "%")
  }
  
  ba <- paare %>% left_join(mess %>% select(outcome, bias, sd_diff), by = "outcome")
  p12 <- ggplot(ba, aes(mittel, diff)) +
    geom_hline(yintercept = 0, colour = "grey72") +
    geom_point(alpha = 0.32, size = 0.95, colour = INKC) +
    geom_hline(aes(yintercept = bias), colour = E2C, linewidth = 0.6) +
    geom_hline(aes(yintercept = bias + 1.96 * sd_diff), colour = E2C,
               linetype = "dashed", linewidth = 0.45) +
    geom_hline(aes(yintercept = bias - 1.96 * sd_diff), colour = E2C,
               linetype = "dashed", linewidth = 0.45) +
    facet_raster("free") +
    labs(title = "Agreement between the two trials",
         subtitle = "Difference (trial 2 - trial 1) against their mean. COP on the log scale, reach in %LL. Solid = bias, dashed = 95 % limits of agreement.",
         x = "Mean of the two trials", y = "Difference")
  save_fig(zwei_bloecke(p12), "30_trial_agreement.png", 11.0, 6.0)
  
  p13 <- ggplot(mess %>% mutate(outcome = droplevels(outcome)),
                aes(stats::reorder(outcome, sdd_pct), sdd_pct)) +
    geom_col(fill = "#C3CDD6", width = 0.62) +
    geom_text(aes(label = sprintf("%.1f %%", sdd_pct)), hjust = -0.15,
              size = 3.2, colour = INKC) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
    labs(title = "Smallest detectable difference, from this study's own data",
         subtitle = "SDD95 in percent. A change smaller than this cannot be told apart from measurement error.",
         x = NULL, y = "SDD95 (%)")
  save_fig(p13, "30_sdd.png", 8.5, 3.4)
  
  # =============================================================================
  # BLOCK 7  Varianzbudget ueber vier Ebenen
  # -----------------------------------------------------------------------------
  #   Person  - stabile Unterschiede zwischen Frauen
  #   Session - Tagesform, Person x Phase; beide Beine teilen sie sich.
  #             DAS IST DIE EBENE, AUF DER DIE HORMONE VARIIEREN.
  #   Bein    - Seite innerhalb einer Session
  #   Trial   - Residuum, also die Wiederholbarkeit
  #
  # Ist der Session-Anteil klein, ist wenig zu erklaeren - unabhaengig davon,
  # wie gross der Effekt biologisch waere.
  #
  # Vorsicht bei der Auslegung: mit zwei Versuchen je Zelle sind Bein- und
  # Trialebene nur schwach getrennt. Faellt eine Komponente auf null (singulaer),
  # wird sie als 0 ausgewiesen - das heisst "nicht unterscheidbar von null",
  # nicht "existiert nicht". Das Skript meldet solche Faelle.
  # =============================================================================
  if (!requireNamespace("lme4", quietly = TRUE)) {
    cat("\n  [i] lme4 fehlt - Block 7 wird uebersprungen.\n")
  } else {
    cat("\n--- 7) Varianzbudget ueber vier Ebenen -------------\n")
    
    vb <- lapply(levels(trials$outcome), function(oc) {
      d <- trials %>% filter(outcome == oc) %>%
        mutate(y    = if (oc %in% LOG_OC) log(wert) else wert,
               sess = interaction(record_id, phase, drop = TRUE),
               cell = interaction(record_id, phase, standing_leg, drop = TRUE))
      if (nrow(d) < 40) return(NULL)
      m <- try(suppressMessages(suppressWarnings(lme4::lmer(
        y ~ 1 + (1 | record_id) + (1 | sess) + (1 | cell), data = d, REML = TRUE,
        control = lme4::lmerControl(check.conv.singular = "ignore")))),
        silent = TRUE)
      if (inherits(m, "try-error")) return(NULL)
      sing <- lme4::isSingular(m, tol = 1e-5)
      vc <- as.data.frame(lme4::VarCorr(m))
      g  <- function(nm) { v <- vc$vcov[vc$grp == nm]; if (length(v)) v[1] else 0 }
      v  <- c(Person = g("record_id"), Session = g("sess"),
              Leg = g("cell"), Trial = g("Residual"))
      if (sing) {
        null_komp <- names(v)[v <= .Machine$double.eps^0.5 * max(v)]
        cat("  [!] ", oc, ": singulaerer Fit. Auf der Grenze null: ",
            paste(null_komp, collapse = ", "),
            "\n       -> nicht unterscheidbar von null, NICHT nachgewiesen abwesend.\n",
            sep = "")
      }
      data.frame(outcome = oc, ebene = names(v), varianz = as.numeric(v),
                 anteil = as.numeric(v) / sum(v), singulaer = sing,
                 stringsAsFactors = FALSE)
    })
    vb <- bind_rows(vb)
    
    if (nrow(vb)) {
      vb$outcome <- factor(vb$outcome, levels = OUT_ORDER)
      vb$ebene   <- factor(vb$ebene, levels = c("Trial", "Leg", "Session", "Person"))
      print(as.data.frame(vb %>% mutate(anteil = round(anteil, 3)) %>%
                            select(outcome, ebene, anteil) %>%
                            pivot_wider(names_from = ebene, values_from = anteil)),
            row.names = FALSE)
      write.csv(vb, file.path(DESK_DIR, "30_varianzbudget.csv"), row.names = FALSE)
      for (i in seq_len(nrow(vb)))
        add_num("7 Varianz", paste0(vb$outcome[i], " - ", vb$ebene[i]),
                vb$anteil[i], "Anteil")
      
      # BESCHRIFTUNG DER EBENEN
      # Person/Session/Leg/Trial benennen, WO eine Varianzkomponente sitzt - nicht,
      # WEM sie gehoert. Das wird regelmaessig missverstanden: "Person" klingt so,
      # als waere das die interessante Ebene, dabei ist es genau die, die sich
      # innerhalb einer Frau NICHT bewegt. Deshalb stehen in der Legende jetzt
      # ausgeschriebene Kontraste statt Ein-Wort-Etiketten.
      #
      # Die kurzen Namen bleiben in 30_varianzbudget.csv erhalten - dort sind sie
      # als Spaltenwerte praktischer.
      EBENE_LAB <- c(Person  = "Between women (stable)",
                     Session = "Within a woman, between sessions",
                     Leg     = "Between legs, same session",
                     Trial   = "Between trials, same leg")
      EBENE_COL <- setNames(c(INKC, P4C, "#9FBBD4", "#C3CDD6"),
                            EBENE_LAB[c("Person", "Session", "Leg", "Trial")])
      
      # SINGULAERE FITS KENNZEICHNEN
      # Faellt eine Varianzkomponente exakt auf null, ist das eine Randloesung des
      # Schaetzers, keine Messung. Mit nur zwei Versuchen je Bein und zwei Beinen
      # je Messgelegenheit konkurrieren die Ebenen Session und Leg um dieselbe
      # Variation; gewinnt Leg, kann Session auf null kippen. Ohne Markierung
      # liest man aus der Abbildung "es gibt keine Session-Varianz" - und das
      # steht dort nicht.
      sing_oc <- unique(as.character(vb$outcome[vb$singulaer]))
      if (length(sing_oc))
        cat("  Randloesung (Komponente auf null) bei: ",
            paste(sing_oc, collapse = ", "), "\n", sep = "")
      
      vb_plot <- vb %>%
        mutate(ebene = factor(EBENE_LAB[as.character(ebene)],
                              levels = unname(EBENE_LAB[c("Trial", "Leg",
                                                          "Session", "Person")])),
               outcome = factor(ifelse(singulaer,
                                       paste0(as.character(outcome), "  *"),
                                       as.character(outcome)),
                                levels = ifelse(OUT_ORDER %in% sing_oc,
                                                paste0(OUT_ORDER, "  *"),
                                                OUT_ORDER)))
      
      p14 <- ggplot(vb_plot, aes(outcome, anteil, fill = ebene)) +
        geom_col(width = 0.66) +
        geom_text(aes(label = ifelse(anteil >= 0.07,
                                     sprintf("%.0f%%", 100 * anteil), "")),
                  position = position_stack(vjust = 0.5), size = 3.1,
                  colour = "white") +
        # rechts etwas Luft, sonst wird die Beschriftung "100%" am Rand abgeschnitten
        scale_y_continuous(labels = scales::percent,
                           expand = ggplot2::expansion(mult = c(0, 0.035))) +
        scale_fill_manual(values = EBENE_COL) +
        guides(fill = guide_legend(nrow = 2, reverse = TRUE)) +
        coord_flip() +
        labs(title = "Where the outcome variance sits",
             subtitle = paste0(
               "Only the session component can respond to a hormone \u2014 both legs ",
               "and both trials of one\noccasion share one hormone value. The person ",
               "component is what does not move."),
             x = NULL, y = NULL, fill = NULL,
             caption = if (length(sing_oc))
               paste0("*  boundary estimate: a component fell to exactly zero. With ",
                      "two trials per leg and two legs per session, the session and\n",
                      "leg levels compete for the same variation \u2014 a zero here ",
                      "means not distinguishable from zero, not absent.")
             else NULL) +
        theme(legend.position = "bottom",
              plot.caption = element_text(hjust = 0, size = 8, colour = "grey40"))
      save_fig(p14, "30_variance_budget.png", 10.0, 4.9)
    }
  }
  
  # =============================================================================
  # BLOCK 8  Bein-Symmetrie
  # -----------------------------------------------------------------------------
  # standing_leg steht als Fixed Effect im Modell, ohne dass je gezeigt wurde,
  # wie gross der Unterschied ueberhaupt ist. Punkte auf der Winkelhalbierenden
  # hiessen: der Term ist Ballast. Systematische Abweichung belegt ihn.
  # =============================================================================
  cat("\n--- 8) Bein-Symmetrie -------------------------------\n")
  
  beine <- trials %>%
    filter(standing_leg %in% c("left", "right")) %>%
    group_by(record_id, outcome, standing_leg) %>%
    summarise(m = mean(wert), .groups = "drop") %>%
    pivot_wider(names_from = standing_leg, values_from = m) %>%
    filter(is.finite(left), is.finite(right))
  
  sym <- beine %>% group_by(outcome) %>%
    summarise(n = n(),
              diff_mittel = mean(right - left),
              diff_pct = 100 * mean(right - left) / mean(c(left, right)),
              r = suppressWarnings(stats::cor(left, right, use = "complete.obs")),
              .groups = "drop")
  print(as.data.frame(sym %>% mutate(across(where(is.numeric), ~round(.x, 3)))),
        row.names = FALSE)
  write.csv(sym, file.path(DESK_DIR, "30_bein_symmetrie.csv"), row.names = FALSE)
  for (i in seq_len(nrow(sym))) {
    add_num("8 Beine", paste0(sym$outcome[i], " Differenz rechts-links %"),
            sym$diff_pct[i], "%")
    add_num("8 Beine", paste0(sym$outcome[i], " r links-rechts"), sym$r[i])
  }
  
  p15 <- ggplot(beine, aes(left, right)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey65", linetype = "dashed") +
    geom_point(alpha = 0.55, size = 1.5, colour = INKC) +
    facet_raster("free") +
    labs(title = "Left versus right stance leg",
         subtitle = "One point per participant, mean over her sessions. Dashed line = perfect symmetry.",
         x = "Left leg", y = "Right leg")
  save_fig(zwei_bloecke(p15), "30_leg_symmetry.png", 11.0, 6.0)
  
}  # Ende haben_trials

# =============================================================================
# BLOCK 9  Wer liefert die Within-Variation?
# -----------------------------------------------------------------------------
# Die Within-SD auf Log-Skala je Frau - genau die Groesse, die als estr_w /
# prog_w ins Modell geht. Sortiert. Kommt die Variation von allen Frauen oder
# von wenigen? Bewusst NICHT max/min: ein Quotient aus zwei Extremwerten ist
# bei vier Messungen von einem einzigen Wert abhaengig und daher instabil.
# =============================================================================
cat("\n--- 9) Within-Streuung je Person --------------------\n")

horm_p <- bind_rows(
  df_slb_mean   %>% select(record_id, cycle_phase, conc_estr, conc_prog),
  df_msebt_mean %>% select(record_id, cycle_phase, conc_estr, conc_prog)
) %>%
  group_by(record_id, cycle_phase) %>%
  summarise(E2 = mean(conc_estr, na.rm = TRUE),
            P4 = mean(conc_prog, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(c(E2, P4), names_to = "hormon", values_to = "conc") %>%
  filter(is.finite(conc), conc > 0)

streu <- horm_p %>%
  group_by(hormon, record_id) %>%
  filter(n() >= 3) %>%
  summarise(sd_log = stats::sd(log(conc)), n_fenster = n(), .groups = "drop") %>%
  group_by(hormon) %>% arrange(sd_log) %>%
  mutate(rang = row_number()) %>% ungroup()

zus <- streu %>% group_by(hormon) %>%
  summarise(n_personen = n(), median_sd_log = median(sd_log),
            q25 = stats::quantile(sd_log, .25),
            q75 = stats::quantile(sd_log, .75), .groups = "drop")
print(as.data.frame(zus %>% mutate(across(where(is.numeric), ~round(.x, 3)))),
      row.names = FALSE)
for (i in seq_len(nrow(zus)))
  add_num("9 Within-Streuung", paste0(zus$hormon[i], " Median SD(log)"),
          zus$median_sd_log[i])

p16 <- ggplot(streu, aes(rang, sd_log, fill = hormon)) +
  geom_col(width = 0.85) +
  geom_hline(data = zus, aes(yintercept = median_sd_log),
             linetype = "dashed", colour = INKC, linewidth = 0.5) +
  facet_wrap(~ hormon, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = c(E2 = E2C, P4 = P4C), guide = "none") +
  labs(title = "How much does each woman's hormone level actually move?",
       subtitle = "Within-person SD on the log scale - the quantity the within-person analysis uses. Sorted, dashed line = median.",
       x = "Participants, sorted", y = "SD of log concentration")
save_fig(p16, "30_within_spread.png", 10.0, 4.0)

# =============================================================================
# BLOCK 10  Kovariablen untereinander
# -----------------------------------------------------------------------------
# Nur Kovariablen, KEIN Outcome. Zeigt Kollinearitaet zwischen den Groessen,
# mit denen adjustiert wird. Spearman, weil Trainingsumfang und Kraft nicht
# zwingend linear zusammenhaengen.
# =============================================================================
cat("\n--- 10) Kovariablen untereinander -------------------\n")

KOV <- c(bmi_lab = "BMI (kg/m2)", sports_min = "Training volume (min/wk)",
         hip_abd_stand = "Hip abduction (N)", leglength_mm = "Leg length (mm)")
kov <- bind_rows(
  df_slb_mean   %>% select(dplyr::any_of(c("record_id", names(KOV)))),
  df_msebt_mean %>% select(dplyr::any_of(c("record_id", names(KOV))))
) %>%
  group_by(record_id) %>%
  summarise(across(dplyr::any_of(names(KOV)),
                   ~ suppressWarnings(mean(.x[is.finite(.x)]))), .groups = "drop")
kv <- intersect(names(KOV), names(kov))

if (length(kv) >= 2) {
  cat("  n je Kovariable:\n")
  print(setNames(sapply(kv, function(v) sum(is.finite(kov[[v]]))), unname(KOV[kv])))
  km <- suppressWarnings(stats::cor(kov[kv], use = "pairwise.complete.obs",
                                    method = "spearman"))
  colnames(km) <- rownames(km) <- unname(KOV[kv])
  kl <- as.data.frame(as.table(km)); names(kl) <- c("x", "y", "r")
  p17 <- ggplot(kl, aes(x, y, fill = r)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.2f", r)), size = 3.3,
              colour = ifelse(abs(kl$r) > 0.6, "white", INKC)) +
    scale_fill_gradient2(low = E2C, mid = "white", high = P4C, midpoint = 0,
                         limits = c(-1, 1)) +
    labs(title = "How the covariates relate to each other",
         subtitle = "Spearman correlations at person level. No outcome is involved.",
         x = NULL, y = NULL, fill = "r") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 20, hjust = 1))
  save_fig(p17, "30_covariate_correlations.png", 7.2, 5.2)
  write.csv(kl, file.path(DESK_DIR, "30_kovariablen_korrelation.csv"),
            row.names = FALSE)
}

# =============================================================================
# SAMMEL-PDF
# -----------------------------------------------------------------------------
# Alle Abbildungen in einer Datei, in der Reihenfolge, in der sie entstanden
# sind. Die PNGs bleiben daneben bestehen - das PDF ist zum Durchblaettern und
# Ausdrucken, die PNGs zum Einbauen in Folien und Text.
#
# Jede Abbildung behaelt ihr eigenes Seitenverhaeltnis: sie wird in ein
# Viewport der Originalgroesse gezeichnet und mittig auf der Seite platziert.
# Ohne das wuerde eine breite, flache Abbildung (10.5 x 3.4) auf eine
# quadratische Seite gestreckt und saehe verzerrt aus.
# =============================================================================
if (length(PLOTS)) {
  cat("\n--- Sammel-PDF -------------------------------------\n")
  PDF_W <- 11.5; PDF_H <- 7.5           # Querformat, etwas groesser als A4
  pdf_datei <- file.path(DESK_DIR, "30_alle_abbildungen.pdf")
  
  grDevices::pdf(pdf_datei, width = PDF_W, height = PDF_H, onefile = TRUE)
  
  # --- Titelseite ------------------------------------------------------------
  grid::grid.newpage()
  grid::grid.text("FeHBI Paper 1 - Balance and sex hormones",
                  y = 0.74, gp = grid::gpar(fontsize = 20, fontface = "bold"))
  grid::grid.text("Descriptive figures",
                  y = 0.67, gp = grid::gpar(fontsize = 14))
  grid::grid.text(sprintf("%d participants  |  %d SLB rows  |  %d mSEBT rows",
                          dplyr::n_distinct(df_slb_mean$record_id),
                          nrow(df_slb_mean), nrow(df_msebt_mean)),
                  y = 0.57, gp = grid::gpar(fontsize = 11))
  grid::grid.text(paste("Generated", format(Sys.time(), "%d.%m.%Y %H:%M"),
                        "by 30_descriptives.R"),
                  y = 0.52, gp = grid::gpar(fontsize = 9, col = "grey40"))
  grid::grid.text(paste(sprintf("%2d  %s", seq_along(PLOTS),
                                sub("\\.png$", "", names(PLOTS))),
                        collapse = "\n"),
                  y = 0.28, gp = grid::gpar(fontsize = 8, fontfamily = "mono",
                                            col = "grey30"))
  
  # --- eine Seite je Abbildung ----------------------------------------------
  for (i in seq_along(PLOTS)) {
    e <- PLOTS[[i]]
    sk <- min((PDF_W - 0.7) / e$w, (PDF_H - 0.7) / e$h)   # Rand von 0.35 je Seite
    vp <- grid::viewport(width  = grid::unit(e$w * sk, "in"),
                         height = grid::unit(e$h * sk, "in"))
    grid::grid.newpage()
    grid::grid.text(sprintf("%s   -   %d / %d", names(PLOTS)[i], i, length(PLOTS)),
                    x = 0.99, y = 0.015, just = c("right", "bottom"),
                    gp = grid::gpar(fontsize = 7, col = "grey55"))
    if (inherits(e$p, "gtable")) {      # zweizeiliges Raster
      grid::pushViewport(vp); grid::grid.draw(e$p); grid::popViewport()
    } else {
      print(e$p, vp = vp)
    }
  }
  
  invisible(grDevices::dev.off())
  cat("  geschrieben: 30_alle_abbildungen.pdf  (", length(PLOTS) + 1,
      " Seiten )\n", sep = "")
}

# =============================================================================
# AUSGABE der Foliennzahlen
# =============================================================================
num_df <- bind_rows(NUM)
write.csv(num_df, file.path(DESK_DIR, "30_slide_numbers.csv"), row.names = FALSE)
write.csv(med_check, file.path(DESK_DIR, "30_median_check.csv"), row.names = FALSE)

cat("\n=====================================================\n")
cat("FERTIG. Geschrieben nach:", normalizePath(DESK_DIR), "\n")
cat("  18 PNG-Abbildungen mit Praefix 01b_\n")
cat("  30_outliers.png / 30_outliers_plain.png - mit und ohne Beschriftung\n")
cat("  30_slide_numbers.csv  - Kennzahlen fuer die Folien\n")
cat("  30_median_check.csv   - Median-Ebenen-Vergleich\n")
cat("  30_outliers.csv       - Ausreisserliste\n")
cat("  30_messfehler.csv     - SEM und SDD aus den zwei Versuchen\n")
cat("  30_varianzbudget.csv  - Varianz je Ebene\n")
cat("  30_bein_symmetrie.csv - links gegen rechts\n")
cat("  30_kovariablen_korrelation.csv\n")
cat("  30_alle_abbildungen.pdf - alle Abbildungen in einer Datei\n")
cat("\nAlle CSV enthalten NUR Aggregate, keine Zeilendaten.\n")
cat("=====================================================\n")