# =============================================================================
# 00_setup.R  |  FeHBI Paper 1 - Balance  |  Pipeline-Schritt 1 von 10
# Einheitlicher Look fuer ALLE FeHBI Paper-1-Plots
# -----------------------------------------------------------------------------
# Eine Textfarbe, konsistente Schriftgroessen/-dichte, horizontale Achsen-/
# Strip-Beschriftungen. In jedem Plot-Script:  source("00_setup.R")
# und  ... + theme_fehbi()  statt  theme_minimal() + theme(...)
# =============================================================================
suppressMessages(library(ggplot2))

FEHBI_INK        <- "#1F2933"   # EINE Textfarbe fuer alles (Achsen, Titel, Zahlen)
FEHBI_BLUE       <- "#3D7ABF"   # P4 / Balken
FEHBI_ORANGE     <- "#C9663A"   # E2
FEHBI_BASE       <- 12          # Basis-Schriftgroesse ueberall gleich
FEHBI_LABEL_SIZE <- 3.4         # geom_text/Zahlen im Plot (~10 pt)

theme_fehbi <- function(base_size = FEHBI_BASE) {
  theme_minimal(base_size = base_size) +
    theme(
      text              = element_text(colour = FEHBI_INK),
      plot.title        = element_text(colour = FEHBI_INK, size = base_size + 2, face = "bold"),
      plot.subtitle     = element_text(colour = FEHBI_INK, size = base_size - 1.5),
      plot.caption      = element_text(colour = FEHBI_INK, size = base_size - 2, hjust = 0),
      axis.title        = element_text(colour = FEHBI_INK, size = base_size),
      axis.text         = element_text(colour = FEHBI_INK, size = base_size - 1.5),
      strip.text        = element_text(colour = FEHBI_INK, size = base_size),
      # horizontale Zeilen-Beschriftung (Outcome-Namen links), nicht vertikal:
      strip.text.y.left = element_text(colour = FEHBI_INK, size = base_size, angle = 0),
      strip.text.y      = element_text(colour = FEHBI_INK, size = base_size, angle = 0),
      legend.text       = element_text(colour = FEHBI_INK, size = base_size - 1.5),
      legend.title      = element_text(colour = FEHBI_INK, size = base_size - 1.5),
      legend.position   = "bottom"
    )
}
