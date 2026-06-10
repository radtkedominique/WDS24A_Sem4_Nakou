# =============================================================================
# run_all.R – Lebensmittelpreise & Inflation in Deutschland
# =============================================================================
# Voraussetzung: Projektordner muss folgende Struktur haben:
#   data_processed/destatis_statistik.csv
#   data_processed/features_destatis_lag12.csv
#   data_raw/cpi_de.csv
#   outputs/                 (von stats_mvp.R / Person A)
# =============================================================================

dir.create("output/plots",    showWarnings = FALSE, recursive = TRUE)
dir.create("output/tabellen", showWarnings = FALSE, recursive = TRUE)

# ── Pakete ──────────────────────────────────────────────────────────────────
required_pkgs <- c(
  "tidyverse", "lubridate", "zoo",
  "tseries", "forecast",
  "broom",
  "shiny", "bslib", "plotly", "DT", "RColorBrewer"
)
missing <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing) > 0) {
  message("Installiere: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ── Ausführung ──────────────────────────────────────────────────────────────
cat("╔══════════════════════════════════════════════╗\n")
cat("║  Lebensmittelpreise & Inflation – Pipeline  ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

cat("[1/3] 02_zeitreihenanalyse.R ...\n")
source("R/02_zeitreihenanalyse.R")

cat("\n[2/3] 03_arima.R ...  (dauert einige Minuten)\n")
source("R/03_arima.R")

cat("\n[3/3] 04_regression.R ...\n")
source("R/04_regression.R")

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  ✓ Alle Analysen abgeschlossen              ║\n")
cat("║                                              ║\n")
cat("║  Dashboard starten:                          ║\n")
cat('║  shiny::runApp("shiny/")                     ║\n')
cat("╚══════════════════════════════════════════════╝\n")
