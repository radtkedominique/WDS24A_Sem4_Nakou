# =============================================================================
# 02_zeitreihenanalyse.R  –  Person B: Zeitreihenanalyse
# Lebensmittelpreise & Inflation in Deutschland
# =============================================================================
# Datenstruktur (destatis_statistik.csv):
#   COICOP-Index | Produkt | Datum (YYYY-MM-DD) | Monat_Nr | Preisindex
#   168 Produkte, 65 Monate (Jan 2020 – Jun 2025), 0 NaN in Preisindex
#
# Kategorie-Mapping über COICOP-Stellen 7-9 (0-indexed: str[6:9])
# =============================================================================

library(tidyverse)
library(lubridate)
library(zoo)
library(tseries)    # adf.test()
library(forecast)   # stl(), na.interp()

# ── Ausgabe-Verzeichnisse ────────────────────────────────────────────────────
dir.create("output/plots",    showWarnings = FALSE, recursive = TRUE)
dir.create("output/tabellen", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 0. Daten laden & Kategorie ergänzen
# =============================================================================
df_raw <- read_csv("data_processed/destatis_statistik.csv",
                   show_col_types = FALSE) |>
  mutate(Datum = as.Date(Datum),
         Preisindex = as.numeric(Preisindex)) |>
  filter(!is.na(Datum))          # Summenzeilen ohne Datum entfernen (1 je Produkt)

# COICOP-Kategorien (Stellen 7-9 des Index, z.B. CC13-0116... → "116" → Obst)
COICOP_MAP <- c(
  "111" = "Getreide & Backwaren",
  "112" = "Fleisch & Wurst",
  "113" = "Fisch & Meeresfrüchte",
  "114" = "Milch & Milchgetränke",
  "115" = "Öle & Fette",
  "116" = "Obst & Obstkonserven",
  "117" = "Gemüse & Gemüsekonserven",
  "118" = "Zucker & Süßwaren",
  "119" = "Sonstige Nahrungsmittel",
  "121" = "Kaffee, Tee & Kakao",
  "122" = "Säfte & Softdrinks"
)

df <- df_raw |>
  mutate(
    coicop_sub = str_sub(`COICOP-Index`, 7, 9),
    Kategorie  = coarsen <- COICOP_MAP[coicop_sub],
    Kategorie  = if_else(is.na(Kategorie), "Sonstiges", Kategorie)
  )

# Kategorie-Mapping exportieren (für R shiny)
df |>
  distinct(Produkt, Kategorie) |>
  write_csv("output/tabellen/kategorie_mapping.csv")

cat("Produkte:", n_distinct(df$Produkt), "\n")
cat("Zeitraum:", format(min(df$Datum), "%b %Y"), "–", format(max(df$Datum), "%b %Y"), "\n")
cat("Kategorien:", n_distinct(df$Kategorie), "\n")

# =============================================================================
# 1. Hilfsfunktion: ts-Objekt aus Long-Format
# =============================================================================
# Keine NaN in den Daten → direkte Umwandlung möglich
make_ts <- function(data, produkt_name) {
  d <- data |>
    filter(Produkt == produkt_name) |>
    arrange(Datum)
  ts(d$Preisindex,
     start     = c(year(min(d$Datum)), month(min(d$Datum))),
     frequency = 12)
}

# =============================================================================
# 2. Aggregierte Zeitreihe (Ø aller Produkte) + STL
# =============================================================================
ts_gesamt <- df |>
  group_by(Datum) |>
  summarise(Preisindex = mean(Preisindex, na.rm = TRUE), .groups = "drop") |>
  arrange(Datum)

ts_ges_obj <- ts(ts_gesamt$Preisindex,
                 start = c(2020, 1), frequency = 12)

stl_ges <- stl(ts_ges_obj, s.window = "periodic")

cat("\n--- STL-Dekomposition (Gesamt-Durchschnitt) ---\n")
print(summary(stl_ges))

png("output/plots/stl_gesamt.png", width = 1200, height = 800, res = 150)
plot(stl_ges,
     main = "STL-Dekomposition: Ø Preisindex (alle 168 Produkte, Jan 2020–Jun 2025)")
dev.off()

# =============================================================================
# 3. STL-Saisonstärke für alle 168 Produkte
# =============================================================================
saisonstaerke_list <- map_dfr(unique(df$Produkt), function(p) {
  ts_p <- make_ts(df, p)
  if (length(ts_p) < 24) return(NULL)

  stl_p <- tryCatch(
    stl(ts_p, s.window = "periodic"),
    error = function(e) NULL
  )
  if (is.null(stl_p)) return(NULL)

  seasonal  <- stl_p$time.series[, "seasonal"]
  remainder <- stl_p$time.series[, "remainder"]
  trend_c   <- stl_p$time.series[, "trend"]

  # Saisonstärke nach Wang et al. (2006)
  Fs <- max(0, 1 - var(remainder) / var(seasonal + remainder))
  # Trendstärke
  Ft <- max(0, 1 - var(remainder) / var(trend_c + remainder))

  tibble(
    Produkt      = p,
    saisonstaerke = Fs,
    trendstaerke  = Ft
  )
})

# Kategorie ergänzen
saisonstaerke_list <- saisonstaerke_list |>
  left_join(df |> distinct(Produkt, Kategorie), by = "Produkt") |>
  arrange(desc(saisonstaerke))

cat("\n--- Top 10 saisonalste Produkte ---\n")
print(head(saisonstaerke_list, 10))

cat("\n--- Top 10 stärkste Trends ---\n")
print(head(arrange(saisonstaerke_list, desc(trendstaerke)), 10))

write_csv(saisonstaerke_list, "output/tabellen/saisonstaerke.csv")

# =============================================================================
# 4. Trend-Statistiken (auf Basis der echten Daten aus stats_mvp.R)
# =============================================================================
# Verwende die bereits von Person A berechnete mom_summary wenn vorhanden
if (file.exists("outputs/mom_summary_by_produkt.csv")) {
  trend_stats <- read_csv("outputs/mom_summary_by_produkt.csv",
                           show_col_types = FALSE) |>
    left_join(df |> distinct(Produkt, Kategorie), by = "Produkt")
} else {
  trend_stats <- df |>
    arrange(Produkt, Datum) |>
    group_by(Produkt) |>
    mutate(MoM = 100 * (Preisindex / lag(Preisindex) - 1)) |>
    summarise(
      n              = sum(!is.na(MoM)),
      mean_mom       = mean(MoM, na.rm = TRUE),
      sd_mom         = sd(MoM, na.rm = TRUE),
      start_preis    = first(Preisindex),
      end_preis      = last(Preisindex),
      preisanstieg_pct = 100 * (last(Preisindex) / first(Preisindex) - 1),
      .groups = "drop"
    ) |>
    left_join(df |> distinct(Produkt, Kategorie), by = "Produkt")
}

cat("\n--- Top 10 Preisanstiege ---\n")
print(head(arrange(trend_stats, desc(preisanstieg_pct)), 10))

cat("\n--- Top 5 Preisrückgänge ---\n")
print(head(arrange(trend_stats, preisanstieg_pct), 5))

write_csv(trend_stats, "output/tabellen/trend_stats.csv")

# =============================================================================
# 5. Monatliche Saisonmuster (Abweichung vom Jahresmittel)
# =============================================================================
saisonmuster <- df |>
  mutate(Monat = month(Datum, label = TRUE)) |>
  group_by(Produkt, Kategorie, Monat) |>
  summarise(mean_idx = mean(Preisindex, na.rm = TRUE), .groups = "drop") |>
  group_by(Produkt) |>
  mutate(
    jahres_mean       = mean(mean_idx),
    abweichung        = mean_idx - jahres_mean,
    saison_range      = max(abweichung) - min(abweichung)
  ) |>
  ungroup()

write_csv(saisonmuster, "output/tabellen/saisonmuster.csv")

# =============================================================================
# 6. ADF-Stationaritätstests (Vorbereitung für ARIMA)
# =============================================================================
# Alle 168 Produkte haben vollständige Reihen → kein Filtering nötig
adf_ergebnisse <- map_dfr(unique(df$Produkt), function(p) {
  ts_p <- make_ts(df, p)
  adf  <- tryCatch(adf.test(ts_p), error = function(e) NULL)
  if (is.null(adf)) return(NULL)
  tibble(
    Produkt    = p,
    adf_stat   = adf$statistic,
    p_value    = adf$p.value,
    stationaer = adf$p.value < 0.05
  )
})

cat("\n--- ADF-Test: Stationarität ---\n")
cat(sprintf("Stationär: %d / %d (%.1f%%)\n",
    sum(adf_ergebnisse$stationaer),
    nrow(adf_ergebnisse),
    mean(adf_ergebnisse$stationaer) * 100))

write_csv(adf_ergebnisse, "output/tabellen/adf_tests.csv")

cat("\n✓ 02_zeitreihenanalyse.R abgeschlossen.\n")
cat("  → output/tabellen/kategorie_mapping.csv\n")
cat("  → output/tabellen/saisonstaerke.csv\n")
cat("  → output/tabellen/saisonmuster.csv\n")
cat("  → output/tabellen/trend_stats.csv\n")
cat("  → output/tabellen/adf_tests.csv\n")
cat("  → output/plots/stl_gesamt.png\n")
