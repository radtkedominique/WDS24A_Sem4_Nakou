# =============================================================================
# 03_arima.R  –  Person B: ARIMA-Prognosemodell
# Lebensmittelpreise & Inflation in Deutschland
# =============================================================================
# Input:  data_processed/destatis_statistik.csv
#         data_processed/features_destatis_lag12.csv  (Risikogruppen)
# Output: output/tabellen/arima_guete.csv
#         output/tabellen/arima_prognosen.csv
#
# Strategie:
#   - auto.arima() mit saisonaler Komponente je Produkt (alle 168)
#   - Box-Cox-Transformation (lambda = "auto")
#   - Hold-out-Validierung: letzte 12 Monate
#   - 12-Monats-Prognose: Jul 2025 – Jun 2026
# =============================================================================

library(tidyverse)
library(lubridate)
library(forecast)

dir.create("output/tabellen", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 0. Daten laden
# =============================================================================
df <- read_csv("data_processed/destatis_statistik.csv",
               show_col_types = FALSE) |>
  mutate(Datum = as.Date(Datum), Preisindex = as.numeric(Preisindex)) |>
  filter(!is.na(Datum)) |>          # Summenzeilen ohne Datum entfernen (1 je Produkt)
  arrange(Produkt, Datum)

# Risikogruppen (von Person A, aus features_destatis_lag12.csv)
risikogruppen <- read_csv("data_processed/features_destatis_lag12.csv",
                          show_col_types = FALSE) |>
  distinct(Produkt, Risikogruppe, preisanstieg_pct)

# COICOP-Kategorien
COICOP_MAP <- c(
  "111" = "Getreide & Backwaren",   "112" = "Fleisch & Wurst",
  "113" = "Fisch & Meeresfrüchte",  "114" = "Milch & Milchgetränke",
  "115" = "Öle & Fette",            "116" = "Obst & Obstkonserven",
  "117" = "Gemüse & Gemüsekonserven","118" = "Zucker & Süßwaren",
  "119" = "Sonstige Nahrungsmittel","121" = "Kaffee, Tee & Kakao",
  "122" = "Säfte & Softdrinks"
)

df <- df |>
  mutate(
    coicop_sub = str_sub(`COICOP-Index`, 7, 9),
    Kategorie  = COICOP_MAP[coicop_sub],
    Kategorie  = if_else(is.na(Kategorie), "Sonstiges", Kategorie)
  ) |>
  left_join(risikogruppen, by = "Produkt")

cat("Produkte:", n_distinct(df$Produkt), "\n")
cat("Zeitraum:", format(min(df$Datum), "%b %Y"), "–", format(max(df$Datum), "%b %Y"), "\n")

# =============================================================================
# 1. Hilfsfunktion: ts-Objekt
# =============================================================================
make_ts <- function(data, produkt_name) {
  d <- data |> filter(Produkt == produkt_name) |> arrange(Datum)
  ts(d$Preisindex, start = c(year(min(d$Datum)), month(min(d$Datum))), frequency = 12)
}

# =============================================================================
# 2. ARIMA für alle 168 Produkte
# =============================================================================
HOLDOUT   <- 12   # Monate für Validierung
HORIZONT  <- 12   # Monate Prognose (Jul 2025 – Jun 2026)
produkte  <- unique(df$Produkt)

arima_guete      <- vector("list", length(produkte))
arima_prognosen  <- vector("list", length(produkte))
arima_validierung <- vector("list", length(produkte))  # Hold-out: Prognose vs. echt

cat("\nFitte ARIMA-Modelle...\n")
pb_step <- max(1, floor(length(produkte) / 10))

for (i in seq_along(produkte)) {
  p    <- produkte[i]
  ts_p <- make_ts(df, p)
  n    <- length(ts_p)

  if (n < (HOLDOUT + 24)) next  # mindestens 2 Jahre Training

  train_ts <- head(ts_p, n - HOLDOUT)
  test_ts  <- tail(ts_p, HOLDOUT)

  model <- tryCatch(
    auto.arima(
      train_ts,
      seasonal      = TRUE,
      stepwise      = FALSE,
      approximation = FALSE,
      lambda        = "auto",
      max.p = 3, max.q = 3,
      max.P = 2, max.Q = 2
    ),
    error = function(e) NULL
  )
  if (is.null(model)) next

  fc <- forecast(model, h = HOLDOUT + HORIZONT)

  # Gütemetriken auf Hold-out
  pred_val <- as.numeric(fc$mean[1:HOLDOUT])
  rmse_val <- sqrt(mean((pred_val - test_ts)^2))
  mae_val  <- mean(abs(pred_val - test_ts))
  mape_val <- mean(abs((pred_val - test_ts) / test_ts)) * 100

  # Produktmeta
  meta <- df |> filter(Produkt == p) |>
    slice(1) |>
    select(Kategorie, Risikogruppe, preisanstieg_pct)

  arima_guete[[i]] <- tibble(
    Produkt          = p,
    Kategorie        = meta$Kategorie[1],
    Risikogruppe     = meta$Risikogruppe[1],
    preisanstieg_pct = meta$preisanstieg_pct[1],
    modell           = as.character(model),
    aic              = model$aic,
    bic              = model$bic,
    rmse             = rmse_val,
    mae              = mae_val,
    mape             = mape_val
  )

  # Prognose-Daten (nur die echten 12 Monate ab Jul 2025)
  start_prog <- as.Date("2025-07-01")
  daten_prog <- seq(start_prog, by = "month", length.out = HORIZONT)
  idx        <- (HOLDOUT + 1):(HOLDOUT + HORIZONT)

  arima_prognosen[[i]] <- tibble(
    Produkt   = p,
    Datum     = daten_prog,
    prognose  = as.numeric(fc$mean[idx]),
    lo80      = as.numeric(fc$lower[idx, "80%"]),
    hi80      = as.numeric(fc$upper[idx, "80%"]),
    lo95      = as.numeric(fc$lower[idx, "95%"]),
    hi95      = as.numeric(fc$upper[idx, "95%"])
  )

  # Validierungs-Daten: Hold-out-Prognose vs. echte Werte (letzte 12 Monate)
  # Diese Monate kennt das Modell NICHT (Training endete davor)
  datum_alle  <- df |> filter(Produkt == p) |> arrange(Datum) |> pull(Datum)
  datum_holdout <- tail(datum_alle, HOLDOUT)

  arima_validierung[[i]] <- tibble(
    Produkt      = p,
    Datum        = datum_holdout,
    prognose_val = pred_val,                                  # was das Modell vorhersagte
    echt         = as.numeric(test_ts),                       # was wirklich passierte
    lo80         = as.numeric(fc$lower[1:HOLDOUT, "80%"]),
    hi80         = as.numeric(fc$upper[1:HOLDOUT, "80%"]),
    lo95         = as.numeric(fc$lower[1:HOLDOUT, "95%"]),
    hi95         = as.numeric(fc$upper[1:HOLDOUT, "95%"])
  )

  if (i %% pb_step == 0) {
    cat(sprintf("  [%d/%d] %-40s | %s | MAPE: %.1f%%\n",
                i, length(produkte), p, as.character(model), mape_val))
  }
}

# =============================================================================
# 3. Ergebnisse zusammenführen & exportieren
# =============================================================================
guete_df       <- bind_rows(arima_guete)
prognose_df    <- bind_rows(arima_prognosen)
validierung_df <- bind_rows(arima_validierung)

cat("\n--- Modellgüte (Median MAPE je Risikogruppe) ---\n")
guete_df |>
  group_by(Risikogruppe) |>
  summarise(
    n           = n(),
    median_mape = median(mape, na.rm = TRUE),
    median_rmse = median(rmse, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print()

cat("\n--- Modellgüte (Median MAPE je Kategorie) ---\n")
guete_df |>
  group_by(Kategorie) |>
  summarise(median_mape = median(mape, na.rm = TRUE), n = n(), .groups = "drop") |>
  arrange(median_mape) |>
  print()

cat(sprintf("\nGesamt Median MAPE: %.2f%%\n", median(guete_df$mape, na.rm = TRUE)))
cat(sprintf("Modelle mit MAPE < 5%%: %d / %d\n",
    sum(guete_df$mape < 5, na.rm = TRUE), nrow(guete_df)))

write_csv(guete_df,       "output/tabellen/arima_guete.csv")
write_csv(prognose_df,    "output/tabellen/arima_prognosen.csv")
write_csv(validierung_df, "output/tabellen/arima_validierung.csv")

cat("\n✓ 03_arima.R abgeschlossen.\n")
cat(sprintf("  Modelle gefittet: %d / %d\n", nrow(guete_df), length(produkte)))
cat("  → output/tabellen/arima_guete.csv\n")
cat("  → output/tabellen/arima_prognosen.csv\n")
cat("  → output/tabellen/arima_validierung.csv\n")
