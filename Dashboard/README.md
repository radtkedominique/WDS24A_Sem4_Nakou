# Lebensmittelpreise & Inflation in Deutschland
**R · Shiny · ggplot2 · tidyverse** | Daten: Destatis / Open Food Facts

---

## Projektstruktur

```
lebensmittel_projekt/
│
├── data_processed/                      ← Von Person A (Git)
│   ├── destatis_statistik.csv           ← 168 Produkte × 65 Monate, Long-Format
│   │                                       Spalten: COICOP-Index | Produkt | Datum | Monat_Nr | Preisindex
│   │                                       Kein NaN im Preisindex
│   ├── destatis_deskriptiv.csv          ← Ø, Median, SD, Min, Max je Produkt
│   ├── features_destatis.csv            ← + Lag-Features + Risikogruppe
│   └── features_destatis_lag12.csv      ← + lag_12 + rolling_std_3 + CPI_mom
│
├── data_raw/
│   └── cpi_de.csv                       ← Gesamt-CPI Jan 2020 – Jun 2025
│
├── outputs/                             ← Von Person A (stats_mvp.R) bereits erzeugt
│   ├── anova_mom_produkt.csv            ← ANOVA MoM ~ Produkt (p ≈ 1, nicht signifikant)
│   ├── tukey_mom_top20.csv              ← Tukey HSD auf Top-20-Produkte nach MoM
│   ├── ttest_period_mom.csv             ← t-Test früh (≤2022) vs. spät (≥2023): p<0.001
│   ├── corr_mom_cpi_by_produkt.csv      ← MoM-CPI-Korrelation je Produkt
│   ├── mom_summary_by_produkt.csv       ← MoM-Zusammenfassung je Produkt
│   └── boxplot_top20_mom.png
│
├── R/                                   ← Person B
│   ├── 02_zeitreihenanalyse.R           ← STL, Saisonstärke, ADF-Tests, COICOP-Mapping
│   ├── 03_arima.R                       ← auto.arima, Hold-out, 12-Monats-Prognose
│   └── 04_regression.R                 ← ANOVA Kategorie, Risikogruppen-ANOVA,
│                                           t-Tests (Preisindex-Niveau), Regression M1–M5,
│                                           CPI-Niveau-Korrelation
│
├── shiny/                               ← Person C
│   ├── ui.R                             ← 5-Tab bslib-Dashboard
│   └── server.R                         ← Reaktive Logik
│
├── output/                              ← Wird von run_all.R erzeugt
│   ├── plots/
│   └── tabellen/
│
└── run_all.R                            ← Startet alle Scripts
```

---

## Schnellstart

```r
setwd("/pfad/zum/projekt")   # Projektordner mit data_processed/ etc.
source("run_all.R")          # Analysen ausführen (~5–15 min wegen ARIMA)
shiny::runApp("shiny/")      # Dashboard starten
```

---

## Daten-Überblick (aus Übergabeprotokoll)

| Kennzahl | Wert |
|---|---|
| Produkte | 168 |
| Zeitraum | Jan 2020 – Jun 2025 (65 Monate) |
| NaN in Preisindex | 0 (vollständige Reihen) |
| Ø Preisanstieg | +37,4 % |
| Stärkster Anstieg | Gemüsesaft +66,5 %, Apfelsaft +65,7 % |
| Stärkster Rückgang | Kopfsalat −13,9, Tomaten −12,9 |
| Risikogruppen | hoch: 57 / mittel: 55 / stabil: 56 Produkte |

### COICOP-Kategorien (abgeleitet aus Index, Stellen 7–9)

| Code | Kategorie | Produkte |
|---|---|---|
| 112 | Fleisch & Wurst | 27 |
| 117 | Gemüse & Gemüsekonserven | 25 |
| 111 | Getreide & Backwaren | 24 |
| 119 | Sonstige Nahrungsmittel | 21 |
| 116 | Obst & Obstkonserven | 15 |
| 114 | Milch & Milchgetränke | 14 |
| 113 | Fisch & Meeresfrüchte | 11 |
| 118 | Zucker & Süßwaren | 11 |
| 122 | Säfte & Softdrinks | 9 |
| 121 | Kaffee, Tee & Kakao | 6 |
| 115 | Öle & Fette | 5 |

---

## Was die Scripts ergänzen (zu stats_mvp.R von Person A)

### `02_zeitreihenanalyse.R`
- COICOP-Kategorie-Mapping (Stellen 7–9 → 11 Kategorien)
- STL-Dekomposition Gesamt + Saisonstärke-Kennzahl (Wang et al.) je Produkt
- ADF-Stationaritätstests für alle 168 Produkte
- Monatliche Saisonmuster (Abweichung vom Jahresmittel)

### `03_arima.R`
- `auto.arima()` mit Box-Cox + saisonal für alle 168 Produkte
- Hold-out-Validierung letzte 12 Monate → RMSE, MAE, MAPE
- 12-Monats-Prognose Jul 2025 – Jun 2026 mit 80%/95%-KI
- Risikogruppen-Auswertung der Modellgüte

### `04_regression.R`
- **ANOVA Kategorie**: Preisanstieg % ~ COICOP-Kategorie + Tukey HSD
- **ANOVA Risikogruppe**: Preisanstieg % ~ hoch/mittel/stabil + Tukey HSD
- **t-Tests**: Preisindex-Niveau Pre-Inflation vs. Hochinflation vs. Normalisierung
- **Regression M1–M5**: schrittweise von `t` bis `t + Saison + Periode + Kategorie + Risikogruppe`
- **CPI-Niveau-Korrelation**: Pearson r(Preisindex, CPI) je Produkt (ergänzt MoM-Korrelation)

### `shiny/` (Dashboard 5 Tabs)
- Preistrends · Kategorien · Inflation & CPI · Prognose · Preis-Rechner
- Sidebar: Kategorie-Filter → Produkt-Filter (abhängig) · Zeitraum-Slider · CPI/Prognose-Toggle
- Preis-Rechner greift auf ARIMA-Prognosen für Zieldaten > Jun 2025 zurück

---

## Pakete

```r
install.packages(c(
  "tidyverse", "lubridate", "zoo",
  "tseries", "forecast",
  "broom",
  "shiny", "bslib", "plotly", "DT", "RColorBrewer"
))
```
