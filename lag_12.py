"""
Feature Engineering – vollständiges Preprocessing
===================================================
Eingabe:  Wide-Format CSV mit Spalten:
          COICOP-Index, Produkt, Januar, Februar, ..., Juni.5

Pipeline:
  1. Wide → Long Format (melt)
  2. MoM-Rate berechnen
  3. Bestehende Features: lag_1, lag_2, lag_3, rolling_mean_3, rolling_mean_6
  4. Neue Features:       lag_12, rolling_std_3, CPI_mom
  5. Zielspalten anhängen: Risikogruppe, preisanstieg_pct

Ausgabe:  df_features (Long-Format, eine Zeile pro Kategorie × Monat)
"""

import pandas as pd
import numpy as np

# ── Konfiguration ─────────────────────────────────────────────────────────
CSV_PATH      = "data_processed/features_destatis.csv"          # <- Pfad zu deiner CSV-Datei
DESTATIS_PATH = "data_processed/destatis_df.csv"  # für CPI_mom

MONTH_NAMES = [
    "Januar", "Februar", "März", "April", "Mai", "Juni",
    "Juli", "August", "September", "Oktober", "November", "Dezember",
]
MONTH_TO_NUM = {m: i + 1 for i, m in enumerate(MONTH_NAMES)}
BASE_YEAR = 2020

# ══════════════════════════════════════════════════════════════════════════
# SCHRITT 1: Bestehenden Datensatz laden
# ══════════════════════════════════════════════════════════════════════════

df = pd.read_csv(CSV_PATH, parse_dates=["Datum"])
df = df.sort_values(["COICOP-Index", "Datum"]).reset_index(drop=True)

print(f"Datensatz geladen: {df.shape[0]:,} Zeilen, {df.shape[1]} Spalten")

# ══════════════════════════════════════════════════════════════════════════
# SCHRITT 2: lag_12 – Vorjahreswert
# ══════════════════════════════════════════════════════════════════════════
# Begründung: ACF-Analyse zeigt für saisonale Kategorien (Paprika r=+0.36**,
# Gurken) signifikante Jahresautokorrelation. Der Vorjahresmonat kodiert
# die saisonale Grunderwartung des Modells.
# NaN: erste 12 Monate je Kategorie – erwartet, kein Problem.

df["lag_12"] = df.groupby("COICOP-Index")["Preisindex"].shift(12)

# ══════════════════════════════════════════════════════════════════════════
# SCHRITT 3: rolling_std_3 – lokale Volatilität
# ══════════════════════════════════════════════════════════════════════════
# Begründung: Signalisiert dem Modell ob eine turbulente Phase vorliegt
# (Energiepreisschock 2022, saisonale Erntespitzen).
# shift(1) stellt sicher: kein Look-ahead, nur Vergangenheitswerte.

df["rolling_std_3"] = df.groupby("COICOP-Index")["Preisindex"].transform(
    lambda x: x.shift(1).rolling(window=3).std()
)

# ══════════════════════════════════════════════════════════════════════════
# SCHRITT 4: CPI_mom – mittlere MoM-Rate über alle 168 Kategorien
# ══════════════════════════════════════════════════════════════════════════
# Da der Gesamtindex CC13-01 nicht in destatis_df.csv enthalten ist,
# wird CPI_mom als ungewichteter Durchschnitt der MoM-Raten aller
# 168 Einzelkategorien berechnet – ein zuverlässiger Proxy für den
# makroökonomischen Inflationstrend im Nahrungsmittelsektor.

# Wide-Format CSV laden
df_wide = pd.read_csv(DESTATIS_PATH, index_col=0)
value_cols = [c for c in df_wide.columns if c not in ("COICOP-Index", "Produkt")]


# Spaltenname → Datum
def col_to_timestamp(col_name: str) -> pd.Timestamp:
    if "." in col_name:
        month_str, yr_offset = col_name.rsplit(".", 1)
        year = BASE_YEAR + int(yr_offset)
    else:
        month_str, year = col_name, BASE_YEAR
    return pd.Timestamp(f"{year}-{MONTH_TO_NUM[month_str]:02d}-01")


col_date_map = {c: col_to_timestamp(c) for c in value_cols}

# Wide → Long
df_destatis_long = df_wide.melt(
    id_vars=["COICOP-Index", "Produkt"],
    value_vars=value_cols,
    var_name="col",
    value_name="Preisindex",
)
df_destatis_long["Datum"] = df_destatis_long["col"].map(col_date_map)
df_destatis_long["Preisindex"] = pd.to_numeric(
    df_destatis_long["Preisindex"], errors="coerce"
)
df_destatis_long = (
    df_destatis_long
    .drop(columns="col")
    .dropna(subset=["Preisindex", "Datum"])
    .sort_values(["COICOP-Index", "Datum"])
)

# MoM je Kategorie berechnen
df_destatis_long["MoM"] = (
        df_destatis_long
        .groupby("COICOP-Index")["Preisindex"]
        .pct_change() * 100
)

# CPI_mom = mittlere MoM über alle Kategorien pro Monat
cpi_mom = (
    df_destatis_long
    .dropna(subset=["MoM"])
    .groupby("Datum")["MoM"]
    .mean()
    .reset_index()
    .rename(columns={"MoM": "CPI_mom"})
)

# In Hauptdatensatz einmergen
df = df.merge(cpi_mom, on="Datum", how="left")

# ══════════════════════════════════════════════════════════════════════════
# ERGEBNIS
# ══════════════════════════════════════════════════════════════════════════

print("\n" + "=" * 55)
print(f"  Fertig: {df.shape[0]:,} Zeilen, {df.shape[1]} Spalten")
print("=" * 55)
print("Spalten:", list(df.columns))
print("\nNaN pro neuem Feature:")
for col in ["lag_12", "rolling_std_3", "CPI_mom"]:
    n = df[col].isna().sum()
    print(f"  {col:<20} {n:>5} NaN  ({n / len(df) * 100:.1f}%)")

# Optional: CSV speichern
df.to_csv("data_processed/features_destatis_lag12.csv", index=False)
