# =============================================================================
# 04_regression.R  –  Person B: Regression & Inferenzstatistik
# Lebensmittelpreise & Inflation in Deutschland
# =============================================================================
# HINWEIS: Person A hat bereits in stats_mvp.R geliefert:
#   - ANOVA MoM ~ Produkt        → outputs/anova_mom_produkt.csv
#   - Tukey HSD (Top-20)         → outputs/tukey_mom_top20.csv
#   - t-Test früh vs. spät       → outputs/ttest_period_mom.csv
#   - CPI-Korrelation je Produkt → outputs/corr_mom_cpi_by_produkt.csv
#   - MoM-Summary je Produkt     → outputs/mom_summary_by_produkt.csv
#
# Dieses Script ERGÄNZT:
#   1. ANOVA auf Kategorieebene (Preisanstieg % ~ Kategorie) + Tukey
#   2. t-Test auf Indexebene (nicht MoM): Phasen-Vergleich
#   3. Lineare Regression: Preisindex ~ t + Kategorie + Risikogruppe + Saison
#   4. CPI-Niveau-Korrelation (nicht MoM, sondern absoluter Index ~ CPI)
#   5. Risikogruppen-Vergleich (ANOVA hoch vs. mittel vs. stabil)
# =============================================================================

library(tidyverse)
library(lubridate)
library(broom)

dir.create("output/tabellen", showWarnings = FALSE, recursive = TRUE)
dir.create("output/plots",    showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 0. Daten laden
# =============================================================================
df_raw <- read_csv("data_processed/destatis_statistik.csv",
                   show_col_types = FALSE) |>
  mutate(Datum = as.Date(Datum), Preisindex = as.numeric(Preisindex)) |>
  filter(!is.na(Datum))          # Summenzeilen ohne Datum entfernen (1 je Produkt)

cpi <- read_csv("data_raw/cpi_de.csv", show_col_types = FALSE) |>
  mutate(Datum = as.Date(Datum), CPI = as.numeric(CPI)) |>
  arrange(Datum) |>
  mutate(CPI_MoM = 100 * (CPI / lag(CPI) - 1))

features <- read_csv("data_processed/features_destatis_lag12.csv",
                     show_col_types = FALSE) |>
  distinct(Produkt, Risikogruppe, preisanstieg_pct)

COICOP_MAP <- c(
  "111" = "Getreide & Backwaren",   "112" = "Fleisch & Wurst",
  "113" = "Fisch & Meeresfrüchte",  "114" = "Milch & Milchgetränke",
  "115" = "Öle & Fette",            "116" = "Obst & Obstkonserven",
  "117" = "Gemüse & Gemüsekonserven","118" = "Zucker & Süßwaren",
  "119" = "Sonstige Nahrungsmittel","121" = "Kaffee, Tee & Kakao",
  "122" = "Säfte & Softdrinks"
)

df <- df_raw |>
  mutate(
    coicop_sub  = str_sub(`COICOP-Index`, 7, 9),
    Kategorie   = COICOP_MAP[coicop_sub],
    Kategorie   = if_else(is.na(Kategorie), "Sonstiges", Kategorie),
    # Numerische Zeit: Monate seit Jan 2020
    t           = interval(as.Date("2020-01-01"), Datum) %/% months(1),
    Monat_fakt  = factor(month(Datum)),
    Periode     = case_when(
      year(Datum) <= 2021 ~ "Pre-Inflation",
      year(Datum) <= 2022 ~ "Hochinflation",
      TRUE                ~ "Normalisierung"
    ),
    Periode = factor(Periode, levels = c("Pre-Inflation", "Hochinflation", "Normalisierung"))
  ) |>
  left_join(features, by = "Produkt") |>
  left_join(cpi |> select(Datum, CPI, CPI_MoM), by = "Datum")

# MoM berechnen
df <- df |>
  group_by(Produkt) |>
  arrange(Datum) |>
  mutate(MoM = 100 * (Preisindex / lag(Preisindex) - 1)) |>
  ungroup()

cat("Daten geladen: ", nrow(df), "Zeilen,", n_distinct(df$Produkt), "Produkte\n")

# =============================================================================
# 1. ANOVA: Preisanstieg (%) nach COICOP-Kategorie
# =============================================================================
cat("\n════════════════════════════════════════\n")
cat("1. ANOVA: Preisanstieg % ~ Kategorie\n")
cat("════════════════════════════════════════\n")

# Gesamtpreisänderung je Produkt (von features bereits vorhanden)
preisaenderung <- features |>
  left_join(df |> distinct(Produkt, Kategorie, Risikogruppe), by = c("Produkt", "Risikogruppe"))

anova_kat <- aov(preisanstieg_pct ~ Kategorie, data = preisaenderung)
anova_sum <- summary(anova_kat)
print(anova_sum)

# Effektgröße Eta²
ss_total <- sum(anova_sum[[1]]$`Sum Sq`)
eta2_kat  <- anova_sum[[1]]$`Sum Sq`[1] / ss_total
cat(sprintf("Eta² = %.4f  (%s Effekt)\n", eta2_kat,
    ifelse(eta2_kat > 0.14, "groß", ifelse(eta2_kat > 0.06, "mittel", "klein"))))

# Tukey HSD (wenn signifikant)
p_anova_kat <- anova_sum[[1]]$`Pr(>F)`[1]
tukey_kat   <- tibble()

if (!is.na(p_anova_kat) && p_anova_kat < 0.05) {
  tukey_kat <- TukeyHSD(anova_kat)$Kategorie |>
    as_tibble(rownames = "Vergleich") |>
    rename(diff = diff, lwr = lwr, upr = upr, p_adj = `p adj`) |>
    filter(p_adj < 0.05) |>
    arrange(p_adj)
  cat("\nTukey HSD (signifikante Paare):\n")
  print(tukey_kat)
}

write_csv(tidy(anova_kat), "output/tabellen/anova_kategorie.csv")
if (nrow(tukey_kat) > 0)
  write_csv(tukey_kat, "output/tabellen/tukey_kategorie.csv")

# Deskriptiv je Kategorie
desk_kat <- preisaenderung |>
  group_by(Kategorie) |>
  summarise(
    n             = n(),
    mean_anstieg  = mean(preisanstieg_pct, na.rm = TRUE),
    sd_anstieg    = sd(preisanstieg_pct, na.rm = TRUE),
    median_anstieg = median(preisanstieg_pct, na.rm = TRUE),
    min_anstieg   = min(preisanstieg_pct, na.rm = TRUE),
    max_anstieg   = max(preisanstieg_pct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(mean_anstieg))
write_csv(desk_kat, "output/tabellen/deskriptiv_je_kategorie.csv")
print(desk_kat)

# =============================================================================
# 2. ANOVA: Risikogruppen (hoch / mittel / stabil)
# =============================================================================
cat("\n════════════════════════════════════════\n")
cat("2. ANOVA: Preisanstieg % ~ Risikogruppe\n")
cat("════════════════════════════════════════\n")

anova_rg  <- aov(preisanstieg_pct ~ Risikogruppe, data = preisaenderung)
print(summary(anova_rg))

# Tukey für Risikogruppen (fast sicher signifikant)
tukey_rg <- TukeyHSD(anova_rg)$Risikogruppe |>
  as_tibble(rownames = "Vergleich") |>
  rename(p_adj = `p adj`)
cat("\nTukey Risikogruppen:\n")
print(tukey_rg)
write_csv(tidy(anova_rg), "output/tabellen/anova_risikogruppe.csv")
write_csv(tukey_rg,       "output/tabellen/tukey_risikogruppe.csv")

# =============================================================================
# 3. t-Tests
# =============================================================================
cat("\n════════════════════════════════════════\n")
cat("3. t-Tests\n")
cat("════════════════════════════════════════\n")

# t-Test 3a: Preisindex Pre-Inflation vs. Hochinflation
t3a_pre  <- df |> filter(Periode == "Pre-Inflation") |> pull(Preisindex)
t3a_hoch <- df |> filter(Periode == "Hochinflation") |> pull(Preisindex)
t3a <- t.test(t3a_hoch, t3a_pre, alternative = "greater")
cat("--- t-Test: Preisindex Hochinflation > Pre-Inflation ---\n")
print(t3a)

# t-Test 3b: Hochinflation vs. Normalisierung (Preisindex)
t3b_norm <- df |> filter(Periode == "Normalisierung") |> pull(Preisindex)
t3b <- t.test(t3b_norm, t3a_hoch)
cat("\n--- t-Test: Preisindex Normalisierung vs. Hochinflation ---\n")
print(t3b)

# t-Test 3c: MoM früh vs. spät (repliziert + ergänzt stats_mvp.R)
# (Ergebnis aus stats_mvp.R: t=5.05, p<0.001 – frühe MoM höher)
if (file.exists("outputs/ttest_period_mom.csv")) {
  cat("\n(MoM-Perioden-t-Test aus stats_mvp.R bereits vorhanden:)\n")
  print(read_csv("outputs/ttest_period_mom.csv", show_col_types = FALSE))
}

# Ergebnisse speichern
ttest_ergebnisse <- tribble(
  ~Test,                              ~t,             ~df,             ~p_wert,         ~ci_low,            ~ci_high,
  "Preisindex: Hochinflation > Pre",  t3a$statistic,  t3a$parameter,  t3a$p.value,     t3a$conf.int[1],    t3a$conf.int[2],
  "Preisindex: Normalisierung vs Hoch", t3b$statistic, t3b$parameter, t3b$p.value,     t3b$conf.int[1],    t3b$conf.int[2]
)
write_csv(ttest_ergebnisse, "output/tabellen/ttest_ergebnisse.csv")

# =============================================================================
# 4. Lineare Regression
# =============================================================================
cat("\n════════════════════════════════════════\n")
cat("4. Lineare Regression: Preisindex ~ ...\n")
cat("════════════════════════════════════════\n")

# Modell 1: Einfacher Zeittrend
m1 <- lm(Preisindex ~ t, data = df)

# Modell 2: + Saisoneffekt
m2 <- lm(Preisindex ~ t + Monat_fakt, data = df)

# Modell 3: + Periode (Pre/Hoch/Normal)
m3 <- lm(Preisindex ~ t + Monat_fakt + Periode, data = df)

# Modell 4: + Kategorie
m4 <- lm(Preisindex ~ t + Monat_fakt + Periode + Kategorie, data = df)

# Modell 5: + Risikogruppe (Kernmodell)
m5 <- lm(Preisindex ~ t + Monat_fakt + Periode + Kategorie + Risikogruppe, data = df)

# Vergleich
modell_vergleich <- bind_rows(
  glance(m1) |> mutate(Modell = "M1: t"),
  glance(m2) |> mutate(Modell = "M2: + Saison"),
  glance(m3) |> mutate(Modell = "M3: + Periode"),
  glance(m4) |> mutate(Modell = "M4: + Kategorie"),
  glance(m5) |> mutate(Modell = "M5: + Risikogruppe")
) |>
  select(Modell, r.squared, adj.r.squared, AIC, BIC, df.residual)

cat("\n--- Modellvergleich ---\n")
print(modell_vergleich)

cat("\n--- ANOVA Modellvergleich ---\n")
print(anova(m1, m2, m3, m4, m5))

# Koeffizienten M3 (ohne Kategorie – für Dashboard Interpretation)
cat("\n--- Koeffizienten M3 ---\n")
print(tidy(m3) |> filter(p.value < 0.05))

write_csv(modell_vergleich,       "output/tabellen/regression_vergleich.csv")
write_csv(tidy(m3),               "output/tabellen/regression_m3_koeff.csv")
write_csv(tidy(m5),               "output/tabellen/regression_m5_koeff.csv")

# =============================================================================
# 5. CPI-Korrelation (Niveau, ergänzt die MoM-Korrelation aus stats_mvp.R)
# =============================================================================
cat("\n════════════════════════════════════════\n")
cat("5. CPI-Korrelation (Niveau-Index)\n")
cat("════════════════════════════════════════\n")

# Gesamt-Korrelation Ø Preisindex ~ CPI
gesamt_avg <- df |>
  group_by(Datum) |>
  summarise(avg_idx = mean(Preisindex, na.rm = TRUE), .groups = "drop") |>
  left_join(cpi |> select(Datum, CPI), by = "Datum") |>
  filter(!is.na(CPI))

cor_niveau <- cor(gesamt_avg$avg_idx, gesamt_avg$CPI, method = "pearson")
cat(sprintf("Pearson r (Ø Preisindex ~ CPI Niveau): %.4f\n", cor_niveau))

# Je Produkt
kor_produkt <- df |>
  filter(!is.na(CPI)) |>
  group_by(Produkt, Kategorie, Risikogruppe) |>
  summarise(
    r_cpi_niveau = cor(Preisindex, CPI, method = "pearson"),
    r_cpi_mom    = cor(MoM, CPI_MoM, use = "complete.obs", method = "pearson"),
    .groups = "drop"
  ) |>
  arrange(desc(abs(r_cpi_niveau)))

cat("\n--- Top 10 CPI-Korrelation (Niveau) ---\n")
print(head(kor_produkt, 10))

cat("\n--- Median-Korrelation je Risikogruppe ---\n")
kor_produkt |>
  group_by(Risikogruppe) |>
  summarise(
    median_r_niveau = median(r_cpi_niveau, na.rm = TRUE),
    median_r_mom    = median(r_cpi_mom, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print()

write_csv(kor_produkt, "output/tabellen/korrelation_cpi.csv")

cat("\n✓ 04_regression.R abgeschlossen.\n")
cat("  → output/tabellen/anova_kategorie.csv\n")
cat("  → output/tabellen/anova_risikogruppe.csv\n")
cat("  → output/tabellen/tukey_kategorie.csv\n")
cat("  → output/tabellen/tukey_risikogruppe.csv\n")
cat("  → output/tabellen/ttest_ergebnisse.csv\n")
cat("  → output/tabellen/deskriptiv_je_kategorie.csv\n")
cat("  → output/tabellen/regression_vergleich.csv\n")
cat("  → output/tabellen/regression_m3_koeff.csv\n")
cat("  → output/tabellen/regression_m5_koeff.csv\n")
cat("  → output/tabellen/korrelation_cpi.csv\n")
