# stats_mvp.R
# Ziel: ANOVA, t-Test und optionale CPI-Korrelation auf Produktebene

library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(broom)

# -----------------------------
# 1) Daten laden
# -----------------------------
destatis <- read_csv("data_processed/destatis_statistik.csv", show_col_types = FALSE) %>%
  mutate(
    Datum = ymd(Datum),
    Preisindex = as.numeric(Preisindex)
  ) %>%
  arrange(Produkt, Datum)

print(names(destatis))

# -----------------------------
# 2) MoM berechnen
# -----------------------------
destatis <- destatis %>%
  group_by(Produkt) %>%
  arrange(Datum, .by_group = TRUE) %>%
  mutate(
    MoM = 100 * (Preisindex / lag(Preisindex) - 1)
  ) %>%
  ungroup()

# -----------------------------
# 3) ANOVA: MoM ~ Produkt (alle Produkte)
# -----------------------------
anova_data <- destatis %>%
  filter(!is.na(MoM), !is.na(Produkt))

anova_fit <- aov(MoM ~ Produkt, data = anova_data)
anova_tbl <- tidy(anova_fit)
print(anova_tbl)

# -----------------------------
# 3b) Tukey: nur auf Top-20-Produkte nach mittlerer MoM-Abweichung
# -----------------------------
# Produkte mit den extremsten mittleren MoM auswählen (je 10 oben/unten)
top_produkte <- anova_data %>%
  group_by(Produkt) %>%
  summarise(mean_mom = mean(MoM, na.rm = TRUE), .groups = "drop") %>%
  slice_max(abs(mean_mom), n = 20) %>%
  pull(Produkt)

tukey_data <- anova_data %>%
  filter(Produkt %in% top_produkte) %>%
  mutate(Produkt = factor(Produkt))  # factor neu setzen, damit keine leeren Levels

tukey_fit  <- aov(MoM ~ Produkt, data = tukey_data)
tukey_tbl  <- TukeyHSD(tukey_fit) %>% broom::tidy()

print(head(tukey_tbl, 20))

p_boxplot <- anova_data %>%
  filter(Produkt %in% top_produkte) %>%
  mutate(Produkt = reorder(Produkt, MoM, median)) %>%
  ggplot(aes(x = MoM, y = Produkt)) +
  geom_boxplot(fill = "#E1F5EE", color = "#0F6E56", linewidth = 0.4, outlier.size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank()) +
  labs(
    title = "Verteilung der monatlichen Preisveränderung – Top-20-Produkte",
    x     = "MoM (%)", y = NULL
  )

 ggsave("outputs/boxplot_top20_mom.png", p_boxplot,
      width = 12, height = 8, dpi = 150)

# -----------------------------
# 4) t-Test: frühe vs späte Periode
# -----------------------------
period_data <- destatis %>%
  filter(!is.na(MoM), !is.na(Datum)) %>%
  mutate(
    Periode = case_when(
      year(Datum) <= 2022 ~ "frueh",
      year(Datum) >= 2023 ~ "spaet",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Periode))

ttest_period <- t.test(MoM ~ Periode, data = period_data)
print(ttest_period)

ttest_period_tbl <- tibble(
  statistik = unname(ttest_period$statistic),
  p_wert = ttest_period$p.value,
  conf_low = ttest_period$conf.int[1],
  conf_high = ttest_period$conf.int[2],
  mean_frueh = unname(ttest_period$estimate[1]),
  mean_spaet = unname(ttest_period$estimate[2])
)

print(ttest_period_tbl)

# -----------------------------
# 5) Produkt-Zusammenfassung
# -----------------------------
produkt_summary <- destatis %>%
  group_by(Produkt) %>%
  summarise(
    n = sum(!is.na(MoM)),
    mean_mom = mean(MoM, na.rm = TRUE),
    median_mom = median(MoM, na.rm = TRUE),
    sd_mom = sd(MoM, na.rm = TRUE),
    min_mom = min(MoM, na.rm = TRUE),
    max_mom = max(MoM, na.rm = TRUE),
    start_preis = first(Preisindex[!is.na(Preisindex)]),
    end_preis = last(Preisindex[!is.na(Preisindex)]),
    preisanstieg_pct = 100 * (end_preis / start_preis - 1),
    .groups = "drop"
  ) %>%
  arrange(desc(preisanstieg_pct))

print(produkt_summary)

# -----------------------------
# 6) Optional: CPI laden + Korrelation
# -----------------------------
if (file.exists("data_raw/cpi_de.csv")) {
  cpi <- read_csv("data_raw/cpi_de.csv", show_col_types = FALSE) %>%
    mutate(
      Datum = ymd(Datum),
      CPI = as.numeric(CPI)
    ) %>%
    arrange(Datum) %>%
    mutate(
      CPI_MoM = 100 * (CPI / lag(CPI) - 1)
    )

  merged <- destatis %>%
    left_join(cpi %>% select(Datum, CPI, CPI_MoM), by = "Datum")

  corr_all <- merged %>%
    filter(!is.na(MoM), !is.na(CPI_MoM)) %>%
    summarise(
      cor_mom_cpi = cor(MoM, CPI_MoM, method = "pearson")
    )

  print(corr_all)

  corr_produkt <- merged %>%
    filter(!is.na(MoM), !is.na(CPI_MoM), !is.na(Produkt)) %>%
    group_by(Produkt) %>%
    summarise(
      n = n(),
      cor_mom_cpi = cor(MoM, CPI_MoM, method = "pearson"),
      .groups = "drop"
    ) %>%
    arrange(desc(abs(cor_mom_cpi)))

  print(corr_produkt)

  dir.create("outputs", showWarnings = FALSE)
  write_csv(corr_all, "outputs/corr_mom_cpi_all.csv")
  write_csv(corr_produkt, "outputs/corr_mom_cpi_by_produkt.csv")
}

# -----------------------------
# 7) Export
# -----------------------------
dir.create("outputs", showWarnings = FALSE)

write_csv(anova_tbl, "outputs/anova_mom_produkt.csv")
write_csv(tukey_tbl, "outputs/tukey_mom_produkt.csv")
write_csv(ttest_period_tbl, "outputs/ttest_period_mom.csv")
write_csv(produkt_summary, "outputs/mom_summary_by_produkt.csv")
write_csv(tukey_tbl, "outputs/tukey_mom_top20.csv")

p <- anova_data %>%
  ggplot(aes(x = Produkt, y = MoM)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(
    title = "Monatliche Preisveränderung (MoM) nach Produkt",
    x = "Produkt",
    y = "MoM (%)"
  )

ggsave("outputs/boxplot_mom_produkt.png", p, width = 12, height = 6, dpi = 150)

cat("Fertig. Ergebnisse liegen in /outputs\n")


# Ein Welch-Zweistichproben-t-Test wurde verwendet, um die durchschnittliche monatliche Preisveränderung (MoM) zwischen einer frühen Periode (bis 2022) und einer späten Periode (ab 2023) zu vergleichen.
# Die mittlere monatliche Preisänderung war in der frühen Periode mit 0.684 % höher als in der späten Periode mit 0.345 %.
# Ergebnis heißt: früher waren die monatlichen Preisänderungen signifikant höher als später
#
# Interpretation:
# Wenn der p-Wert kleiner als 0.05 ist, gilt der Unterschied meist als statistisch signifikant
# Hier ist er deutlich kleiner als 0.05
#  Das heißt:
# Die mittlere monatliche Preisveränderung in der frühen Periode und in der späten Periode unterscheidet sich signifikant.



