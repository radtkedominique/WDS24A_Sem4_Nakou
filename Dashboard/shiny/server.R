# =============================================================================
# shiny/server.R  –  Person C: Shiny Dashboard Server
# Lebensmittelpreise & Inflation in Deutschland
# =============================================================================

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(zoo)

# =============================================================================
# Pfad-Hilfsfunktion (App kann aus shiny/ oder Projektroot gestartet werden)
# =============================================================================
proj_path <- function(...) {
  base <- if (file.exists("data_processed")) "." else ".."
  file.path(base, ...)
}

# =============================================================================
# COICOP-Mapping (identisch zu Analysis-Scripts)
# =============================================================================
COICOP_MAP <- c(
  "111" = "Getreide & Backwaren",   "112" = "Fleisch & Wurst",
  "113" = "Fisch & Meeresfrüchte",  "114" = "Milch & Milchgetränke",
  "115" = "Öle & Fette",            "116" = "Obst & Obstkonserven",
  "117" = "Gemüse & Gemüsekonserven","118" = "Zucker & Süßwaren",
  "119" = "Sonstige Nahrungsmittel","121" = "Kaffee, Tee & Kakao",
  "122" = "Säfte & Softdrinks"
)

# =============================================================================
# Dunkles Plotly-Layout (passend zum dark Theme)
# =============================================================================
DARK_BG   <- "#2A2928"   # = card-bg
DARK_FG   <- "#ECEAE4"
DARK_GRID <- "#3A3937"
DARK_MUTE <- "#9A968E"

dark_layout <- function(p) {
  plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font   = list(color = DARK_FG, family = "Inter"),
    xaxis  = list(gridcolor = DARK_GRID, zerolinecolor = DARK_GRID,
                  linecolor = DARK_GRID, tickcolor = DARK_MUTE),
    yaxis  = list(gridcolor = DARK_GRID, zerolinecolor = DARK_GRID,
                  linecolor = DARK_GRID, tickcolor = DARK_MUTE),
    legend = list(font = list(color = DARK_FG)),
    hoverlabel = list(bgcolor = "#353432", bordercolor = DARK_GRID,
                      font = list(color = DARK_FG))
  ) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

# Feste Akzent-Palette (funktioniert ab 1 Produkt – anders als brewer.pal,
# das mind. 3 Farben braucht). Reicht für bis zu 8 Produkte.
PALETTE <- c("#7FB096", "#E0A87E", "#8A9CC4", "#C98A9E",
             "#B0A878", "#7EB8B0", "#C7906E", "#9A8FB8")

farbpalette <- function(n) {
  n <- max(1, n)
  rep(PALETTE, length.out = n)
}

server <- function(input, output, session) {

  # ─── 0. Daten laden (einmalig) ─────────────────────────────────────────────
  df_raw <- reactive({
    read_csv(proj_path("data_processed/destatis_statistik.csv"),
             show_col_types = FALSE) |>
      mutate(
        Datum         = as.Date(Datum),
        Preisindex    = as.numeric(Preisindex),
        coicop_sub    = str_sub(`COICOP-Index`, 7, 9),
        Kategorie     = COICOP_MAP[coicop_sub],
        Kategorie     = if_else(is.na(Kategorie), "Sonstiges", Kategorie),
        t             = interval(as.Date("2020-01-01"), Datum) %/% months(1),
        Periode       = case_when(
          year(Datum) <= 2021 ~ "Pre-Inflation",
          year(Datum) <= 2022 ~ "Hochinflation",
          TRUE                ~ "Normalisierung"
        )
      ) |>
      filter(!is.na(Datum)) |>          # Summenzeilen ohne Datum entfernen
      group_by(Produkt) |>
      arrange(Datum) |>
      mutate(MoM = 100 * (Preisindex / lag(Preisindex) - 1)) |>
      ungroup()
  })

  features <- reactive({
    read_csv(proj_path("data_processed/features_destatis_lag12.csv"),
             show_col_types = FALSE) |>
      distinct(Produkt, Risikogruppe, preisanstieg_pct)
  })

  cpi <- reactive({
    read_csv(proj_path("data_raw/cpi_de.csv"), show_col_types = FALSE) |>
      mutate(Datum = as.Date(Datum), CPI = as.numeric(CPI)) |>
      arrange(Datum) |>
      mutate(CPI_MoM = 100 * (CPI / lag(CPI) - 1))
  })

  df <- reactive({
    df_raw() |>
      left_join(features(), by = "Produkt")
  })

  # Output-Tabellen (aus Analysis-Scripts)
  load_csv <- function(pfad) {
    full <- proj_path(pfad)
    if (file.exists(full)) read_csv(full, show_col_types = FALSE) else tibble()
  }

  trend_stats   <- reactive({ load_csv("output/tabellen/trend_stats.csv") })
  arima_prog    <- reactive({ load_csv("output/tabellen/arima_prognosen.csv") |>
                              mutate(Datum = as.Date(Datum)) })
  arima_valid   <- reactive({
    d <- load_csv("output/tabellen/arima_validierung.csv")
    if (nrow(d) > 0) d <- mutate(d, Datum = as.Date(Datum))
    d
  })
  arima_guete   <- reactive({ load_csv("output/tabellen/arima_guete.csv") })
  kor_cpi       <- reactive({ load_csv("output/tabellen/korrelation_cpi.csv") })
  reg_koeff     <- reactive({ load_csv("output/tabellen/regression_m3_koeff.csv") })
  desk_kat      <- reactive({ load_csv("output/tabellen/deskriptiv_je_kategorie.csv") })
  mom_summary   <- reactive({
    p <- proj_path("outputs/mom_summary_by_produkt.csv")
    if (file.exists(p)) read_csv(p, show_col_types = FALSE) else tibble()
  })

  # ─── 1. Filter-Dropdowns ───────────────────────────────────────────────────
  observe({
    kats <- sort(unique(df()$Kategorie))
    updateSelectInput(session, "sel_kategorie",
                      choices = c("Alle Kategorien" = "alle", kats),
                      selected = "alle")
    updateSelectInput(session, "rechner_produkt",
                      choices = sort(unique(df()$Produkt)))
  })

  observeEvent(input$sel_kategorie, {
    prods <- if (input$sel_kategorie == "alle") {
      sort(unique(df()$Produkt))
    } else {
      df() |> filter(Kategorie == input$sel_kategorie) |>
        pull(Produkt) |> unique() |> sort()
    }
    updateSelectizeInput(session, "sel_produkte",
                         choices  = prods,
                         selected = head(prods, 3),
                         server   = TRUE)
  })

  # ─── Reaktive gefilterte Daten ─────────────────────────────────────────────
  df_filt <- reactive({
    d <- df() |>
      filter(Datum >= input$zeitraum[1], Datum <= input$zeitraum[2])
    if (!is.null(input$sel_produkte) && length(input$sel_produkte) > 0)
      d <- d |> filter(Produkt %in% input$sel_produkte)
    d
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # TAB 1: PREISTRENDS
  # ═══════════════════════════════════════════════════════════════════════════
  output$plot_trend <- renderPlotly({
    d <- df_filt()
    req(nrow(d) > 0)

    farben <- farbpalette(length(input$sel_produkte))

    p <- plot_ly()
    for (i in seq_along(unique(d$Produkt))) {
      prod <- unique(d$Produkt)[i]
      pd   <- d |> filter(Produkt == prod) |> arrange(Datum)
      p <- p |> add_lines(
        data = pd, x = ~Datum, y = ~Preisindex,
        name = prod,
        line = list(color = farben[min(i, length(farben))], width = 2),
        hovertemplate = paste0("<b>", prod, "</b><br>%{x|%b %Y}: %{y:.1f}<extra></extra>")
      )
    }

    # CPI einblenden
    if (isTRUE(input$t_show_cpi)) {
      cpi_f <- cpi() |>
        filter(Datum >= input$zeitraum[1], Datum <= input$zeitraum[2])
      p <- p |> add_lines(
        data = cpi_f, x = ~Datum, y = ~CPI,
        name = "Gesamt-CPI",
        line = list(color = "#F2B544", dash = "dash", width = 3),
        hovertemplate = "CPI: %{y:.1f}<extra></extra>"
      )
    }

    p |> layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Preisindex (Jan 2020 = 100)"),
      hovermode  = "x unified",
      legend     = list(orientation = "h", y = -0.2)
    ) |> dark_layout()
  })

  output$plot_saison <- renderPlotly({
    d <- df_filt() |>
      mutate(Monat = month(Datum, label = TRUE)) |>
      group_by(Produkt, Monat) |>
      summarise(mean_idx = mean(Preisindex, na.rm = TRUE), .groups = "drop") |>
      group_by(Produkt) |>
      mutate(abweichung = mean_idx - mean(mean_idx)) |>
      ungroup()
    req(nrow(d) > 0)

    plot_ly(d, x = ~Monat, y = ~abweichung, color = ~Produkt,
            type = "scatter", mode = "lines+markers",
            hovertemplate = "%{x}: %{y:+.2f}<extra>%{fullData.name}</extra>") |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Abweichung vom Jahresmittel"),
        hovermode = "x unified"
      ) |> dark_layout()
  })

  output$plot_mom <- renderPlotly({
    d <- df_filt() |> filter(!is.na(MoM))
    req(nrow(d) > 0)

    plot_ly(d, x = ~Datum, y = ~MoM, color = ~Produkt,
            type = "bar",
            hovertemplate = "%{x|%b %Y}: %{y:+.2f}%<extra>%{fullData.name}</extra>") |>
      layout(
        barmode = "overlay",
        xaxis   = list(title = ""),
        yaxis   = list(title = "MoM (%)"),
        hovermode = "x unified"
      ) |> dark_layout()
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # TAB 2: KATEGORIEN
  # ═══════════════════════════════════════════════════════════════════════════
  output$plot_anova_box <- renderPlotly({
    d <- df() |>
      filter(!is.na(preisanstieg_pct)) |>
      distinct(Produkt, Kategorie, Risikogruppe, preisanstieg_pct)
    req(nrow(d) > 0)

    plot_ly(d |> mutate(Kategorie = reorder(Kategorie, preisanstieg_pct, median)),
            x = ~preisanstieg_pct, y = ~Kategorie,
            type = "box", boxpoints = "suspectedoutliers",
            color = ~Kategorie,
            hovertemplate = "<b>%{y}</b><br>Preisanstieg: %{x:.1f}%<extra></extra>") |>
      layout(
        xaxis = list(title = "Preisanstieg Jan 2020 → Jun 2025 (%)"),
        yaxis = list(title = ""),
        showlegend = FALSE
      ) |> dark_layout()
  })

  output$plot_risikogruppe <- renderPlotly({
    d <- df() |>
      filter(!is.na(Risikogruppe), !is.na(preisanstieg_pct)) |>
      distinct(Produkt, Risikogruppe, preisanstieg_pct)

    plot_ly(d, x = ~Risikogruppe, y = ~preisanstieg_pct,
            color = ~Risikogruppe,
            type = "box", boxpoints = "all", jitter = 0.3,
            colors = c("hoch" = "#e63946", "mittel" = "#f4a261", "stabil" = "#2a9d8f"),
            hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<extra></extra>") |>
      layout(
        xaxis = list(title = "Risikogruppe"),
        yaxis = list(title = "Preisanstieg (%)"),
        showlegend = FALSE
      ) |> dark_layout()
  })

  output$anova_output <- renderPrint({
    d <- df() |>
      filter(!is.na(preisanstieg_pct)) |>
      distinct(Produkt, Kategorie, preisanstieg_pct)
    m <- aov(preisanstieg_pct ~ Kategorie, data = d)
    cat("ANOVA: Preisanstieg % ~ Kategorie\n")
    print(summary(m))
    eta2 <- summary(m)[[1]]$`Sum Sq`[1] / sum(summary(m)[[1]]$`Sum Sq`)
    cat(sprintf("\nEta² = %.4f\n", eta2))
  })

  output$deskriptiv_table <- renderDT({
    req(nrow(desk_kat()) > 0)
    d <- desk_kat()
    num_cols <- names(d)[sapply(d, is.numeric)]   # echte numerische Spalten
    dt <- datatable(d,
                    filter = "top",
                    options = list(pageLength = 12, scrollX = TRUE),
                    rownames = FALSE)
    if (length(num_cols) > 0)
      dt <- formatRound(dt, columns = num_cols, digits = 2)
    dt
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # TAB 3: INFLATION
  # ═══════════════════════════════════════════════════════════════════════════
  output$plot_cpi <- renderPlotly({
    avg <- df() |>
      filter(Datum >= input$zeitraum[1], Datum <= input$zeitraum[2]) |>
      group_by(Datum) |>
      summarise(avg_idx = mean(Preisindex, na.rm = TRUE), .groups = "drop")

    cpi_f <- cpi() |>
      filter(Datum >= input$zeitraum[1], Datum <= input$zeitraum[2])

    plot_ly() |>
      add_lines(data = avg, x = ~Datum, y = ~avg_idx,
                name = "Ø Preisindex (alle Produkte)",
                line = list(color = "#7FB096", width = 2.5)) |>
      add_lines(data = cpi_f, x = ~Datum, y = ~CPI,
                name = "Gesamt-CPI",
                line = list(color = "#F2B544", dash = "dash", width = 3)) |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Preisindex (Basis Jan 2020 = 100)"),
        hovermode = "x unified"
      ) |> dark_layout()
  })

  output$plot_kor_cpi <- renderPlotly({
    req(nrow(kor_cpi()) > 0)
    d <- kor_cpi()

    # Auf gewählte Kategorie filtern (Kategorie-Filter wird hier wirksam)
    if (!is.null(input$sel_kategorie) && input$sel_kategorie != "alle") {
      d <- d |> filter(Kategorie == input$sel_kategorie)
    }
    req(nrow(d) > 0)

    d <- d |>
      arrange(desc(r_cpi_niveau)) |>
      head(20) |>
      mutate(Produkt = reorder(Produkt, r_cpi_niveau))

    # Achse auf tatsächlichen Bereich zoomen, damit kleine Unterschiede sichtbar sind
    r_min <- min(d$r_cpi_niveau, na.rm = TRUE)
    r_max <- max(d$r_cpi_niveau, na.rm = TRUE)
    spanne <- max(r_max - r_min, 0.01)
    achse_von <- max(0, r_min - spanne * 0.15)   # etwas Luft links
    achse_bis <- min(1, r_max + spanne * 0.05)

    plot_ly(d, x = ~r_cpi_niveau, y = ~Produkt,
            type = "bar", orientation = "h",
            color = ~Kategorie,
            hovertemplate = "<b>%{y}</b><br>r = %{x:.3f}<extra></extra>") |>
      layout(
        xaxis = list(title = "Pearson r (Preisindex ~ CPI)",
                     range = c(achse_von, achse_bis)),
        yaxis = list(title = ""),
        margin = list(l = 200)
      ) |> dark_layout()
  })

  output$ttest_output <- renderUI({
    pfad <- proj_path("output/tabellen/ttest_ergebnisse.csv")
    if (!file.exists(pfad)) {
      return(p(style = "color:#9A968E;", "Noch nicht vorhanden – bitte 04_regression.R ausführen."))
    }
    d <- read_csv(pfad, show_col_types = FALSE)

    # Hilfsfunktion: ein Ergebnis als kleine "Karte"
    test_karte <- function(titel, t, p_wert, ci_low, ci_high, deutung) {
      signif_txt <- if (p_wert < 0.001) "hoch signifikant (p < 0,001)"
                    else if (p_wert < 0.05) sprintf("signifikant (p = %.3f)", p_wert)
                    else sprintf("nicht signifikant (p = %.3f)", p_wert)
      signif_farbe <- if (p_wert < 0.05) "#7FB096" else "#C98A9E"

      div(style = "padding:0.7rem 0.9rem; margin-bottom:0.7rem; background:#26302A;
                   border-left:3px solid #7FB096; border-radius:0.4rem;",
        div(style = "font-weight:600; color:#ECEAE4; margin-bottom:0.3rem;", titel),
        div(style = "font-size:0.85rem; color:#C9C5BD;", deutung),
        div(style = paste0("font-size:0.82rem; margin-top:0.35rem; color:", signif_farbe, ";"),
            "→ ", signif_txt),
        div(style = "font-size:0.76rem; color:#7E7A72; margin-top:0.2rem;",
            sprintf("t = %.1f · 95%%-Konfidenzintervall [%.1f; %.1f]", t, ci_low, ci_high))
      )
    }

    karten <- list()

    # Test 1: Hochinflation vs. Pre
    r1 <- d[grepl("Hochinflation", d$Test), ][1, ]
    if (nrow(r1) == 1 && !is.na(r1$t)) {
      karten <- c(karten, list(test_karte(
        "Preise stiegen in der Hochinflation",
        r1$t, r1$p_wert, r1$ci_low, r1$ci_high,
        "Der durchschnittliche Preisindex war während der Hochinflationsphase deutlich höher als davor."
      )))
    }
    # Test 2: Normalisierung vs Hoch
    r2 <- d[grepl("Normalisierung", d$Test), ][1, ]
    if (nrow(r2) == 1 && !is.na(r2$t)) {
      karten <- c(karten, list(test_karte(
        "Preise blieben nach dem Peak hoch",
        r2$t, r2$p_wert, r2$ci_low, r2$ci_high,
        "Auch in der Normalisierungsphase liegen die Preise weiterhin signifikant über dem Hochinflations-Niveau – die Preise sind nicht zurückgegangen."
      )))
    }
    # Test 3: MoM früh vs. spät (aus stats_mvp.R)
    mom_pfad <- proj_path("outputs/ttest_period_mom.csv")
    if (file.exists(mom_pfad)) {
      m <- read_csv(mom_pfad, show_col_types = FALSE)
      karten <- c(karten, list(
        div(style = "padding:0.7rem 0.9rem; background:#26302A;
                     border-left:3px solid #E0A87E; border-radius:0.4rem;",
          div(style = "font-weight:600; color:#ECEAE4; margin-bottom:0.3rem;",
              "Monatliche Anstiege waren früher stärker"),
          div(style = "font-size:0.85rem; color:#C9C5BD;",
              sprintf("Frühe Phase (≤2022): Ø +%.2f%% pro Monat – späte Phase (≥2023): Ø +%.2f%% pro Monat. Das Tempo der Teuerung hat sich also verlangsamt.",
                      m$mean_frueh[1], m$mean_spaet[1])),
          div(style = "font-size:0.82rem; margin-top:0.35rem; color:#7FB096;",
              sprintf("→ hoch signifikant (p < 0,001, t = %.1f)", m$statistik[1]))
        )
      ))
    }

    tagList(karten)
  })

  output$regression_table <- renderDT({
    req(nrow(reg_koeff()) > 0)

    monatsnamen <- c("Januar","Februar","März","April","Mai","Juni",
                     "Juli","August","September","Oktober","November","Dezember")

    # Kryptische Faktornamen in Klartext übersetzen
    uebersetze <- function(term) {
      if (term == "(Intercept)")
        return("Ausgangswert (Januar, Start 2020)")
      if (term == "t")
        return("Zeittrend (pro Monat)")
      m <- regmatches(term, regexpr("[0-9]+", term))
      if (grepl("Monat_fakt", term) && length(m) == 1)
        return(paste0("Monat: ", monatsnamen[as.integer(m)], " (vs. Januar)"))
      if (grepl("Periode", term))
        return(paste0("Phase: ", sub("Periode", "", term)))
      term
    }

    d <- reg_koeff() |>
      mutate(
        Einflussfaktor = vapply(term, uebersetze, character(1)),
        Effekt         = round(estimate, 2),
        Signifikanz    = case_when(
          p.value < 0.001 ~ "★★★",
          p.value < 0.01  ~ "★★",
          p.value < 0.05  ~ "★",
          TRUE            ~ "–"
        )
      ) |>
      select(Einflussfaktor, Effekt, Signifikanz)

    datatable(
      d,
      options  = list(pageLength = 15, dom = "t"),
      rownames = FALSE,
      colnames = c("Einflussfaktor", "Effekt auf Preisindex", "Signifikanz")
    )
  })

  # ═══════════════════════════════════════════════════════════════════════════
  # TAB 4: PROGNOSE
  # ═══════════════════════════════════════════════════════════════════════════
  output$plot_arima <- renderPlotly({
    req(length(input$sel_produkte) > 0)

    hist_d <- df() |>
      filter(Produkt %in% input$sel_produkte,
             Datum   >= input$zeitraum[1]) |>
      arrange(Produkt, Datum)

    farben <- farbpalette(length(input$sel_produkte))
    p <- plot_ly()

    # ── Y-Achsen-Obergrenze robust bestimmen (Ausreißer abschneiden) ──────────
    # Basis: historische Werte + Punktprognosen der gewählten Produkte
    werte_hist <- hist_d$Preisindex
    werte_prog <- if (nrow(arima_prog()) > 0)
      arima_prog() |> filter(Produkt %in% input$sel_produkte) |> pull(prognose)
    else numeric(0)
    basis_max <- max(c(werte_hist, werte_prog), na.rm = TRUE)
    basis_min <- min(c(werte_hist, werte_prog), na.rm = TRUE)
    # Deckel: 25% Luft über dem höchsten echten/prognostizierten Wert
    y_cap <- basis_max * 1.25
    y_unten <- basis_min * 0.9

    for (i in seq_along(input$sel_produkte)) {
      prod  <- input$sel_produkte[i]
      farbe <- farben[min(i, length(farben))]

      h <- hist_d |> filter(Produkt == prod)
      p <- p |> add_lines(
        data = h, x = ~Datum, y = ~Preisindex, name = prod,
        line = list(color = farbe, width = 2),
        legendgroup = prod
      )

      if (nrow(arima_prog()) > 0) {
        pr <- arima_prog() |> filter(Produkt == prod)
        if (nrow(pr) > 0) {
          # Konfidenzbänder nur wenn Haken gesetzt
          if (isTRUE(input$p_show_ki)) {
            p <- p |>
              add_ribbons(data = pr, x = ~Datum, ymin = ~lo95, ymax = ~hi95,
                          fillcolor = paste0(farbe, "22"),
                          line = list(color = "transparent"),
                          legendgroup = prod, showlegend = FALSE, inherit = FALSE) |>
              add_ribbons(data = pr, x = ~Datum, ymin = ~lo80, ymax = ~hi80,
                          fillcolor = paste0(farbe, "44"),
                          line = list(color = "transparent"),
                          legendgroup = prod, showlegend = FALSE, inherit = FALSE)
          }
          p <- p |>
            add_lines(data = pr, x = ~Datum, y = ~prognose,
                      line = list(color = farbe, dash = "dash", width = 2),
                      name = paste(prod, "Prognose"),
                      legendgroup = prod,
                      hovertemplate = paste0(prod, " Prog.: %{y:.1f}<extra></extra>"),
                      inherit = FALSE)
        }
      }

      # Validierungs-Layer: Hold-out-Prognose (gepunktet) vs. echte Werte (Marker)
      if (isTRUE(input$p_show_validierung) && nrow(arima_valid()) > 0) {
        va <- arima_valid() |> filter(Produkt == prod)
        if (nrow(va) > 0) {
          p <- p |>
            add_lines(data = va, x = ~Datum, y = ~prognose_val,
                      line = list(color = farbe, dash = "dot", width = 2),
                      name = paste(prod, "Validierung (Prognose)"),
                      legendgroup = prod,
                      hovertemplate = paste0(prod, " vorhergesagt: %{y:.1f}<extra></extra>"),
                      inherit = FALSE) |>
            add_markers(data = va, x = ~Datum, y = ~echt,
                        marker = list(color = farbe, size = 7, symbol = "circle-open",
                                      line = list(width = 2)),
                        name = paste(prod, "echte Werte"),
                        legendgroup = prod,
                        hovertemplate = paste0(prod, " echt: %{y:.1f}<extra></extra>"),
                        inherit = FALSE)
        }
      }
    }

    # Y-Achse: bei aktivem Deckel feste Range, sonst automatisch
    yaxis_cfg <- if (isTRUE(input$p_cap_yachse)) {
      list(title = "Preisindex", range = c(y_unten, y_cap))
    } else {
      list(title = "Preisindex")
    }

    p |> layout(
      xaxis     = list(title = ""),
      yaxis     = yaxis_cfg,
      hovermode = "x unified",
      shapes    = list(list(
        type = "line",
        x0 = "2025-06-01", x1 = "2025-06-01",
        y0 = 0, y1 = 1, yref = "paper",
        line = list(color = "#7E7A72", dash = "dot")
      ))
    ) |> dark_layout()
  })

  output$arima_guete_table <- renderTable({
    req(nrow(arima_guete()) > 0, length(input$sel_produkte) > 0)
    arima_guete() |>
      filter(Produkt %in% input$sel_produkte) |>
      transmute(
        Produkt,
        Risikogruppe,
        `Treffsicherheit` = case_when(
          mape < 3  ~ "sehr gut",
          mape < 5  ~ "gut",
          mape < 10 ~ "okay",
          TRUE      ~ "ungenau"
        ),
        `Ø Fehler (%)` = round(mape, 1),
        `Modelltyp`    = modell
      )
  }, striped = TRUE, hover = TRUE, width = "100%")

  # ═══════════════════════════════════════════════════════════════════════════
  # TAB 5: PREIS-RECHNER
  # ═══════════════════════════════════════════════════════════════════════════
  rechner_result <- eventReactive(input$rechner_btn, {
    req(input$rechner_produkt, input$rechner_preis)

    d <- df() |>
      filter(Produkt == input$rechner_produkt) |>
      arrange(Datum)

    ref_row  <- d |> filter(format(Datum, "%Y-%m") ==
                              format(input$rechner_ref_datum,  "%Y-%m"))
    ziel_row <- d |> filter(format(Datum, "%Y-%m") ==
                              format(input$rechner_ziel_datum, "%Y-%m"))

    # Zielmonat in Prognose-Tabelle suchen falls nicht in historischen Daten
    if (nrow(ziel_row) == 0 && nrow(arima_prog()) > 0) {
      ziel_row <- arima_prog() |>
        filter(Produkt == input$rechner_produkt,
               format(Datum, "%Y-%m") == format(input$rechner_ziel_datum, "%Y-%m")) |>
        rename(Preisindex = prognose) |>
        mutate(ist_prognose = TRUE)
    }

    req(nrow(ref_row) > 0, nrow(ziel_row) > 0)

    faktor     <- ziel_row$Preisindex[1] / ref_row$Preisindex[1]
    preis_neu  <- input$rechner_preis * faktor

    list(
      preis_alt  = input$rechner_preis,
      preis_neu  = preis_neu,
      idx_ref    = ref_row$Preisindex[1],
      idx_ziel   = ziel_row$Preisindex[1],
      pct        = (faktor - 1) * 100,
      ref_label  = format(input$rechner_ref_datum,  "%b %Y"),
      ziel_label = format(input$rechner_ziel_datum, "%b %Y"),
      df_hist    = d
    )
  })

  output$rechner_ergebnis <- renderUI({
    r <- rechner_result()
    farbe  <- if (r$pct > 0) "danger" else "success"
    pfeil  <- if (r$pct > 0) "↑" else "↓"
    tagList(
      div(class = "text-center my-3",
          h3(sprintf("%.2f €", r$preis_alt), style = "color: #9A968E"),
          h2(icon("arrow-right"), sprintf(" %.2f €", r$preis_neu),
             style = "color: #ECEAE4"),
          tags$span(class = paste("badge bg-" , farbe, "fs-5"),
                    sprintf("%s %.1f%%", pfeil, abs(r$pct))),
          p(class = "mt-2", style = "color:#9A968E;",
            sprintf("Index: %.1f → %.1f  (%s → %s)",
                    r$idx_ref, r$idx_ziel, r$ref_label, r$ziel_label))
      )
    )
  })

  output$rechner_chart <- renderPlotly({
    r <- rechner_result()
    hist_r <- r$df_hist |>
      filter(!is.na(Preisindex)) |>
      mutate(preis_real = (Preisindex / r$idx_ref) * r$preis_alt)

    plot_ly(hist_r, x = ~Datum, y = ~preis_real,
            type = "scatter", mode = "lines",
            name = input$rechner_produkt,
            line = list(color = "#7FB096", width = 2),
            hovertemplate = "%{x|%b %Y}: %{y:.2f} €<extra></extra>") |>
      add_markers(x = c(input$rechner_ref_datum, input$rechner_ziel_datum),
                  y = c(r$preis_alt, r$preis_neu),
                  marker = list(color = c("#7FB096", "#E0876E"), size = 10),
                  name = "Referenz / Ziel",
                  hovertemplate = "%{x|%b %Y}: %{y:.2f} €<extra></extra>",
                  inherit = FALSE) |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = sprintf("Preis (€), Basis %.2f € = %s",
                                     r$preis_alt, r$ref_label))
      ) |> dark_layout()
  })
}
