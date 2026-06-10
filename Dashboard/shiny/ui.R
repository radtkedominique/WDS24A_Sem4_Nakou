# =============================================================================
# shiny/ui.R  –  Person C: Shiny Dashboard UI  (modern-dark Redesign)
# Lebensmittelpreise & Inflation in Deutschland
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)

# ── Design-Token: modern-dark (Claude-inspiriert) ────────────────────────────
#   bg_app    #1F1E1D  – tiefer warmer Anthrazit (App-Hintergrund)
#   bg_card   #2A2928  – etwas heller (Cards / Panels)
#   bg_input  #353432  – Inputs / Hover
#   line      #3A3937  – feine Trennlinien
#   fg        #ECEAE4  – warmes Off-White (Text)
#   muted     #9A968E  – gedämpfter Text
#   accent    #7FB096  – weiches Grün (Akzent, lesbar auf dunkel)
theme_dark_food <- bs_theme(
  version      = 5,
  bg           = "#1F1E1D",
  fg           = "#ECEAE4",
  primary      = "#7FB096",
  secondary    = "#9A968E",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  "font-size-base"    = "0.92rem",
  "border-radius"     = "0.65rem",
  "card-border-width" = "1px",
  "card-border-color" = "#3A3937",
  "card-cap-bg"       = "#2A2928",
  "card-bg"           = "#2A2928"
)

tab_filterbar <- function(...) div(class = "tab-filterbar", ...)

ui <- page_navbar(
  title = span(class = "brand", "Lebensmittelpreise & Inflation"),
  theme = theme_dark_food,
  fillable = FALSE,

  header = tags$head(
    tags$style(HTML("
      /* ---- Grundlayout ---- */
      body { letter-spacing: -0.01em; background: #1F1E1D; }
      .bslib-page-navbar > .navbar { border-bottom: 1px solid #3A3937;
                box-shadow: none !important; background: #1A1918 !important; }
      .brand { font-weight: 600; font-size: 1.05rem; letter-spacing: -0.02em;
               color: #ECEAE4; }
      .navbar .nav-link { color: #9A968E !important; font-weight: 450;
               border-radius: 0.5rem; padding: 0.4rem 0.85rem !important; }
      .navbar .nav-link:hover { color: #ECEAE4 !important; background: #2A2928; }
      .navbar .nav-link.active { color: #7FB096 !important; background: #26302A; }

      /* ---- Cards ---- */
      .card { box-shadow: none !important; border: 1px solid #3A3937 !important;
              background: #2A2928 !important; }
      .card-header { font-weight: 550; font-size: 0.95rem; color: #ECEAE4;
              background: #2A2928 !important; border-bottom: 1px solid #353432;
              padding: 0.85rem 1.1rem; letter-spacing: -0.01em; }
      .card-body { padding: 1.1rem; }
      .card-footer { background: transparent; border-top: 1px solid #353432;
              color: #7E7A72; font-size: 0.8rem; }

      /* ---- Sidebar ---- */
      .sidebar, .bslib-sidebar-layout > .sidebar {
              background: #1A1918 !important; border-right: 1px solid #3A3937; }
      .sidebar .form-label, .control-label { font-weight: 500; font-size: 0.82rem;
              color: #9A968E; text-transform: uppercase; letter-spacing: 0.04em; }
      .sidebar-title { color: #ECEAE4 !important; font-weight: 600; }

      /* ---- Tab-interner Filterbalken ---- */
      .tab-filterbar { display: flex; flex-wrap: wrap; gap: 1.5rem; align-items: center;
              padding: 0.5rem 0.2rem 1rem 0.2rem; }
      .tab-filterbar .form-group, .tab-filterbar .checkbox { margin-bottom: 0 !important; }
      .tab-filterbar .form-check-label { font-size: 0.86rem; color: #C9C5BD; }

      /* ---- Inputs ---- */
      .form-control, .selectize-input, .selectize-dropdown {
              background: #353432 !important; color: #ECEAE4 !important;
              border-color: #45433F !important; box-shadow: none !important;
              border-radius: 0.5rem !important; }
      .selectize-input.focus, .form-control:focus { border-color: #7FB096 !important; }
      .selectize-dropdown .active { background: #26302A !important; color: #ECEAE4 !important; }
      .selectize-input > .item { background: #26302A !important; color: #ECEAE4 !important;
              border-radius: 0.35rem; }
      /* Entfernen-Button (×) an Produkt-Tags */
      .selectize-input > .item > .remove { border-left-color: #3D5142 !important;
              color: #9CC5AE !important; padding: 0 6px; }
      .selectize-input > .item > .remove:hover { background: #3D5142 !important;
              color: #ECEAE4 !important; }
      .irs--shiny .irs-bar { background: #7FB096; border-color: #7FB096; }
      .irs--shiny .irs-handle { background: #ECEAE4; }
      .irs--shiny .irs-line { background: #45433F; }
      .irs--shiny .irs-min, .irs--shiny .irs-max, .irs--shiny .irs-from,
      .irs--shiny .irs-to, .irs--shiny .irs-single {
              background: #26302A; color: #ECEAE4; }

      /* checkbox accent */
      input[type=checkbox] { accent-color: #7FB096; }

      /* ---- Buttons ---- */
      .btn-success { background: #7FB096; border-color: #7FB096; color: #16201A;
              font-weight: 600; }
      .btn-success:hover { background: #6FA086; border-color: #6FA086; color: #16201A; }

      /* ---- Tabellen (DT) ---- */
      .dataTables_wrapper { font-size: 0.86rem; color: #C9C5BD; }
      table.dataTable { color: #C9C5BD !important; }
      table.dataTable thead th { font-weight: 600; font-size: 0.82rem; color: #ECEAE4;
              border-bottom: 1px solid #45433F !important; }
      table.dataTable tbody tr { background: #2A2928 !important; }
      table.dataTable tbody tr:hover { background: #353432 !important; }
      table.dataTable.stripe tbody tr.odd { background: #262524 !important; }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
              color: #C9C5BD !important; }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
              background: #353432; color: #ECEAE4; border: 1px solid #45433F; }

      /* ---- shiny renderTable (Modellgüte) ---- */
      .shiny-table { color: #C9C5BD; width: 100%; }
      .shiny-table thead th { color: #ECEAE4; border-bottom: 1px solid #45433F !important;
              border-top: none !important; font-weight: 600; }
      .shiny-table tbody td { border-top: 1px solid #353432 !important; }
      .table-striped > tbody > tr:nth-of-type(odd) > * {
              background: #262524 !important; color: #C9C5BD !important; }
      .table-hover > tbody > tr:hover > * { background: #353432 !important; }

      /* ---- verbatim / Code ---- */
      pre { background: #1A1918; border: 1px solid #3A3937; border-radius: 0.5rem;
            font-size: 0.82rem; color: #C9C5BD; }

      /* Scrollbar dezent */
      ::-webkit-scrollbar { width: 10px; height: 10px; }
      ::-webkit-scrollbar-thumb { background: #45433F; border-radius: 5px; }
      ::-webkit-scrollbar-track { background: #1F1E1D; }
    "))
  ),

  # ─── Globale Sidebar ──────────────────────────────────────────────────────
  sidebar = sidebar(
    title = "Filter",
    width = 270,

    selectInput("sel_kategorie", "Produktkategorie",
                choices = NULL, selected = NULL),

    selectizeInput("sel_produkte", "Produkte (1–5, einzeln entfernbar)",
                   choices = NULL, selected = NULL, multiple = TRUE,
                   options = list(maxItems = 5,
                                  placeholder = "Produkt wählen…",
                                  plugins = list("remove_button"))),

    sliderInput("zeitraum", "Zeitraum",
                min        = as.Date("2020-01-01"),
                max        = as.Date("2025-06-01"),
                value      = c(as.Date("2020-01-01"), as.Date("2025-06-01")),
                step       = 30,
                timeFormat = "%b %Y"),

    hr(),
    helpText(
      tags$small(
        style = "color:#7E7A72; line-height:1.6;",
        "Destatis / Open Food Facts", br(),
        "Jan 2020 – Jun 2025 · 65 Monate", br(),
        "168 Produkte · 3 Risikogruppen"
      )
    )
  ),

  # ─── Tab 1: Preistrends ───────────────────────────────────────────────────
  nav_panel(
    title = "Preistrends",
    card(
      card_header("Preisentwicklung im Zeitverlauf"),
      tab_filterbar(
        checkboxInput("t_show_cpi", "Gesamt-CPI einblenden", value = TRUE)
      ),
      plotlyOutput("plot_trend", height = "440px"),
      card_footer("Basis: Preisindex Jan 2020 = 100. Gepunktet: Gesamt-CPI.")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Saisonale Muster"),
        plotlyOutput("plot_saison", height = "300px"),
        card_footer("Abweichung vom Jahresmittel je Monat.")
      ),
      card(
        card_header("Monat-über-Monat-Veränderung"),
        plotlyOutput("plot_mom", height = "300px"),
        card_footer("Prozentuale Veränderung zum Vormonat.")
      )
    )
  ),

  # ─── Tab 2: Kategorien ────────────────────────────────────────────────────
  nav_panel(
    title = "Kategorien",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Preisanstieg nach Kategorie"),
        plotlyOutput("plot_anova_box", height = "420px"),
        card_footer("Jan 2020 → Jun 2025. Punkte: statistische Ausreißer.")
      ),
      card(
        card_header("Preisanstieg nach Risikogruppe"),
        plotlyOutput("plot_risikogruppe", height = "260px"),
        hr(style = "margin:0.4rem 0; border-color:#353432;"),
        verbatimTextOutput("anova_output")
      )
    ),
    card(
      card_header("Deskriptive Statistik je Kategorie"),
      DTOutput("deskriptiv_table")
    )
  ),

  # ─── Tab 3: Inflation & CPI ───────────────────────────────────────────────
  nav_panel(
    title = "Inflation & CPI",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Ø Preisindex vs. Gesamt-CPI"),
        plotlyOutput("plot_cpi", height = "360px")
      ),
      card(
        card_header("Stärkste Korrelation mit CPI"),
        plotlyOutput("plot_kor_cpi", height = "360px"),
        card_footer("Top 20 Produkte, Pearson-Korrelation (Niveau).")
      )
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("t-Test-Ergebnisse"),
        uiOutput("ttest_output")
      ),
      card(
        card_header("Lineare Regression"),
        DTOutput("regression_table"),
        card_footer(paste(
          "Wie viel höher (+) oder tiefer (−) der Preisindex liegt, wenn dieser Faktor zutrifft.",
          "Beispiel: Zeittrend +0,26 = pro Monat steigt der Index um 0,26 Punkte.",
          "★★★ = sehr sicher, – = statistisch nicht belegt."
        ))
      )
    )
  ),

  # ─── Tab 4: Prognose ──────────────────────────────────────────────────────
  nav_panel(
    title = "Prognose",
    card(
      card_header("ARIMA-Prognose: Juli 2025 – Juni 2026"),
      tab_filterbar(
        checkboxInput("p_show_ki",          "Konfidenzbänder", value = FALSE),
        checkboxInput("p_show_validierung",  "Validierung (Prognose vs. echt)", value = FALSE),
        checkboxInput("p_cap_yachse",        "Ausreißer abschneiden", value = TRUE)
      ),
      plotlyOutput("plot_arima", height = "460px"),
      card_footer(paste(
        "Gestrichelt: Zukunftsprognose. Gepunktet + Kreise: Hold-out-Validierung",
        "(Modell sagt die letzten 12 bekannten Monate voraus)."
      ))
    ),
    card(
      card_header("Wie zuverlässig ist die Prognose?"),
      tableOutput("arima_guete_table"),
      card_footer(paste(
        "Test: Das Modell musste die letzten 12 bekannten Monate vorhersagen, ohne sie zu kennen.",
        "Der Ø Fehler zeigt, wie weit es im Schnitt daneben lag – je kleiner, desto verlässlicher",
        "auch die Zukunftsprognose. Unter 5% gilt als gut."
      ))
    )
  ),

  # ─── Tab 5: Preis-Rechner ─────────────────────────────────────────────────
  nav_panel(
    title = "Preis-Rechner",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Eingabe"),
        selectInput("rechner_produkt", "Produkt", choices = NULL),
        numericInput("rechner_preis", "Preis im Referenzmonat (€)",
                     value = 1.99, min = 0.01, step = 0.10),
        dateInput("rechner_ref_datum", "Referenzmonat",
                  value = "2020-01-01", min = "2020-01-01", max = "2025-06-01",
                  format = "MM yyyy", startview = "year"),
        dateInput("rechner_ziel_datum", "Zielmonat",
                  value = "2025-06-01", min = "2020-01-01", max = "2026-06-01",
                  format = "MM yyyy", startview = "year"),
        actionButton("rechner_btn", "Berechnen", class = "btn-success w-100 mt-2")
      ),
      card(
        card_header("Ergebnis"),
        uiOutput("rechner_ergebnis"),
        hr(style = "margin:0.6rem 0; border-color:#353432;"),
        plotlyOutput("rechner_chart", height = "280px")
      )
    )
  )
)
