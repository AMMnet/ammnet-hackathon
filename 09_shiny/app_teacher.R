# ══════════════════════════════════════════════════════
# Malaria Burden Explorer — Burkina Faso
# AMMNet R Shiny Hackathon — Demo App
# Author: Christina Myalla
# Date:   July 2026
# ══════════════════════════════════════════════════════

# ── packages ───────────────────────────────────────────
library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(sf)
library(plotly)
library(DT)
library(bsicons)
library(scales)
library(htmltools)

# ── data ───────────────────────────────────────────────
malaria_data <- readRDS("data/malaria_data.rds")
annual_df    <- readRDS("data/annual_summary.rds")
district_shp <- readRDS("data/bfa_districts.rds")

# ── helpers ────────────────────────────────────────────

# colour palette for choropleth map
make_pal <- function(values) {
  colorBin(
    palette  = "YlOrRd",
    domain   = values,
    bins     = c(0, 50, 100, 200,
                 300, 400, 500, Inf),
    na.color = "#f0f0f0"
  )
}

# top n districts by mean incidence
top_districts <- function(df, n = 10) {
  df |>
    group_by(district, region) |>
    summarise(
      mean_inc = round(
        mean(incidence,    na.rm = TRUE), 1),
      mean_itn = round(
        mean(itn_coverage, na.rm = TRUE), 1),
      .groups  = "drop"
    ) |>
    slice_max(mean_inc, n = n) |>
    arrange(mean_inc)
}

# ══════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════

ui <- page_sidebar(

  title = "Malaria Burden Explorer — Burkina Faso",

  # ── theme ─────────────────────────────────────────
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    base_font  = font_google("Inter")
  ),

  # ── sidebar ───────────────────────────────────────
  sidebar = sidebar(

    # logo — save your image as www/logo.png
    # tags$img(
    #   src   = "logo.png",
    #   width = "100%",
    #   style = "margin-bottom: 10px;"
    # ),

    # dark / light toggle
    input_dark_mode(mode = "light"),

    p("Explore district-level malaria burden
       across Burkina Faso. Select a year,
       region and district to update all panels."),

    hr(),

    # year selector
    selectInput(
      inputId  = "year",
      label    = "Year",
      choices  = sort(unique(malaria_data$year)),
      selected = max(malaria_data$year)
    ),

    # region selector
    selectInput(
      inputId  = "region",
      label    = "Region",
      choices  = c("All regions",
                   sort(unique(malaria_data$region))),
      selected = "All regions"
    ),

    # district selector — for trend chart
    selectInput(
      inputId  = "district",
      label    = "District (trend chart)",
      choices  = sort(unique(malaria_data$district)),
      selected = sort(unique(malaria_data$district))[1]
    ),

    hr(),

    # burden threshold slider
    sliderInput(
      inputId = "threshold",
      label   = "High burden threshold (per 1,000)",
      min     = 0,
      max     = 500,
      value   = 200,
      step    = 10
    ),

    # number of districts in bar chart
    selectInput(
      inputId  = "n_districts",
      label    = "Districts in bar chart",
      choices  = c(5, 10, 15, 20),
      selected = 10
    ),

    hr(),

    # reactive summary
    textOutput("sidebar_summary"),

    br(),

    # download button
    downloadButton(
      outputId = "download_data",
      label    = "Download filtered data"
    )

  ),

  # ── row 1: value boxes ──────────────────────────────
  layout_columns(
    height = "150px",

    value_box(
      title    = "Mean incidence per 1,000",
      value    = textOutput("kpi_mean"),
      showcase = bs_icon("activity"),
      theme    = "danger"
    ),

    value_box(
      title    = "High burden districts",
      value    = textOutput("kpi_count"),
      showcase = bs_icon("exclamation-triangle"),
      theme    = "warning"
    ),

    value_box(
      title    = "Mean ITN coverage",
      value    = textOutput("kpi_itn"),
      showcase = bs_icon("shield-check"),
      theme    = "success"
    ),

    value_box(
      title    = "Highest burden district",
      value    = textOutput("kpi_top"),
      showcase = bs_icon("geo-alt"),
      theme    = "info"
    )

  ),

  # ── row 2: map + bar chart ──────────────────────────
  layout_columns(

    # choropleth map
    card(
      full_screen = TRUE,
      card_header(
        "Incidence by district",
        class = "bg-danger text-white"
      ),
      card_body(
        leafletOutput("map", height = "420px")
      )
    ),

    # ranked bar chart
    card(
      full_screen = TRUE,
      card_header(
        "Top districts by burden",
        class = "bg-warning text-dark"
      ),
      card_body(
        plotlyOutput("bar_chart", height = "420px")
      )
    )

  ),

  # ── row 3: trend chart ──────────────────────────────
  card(
    full_screen = TRUE,
    card_header(
      "Monthly incidence & rainfall",
      class = "bg-info text-dark"
    ),
    card_body(
      plotlyOutput("trend_chart", height = "320px")
    ),
    card_footer(
      "Rainfall peaks July–August ·
       Incidence peaks September–October ·
       Source: Malaria Atlas Project ·
       Simulated indicators"
    )
  ),

  # # ── row 4: data table ───────────────────────────────
  # card(
  #   full_screen = TRUE,
  #   card_header("District data table"),
  #   card_body(
  #     DTOutput("data_table")
  #   )
  # )

)

# ══════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── reactive dataset ────────────────────────────────
  # filters once — shared by all outputs
  filtered_data <- reactive({
    malaria_data |>
      filter(
        year == input$year,
        input$region == "All regions" |
          region == input$region
      )
  })

  # ── update district when region changes ─────────────
  observeEvent(input$region, {

    choices <- if (input$region == "All regions") {
      sort(unique(malaria_data$district))
    } else {
      malaria_data |>
        filter(region == input$region) |>
        pull(district) |>
        unique() |>
        sort()
    }

    updateSelectInput(
      session  = session,
      inputId  = "district",
      choices  = choices,
      selected = choices[1]
    )

  })

  # ── sidebar summary ─────────────────────────────────
  output$sidebar_summary <- renderText({
    df <- filtered_data()
    paste0(
      n_distinct(df$district),
      " districts · ",
      input$year
    )
  })

  # ── KPI: mean incidence ─────────────────────────────
  output$kpi_mean <- renderText({
    round(
      mean(filtered_data()$incidence,
           na.rm = TRUE), 1
    )
  })

  # ── KPI: high burden district count ─────────────────
  output$kpi_count <- renderText({
    df <- filtered_data()
    n_distinct(
      df$district[df$incidence >= input$threshold]
    )
  })

  # ── KPI: mean ITN coverage ──────────────────────────
  output$kpi_itn <- renderText({
    paste0(
      round(
        mean(filtered_data()$itn_coverage,
             na.rm = TRUE), 1
      ), "%"
    )
  })

  # ── KPI: highest burden district ────────────────────
  output$kpi_top <- renderText({
    filtered_data() |>
      group_by(district) |>
      summarise(
        mean_inc = mean(incidence, na.rm = TRUE),
        .groups  = "drop"
      ) |>
      slice_max(mean_inc, n = 1) |>
      pull(district)
  })

  # ── choropleth map ──────────────────────────────────
  output$map <- renderLeaflet({

    # summarise to one row per district
    df <- filtered_data() |>
      group_by(district) |>
      summarise(
        mean_inc = round(
          mean(incidence,    na.rm = TRUE), 1),
        mean_itn = round(
          mean(itn_coverage, na.rm = TRUE), 1),
        .groups  = "drop"
      )

    # join to shapefile
    map_data <- district_shp |>
      left_join(df, by = "district")

    # colour palette
    pal <- make_pal(map_data$mean_inc)

    # build map
    leaflet(map_data) |>
      addProviderTiles(
        providers$CartoDB.Positron
      ) |>
      addPolygons(
        fillColor   = ~pal(mean_inc),
        fillOpacity = 0.8,
        color       = "#444444",
        weight      = 0.8,
        label = ~paste0(
          "<b>", district,    "</b><br>",
          "Incidence: ", mean_inc,
          " per 1,000<br>",
          "ITN coverage: ", mean_itn, "%"
        ) |> lapply(htmltools::HTML),
        highlightOptions = highlightOptions(
          weight       = 2,
          color        = "#222222",
          fillOpacity  = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal      = pal,
        values   = ~mean_inc,
        position = "bottomright",
        title    = "Incidence<br>per 1,000"
      )
  })

  # ── ranked bar chart ────────────────────────────────
  output$bar_chart <- renderPlotly({

    df <- top_districts(
      filtered_data(),
      n = as.integer(input$n_districts)
    )

    p <- ggplot(df,
                aes(
                  x    = mean_inc,
                  y    = reorder(district, mean_inc),
                  fill = mean_inc,
                  text = paste0(
                    "<b>", district,      "</b><br>",
                    "Region: ",    region,   "<br>",
                    "Incidence: ", mean_inc,
                    " per 1,000<br>",
                    "ITN coverage: ", mean_itn, "%"
                  )
                )
    ) +
      geom_col() +
      geom_vline(
        xintercept = input$threshold,
        linetype   = "dashed",
        colour     = "#333333",
        linewidth  = 0.6
      ) +
      annotate(
        "text",
        x      = input$threshold,
        y      = 1,
        label  = paste("Threshold:", input$threshold),
        hjust  = -0.1,
        size   = 3,
        colour = "#333333"
      ) +
      scale_fill_gradient(
        low  = "#fdae61",
        high = "#a50026"
      ) +
      scale_x_continuous(
        labels = comma,
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(
        x = "Mean incidence per 1,000",
        y = NULL
      ) +
      theme_minimal(12) +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text")
  })

  # ── trend chart ─────────────────────────────────────
  output$trend_chart <- renderPlotly({

    # all years for selected district
    df <- malaria_data |>
      filter(district == input$district)

    p <- ggplot(df, aes(x = month)) +

      # rainfall bars — background
      geom_col(
        aes(
          y    = rainfall_mm / 3,
          text = paste0(
            "Month: ",    month.abb[month], "<br>",
            "Rainfall: ", rainfall_mm, " mm"
          )
        ),
        fill  = "#378ADD",
        alpha = 0.4
      ) +

      # incidence lines — one per year
      geom_line(
        aes(
          y      = monthly_incidence,
          colour = factor(year),
          group  = year,
          text   = paste0(
            "Month: ",     month.abb[month], "<br>",
            "Year: ",      year,             "<br>",
            "Incidence: ", monthly_incidence,
            " per 1,000"
          )
        ),
        linewidth = 1
      ) +
      geom_point(
        aes(
          y      = monthly_incidence,
          colour = factor(year),
          group  = year
        ),
        size = 2
      ) +

      scale_x_continuous(
        breaks = 1:12,
        labels = month.abb
      ) +
      scale_colour_brewer(
        palette = "Set2",
        name    = "Year"
      ) +
      scale_y_continuous(
        name     = "Monthly incidence per 1,000",
        sec.axis = sec_axis(
          transform = ~. * 3,
          name      = "Rainfall (mm)"
        )
      ) +
      labs(
        x     = NULL,
        title = paste0(
          input$district,
          " — seasonal pattern"
        )
      ) +
      theme_minimal(12) +
      theme(
        axis.title.y.right = element_text(
          colour = "#378ADD"
        )
      )

    ggplotly(p, tooltip = "text") |>
      layout(
        legend = list(
          orientation = "h",
          y           = -0.2
        )
      )
  })

  # # ── data table ──────────────────────────────────────
  # output$data_table <- renderDT({
  #
  #   filtered_data() |>
  #     group_by(district, region) |>
  #     summarise(
  #       `Mean incidence` = round(
  #         mean(incidence,    na.rm = TRUE), 1),
  #       `ITN coverage`   = round(
  #         mean(itn_coverage, na.rm = TRUE), 1),
  #       Population       = comma(
  #         mean(population,   na.rm = TRUE)),
  #       .groups          = "drop"
  #     ) |>
  #     arrange(desc(`Mean incidence`)) |>
  #     datatable(
  #       rownames  = FALSE,
  #       options   = list(
  #         pageLength = 10,
  #         scrollX    = TRUE
  #       ),
  #       colnames  = c(
  #         "District", "Region",
  #         "Mean incidence (per 1,000)",
  #         "ITN coverage (%)",
  #         "Population"
  #       )
  #     ) |>
  #     formatStyle(
  #       "Mean incidence (per 1,000)",
  #       background = styleColorBar(
  #         range(filtered_data()$incidence,
  #               na.rm = TRUE),
  #         "#FDAE61"
  #       )
  #     )
  # })

  # ── download handler ────────────────────────────────
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        "malaria_bfa_",
        input$year, "_",
        gsub(" ", "_", input$region),
        ".csv"
      )
    },
    content = function(file) {
      filtered_data() |>
        group_by(district, region) |>
        summarise(
          mean_incidence = round(
            mean(incidence,    na.rm = TRUE), 1),
          itn_coverage   = round(
            mean(itn_coverage, na.rm = TRUE), 1),
          population     = round(
            mean(population,   na.rm = TRUE)),
          .groups        = "drop"
        ) |>
        arrange(desc(mean_incidence)) |>
        write.csv(file, row.names = FALSE)
    }
  )

}

# ══════════════════════════════════════════════════════
# LAUNCH
# ══════════════════════════════════════════════════════

shinyApp(ui, server)