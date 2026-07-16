# pkgs <- c("shiny", "bslib", "tidyverse", "leaflet",
#           "sf", "plotly","DT", "bsicons","scales")
#
# install.packages(pkgs[!pkgs%in%
#                         rownames(installed.packages())])
#
#load libraries
library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(sf)
library(plotly)
library(DT)
library(bsicons)
library(scales)
#
#
# # set.seed(42)
# #
# # districts <- paste("District", 1:50)
# # years     <- 2020:2023
# # months    <- 1:12
# #
# # malaria_data <- expand.grid(
# #   district = districts,
# #   year     = years,
# #   month    = months
# # ) |>
# #   mutate(
# #     rainfall_mm  = round(runif(n(), 10, 300), 1),
# #     incidence    = round(
# #       50 + 0.8 * rainfall_mm +
# #         rnorm(n(), 0, 20), 1),
# #     incidence    = pmax(incidence, 0),
# #     population   = sample(10000:200000, n(),
# #                           replace = TRUE),
# #     region       = case_when(
# #       district %in% paste("District", 1:10)
# #       ~ "Northern",
# #       district %in% paste("District", 11:20)
# #       ~ "Southern",
# #       district %in% paste("District", 21:30)
# #       ~ "Eastern",
# #       district %in% paste("District", 31:40)
# #       ~ "Western",
# #       TRUE ~ "Central"
# #     )
# #   )
# #
# # saveRDS(malaria_data, "data/malaria_data.rds")
#
#
# # 1. UI — what the user sees
#
# ui <- fluidPage(
#   titlePanel("My first shiny app"),
#
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput(
#         inputId = "bins",
#         label = "Number of bins",
#         min = 1, max = 50, value = 30
#       )
#     ),
#
#     mainPanel(
#       plotOutput("histogram")
#     )
#   )
#
# )
#
# server <- function(input, output, session) {
#
#   output$histogram <- renderPlot({
#     hist(faithful$waiting,
#          breaks = input$bins,
#          col = "steelblue",
#          main = "waiting times",
#          xlab = "Minutes"
#     )
#
#   })
# }
#
# shinyApp(ui,server)

ui <- page_sidebar(
  title = "Malaria Burden Explorer",

  sidebar = sidebar(
    "Filters go here"
  ),

  #row1 - value boxes

  layout_columns(

    value_box(
      title="Mean incidence per 1000",
      value = "--",
      showcase = bs_icon("activity"),
      theme = "danger"
    ),

    value_box(
      title = "High Burden districts",
      value = "--",
      showcase = bs_icon("exclamation-triangle"),
      theme = "warning"
    ),

    value_box(
      title = "People at high risk",
      value = "--",
      showcase = bs_icon("people-fill"),
      theme = "success"
    )
  ),

#Row 2
layout_columns(
  card(
    full_screen = TRUE,
    card_header("Incidence by district"),
    card_body(
      leafletOutput("map", height = 380)
    )
  ),

  card(
    full_screen = TRUE,
    card_header("Top district by burden"),
    card_body(
      plotlyOutput("bar_chart", height = 380)
    )
  )
),

#Row 3 - trend panel
card(
  full_screen = TRUE,
  card_header(
    "Monthly incidence and rainfall"
  ),
  card_body(
    plotlyOutput("trend_chart", height = 300)
  )
)

)


#---Server------
server <- function(input, output, session) {




}

#Launch
shinyApp(ui,server)