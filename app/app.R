#===============================================================================
#
# Shiny app of oes data.
#
#===============================================================================

library(shiny)
library(shinydashboard)
library(shinycssloaders)

library(readr)
library(dplyr)
library(stringr)
library(purrr)

library(sf)
library(RColorBrewer)
library(leaflet)

source("map.R")

# data ==========

cleaned <- read_rds("cleaned.rds")
shape_metro <- read_rds("shape_metro.rds")
shape_state <- read_rds("shape_state.rds")

cleaned <- cleaned %>% 
  # filter(str_sub(occupation_code, 3, 6) == "0000") %>%
  mutate(occ_code_name = str_c(occupation_code, " - ", occupation_name))

occ_code_names <- sort(unique(cleaned[["occ_code_name"]]))
stat_types <- c("Employment rate" = "emp_rate", "Median salary" = "wage_p50")

# user interface ==========

header <- dashboardHeader(title = "BLS OES data", titleWidth = 225)

sidebar <- dashboardSidebar(
  width = 225,
  sidebarMenu(
    menuItem("Map", tabName = "tab_map")
  )
)

tab_item_map <- tabItem(
  tabName = "tab_map",
  fluidRow(
    column(
      width = 2,
      box(
        title = "Start here",
        status = "primary",
        width = NULL,
        selectInput(
          inputId  = "occupation",
          label    = "Step 1 - Choose an occupation:",
          choices  = occ_code_names,
          selected = occ_code_names[[1]],
          multiple = FALSE
        ),
        radioButtons(
          inputId  = "type",
          label    = "Step 2 - Choose a statistic:",
          choices  = stat_types,
          selected = stat_types[[1]]
        )
      )
    ),
    column(
      width = 10,
      box(
        title = "Occupational employment stats by metro",
        footer = "Source: BLS OES.",
        status = "primary",
        width = NULL,
        withSpinner(leafletOutput("mapmetro", height = 725))
      )
    )
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tab_item_map
  )
)

ui <- dashboardPage(header, sidebar, body)

# server ==========

server <- function(input, output) {
  output$mapmetro <- renderLeaflet({
    map_metro(
      df = cleaned,
      shape_metro = shape_metro,
      shape_state = shape_state,
      code_name = input$occupation,
      type = input$type
    )
  })
}

# run ==========

shinyApp(ui, server)
