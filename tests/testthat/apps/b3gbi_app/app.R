# Test App for b3gbiGUI Integration Testing
# 
# This is a wrapper app for shinytest2 that properly sets up paths
# to source modules and data from the main package.

# Get the package root (parent of tests directory)
pkg_root <- normalizePath(file.path("..", "..", ".."))

# Load required libraries
library(shiny)
library(shinyWidgets)
library(b3gbi)
library(DT)
library(stringr)
library(rnaturalearthdata)
library(lubridate)
library(shinyjs)
library(jsonlite)
library(colourpicker)
library(ggplot2)
library(ggspatial)
library(shinyvalidate)

# Check for required package version
if (packageVersion("b3gbi") < "0.8.6") {
  stop("This app requires b3gbi version 0.8.6 or higher.")
}

# Source all modular R components from package
module_files <- list.files(
  path = file.path(pkg_root, "R"), 
  pattern = "\\.R$", 
  full.names = TRUE
)
for (file in module_files) {
  source(file, local = TRUE)
}

# Load spatial data from package data directory
data_dir <- file.path(pkg_root, "data")
continents <- readRDS(file.path(data_dir, "rnecontinents.RData"))
countries10 <- readRDS(file.path(data_dir, "rnecountries10.RData"))
sovereignties10 <- readRDS(file.path(data_dir, "rnesov10.RData"))
mapunits10 <- readRDS(file.path(data_dir, "rnemapunits10.RData"))
countries50 <- readRDS(file.path(data_dir, "rnecountries50.RData"))
sovereignties50 <- readRDS(file.path(data_dir, "rnesov50.RData"))
mapunits50 <- readRDS(file.path(data_dir, "rnemapunits50.RData"))
tinycountries50 <- readRDS(file.path(data_dir, "rnetiny50.RData"))
countries110 <- readRDS(file.path(data_dir, "rnecountries110.RData"))
sovereignties110 <- readRDS(file.path(data_dir, "rnesov110.RData"))
mapunits110 <- readRDS(file.path(data_dir, "rnemapunits110.RData"))
tinycountries110 <- readRDS(file.path(data_dir, "rnetiny110.RData"))

# UI Definition - identical to main app
ui <- fluidPage(
  useShinyjs(),
  
  # Page styling
  tags$head(
    tags$title("B-Cubed Indicators - Test"),
    tags$meta(name = "viewport", content = "width=device-width"),
    tags$style(HTML("
      body, html { overflow: hidden; }
      .custom-inline { display: inline-block; align-items: right; width: 90%; }
      .checkbox-container { display: inline-block; align-items: left; width: 8%; }
      .scrollable-tab { overflow-y: auto; overflow-x: hidden; height: 60vh; padding: 10px; }
      .tab-content { height: 100%; }
    ")),
    # Include basic styling for testing
    tags$style(HTML("
      .navbar-default { background-color: #f8f8f8; border-color: #e7e7e7; }
      .sidebar-panel { padding: 15px; }
      .main-panel { padding: 15px; }
    "))
  ),
  
  # Application title
  titlePanel(
    title = span(
      "Biodiversity Indicators for Data Cubes (Test)"
    )
  ),
  
  # Sidebar with input tabs
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "input_tabs",
        tabPanel("Data", data_tab_ui()),
        tabPanel("Analysis", analysis_tab_ui()),
        tabPanel("Viz Options", viz_options_tab_ui())
      ),
      width = 3
    ),
    
    # Main panel with output tabs
    mainPanel(
      tabsetPanel(
        id = "output_tabs",
        tabPanel("Summary", summary_tab_output_ui()),
        tabPanel("Map", map_tab_output_ui()),
        tabPanel("Time Series", timeseries_tab_output_ui()),
        tabPanel("Table", datatable_tab_output_ui())
      ),
      width = 9
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Increase max request size for file uploads (500 MB)
  options(shiny.maxRequestSize = 500 * 1024^2)
  
  # Initialize all server modules
  server_modules <- initialize_server_modules(
    input, output, session,
    continents_data = continents
  )
  
  # Extract reactive values from modules for potential direct use
  r <- server_modules$r
  shapefile_path <- server_modules$shapefile_path
  plot_to_print_map <- server_modules$plot_to_print_map
  plot_to_print_ts <- server_modules$plot_to_print_ts
  parsed_inputs <- server_modules$parsed_inputs
  iv <- server_modules$iv
}

# Run the application
shinyApp(ui = ui, server = server)
