# B-Cubed Indicators Shiny Application
# Integrated modular version

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

# Source all modular R components with error handling
module_files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
module_files <- module_files[!grepl("~$|\\#", module_files)]  # Exclude backup/temp files
for (file in module_files) {
  tryCatch({
    source(file, local = TRUE)
  }, error = function(e) {
    stop(paste("Error sourcing", basename(file), ":", e$message))
  })
}

# Load spatial data
continents <- readRDS("data/rnecontinents.RData")
countries10 <- readRDS("data/rnecountries10.RData")
sovereignties10 <- readRDS("data/rnesov10.RData")
mapunits10 <- readRDS("data/rnemapunits10.RData")
countries50 <- readRDS("data/rnecountries50.RData")
sovereignties50 <- readRDS("data/rnesov50.RData")
mapunits50 <- readRDS("data/rnemapunits50.RData")
tinycountries50 <- readRDS("data/rnetiny50.RData")
countries110 <- readRDS("data/rnecountries110.RData")
sovereignties110 <- readRDS("data/rnesov110.RData")
mapunits110 <- readRDS("data/rnemapunits110.RData")
tinycountries110 <- readRDS("data/rnetiny110.RData")

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  
  # Page styling
  tags$head(
    tags$title("B-Cubed Indicators"),
    tags$link(rel = "icon", type = "image/png", size = "32x32", href = "B3_logomark.png"),
    tags$meta(name = "viewport", content = "width=device-width"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=PT+Sans+Narrow:wght@400;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body, html { overflow: hidden; }
      .custom-inline { display: inline-block; align-items: right; width: 90%; }
      .checkbox-container { display: inline-block; align-items: left; width: 8%; }
      .scrollable-tab { overflow-y: auto; overflow-x: hidden; height: 60vh; padding: 10px; }
      .tab-content { height: 100%; }
    "))
  ),
  
  # Application title
  titlePanel(
    title = span(
      img(src = "B3_logomark.png", height = 50),
      "Biodiversity Indicators for Data Cubes"
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
