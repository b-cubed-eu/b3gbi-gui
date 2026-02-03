# UI Inputs Module
# 
# This file contains all sidebar input UI components extracted from app.R
# Functions are designed to be called from the main UI definition
#

#' Create Data Tab UI
#' 
#' @return UI elements for data upload section
data_tab_ui <- function() {
  tagList(
    fileInput(
      inputId = "dataCube",
      label = HTML("Upload a data cube"),
      accept = ".csv"
    ),
    div(style = "display: flex; align-items: center;",
        div(class = "checkbox-container",
            checkboxInput(
              inputId = "shapefile",
              label = "",
              value = FALSE
            )
        ),
        div(class = "custom-inline",
            fileInput(
              inputId = "shapefile_zip",
              label = HTML("Add external shape file (.zip)"),
              accept = ".zip"
    )
  )
}

#' Create General Visualization Options UI
#' 
#' @return UI elements for general visualization options section
viz_general_options_ui <- function() {
  tagList(
    checkboxInput(
      "gen_options",
      "Show General Visualization Options"
    ),
    conditionalPanel(
      condition = "input.gen_options == true",
      tags$hr(),
      fluidRow(
        column(width = 6,
               numericInput(
                 "plot_width",
                 label = "Plot Width (in pixels)",
                 min = 100,
                 max = 2000,
                 step = 10,
                 value = 600
               )
        ),
        column(width = 6,
               numericInput(
                 "plot_height",
                 label = "Plot Height (in pixels)",
                 min = 100,
                 max = 2000,
                 step = 10,
                 value = 400
               )
        )
      ),
      textInput(
        "title",
        label = "Plot Title"
      ),
      textInput(
        "subtitle",
        label = "Plot Subtitle"
      ),
      fluidRow(
        column(width = 6,
               numericInput(
                 "title_size",
                 label = "Title Font Size",
                 min = 8,
                 max = 40,
                 step = 1,
                 value = 13
               )
        ),
        column(width = 6,
               numericInput(
                 "subtitle_size",
                 label = "Subtitle Font Size",
                 min = 6,
                 max = 30,
                 step = 1,
                 value = 11
               )
        )
      ),
      fluidRow(
        column(width = 6,
               numericInput(
                 "title_wrap_length",
                 label = paste("Title Wrap Length (max. characters ",
                               "on a single line)"),
                 min = 20,
                 max = 200,
                 step = 5,
                 value = 60
               )
        ),
        column(width = 6,
               numericInput(
                 "subtitle_wrap_length",
                 label = paste("Subtitle Wrap Length (max. ",
                               "characters on a single line)"),
                 min = 20,
                 max = 200,
                 step = 5,
                 value = 60
               )
        )
      ),
      fluidRow(
        column(width = 6,
               colourInput(
                 "title_color",
                 label = "Title Colour",
                 value = "black"
               )
        ),
        column(width = 6,
               colourInput(
                 "subtitle_color",
                 label = "Subtitle Colour",
                 value = "black"
               )
        )
      ),
      textInput(
        "caption",
        label = "Plot Caption"
      ),
      fluidRow(
        column(width = 6,
               numericInput(
                 "caption_size",
                 label = "Caption Font Size",
                 min = 6,
                 max = 20,
                 step = 1,
                 value = 9
               )
        ),
        column(width = 6,
               numericInput(
                 "caption_wrap_length",
                 label = "Caption Wrap Length",
                 min = 20,
                 max = 200,
                 step = 5,
                 value = 60
               )
        )
      ),
      colourInput(
        "caption_color",
        label = "Caption Colour",
        value = "black"
      ),
      tags$hr()
    )
  )
}


#' Create Analysis & Filters Tab UI
#' 
#' @return UI elements for analysis and filtering section
analysis_tab_ui <- function() {
  tagList(
    # Indicator
    selectInput(
      inputId = "indicatorsToAnalyse",
      label = "Biodiversity Indicator",
      multiple = FALSE,
      choices = as.character(sapply(b3gbi::available_indicators, "[[", 2))
    ),
    
    # Custom Region
    checkboxInput(
      inputId = "customregion",
      label = "Customize Region",
      value = FALSE
    ),
    
    conditionalPanel(
      condition = "input.customregion === true",
      div(class = "checkbox-container"),
      div(class = "custom-inline",
          fluidRow(
                column(width = 6,
                       # Spatial level
                       selectInput(
                         inputId = "spatiallevel",
                         label = "Spatial level",
                         choices = c(
                           "cube",
                           "world",
                           "continent",
                           "country",
                           "sovereignty",
                           "geounit"
                         ),
                         selected = "cube"
                       )
                ),
                column(width = 6,
                       # Country type
                       selectInput(
                         inputId = "countrytype",
                         label = "Country type",
                         choices = c(
                           "countries",
                           "map_units",
                           "sovereignty",
                           "tiny_countries"
                         ),
                         selected = "countries"
                       )
                )
          ),
          # Spatial region
          selectizeInput(
            inputId = "region",
            label = "Subset by region",
            choices = NULL,
            multiple = TRUE,
            options = list(
              create = TRUE,
              delimiter = " ",
              persist = FALSE,
              plugins = list("remove_button"),
              createOnBlur = TRUE
            )
          )
      )
    ),
    
    # Include land/ocean
    checkboxInput(
      "include_land",
      "Include land areas",
      value = TRUE
    ),
    checkboxInput(
      "include_ocean",
      "Include ocean areas",
      value = TRUE
    ),
    
    # Map resolution
    selectInput(
      inputId = "mapres",
      label = "Map resolution",
      choices = c(
        "Small (110m)" = "110",
        "Medium (50m)" = "50",
        "Large (10m)" = "10"
      ),
      selected = "50"
    ),
    
    # Spatial resolution
    numericInput(
      "cellsize",
      paste0("Spatial resolution in kilometers or degrees (depending on ",
             "grid type)"),
      min = 0,
      max = 100,
      step = 1,
      value = 10
    ),
    
    # Date range
    sliderInput("daterange",
                "Date range:",
                min = 1100,
                max = year(Sys.Date()),
                value = c(1100, year(Sys.Date())),
                sep = ""
    ),
    
    # Family selection
    selectInput(
      inputId = "family",
      label = "Subset by family",
      choices = NULL,
      multiple = TRUE
    ),
    
    # Species selection
    selectInput(
      inputId = "species",
      label = "Subset by species",
      choices = NULL,
      multiple = TRUE
    )
  )
}
