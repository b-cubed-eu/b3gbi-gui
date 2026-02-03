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
    ),
    checkboxInput(
      inputId = "invert_shapefile",
      label = "Invert shapefile (not working properly)",
      value = FALSE
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
