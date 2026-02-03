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


#' Create Time Series Visualization Options UI
#' 
#' @return UI elements for time series visualization options section
viz_timeseries_options_ui <- function() {
  tagList(
    checkboxInput(
      "ts_options",
      "Show Time Series Visualization Options"
    ),
    conditionalPanel(
      condition = "input.ts_options == true",
      tags$hr(),
      textInput(
        "ts_x_label",
        label = "Custom X-Axis Title",
        value = ""
      ),
      textInput(
        "ts_y_label",
        label = "Custom Y-Axis Title",
        value = ""
      ),
      fluidRow(
        column(width = 6,
               checkboxInput(
                 "suppress_x",
                 label = "Suppress X-Axis Labels",
                 value = FALSE
               ),
               checkboxInput(
                 "suppress_xt",
                 label = "Suppress X-Axis Title",
                 value = FALSE
               )
        ),
        column(width = 6,
               checkboxInput(
                 "suppress_y",
                 label = "Suppress Y-Axis Labels",
                 value = FALSE
               ),
               checkboxInput(
                 "suppress_yt",
                 label = "Suppress Y-Axis Title",
                 value = FALSE
               )
        )
      ),
      checkboxInput(
        "ts_expand",
        label = "Expand Axes"
      ),
      conditionalPanel(
        condition = "input.ts_expand == true",
        div(class = "checkbox-container"),
        div(class = "custom-inline",
            fluidRow(
              column(width = 6,
                     numericInput(
                       "ts_x_expand_left",
                       paste0("Left"),
                       min = 0,
                       max = 1,
                       step = 0.01,
                       value = 0
                     )
              ),
              column(width = 6,
                     numericInput(
                       "ts_x_expand_right",
                       paste0("Right"),
                       min = 0,
                       max = 1,
                       step = 0.01,
                       value = 0
                     )
              )
            ),
            fluidRow(
              column(width = 6,
                     numericInput(
                       "ts_y_expand_top",
                       paste0("Top"),
                       min = 0,
                       max = 1,
                       step = 0.01,
                       value = 0
                     )
              ),
              column(width = 6,
                     numericInput(
                       "ts_y_expand_bottom",
                       paste0("Bottom"),
                       min = 0,
                       max = 1,
                       step = 0.01,
                       value = 0
                     )
              )
            )
        )
      ),
      fluidRow(
        column(width = 6,
               numericInput(
                 "ts_x_breaks",
                 paste0("Number of X Axis Breaks (approximate)"),
                 min = 2,
                 max = 100,
                 step = 1,
                 value = 10
               )
        ),
        column(width = 6,
               numericInput(
                 "ts_y_breaks",
                 paste0("Number of Y Axis Breaks (approximate)"),
                 min = 2,
                 max = 100,
                 step = 1,
                 value = 6
               )
        )
      ),
      checkboxInput(
        "ts_gridlines",
        label = "Plot grid lines",
        value = TRUE
      ),

      selectInput(
        "point_line",
        label = "Plot Indicator Values as Points or Line",
        choices = c(
          "point",
          "line"
        ),
        selected = "Points"
      ),
      conditionalPanel(
        condition = "input.point_line === 'point'",
        numericInput(
          "pointsize",
          label = "Size of Indicator Points",
          min = 0.1,
          max = 10,
          step = 0.1,
          value = 2
        )
      ),
      conditionalPanel(
        condition = "input.point_line === 'line'",
        numericInput(
          "linewidth",
          label = "Width of Indicator Line",
          min = 0.1,
          max = 10,
          step = 0.1,
          value = 1
        )
      ),
      colourInput(
        "linecolour",
        label = "Colour of Indicator Line or Points",
        value = "darkorange"
      ),
      numericInput(
        "linealpha",
        paste0("Transparency of Indicator Line or Points"),
        min = 0,
        max = 1,
        step = 0.05,
        value = 1
      ),

      selectInput(
        "ci_vis_type",
        label = "Confidence Interval Type",
        choices = c(
          "Error Bars",
          "Ribbon",
          "None"
        ),
        selected = "None"
      ),
      conditionalPanel(
        condition = "input.ci_vis_type === 'Error Bars'",
        div(class = "checkbox-container"),
        div(class = "custom-inline",
            numericInput(
              "error_width",
              paste0("Width of Error Bars"),
              min = 0.1,
              max = 10,
              step = 0.1,
              value = 1
            ),
            numericInput(
              "error_thickness",
              paste0("Thickness of Error Bars"),
              min = 0.1,
              max = 10,
              step = 0.1,
              value = 1
            ),
            numericInput(
              "error_alpha",
              paste0("Error Bar Transparency"),
              min = 0,
              max = 1,
              step = 0.05,
              value = 1
            )
        )
      ),
      conditionalPanel(
        condition = "input.ci_vis_type === 'Ribbon'",
        div(class = "checkbox-container"),
        div(class = "custom-inline",
            colourInput(
              "ribboncolour",
              label = "Confidence Interval Ribbon Colour",
              value = "goldenrod1"
            ),
            numericInput(
              "ribbonalpha",
              paste0("Confidence Interval Ribbon Transparency"),
              min = 0,
              max = 1,
              step = 0.05,
              value = 0.2
            )
        )
      ),

      checkboxInput(
        "smoothed_trend",
        label = "Plot Smoothed Trend Line",
        value = TRUE
      ),
      conditionalPanel(
        condition = "input.smoothed_trend == true",
        div(class = "checkbox-container"),
        div(class = "custom-inline",
            selectInput(
              "smooth_linetype",
              label = "Smoothed Trend Line Type",
              choices = c(
                "solid",
                "dashed",
                "dotted",
                "dotdash",
                "longdash",
                "twodash"
              ),
              selected = "solid"
            ),
            numericInput(
              "smooth_linewidth",
              paste0("Trend Line Width"),
              min = 0.1,
              max = 10,
              step = 0.1,
              value = 1
            ),
            colourInput(
              "trendlinecolour",
              paste0("Trend Line Colour"),
              value = "blue"
            ),
            numericInput(
              "trendlinealpha",
              paste0("Trend Line Transparency"),
              min = 0,
              max = 1,
              step = 0.05,
              value = 0.5
            ),
            numericInput(
              "smooth_cilinewidth",
              paste0("Trend Envelope Edge Width"),
              min = 0.1,
              max = 10,
              step = 0.1,
              value = 1
            ),
            colourInput(
              "envelopecolour",
              paste0("Trend Envelope Colour"),
              value = "lightsteelblue"
            ),
            numericInput(
              "envelopealpha",
              paste0("Trend Envelope Transparency"),
              min = 0,
              max = 1,
              step = 0.05,
              value = 0.2
            ),
            numericInput(
              "smooth_cialpha",
              paste0("Trend Envelope Edge Transparency"),
              min = 0,
              max = 1,
              step = 0.05,
              value = 1
            )
        )
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
