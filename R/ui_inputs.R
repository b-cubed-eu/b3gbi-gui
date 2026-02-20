# UI Inputs Module
#
# This file contains all sidebar input UI components extracted from app.R
# Functions are designed to be called from the main UI definition
#

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

#' Create Visualization Options Tab UI
#'
#' @return UI elements for visualization options
viz_options_tab_ui <- function() {
  tagList(
    viz_general_options_ui(),
    viz_timeseries_options_ui(),
    viz_map_options_ui()
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


#' Create Map Visualization Options UI
#'
#' @return UI elements for map visualization options section
viz_map_options_ui <- function() {
  tagList(
    checkboxInput(
      "map_options",
      "Show Map Visualization Options"
    ),
    conditionalPanel(
      condition = "input.map_options == true",
      tags$hr(),
      checkboxInput(
        "custom_output_crs",
        "Manually Define Output CRS (Coordinate Reference System)",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.custom_output_crs == true",
        div(class = "checkbox-container"),
        div(class = "custom-inline",
            textInput(
              "output_crs",
              "EPSG code",
              value = "",
              placeholder = "e.g. 4326"
            ),
            checkboxInput(
              "crs_unit_convert",
              paste("Force conversion even if input and output units ",
                    "do not match (e.g. degrees to km; can lead to invalid ",
                    "output)")
            )
        )
      ),
      checkboxInput(
        "custom_map_axes",
        "Custom X and Y Axis Limits"
      ),
      conditionalPanel(
        condition = "input.custom_map_axes == true",
        div(class = "checkbox-container"),
        div(class = "custom-inline",
            fluidRow(
              column(width = 6,
                     textInput(
                       "xcoord_min",
                       "Min X",
                       value = ""
                     )
              ),
              column(width = 6,
                     textInput(
                       "xcoord_max",
                       "Max X",
                       value = ""
                     )
              )
            ),
            fluidRow(
              column(width = 6,
                     textInput(
                       "ycoord_min",
                       "Min Y",
                       value = ""
                     )
              ),
              column(width = 6,
                     textInput(
                       "ycoord_max",
                       "Max Y",
                       value = ""
                     )
              )
            )
        )
      ),
      checkboxInput(
        "crop_to_grid",
        "Crop map to edges of grid",
        value = FALSE
      ),
      checkboxInput(
        "crop_by_region",
        "Crop map to chosen region",
        value = FALSE
      ),
      div(class = "checkbox-container",
          checkboxInput(
            "custom_bg",
            ""
          )
      ),
      div(class = "custom-inline",
          colourInput(
            "ocean_fill_colour",
            "Customize Background (Ocean) Colour",
            allowTransparent = TRUE,
            value = ""
          )
      ),
      div(class = "checkbox-container",
          checkboxInput(
            "custom_land_fill",
            ""
          )
      ),
      div(class = "custom-inline",
          colourInput(
            "land_fill_colour",
            "Customize Colour for Land Areas Outside of Grid",
            allowTransparent = TRUE,
            value = ""
          )
      ),
      div(class = "checkbox-container",
          style = "vertical-align: top;",
          checkboxInput(
            "trans_yesno",
            ""
          )
      ),
      div(class = "custom-inline",
          selectInput(
            "trans",
            "Apply Scale Transformation to Indicator Values",
            choices = c(
              'Exponential Transformation' = "exp",
              'Square-root Transformation' = "sqrt",
              'Log Transformation' = "log",
              'Log10 Transformation' = "log10",
              'Log1p Transformation' = "log1p",
              'Log2 Transformation' = "log2",
              'Pseudo-log Transformation' = "psseudo_log",
              'Reciprocal Transformation' = "reciprocal",
              'Reverse Transformation' = "reverse",
              'Box-Cox Transformation' = "boxcox",
              'Modulus Transformation' = "modulus",
              'Yeo-Johnson Transformation' = "yj"
            ),
            selected = "exp"
          ),
          conditionalPanel(
            condition = "input.trans == 'boxcox' ||
            input.trans == 'yj' ||
            input.trans == 'modulus'",
            numericInput(
              "bcpower",
              "Box-Cox Power",
              min = -5,
              max = 5,
              step = 0.5,
              value = 1
            )
          )
      ),
      tags$hr(),
      HTML("<span style='font-size: 16px;'><b><u>Legend Customization ",
           "Options</b></u></span><br><br>"),
      textInput(
        "breaks",
        "Custom Break Points for Legend (comma separated)",
        value = ""
      ),
      textInput(
        inputId = "labels",
        label = paste0(
          "Labels for Custom Legend Break Points (must have same ",
          "number of labels as breaks)"
        ),
        value = ""
      ),
      textInput(
        "legend_title",
        "Custom Legend Title",
        value = ""
      ),
      checkboxInput(
        "legend_limits",
        "Custom Legend Scale Limits"
      ),
      conditionalPanel(
        condition = "input.legend_limits == true",
        div(class = "checkbox-container"),
        div(class = "custom-inline",
            fluidRow(
              column(width = 6,
                     textInput(
                       "legend_min",
                       "Min",
                       value = ""
                     )
              ),
              column(width = 6,
                     textInput(
                       "legend_max",
                       "Max",
                       value = ""
                     )
              )
            )
        )
      ),
      numericInput(
        "legend_title_wrap_length",
        "Legend Title Wrap Length (max characters on one line)",
        min = 10,
        max = 100,
        step = 2,
        value = 20
      ),
      tags$hr(),
      HTML("<span style='font-size: 16px;'><b><u>Axis Customization ",
           "Options</b></u></span><br><br>"),
      fluidRow(
        column(width = 6,
               numericInput(
                 "xaxis_fontsize",
                 "X-Axis Label Font Size",
                 min = 2,
                 max = 30,
                 step = 0.5,
                 value = 12
               )
        ),
        column(width = 6,
               numericInput(
                 "yaxis_fontsize",
                 "Y-Axis Label Font Size",
                 min = 2,
                 max = 30,
                 step = 0.5,
                 value = 12
               )
        )
      ),
      textInput(
        "xbreaks",
        "Custom X-Axis Break Points (comma separated)",
        value = ""
      ),
      textInput(
        "ybreaks",
        "Custom Y-Axis Break Points (comma separated)",
        value = ""
      ),
      checkboxInput(
        "gridlines",
        "Grid Lines",
        value = TRUE
      ),
      checkboxInput(
        "panel_gridlines",
        "Panel Grid Lines",
        value = FALSE
      ),
      checkboxInput(
        "grid_outline",
        "Visible Grid Outline",
        value = FALSE
      ),
      tags$hr(),
      checkboxInput(
        "add_scalebar",
        "Add a Scale Bar to the Map"
      ),
      conditionalPanel(
        condition = "input.add_scalebar === true",
        div(class = "checkbox-container"),
        div(class = "custom-inline",
            selectInput(
              "scalebar_location",
              "Scale Bar Location (on map)",
              choices = c(
                "bottom left" = "bl",
                "bottom right" = "br",
                "top left" = "tl",
                "top right" = "tr"
              ),
              selected = "tr"
            ),
            numericInput(
              "scalebar_padx",
              "Scale Bar X-Axis Padding",
              min = 0.01,
              max = 0.5,
              step = 0.01,
              value = 0.01
            ),
            numericInput(
              "scalebar_pady",
              "Scale Bar Y-Axis Padding",
              min = 0,
              max = 0.5,
              step = 0.01,
              value = 0.02
            ),
            numericInput(
              "scalebar_size",
              "Scale Bar Width (approximate)",
              min = 0.1,
              max = 1,
              step = 0.01,
              value = 0.2
            ),
            numericInput(
              "scalebar_height",
              "Scale Bar Height",
              min = 0.01,
              max = 0.1,
              step = 0.01,
              value = 0.02
            ),
            selectInput(
              "scalebar_style",
              "Scale Bar Style",
              choices = c(
                "bar",
                "ticks"
              )
            ),
            conditionalPanel(
              condition = "input.scalebar_style === 'ticks'",
              numericInput(
                "scalebar_tickheight",
                "Scale Bar Tick Height",
                min = 0,
                max = 2,
                step = 0.05,
                value = 0.6
              )
            ),
            conditionalPanel(
              condition = "input.scalebar_style === 'bar'",
              colourInput(
                "scalebar_colour1",
                "Scale Bar Colour 1",
                value = "black"
              ),
              colourInput(
                "scalebar_colour2",
                "Scale Bar Colour 2",
                value = "white"
              )
            ),
            numericInput(
              "scalebar_fontsize",
              "Scale Bar Font Size",
              min = 0,
              max = 10,
              step = 0.1,
              value = 0.8
            ),
            colourInput(
              "scalebar_fontcolour",
              "Scale Bar Font Colour",
              value = "black"
            )
        )
      ),
      checkboxInput(
        "add_northarrow",
        "Add a North Arrow to the Map",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.add_northarrow === true",
        div(class = "checkbox-container"),
        div(class = "custom-inline",
            selectInput(
              "northarrow_location",
              "North Arrow Location (on map)",
              choices = c(
                "bottom left" = "bl",
                "bottom right" = "br",
                "top left" = "tl",
                "top right" = "tr"
              ),
              selected = "tr"
            ),
            numericInput(
              "northarrow_padx",
              "North Arrow X-Axis Padding",
              min = 0,
              max = 0.5,
              step = 0.01,
              value = 0
            ),
            numericInput(
              "northarrow_pady",
              "North Arrow Y-Axis Padding",
              min = 0,
              max = 0.5,
              step = 0.01,
              value = 0.02
            ),
            selectInput(
              "northarrow_style",
              "North Arrow Style",
              choices = c(
                "orienteering",
                "fancy_orienteering",
                "minimal",
                "nautical"
              ),
              selected = "fancy_orienteering"
            ),
            numericInput(
              "northarrow_height",
              "North Arrow Height",
              min = 0.01,
              max = 1,
              step = 0.01,
              value = 0.1
            ),
            numericInput(
              "northarrow_width",
              "North Arrow Width",
              min = 0.01,
              max = 1,
              step = 0.01,
              value = 0.1
            ),
            colourInput(
              "northarrow_fillcolour1",
              "North Arrow Fill Colour 1",
              value = "black"
            ),
            colourInput(
              "northarrow_fillcolour2",
              "North Arrow Fill Colour 2",
              value = "white"
            ),
            colourInput(
              "northarrow_linecolour",
              "North Arrow Line Colour",
              value = "black"
            ),
            colourInput(
              "northarrow_textcolour",
              "North Arrow Text Colour",
              value = "black"
            ),
            numericInput(
              "northarrow_textsize",
              "North Arrow Text Size",
              min = 0,
              max = 100,
              step = 0.5,
              value = 10
            )
        )
      )
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
