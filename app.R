library(plotly)
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
library(ggspatial)

# Check for a specific package version
if (packageVersion("b3gbi") < "0.4.3") {
  stop("This app requires b3gbi version 0.4.3 or higher.")
}

# tiny countries not available at 10
# type country, you get rnecountries, then ADMIN is country, etc..
# type sovereignty, you get rnesov, then ADMIN is country, GEOUNIT is geounit,
#      SOVEREIGNT is sovereignty
# type map_units, you get rnemapunits, then ADMIN is country, etc..
# type tiny_countries, you oget rnetiny, then ADMIN is country, etc...
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


ui <- fluidPage(
  useShinyjs(), # Set up shinyjs

  # Style
  tags$head(
    tags$title(
      "B-Cubed Indicators"
    ),
    tags$link(
      rel = "icon", type = "image/png", size = "32x32", href = "B3_logomark.png"
    ),
    tags$meta(
      name = "viewport", content = "width=device-width"
    ),
    tags$link(
      rel = "stylesheet", type = "text/css", href = "style.css"
    ),
    tags$link(
      href = paste("https://fonts.googleapis.com/css2?family=PT+Sans+Narrow:wght@400",
      ";700&display=swap"),
      rel = "stylesheet"
    ),
    tags$style(
      HTML("
        body, html {
        overflow: hidden;
        }
        .custom-inline {
        display: inline-block;
        align-items: right;
        width: 90%;
        }
        .checkbox-container {
        display: inline-block;
        align-items: left;
        width: 8%;
        }
        .scrollable-tab {
        overflow-y: auto;
        overflow-x: hidden;
        height: 60vh;
        padding: 10px;
        }
        /* Adjust tab content to avoid overlap with tab headers */
        .tab-content {
        height: 100%; /* Adjust based on tab header height */
        }
      ")
    )
  ),

  # input = text fields, action buttons

  # Application title

  titlePanel(
    title = span(
      img(
        src = "B3_logomark.png", height = 50
      ),
      "B-Cubed: General Biodiversity Indicators",
      style = "color:#000"
    )
  ),
  (
    div(
      HTML(
        paste0(
          "<p><span style='font-size: 18px;'><br>Welcome to the B-Cubed: ",
          "Biodiversity Indicators Shiny app!</span><br><br>The B-Cubed: ",
          "Biodiversity Indicators Shiny app uses the R package <a href='",
          "https://github.com/b-cubed-eu/b3gbi' style='color: blue; ",
          "text-decoration: none;'>b3gbi</a> to calculate and visualise widely ",
          "used biodiversity indicators from a data cube; either one created ",
          "using <a href='https://www.gbif.org/' style='color: blue; ",
          "text-decoration: none;'>GBIF</a> or one created from your own data.",
          "<br><br>Start by uploading your data cube using the file browser in ",
          "the left-hand panel. You can also use this panel to choose the ",
          "biodiversity indicator(s), taxa, geographical area, and temporal ",
          "window of interest for your data. Use the tabs to visualize the ",
          "outputs.<br></p>"
        )
      ),
      style = "font-size: 16px; color: #555;"
    )

  ),

  sidebarLayout(

    #######################################################
    ####################### Inputs ########################
    #######################################################
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Data cube",
          fileInput(
            inputId = "dataCube",
            label = HTML("Upload a data cube")
          )
        ),

        tabPanel(
          "Input filters",
          div(class = "scrollable-tab",
              # Indicator
              selectInput(
                inputId = "indicatorsToAnalyse",
                label = "Biodiversity Indicator", multiple = FALSE,
                choices = as.character(sapply(b3gbi::available_indicators, "[[", 2))
              ),
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
                      multiple = T,
                      options = list(
                        create = T,
                        delimiter = " ",
                        persist = F,
                        plugins = list("remove_button"),
                        createOnBlur = T
                      )
                    )
                )
              ),

              # Map resolution
              selectInput(
                inputId = "mapres",
                label = "Map resolution",
                choices = c(
                  "110",
                  "50",
                  "10"
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

              # Select by family name
              selectInput( ## select taxa from the database
                inputId = "family",
                label = "Subset by family",
                choices = NULL,
                multiple = T
              ),

              # Select by species scientific name
              selectInput( ## select taxa from the database
                inputId = "species",
                label = "Subset by species",
                choices = NULL,
                multiple = T
              )
          )
        ),


        ############# Visualization options

        tabPanel(
          title = "Visualization Options",
          div(class = "scrollable-tab",
              HTML("<br>"),

              ############# General visualization options

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
                           "wrap_length",
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
              ),
              checkboxInput(
                "ts_options",
                "Show Time Series Visualization Options"
              ),

              ############# Time series visualization options

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
              ),

              ############# Map visualization options

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
                  "europe_crop_eea",
                  paste0("Crop to mainland Europe (only applies to continental ",
                         "Europe and EEA grid"),
                  value = FALSE
                ),
                checkboxInput(
                  "crop_to_grid",
                  "Crop map to edges of grid",
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
                      "panel_bg",
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
                # # Create a box for custom ggplot2 code
                # # This is commented out due to potential security concerns
                # tags$hr(),
                # textAreaInput(
                #   "ggplot_code",
                #   "Customize plot using ggplot2 code",
                #   value = "",
                #   rows = 5,
                #   placeholder = "Enter custom code here... e.g. theme(panel.border = element_blank())"
                # ),
                # em("*Please note that your code will only be evaluated when you",
                # "manually click the 'Plot Map' button")
              )#,
          )
        )
      )
    ),


    #######################################################
    ####################### Outputs #######################
    #######################################################

    # output = tables, plots, texts
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Summary",
          div(class = "scrollable-tab",
              HTML("<br>"), # Adding line break for spacing
              HTML(
                "Here you can view the metadata summarising your data cube."
              ),
              HTML("<br>"),
              HTML("<br>"),
              verbatimTextOutput("metadata"),
              HTML("<br>"),
              HTML("<br>")
          )
        ),

        ############################# Map tab
        tabPanel(
          title = "Map",
          div(class = "scrollable-tab",
              HTML("<br>"),  # Adding line break for spacing
              HTML(
                "Here you can view your selected biodiversity indicator ",
                "projected onto a map. Use the input filters to select the ",
                "indicator, taxa, geographical area, and temporal window of ",
                "interest, and the visualization options to change how the map ",
                "looks."
              ),
              HTML("<br>"),  # Adding line break for spacing
              HTML("<br>"),  # Adding line break for spacing
              em(
                "(Loading the plot could take a few minutes, depending on the ",
                "options you have selected.)"
              ),
              HTML("<br>"),  # Adding line break for spacing
              HTML("<br>"),
              actionButton("plot_map_bt", "Plot Map"),
              HTML("<br>"), # Adding line break for spacing
              HTML("<br>"), # Adding line break for spacing
              uiOutput("plot_map_container"),
              HTML("<br>"), # Adding line break for spacing
              p(strong("What am I looking at?")),
              textOutput("figure_legend_map_text"),
              HTML("<br>"),
              # Adding line break for spacing
              p(strong("Background information on this indicator:")),
              uiOutput("indicator_background_text_map"),
              HTML("<br>"),
              ########### placer
              fluidRow(
                column(
                  selectInput(
                    inputId = "downloadOptions_map",
                    label = "Download Formats",
                    choices = c(
                      "JPEG",
                      "PDF",
                      "PNG",
                      "SVG",
                      "TIFF"
                    )
                  ),
                  width = 6
                ),
                column(
                  downloadButton("downloadGo_map"),
                  width = 4,
                  style = "padding:18px;"
                )
              ),
              fluidRow(
                column(
                  numericInput(
                    "dlmap_width",
                    "Map Width (in pixels)",
                    min = 100,
                    max = 10000,
                    step = 100,
                    value = 2000
                  ),
                  width = 3
                ),
                column(
                  numericInput(
                    "dlmap_height",
                    "Map Height (in pixels)",
                    min = 100,
                    max = 10000,
                    step = 100,
                    value = 2000
                  ),
                  width = 3
                ),
                column(
                  numericInput(
                    "dlmap_scaling",
                    "Scaling Factor",
                    min = 0.1,
                    max = 5,
                    step = 0.1,
                    value = 1
                  ),
                  width = 2
                )
              ),
              em(
                "Note: if there is extra white space in the downloaded image, try ",
                "adjusting the width or height. If the text and legend are too ",
                "large or too small, try adjusting the scaling factor (only ",
                "valid for JPG, PNG and TIFF, as other formats do not use the ",
                "scaling parameter)."
              )
          )
        ),

        ############################# Time Series tab
        tabPanel(
          title = "Time-series",
          div(class = "scrollable-tab",
              HTML("<br>"), # Adding line break for spacing
              HTML(
                "Here you can view a time-series plot of your selected ",
                "biodiversity indicator. Use the input filters to select the ",
                "indicator, taxa, geographical area, and temporal window of ",
                "interest, and the visualization options to control how the plot ",
                "looks."
              ),
              HTML("<br>"), # Adding line break for spacing
              HTML("<br>"), # Adding line break for spacing
              em(
                "(Loading the plot could take a few minutes, depending on the ",
                "options you have selected.)"
              ),
              HTML("<br>"),  # Adding line break for spacing
              HTML("<br>"),
              actionButton("plot_ts_bt", "Plot Time Series"),
              HTML("<br>"), # Adding line break for spacing
              HTML("<br>"), # Adding line break for spacing
              uiOutput("plot_ts_container"),
              HTML("<br>"),
              p(strong("What am I looking at?")),
              textOutput("figure_legend_ts_text"),
              HTML("<br>"),
              p(strong("Background information on this indicator:")),
              uiOutput("indicator_background_text_ts"),
              HTML("<br>"),
              fluidRow(
                column(
                  selectInput("downloadOptions_ts",
                              "Download Formats",
                              choices = c(
                                "JPEG",
                                "PDF",
                                "PNG",
                                "SVG",
                                "TIFF"
                              )
                  ),
                  width = 6
                ),
                column(
                  downloadButton("downloadGo_ts"),
                  width = 4,
                  style = "padding:18px;"
                )
              ),
              fluidRow(
                column(
                  numericInput(
                    "dlts_width",
                    "Plot Width (in pixels)",
                    min = 100,
                    max = 10000,
                    step = 100,
                    value = 2000
                  ),
                  width = 3
                ),
                column(
                  numericInput(
                    "dlts_height",
                    "Plot Height (in pixels)",
                    min = 100,
                    max = 10000,
                    step = 100,
                    value = 2000
                  ),
                  width = 3
                ),
                column(
                  numericInput(
                    "dlts_scaling",
                    "Scaling Factor",
                    min = 0.1,
                    max = 5,
                    step = 0.1,
                    value = 1
                  ),
                  width = 2
                )
              ),
              em(
                "Note: If the text and legend are too large or too small, try ",
                "adjusting the scaling factor (only valid for JPG, PNG and TIFF, ",
                "as other formats do not use the scaling parameter)."
              )
          )
        ),

        ########################## Table tab
        tabPanel(
          title = "Table",
          div(class = "scrollable-tab",
              HTML("<br>"),  # Adding line break for spacing
              HTML("Here you can view your processed data cube as a table."),
              HTML("<br>"),  # Adding line break for spacing
              HTML("<br>"),  # Adding line break for spacing
              em(
                "(Note that the various export options will only copy the ",
                "data from the current page of the table. To export ",
                "everything, first select 'all' in the 'Show' dropdown menu.)"
              ),
              HTML("<br>"), # Adding line break for spacing
              HTML("<br>"), # Adding line break for spacing
              div(
                style = "overflow-x: auto;",
                dataTableOutput("table")
              )
          )
        ),

        ########################## Export tab
        tabPanel(
          title = "Export",
          div(class = "scrollable-tab",
              HTML("<br>"),  # Adding line break for spacing
              HTML("<div>Download your processed data in .csv format.</div>"),
              HTML("<br>"), # Adding line break for spacing
              downloadButton("downloadProcessedCube",
                             label = "Processed Cube"
              ),
              downloadButton("downloadMappedCube",
                             label = "Mapped Cube"
              ),
              downloadButton("downloadTimeSeriesData",
                             label = "Time Series Data"
              )
          )
        ),
        # tabPanel(
        #   title = "Report",
        #   HTML("<br>"),  # Adding line break for spacing
        #   em(
        #     "In this tab you can view a report summarising the code that was ",
        #     "used to plot biodversity indicators from your data cube."
        #   ),
        #   textOutput("report_text"),
        #   HTML("<br>")  # Adding line break for spacing
        # ),

        ########################### Background tab
        tabPanel(
          title = "Background",
          div(class = "scrollable-tab",
              HTML("<br>"),
              HTML(
                "Here you can view technical information on the available ",
                "biodiversity indicators."
              ),
              h3("Biodiversity Indicators"),
              HTML("<br>"),
              em("Occurrences"),
              p(strong("Total Occurrences")),
              p(
                "The total number of occurrences is calculated by summing the ",
                "occurrences of all species observed for each cell or year. This ",
                "variable provides an overview of the comprehensiveness and ",
                "distribution of data in the cube being analyzed, and may be ",
                "helpful, or even vital, for interpreting the results of ",
                "calculated indicators."
              ),
              p(strong("Density of Occurrences")),
              p(
                "Density is calculated by summing the total number of occurrences ",
                "per square km for each cell or year. This provides similar ",
                "information to total occurrences, but is adjusted for cell area."
              ),
              HTML("<br>"),
              em("Species Richness"),
              p(
                "Species richness is the total number of species present in a ",
                "sample (Magurran, 1988). It is a fundamental and commonly used ",
                "measure of biodiversity, providing a simple and intuitive ",
                "overview of the status of biodiversity. However, richness is not ",
                "well suited to measuring biodiversity change over time, as it ",
                "only decreases when local extinctions occur and thus lags behind ",
                "abundance for negative trends. While it may act as a leading ",
                "indicator of alien species invasions, it will not indicate ",
                "establishment because it ignores abundance. Nor will it ",
                "necessarily indicate changes in local species composition, which ",
                "can occur without any change in richness. Although richness is ",
                "conceptually simple, it can be measured in different ways."
              ),
              p(strong("Observed Richness")),
              p(
                "Observed richness is calculated by summing the number of unique ",
                "species observed for each year or each cell. Observed richness is ",
                "highly dependent on the comprehensiveness of the dataset it is being ",
                "applied to. If some regions are more intensively, carefully, or ",
                "systematically sampled than others, this will likely result in higher ",
                "observed richness. Observed richness also depends on the relative ",
                "abundance and spatial aggregation of each species, with less abundant ",
                "and less aggregated species less likely to be discovered during surveys ",
                "(Hillebrand et al., 2018), as well as the detectability of each species. ",
              ),
              p(strong("Cumulative Species Richness")),
              p(
                "Cumulative richness is calculated by adding the newly observed ",
                "unique species each year to a cumulative sum. This indicator ",
                "provides an estimation of whether and how many new species are ",
                "still being discovered in a region. While an influx of alien ",
                "species could cause an increase in cumulative richness, a fast-",
                "rising trend is likely an indication that the ",
                "dataset is not comprehensive and therefore observed richness ",
                "will provide an underestimate of species richness."
              ),
              HTML("<br>"),
              em("Evenness"),
              p(
                "Species evenness is a commonly used indicator that measures how ",
                "uniformly individuals are distributed across species in a region ",
                "or over time. It provides a complement to richness by taking ",
                "relative abundance into account. Although GBIF provides ",
                "information about abundances as individual counts, the majority ",
                "of entries lack this information. Hence, evenness can only be ",
                "calculated using the proportions of observations rather than ",
                "proportions of individuals. Strictly speaking, the evenness ",
                "measures therefore indicate how uniformly species are ",
                "represented in the respective data set rather than the true ",
                "evenness of the ecological community."
              ),
              p(strong("Pielou's Evenness")),
              shiny::withMathJax(
                p(
                  "Pielou’s evenness (1966) is a well-known and commonly ",
                  "used evenness measure. It is calculated as: ",
                  "$$ E = -\\sum_{i=1}^{S} p_i \\ln(p_i) / \\ln(S) $$ ",
                  "where S is the number of species and pi is the proportion of occurrences ",
                  "represented by species i."
                )
              ),
              p(strong("Williams' Evenness")),
              shiny::withMathJax(
                p(
                "An analysis of evenness properties by Kvålseth (2015) showed ",
                "that an evenness index introduced by Williams in 1977 in an ",
                "unpublished manuscript has two important properties which ",
                "Pielou’s does not. The properties in question are complex ",
                "mathematical properties known as the Schur-Concavity and ",
                "value validity, but we attempt to describe them here more ",
                "simply. If a measure of evenness is Schur-concave, it means ",
                "that when the distribution of individuals becomes more evenly ",
                "spread across species, the measure of evenness will stay the ",
                "same or increase, but never decrease. Value validity means ",
                "that an evenness index should provide sensible and meaningful ",
                "values across its range for any given distribution of species ",
                "abundances. Kvålseth referred to this evenness measure as E9 ",
                "but we refer to it as Williams’ evenness.",
                "Williams' evenness is calculated as: ",
                "$$ 1 - \\left[ \\frac{\\left( S\\sum_{i=1}^{S} p_i^2 - 1 \\right) }{S - 1}\\right]^{1/2} $$ ",
                "where S is the number of species and pi is the proportion of occurrences ",
                "represented by species i."
              )
              ),
              em("Hill Diversity"),
              shiny::withMathJax(
                p(
                  "Hill (1973) introduced the concept of Hill diversity, which assumes ",
                  "that the number and relative abundance of species are inseparable ",
                  "components of diversity. Hill diversity uses a single equation to ",
                  "calculate multiple measures of diversity by varying a single ",
                  "parameter ℓ, which changes the emphasis on rare vs common species ",
                  "(Roswell et al., 2019). It represents the mean rarity of sampled ",
                  "species, and is calculated as: ",
                  "$$ \\displaystyle D = \\left( \\sum_{i=1}^{S} p_i r_i^\\ell \\right)^{1/\\ell} $$ ",
                  "where D is diversity, S is the number of species, pi is the proportion ",
                  "of individuals belonging to species i, ri is the rarity of species i, ",
                  "and ℓ determines the rarity scale for the mean. While ℓ can ",
                  "theoretically take almost any value, three common measures of diversity ",
                  "are special cases: species richness, and modified versions of the ",
                  "Shannon and Simpson diversity indices (Roswell et al., 2019). These ",
                  "three measures occur when ℓ takes the value of 1, 0 (or near-zero, ",
                  "as ℓ cannot actually take the value of 0), or -1, respectively. ",
                  "Richness uses an arithmetic scale (the arithmetic mean), thus giving ",
                  "rare species a lot of leverage. By contrast, Hill-Shannon diversity ",
                  "uses a logarithmic scale (the geometric mean), treating common and ",
                  "rare species equally, and Hill-Simpson diversity uses a reciprocal ",
                  "scale (the harmonic mean), giving common species higher leverage."
                )
              ),
              p(strong("Species Richness")),
              p(
                "Using the Hill diversity equation, richness becomes simply S, ",
                "the number of species, and is thus identical to richness ",
                "calculated without Hill diversity."
              ),
              p(strong("Hill-Shannon Diversity")),
              p(
                "Hill-Shannon diversity is actually e (base of the natural log) raised ",
                "to the power of the Shannon index. It is estimated for each year or ",
                "cell count using the iNEXT package, standardized by coverage, as: ",
                "$$ \\displaystyle e^{-\\sum_{i=1}^{S} p_i \\ln(p_i)} $$ ",
                "where S is the number of species and pi is the proportion of occurrences ",
                "represented by species i."
              ),
              p(strong("Hill-Simpson Diversity")),
              shiny::withMathJax(
              p(
                "Hill-Simpson diversity is the inverse of the Simpson index. ",
                "It is estimated using the iNEXT package for each year or cell, ",
                "standardized by coverage, as:",
                "$$ \\displaystyle \\frac{1}{\\sum_{i=1}^{S} p_i^2} $$",
                "where S is the number of species and pi is the proportion of ",
                "occurrences represented by species i.",
                "Both Hill-Simpson and Hill-Shannon diversity describe a combination ",
                "of richness and evenness that reduce the inadequacies of either ",
                "measure alone."
              )
              ),
              em("Rarity"),
              p(
                "Rarity is the scarcity or infrequency of a particular species in ",
                "an area. A rare species might have a small population size, a ",
                "limited distribution, or a unique ecological niche (Maciel, ",
                "2021; Rabinowitz, 1981). Rarity can also be a biodiversity ",
                "indicator when summed over multiple species in an area, and may ",
                "provide important insight for determining conservation ",
                "priorities. When measured over time, rarity may indicate ",
                "potential threats or changes in the environment."
              ),
              p(strong("Abundance-Based Rarity")),
              shiny::withMathJax(
              p(
                "Abundance-based rarity is the inverse of the proportion of total occurrences ",
                "represented by a particular species. The total summed rarity for each grid ",
                "cell or year is calculated (sum the rarity values of each species present ",
                "there). It is calculated as: ",
                "$$ \\sum_{i=1}^{S} 1/p_i$$ ",
                "where S is the number of species and pi is the proportion of occurrences ",
                "represented by species i."
              )
              ),
              p(strong("Area-Based Rarity")),
              shiny::withMathJax(
                p(
                "Area-based rarity is the inverse of occupancy frequency (proportion of grid ",
                "cells occupied) for a particular species. The total summed rarity for each ",
                "grid cell or year is calculated (sum the rarity values of each species ",
                "present there). It is calculated as: ",
                "$$ \\sum_{i=1}^{S} N/n_i$$ ",
                "where S is the number of species, N is the total number of occupied grid ",
                "cells, and ni is the number of grid cells occupied by species i."
              )
              ),
              HTML("<br>"),
              p(strong("Mean Year of Occurrence")),
              p(
                "The mean year of occurrence is calculated per cell, giving an ",
                "indication of how recent the data is for each cell. A recent ",
                "mean year is not necessarily an indication of quality, as some ",
                "countries or regions have been conducting comprehensive ",
                "biodiversity monitoring for many years and will therefore ",
                "reflect an older mean year of occurrence, while others may show ",
                "a recent mean year due to e.g., the sudden availability of large ",
                "amounts of citizen science data."
              ),
              HTML("<br>"),
              p(strong("Taxonomic Distinctness")),
              shiny::withMathJax(
                p(
                  "Taxonomic distinctness measures the taxonomic relatedness between ",
                  "species, providing a measure of biodiversity that accounts for ",
                  "evolutionary relationships. A distance matrix based on pairwise ",
                  "taxonomic relationships is calculated for each cell using the taxize ",
                  "package (Chamberlain & Szöcs, 2013; Chamberlain et al., 2020), then ",
                  "taxonomic distinctness is calculated as the Taxonomic Distinctness ",
                  "Index (TDI; Clarke & Warwick, 1999): ",
                  "$$ \\displaystyle \\frac{\\sum \\sum_{i<j} \\frac{|R_i - R_j|}{L}}{\\frac{S(S-1)}{2}} $$ ",
                  "where S is the number of species, Ri and Rj are the taxonomic ranks ",
                  "of species i and j (from the GBIF Taxonomic Backbone), and L is the ",
                  "maximum number of taxonomic ranks. The double summation syntax here ",
                  "is to explicitly denote iteration over all unique pairs (i,j) with i < j."
                )
              )
          )
        ),

        ########################### About tab
        tabPanel(
          title = "About",
          div(class = "scrollable-tab",
              HTML("<br>"),  # Adding line break for spacing
              HTML(
                paste0(
                  "This Shiny app was developed by: <br><br>",
                  "Shawn Dove <br>",
                  "Yanina Sica <br>",
                  "Lissa Breugelmans <br>",
                  "Melanie De Nolf <br>",
                  "Arvin C. Diesmos <br>",
                  "Mathias Dillen <br>",
                  "Fábio Matos <br>",
                  "Arman Pili <br>",
                  "<br>",
                  "The app is a graphical front end for the b3gbi R package, ",
                  "an output of the B-cubed project.",
                  "<br><br>",
                  "<div>For more information about the b3gbi R package ",
                  "please visit the ",
                  "<a href='https://github.com/b-cubed-eu/b3gbi/' ",
                  "style='color: blue; text-decoration: none;'> ",
                  "GitHub page</a> or the ",
                  "<a href='https://b-cubed-eu.r-universe.dev/b3gbi' ",
                  "style='color: blue; text-decoration: none;'> ",
                  "R Universe page </a>",
                  "or the <a href='https://ec.europa.eu/info/funding-tenders/",
                  "opportunities/portal/screen/opportunities/horizon-results-",
                  "platform/83298' style='color: blue; text-decoration: ",
                  "none;'> EU Horizon Results Platform page</a>.",
                  "<br><br>",
                  "For more information about the B-Cubed project ",
                  "please visit the <a href='https://b-cubed.eu/' ",
                  "style='color: blue; text-decoration: none;'>B-Cubed ",
                  "website</a>.</div>",
                  "<br>",
                  "B-Cubed (Biodiversity Building Blocks for Policy) receives ",
                  "funding from the European Union’s Horizon Europe Research ",
                  "and Innovation Programme (ID No 101059592).",
                  "<br><br>",
                  "This app is licensed under the MIT License.</div>"
                )
              ),
              HTML("<br>")  # Adding line break for spacing
          )
        )
      )
    )
  )
)


###############################################################################################################
#####   SERVER     ############################################################################################
###########################   SERVER     ######################################################################
##################################################   SERVER     ###############################################
#########################################################################   SERVER     ########################
###############################################################################################################


server <- function(input, output, session) {

  options(shiny.maxRequestSize = 500 * 1024^2)

  ################################ GENERAL reactives and observers

  r <- reactiveValues(dataCube = NULL, dataCube1 = NULL)

  observeEvent(input$dataCube, {
    # Load and prepare GBIF data cube
    r$dataCube <- process_cube(input$dataCube$datapath)

    # Create copy of cube for filtering
    r$dataCube1 <- r$dataCube
  })

  # Update region selection box when region options change
  observeEvent(regionupdate, {
    choices <- regionupdate()
    updateSelectInput(
      inputId = "region",
      choices = choices
    )
  })

  # Change input$region options based on user-selected spatial level
  regionupdate <- reactive({
    if (input$spatiallevel == "continent") {
      continents
    } else if (input$spatiallevel == "country") {
      if (input$countrytype == "countries") {
        if (input$mapres == "10") {
          countries10$ADMIN
        } else if (input$mapres == "50") {
          countries50$ADMIN
        } else if (input$mapres == "110") {
          countries110$ADMIN
        } else {
          NULL
        }
      } else if (input$countrytype == "map_units") {
        if (input$mapres == "10") {
          mapunits10$ADMIN
        } else if (input$mapres == "50") {
          mapunits50$ADMIN
        } else if (input$mapres == "110") {
          mapunits110$ADMIN
        } else {
          NULL
        }
      } else if (input$countrytype == "sovereignty") {
        if (input$mapres == "10") {
          sovereignties10$ADMIN
        } else if (input$mapres == "50") {
          sovereignties50$ADMIN
        } else if (input$mapres == "110") {
          sovereignties110$ADMIN
        } else {
          NULL
        }
      } else if (input$countrytype == "tiny_countries") {
        if (input$mapres == "50") {
          tinycountries50$ADMIN
        } else if (input$mapres == "110") {
          tinycountries110$ADMIN
        } else {
          NULL
        }
      } else {
        NULL
      }
    } else if (input$spatiallevel == "sovereignty") {
      if (input$countrytype == "countries") {
        if (input$mapres == "10") {
          countries10$SOVEREIGNT
        } else if (input$mapres == "50") {
          countries50$SOVEREIGNT
        } else if (input$mapres == "110") {
          countries110$SOVEREIGNT
        } else {
          NULL
        }
      } else if (input$countrytype == "map_units") {
        if (input$mapres == "10") {
          mapunits10$SOVEREIGNT
        } else if (input$mapres == "50") {
          mapunits50$SOVEREIGNT
        } else if (input$mapres == "110") {
          mapunits110$SOVEREIGNT
        } else {
          NULL
        }
      } else if (input$countrytype == "sovereignty") {
        if (input$mapres == "10") {
          sovereignties10$SOVEREIGNT
        } else if (input$mapres == "50") {
          sovereignties50$SOVEREIGNT
        } else if (input$mapres == "110") {
          sovereignties110$SOVEREIGNT
        } else {
          NULL
        }
      } else if (input$countrytype == "tiny_countries") {
        if (input$mapres == "50") {
          tinycountries50$SOVEREIGNT
        } else if (input$mapres == "110") {
          tinycountries110$SOVEREIGNT
        } else {
          NULL
        }
      } else {
        NULL
      }
    } else if (input$spatiallevel == "geounit") {
      if (input$countrytype == "countries") {
        if (input$mapres == "10") {
          countries10$GEOUNIT
        } else if (input$mapres == "50") {
          countries50$GEOUNIT
        } else if (input$mapres == "110") {
          countries110$GEOUNIT
        } else {
          NULL
        }
      } else if (input$countrytype == "map_units") {
        if (input$mapres == "10") {
          mapunits10$GEOUNIT
        } else if (input$mapres == "50") {
          mapunits50$GEOUNIT
        } else if (input$mapres == "110") {
          mapunits110$GEOUNIT
        } else {
          NULL
        }
      } else if (input$countrytype == "sovereignty") {
        if (input$mapres == "10") {
          sovereignties10$GEOUNIT
        } else if (input$mapres == "50") {
          sovereignties50$GEOUNIT
        } else if (input$mapres == "110") {
          sovereignties110$GEOUNIT
        } else {
          NULL
        }
      } else if (input$countrytype == "tiny_countries") {
        if (input$mapres == "50") {
          tinycountries50$GEOUNIT
        } else if (input$mapres == "110") {
          tinycountries110$GEOUNIT
        } else {
          NULL
        }
      } else {
        NULL
      }
    } else {
      NULL
    }
  })

  # Change map resolution options based on selected country type
  reschoiceupdate <- reactive({
    if (input$countrytype == "tiny_countries") {
      c("50", "110")
    } else {
      c("10", "50", "110")
    }
  })

  # Update map resolution selection box when map resolution options change
  observeEvent(reschoiceupdate(), {
    choices <- reschoiceupdate()
    updateSelectInput(
      inputId = "mapres",
      choices = choices,
      selected = "50"
    )
  })

  # Change country type options based on spatial level and map resolution
  countrytypeupdate <- reactive({
    if (input$spatiallevel == "continent" |
      input$spatiallevel == "world" |
      input$spatiallevel == "cube") {
      NULL
    } else if (input$mapres == "10") {
      c("countries", "map_units", "sovereignty")
    } else {
      c("countries", "map_units", "sovereignty", "tiny_countries")
    }
  })

  # Update country type selection box when country type options change
  observeEvent(countrytypeupdate(), {
    choices <- countrytypeupdate()
    updateSelectInput(
      inputId = "countrytype",
      choices = choices
    )
  })

  # Update spatial level options based on user-selected spatial level
  observeEvent(input$dataCube, {
    req(r$dataCube)

    units <- stringr::str_extract(
      r$dataCube$resolutions,
      "(?<=[0-9,.]{1,6})[a-z]*$")
    if (units == "degrees") {
      res_size <- as.numeric(stringr::str_extract(
        r$dataCube$resolutions,
        "[0-9,.]*(?=degrees)"
      ))
      defaultres <- ifelse(res_size > 0.25, res_size, 0.25)
      maxres <- 10
    } else if (units == "km") {
      res_size <- as.numeric(stringr::str_extract(
        r$dataCube$resolutions,
        "[0-9]*(?=km)"
      ))
      defaultres <- ifelse(res_size > 10, res_size, 10)
      maxres <- 100
    }

    updateNumericInput(
      inputId = "cellsize",
      min = res_size,
      max = maxres,
      value = defaultres,
      step = res_size
    )

    daterangemin <- r$dataCube1$first_year
    daterangemax <- r$dataCube1$last_year
    value <- c(daterangemin, daterangemax)

    updateSliderInput(
      inputId = "daterange",
      min = daterangemin,
      max = daterangemax,
      value = value
    )
  })

  # Get all families in the data cube
  all_families <- reactive({
    req(r$dataCube)
    sort(unique(r$dataCube$data$family))
  })

  # Get all species in the data cube
  all_species <- reactive({
    req(r$dataCube)
    sort(unique(r$datCube$data$scientificName))
  })

  # Update family selection based on available families
  observeEvent(all_families(), {
    updateSelectInput(
      session,
      inputId = "family",
      choices = all_families(),
      selected = input$family
    )
  })

  # Update species selection based on family selection
  observeEvent(input$family, {
    available_species <- all_species()

    if (is.null(input$family) || length(input$family) == 0) {
      updateSelectInput(session,
        inputId = "species",
        choices = available_species,
        selected = NULL
      )
      r$dataCube1$data <- r$dataCube$data # Reset data when family is deselected

      # Debug print statement to ensure filtering is being applied
      print(paste("Filtered data rows:", nrow(r$dataCube1$data)))
    } else {
      available_species <- r$dataCube$data %>%
        dplyr::filter(family %in% input$family) %>%
        dplyr::pull(scientificName) %>%
        unique() %>%
        sort()
      updateSelectInput(
        session,
        inputId = "species",
        choices = available_species,
        selected = intersect(input$species, available_species) # Retain valid selections
      )
    }
  })

  # Filter data based on selected families and species
  observeEvent(list(input$family, input$species), {
    req(r$dataCube)

    # Print current input state for debugging
    if (length(input$family) == 0 && length(input$species) == 0) {
      print("Both families and species are deselected.")
    } else {
      print(paste(
        "Current Families:",
        ifelse(length(input$family) == 0, "None", paste(input$family, collapse = ", "))
      ))
      print(paste(
        "Current Species:",
        ifelse(length(input$species) == 0, "None", paste(input$species, collapse = ", "))
      ))
    }

    # Start with the full dataset
    filtered_data <- r$dataCube$data

    # Filter by family if any family is selected
    if (length(input$family) > 0) {
      filtered_data <- filtered_data[filtered_data$family %in% input$family, ]
    }

    # Further filter by species if any species is selected
    if (length(input$species) > 0) {
      filtered_data <- filtered_data[filtered_data$scientificName %in% input$species, ]
    }

    # Update the reactive value with the filtered or reset data
    r$dataCube1$data <- filtered_data

    # Debug print statement to ensure filtering is being applied
    print(paste("Filtered data rows:", nrow(filtered_data)))
  })

  # Update species selection based on family selection
  observeEvent(input$species, {
    if (is.null(input$species) || length(input$species) == 0) {
      if (is.null(input$family) || length(input$family) == 0) {
        r$dataCube1$data <- r$dataCube$data # Reset data when species and family are deselected
      } else {
        r$dataCube1$data <- r$dataCube$data %>%
          dplyr::filter(family %in% input$family)
      }
    }
  })

  ############################ time series visualization observe events

  # Update time series axes expansion when custom axes checkbox is deselected
  observeEvent(input$ts_expand, {
    if (input$ts_expand == FALSE) {
      updateNumericInput(
        session,
        inputId = "ts_x_expand_left",
        value = 0
      )
      updateNumericInput(
        session,
        inputId = "ts_x_expand_right",
        value = 0
      )
      updateNumericInput(
        session,
        inputId = "ts_y_expand_top",
        value = 0
      )
      updateNumericInput(
        session,
        inputId = "ts_y_expand_bottom",
        value = 0
      )
    }
  })

  ############################ map visualization observe events

  # Update map axes when custom map axes checkbox is deselected
  observeEvent(input$custom_map_axes, {
    if (input$custom_map_axes == FALSE) {
      updateNumericInput(
        session,
        inputId = "xcoord_min",
        value = ""
      )
      updateNumericInput(
        session,
        inputId = "xcoord_max",
        value = ""
      )
      updateNumericInput(
        session,
        inputId = "ycoord_min",
        value = ""
      )
      updateNumericInput(
        session,
        inputId = "ycoord_max",
        value = ""
      )
    }
  })

  # Update legend limits when custom legend checkbox is deselected
  observeEvent(input$legend_limits, {
    if (input$legend_limits == FALSE) {
      updateTextInput(
        session,
        inputId = "legend_min",
        value = ""
      )
      updateTextInput(
        session,
        inputId = "legend_max",
        value = ""
      )
    }
  })

  # Update panel background (ocean) colour when custom_bg checkbox is deselected
  observeEvent(input$custom_bg, {
    if (input$custom_bg == FALSE) {
      updateTextInput(
        session,
        inputId = "panel_bg",
        value = "#92c5f0"
      )
    }
  })

  # Update land fill colour when custom_land_fill checkbox is deselected
  observeEvent(input$custom_land_fill, {
    if (input$custom_land_fill == FALSE) {
      updateTextInput(
        session,
        inputId = "land_fill_colour",
        value = "grey85"
      )
    }
  })

  # Convert text input for legend breaks into a numeric vector
  parsed_breaks <- reactive({
    inputText <- input$breaks
    # Remove spaces and split by comma
    breakpointVector <- unlist(strsplit(inputText, ",\\s*"))
    # Convert to numeric, suppress any conversion warnings of invalid entries
    as.numeric(breakpointVector)
  })

  # Convert text input for legend break labels into a character vector
  parsed_labels <- reactive({
    inputLabels <- input$labels
    if (inputLabels == "") {
      return(NULL)
    }
    labelVector <- unlist(strsplit(inputLabels, ",\\s*"))
    labelVector
  })

  # Convert text input for x-axis breaks into a numeric vector
  parsed_xbreaks <- reactive({
    inputText <- input$xbreaks
    # Remove spaces and split by comma
    breakpointVector <- unlist(strsplit(inputText, ",\\s*"))
    # Convert to numeric, suppress any conversion warnings of invalid entries
    as.numeric(breakpointVector)
  })

  # Convert text input for y-axis breaks into a numeric vector
  parsed_ybreaks <- reactive({
    inputText <- input$ybreaks
    # Remove spaces and split by comma
    breakpointVector <- unlist(strsplit(inputText, ",\\s*"))
    # Convert to numeric, suppress any conversion warnings of invalid entries
    as.numeric(breakpointVector)
  })

  ############################ metadata tab outputs

  # output metadata from imported cube
  output$metadata <- renderPrint({
    req(r$dataCube1)
    r$dataCube1
  })

  ############################ table tab outputs

  # output interactive table from imported cube
  output$table <- renderDataTable({
    req(r$dataCube1)
    datatable(r$dataCube1$data,
              # n = 0,
              extensions = "Buttons",
              options = list(
                dom = "Blfrtip",
                buttons = c("colvis", "copy", "csv", "excel", "pdf"),
                lengthMenu = list(c(10, 25, 100, 1000, -1),
                                  c('10', '25', '100', '1000', 'All')),
                pageLength = 10,
                scrollX = TRUE,
                scrollY = "500px"
              )
    )
  })


  ############################ map tab outputs

  # output custom legend breaks
  output$parsed_breaks <- renderPrint({
    parsed_breaks()
  })

  # output custom legend break labels
  output$parsed_labels <- renderPrint({
    parsed_labels()
  })

  # output custom legend breaks
  output$parsed_xbreaks <- renderPrint({
    parsed_xbreaks()
  })

  # output custom legend break labels
  output$parsed_ybreaks <- renderPrint({
    parsed_ybreaks()
  })

  # create map from imported cube
  plot_to_render_map <- eventReactive(input$plot_map_bt, {
    req(r$dataCube1)

    if (!input$mapres %in% c("10", "50", "110")) {
      showNotification("Map resolution is not properly selected.", type = "error")
      return(NULL)
    }

    tryCatch(
      {
        withCallingHandlers(
          {
            mapres <- switch(input$mapres,
              "110" = "small",
              "50" = "medium",
              "10" = "large"
            )

            # When region is empty, ensure parameter is handled
            region_param <- if (length(input$region) > 0) input$region else NULL

            if (input$custom_output_crs == FALSE || input$output_crs == "") {
              output_crs <- NULL
            } else {
              output_crs <- paste0("EPSG: ", input$output_crs)
            }

            params <- list(
              data = r$dataCube1,
              cell_size = input$cellsize,
              level = input$spatiallevel,
              first_year = input$daterange[1],
              last_year = input$daterange[2],
              ne_type = input$countrytype,
              ne_scale = mapres,
              region = region_param,
              output_crs = output_crs
            )

            map <- do.call(
              switch(input$indicatorsToAnalyse,
                "Observed Species Richness" = obs_richness_map,
                "Total Occurrences" = total_occ_map,
                "Pielou's Evenness" = pielou_evenness_map,
                "Williams' Evenness" = williams_evenness_map,
                "Cumulative Species Richness" = NULL,
                "Density of Occurrences" = occ_density_map,
                "Abundance-Based Rarity" = ab_rarity_map,
                "Area-Based Rarity" = area_rarity_map,
                "Mean Year of Occurrence" = newness_map,
                "Taxonomic Distinctness" = tax_distinct_map,
                "Species Richness (Estimated by Coverage-Based Rarefaction)" - hill0_map,
                "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)" - hill1_map,
                "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)" - hill2_map,
                "Species Occurrences" = spec_occ_map,
                "Species Range" = spec_range_map,
                "Occupancy Turnover" = NULL

              ),
              params
            )

            map # Return the map object
          },
          warning = function(w) {
            showNotification(paste("Warning:", conditionMessage(w)),
                             type = "warning")
            invokeRestart("muffleWarning")
          }
        )
      },
      error = function(e) {
        showNotification(paste("Error:", conditionMessage(e)),
                         type = "error", duration = NULL)
        return(NULL)
      }
    )
  })

  # output plot from imported cube
  plot_to_print_map <- reactive({
    req(plot_to_render_map())

    # Check if custom coordinates are provided and format appropriately
    if (input$xcoord_min == "" && input$xcoord_max == "") {
      xlims <- NULL
    } else if (input$xcoord_min == "" || input$xcoord_max == "") {
      stop("To plot custom coordinates you must provide both min and max.")
    } else {
      xlims <- c(as.numeric(input$xcoord_min), as.numeric(input$xcoord_max))
    }
    if (input$ycoord_min == "" && input$ycoord_max == "") {
      ylims <- NULL
    } else if (input$ycoord_min == "" || input$ycoord_max == "") {
      stop("To plot custom coordinates you must provide both min and max.")
    } else {
      ylims <- c(as.numeric(input$ycoord_min), as.numeric(input$ycoord_max))
    }

    # Get parsed breaks and labels
    breaks <- parsed_breaks()
    labels <- parsed_labels()
    xbreaks <- parsed_xbreaks()
    ybreaks <- parsed_ybreaks()

    # Check if breaks are valid
    if (is.null(breaks) ||
        input$breaks == "" ||
        any(is.na(breaks))) {
      breaks <- NULL
    }

    # Check if labels are valid
    if (is.null(labels) ||
        input$labels == "" ||
        length(labels) != length(breaks)) {
      labels <- NULL
    }

    # Check if x-axis breaks are valid
    if (is.null(xbreaks) ||
        input$xbreaks == "" ||
        any(is.na(xbreaks))) {
      xbreaks <- NULL
    }

    # Check if y-axis breaks are valid
    if (is.null(ybreaks) ||
        input$ybreaks == "" ||
        any(is.na(ybreaks))) {
      ybreaks <- NULL
    }

    # Check if custom legend limits are provided and format appropriately
    if (input$legend_min == "" && input$legend_max == "") {
      legend_limits <- NULL
    } else if (input$legend_min == "" || input$legend_max == "") {
      stop("To plot custom legend limits you must provide both min and max.")
    } else {
      legend_limits <- c(as.numeric(input$legend_min),
                         as.numeric(input$legend_max))
    }

    # Check if title is provided
    if (input$title == "") {
      title <- NULL
    } else {
      title <- input$title
    }

    # Check if legend title is provided
    if (input$legend_title == "") {
      legend_title <- NULL
    } else {
      legend_title <- input$legend_title
    }

    # Check if transformation is provided
    if (input$trans_yesno == FALSE ||
        is.null(input$trans_yesno) ||
        is.null(input$trans)) {
      trans <- NULL
    } else {
      trans <- input$trans
    }

    # Check if bcpower is provided
    if (is.null(input$bcpower) || input$bcpower == "") {
      bcpower <- NULL
    } else {
      bcpower <- input$bcpower
    }

    # Check if land fill colour is provided
    if (input$land_fill_colour == "") {
      land_fill_colour <- NULL
    } else {
      land_fill_colour <- input$land_fill_colour
    }

    # Check if panel background colour is provided
    if (input$panel_bg == "" || is.null(input$panel_bg)) {
      panel_bg <- NULL
    } else {
      panel_bg <- input$panel_bg
    }

    # Check if subtitle is provided
    if (input$subtitle == "") {
      subtitle <- NULL
    } else {
      subtitle <- input$subtitle
    }

    # Check if caption is provided
    if (input$caption == "") {
      caption <- NULL
    } else {
      caption <- input$caption
    }

    # Check that title, subtitle and caption font size inputs are ok
    if (input$title_size < 8 ||
        input$title_size > 40 ||
        is.na(input$title_size)
    ) {
      showNotification(
        paste0("Title font size is outside reasonable boundaries.",
               "Resetting to default."),
        type = "error")
      title_size <- NULL
    } else {
      title_size <- input$title_size
    }

    if (input$subtitle_size < 6 ||
        input$subtitle_size > 30 ||
        is.na(input$subtitle_size)
    ) {
      showNotification(
        paste0("Subtitle font size is outside reasonable boundaries.",
               "Resetting to default."),
        type = "error")
      subtitle_size <- NULL
    } else {
      subtitle_size <- input$subtitle_size
    }

    if (input$caption_size < 4 ||
        input$caption_size > 20 ||
        is.na(input$caption_size)
    ) {
      showNotification(
        paste0("Caption font size is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      caption_size <- NULL
    } else {
      caption_size <- input$caption_size
    }

    if (input$wrap_length < 20 ||
        input$wrap_length > 200 ||
        is.na(input$wrap_length)
    ) {
      showNotification(
        paste0("Title wrap length is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      wrap_length <- NULL
    } else {
      wrap_length <- input$wrap_length
    }

    if (input$legend_title_wrap_length < 10 ||
        input$legend_title_wrap_length > 100 ||
        is.na(input$legend_title_wrap_length)
    ) {
      showNotification(
        paste0("Title wrap length is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      legend_title_wrap_length <- NULL
    } else {
      legend_title_wrap_length <- input$legend_title_wrap_length
    }

    if (input$xaxis_fontsize < 0 ||
        input$xaxis_fontsize > 30 ||
        is.na(input$xaxis_fontsize)
    ) {
      showNotification(
        paste0("X-Axis label font size is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      xaxis_fontsize <- NULL
    } else {
      xaxis_fontsize <- input$xaxis_fontsize
    }

    if (input$yaxis_fontsize < 0 ||
        input$yaxis_fontsize > 30 ||
        is.na(input$yaxis_fontsize)
    ) {
      showNotification(
        paste0("Y-Axis label font size is outside reasonable boundaries. ",
               "Resetting to default."),
        type = "error")
      yaxis_fontsize <- NULL
    } else {
      yaxis_fontsize <- input$yaxis_fontsize
    }

    if (input$gridlines == TRUE) {
      gridlines <- element_line()
    } else {
      gridlines <- element_blank()
    }

    # Prepare parameters for plot
    params <- list(
      x = plot_to_render_map(),
      title = title,
      title_wrap_length = wrap_length,
      xlims = xlims,
      ylims = ylims,
      trans = trans,
      bcpower = bcpower,
      breaks = breaks,
      labels = labels,
      Europe_crop_EEA = input$europe_crop_eea,
      crop_to_grid = input$crop_to_grid,
      panel_bg = panel_bg,
      land_fill_colour = land_fill_colour,
      legend_title = legend_title,
      legend_limits = legend_limits,
      legend_title_wrap_length = legend_title_wrap_length
    )

    # Create plot
    map_plot <- do.call(plot, params)

    # Add other options to plot
    map_plot <- map_plot +
      labs(subtitle = subtitle,
           caption = caption) +
      theme(
        axis.text.x = element_text(size = xaxis_fontsize),
        axis.text.y = element_text(size = yaxis_fontsize),
        panel.grid = gridlines,
        plot.title = element_text(color = input$title_color,
                                  size = title_size,
                                  face = "bold"),
        plot.subtitle = element_text(color = input$subtitle_color,
                                     size = subtitle_size),
        plot.caption = element_text(color = input$caption_color,
                                    size = caption_size,
                                    face = "italic")
      )

    if (!is.null(xbreaks)) {
      map_plot <- map_plot + scale_x_continuous(breaks = xbreaks)
    }

    if (!is.null(ybreaks)) {
      map_plot <- map_plot + scale_y_continuous(breaks = ybreaks)
    }

    if (input$add_scalebar == TRUE) {

      if (input$scalebar_height > 0.1 ||
          input$scalebar_height < 0.01 ||
          is.na(input$scalebar_height)
      ) {
        showNotification(
          paste0("Scale bar height is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_height <- NULL
      } else {
        scalebar_height <- input$scalebar_height
      }

      if (input$scalebar_size > 1 ||
          input$scalebar_size < 0.01 ||
          is.na(input$scalebar_size)
      ) {
        showNotification(
          paste0("Scale bar size is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_size <- 0.2
      } else {
        scalebar_size <- input$scalebar_size
      }

      if (input$scalebar_tickheight > 2 ||
          input$scalebar_tickheight < 0 ||
          is.na(input$scalebar_tickheight)
      ) {
        showNotification(
          paste0("Scale bar tick height is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_tickheight <- 0.6
      } else {
        scalebar_tickheight <- input$scalebar_tickheight
      }

      if (input$scalebar_padx > 0.5 ||
          input$scalebar_padx < 0 ||
          is.na(input$scalebar_padx)
      ) {
        showNotification(
          paste0("Scale bar x-axis padding is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_padx <- 0.01
      } else {
        scalebar_padx <- input$scalebar_padx
      }

      if (input$scalebar_pady > 0.5 ||
          input$scalebar_pady < 0 ||
          is.na(input$scalebar_pady)
      ) {
        showNotification(
          paste0("Scale bar y-axis padding is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_pady <- 0.02
      } else {
        scalebar_pady <- input$scalebar_pady
      }

      if (input$scalebar_fontsize > 5 ||
          input$scalebar_fontsize < 0.1 ||
          is.na(input$scalebar_fontsize)
      ) {
        showNotification(
          paste0("Scale bar font size is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        scalebar_fontsize <- NULL
      } else {
        scalebar_fontsize <- input$scalebar_fontsize
      }

      map_plot <- map_plot +
        ggspatial::annotation_scale(
          location = input$scalebar_location,
          width_hint = scalebar_size,
          height = unit(scalebar_height, "npc"),
          pad_x = unit(scalebar_padx, "npc"),
          pad_y = unit(scalebar_pady, "npc"),
          bar_cols = c(input$scalebar_colour1, input$scalebar_colour2),
          text_cex = scalebar_fontsize,
          text_face = "bold",
          text_col = input$scalebar_fontcolour,
          style = input$scalebar_style,
          tick_height = scalebar_tickheight
        )
    }

    if (input$add_northarrow == TRUE) {

      # Define north arrow style function
      north_arrow_style_func <- switch(
        input$northarrow_style,
        "nautical" = ggspatial::north_arrow_nautical,
        "minimal" = ggspatial::north_arrow_minimal,
        "orienteering" = ggspatial::north_arrow_orienteering,
        "fancy_orienteering" = ggspatial::north_arrow_fancy_orienteering
      )

      if (input$northarrow_style == "minimal") {
        north_arrow_fill <- input$northarrow_fillcolour1
      } else {
        north_arrow_fill <- c(input$northarrow_fillcolour1,
                              input$northarrow_fillcolour2)
      }

      if (input$northarrow_height > 1 ||
          input$northarrow_height < 0.01 ||
          is.na(input$northarrow_height)
      ) {
        showNotification(
          paste0("North arrow height is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_height <- 0.1
      } else {
        northarrow_height <- input$northarrow_height
      }

      if (input$northarrow_width > 1 ||
          input$northarrow_width < 0.01 ||
          is.na(input$northarrow_width)
      ) {
        showNotification(
          paste0("North arrow width is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_width <- 0.1
      } else {
        northarrow_width <- input$northarrow_width
      }

      if (input$northarrow_textsize > 100 ||
          input$northarrow_textsize < 0 ||
          is.na(input$northarrow_textsize)
      ) {
        showNotification(
          paste0("North arrow text size is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_textsize <- 10
      } else {
        northarrow_textsize <- input$northarrow_textsize
      }

      if (input$northarrow_padx > 0.5 ||
          input$northarrow_padx < 0 ||
          is.na(input$northarrow_padx)
      ) {
        showNotification(
          paste0("Scale bar x-axis padding is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_padx <- 0
      } else {
        northarrow_padx <- input$northarrow_padx
      }

      if (input$northarrow_pady > 0.5 ||
          input$northarrow_pady < 0 ||
          is.na(input$northarrow_pady)
      ) {
        showNotification(
          paste0("Scale bar y-axis padding is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        northarrow_pady <- 0.02
      } else {
        northarrow_pady <- input$northarrow_pady
      }

      map_plot <- map_plot +
        ggspatial::annotation_north_arrow(
          location = input$northarrow_location,
          which_north = "true",
          pad_x = unit(northarrow_padx, "npc"),
          pad_y = unit(northarrow_pady, "npc"),
          height = unit(northarrow_height, "npc"),
          width = unit(northarrow_width, "npc"),
          style = north_arrow_style_func(
             fill = north_arrow_fill,
             line_col = input$northarrow_linecolour,
             text_col = input$northarrow_textcolour,
             text_size = northarrow_textsize
          )
        )
    }



    # # Add user-defined custom code block
    # # Commented out for security reasons
    # ext_plot <- plot_extension()
    # map_plot <- map_plot + ext_plot

    # Return the plot object
    map_plot

  })

  # # Evaluate user-defined custom code block
  # # This is commented out due to potential security concerns
  # plot_extension <- eventReactive(c(input$plot_map_bt), {
  #
  #   if (input$ggplot_code == "" ||
  #       is.null(input$ggplot_code)) {
  #     return(geom_blank())
  #    } else {
  #     plot_ext <- input$ggplot_code
  #     plot_ext <- tryCatch(
  #       {
  #         eval(parse(text = ggplot_code))
  #       },
  #       error = function(e) {
  #         showNotification(paste("Error in ggplot code:", e$message), type = "error")
  #         geom_blank()
  #       }
  #     )
  #     return(plot_ext)
  #   }
  # })

  # plot_to_print_map <- reactive({
  #   plot(plot_to_render_map())
  # })

  observeEvent(input$plot_map_bt, {
    output$plot_map_container <- renderUI({
      plotOutput("plot_map",
                 width = paste0(reactive({
                   width_val <- input$plot_width
                   if (is.na(width_val) || width_val < 100 || width_val > 2000) {
                     showNotification(
                       "Warning: width is not valid. Setting to default value of 600."
                     )
                     600
                   } else {
                     width_val
                   }
                 })(), "px"),
                 height = paste0(reactive({
                   height_val <- input$plot_height
                   if (is.na(height_val) || height_val < 100 || height_val > 2000) {
                     showNotification(
                       "Warning: height is not valid. Setting to default value of 400."
                     )
                     400
                   } else {
                     height_val
                   }
                 })(), "px")
      )
    })

    output$plot_map <- renderPlot({
      req(plot_to_print_map())
      plot_to_print_map()
    })
  })


  # Download map
  output$downloadGo_map <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*", "", .) %>%
        paste0(
          .,
          "_map.",
          tolower(input$downloadOptions_map)
        )
    },
    content = function(filename) {
      if (is.null(input$dlmap_width) || input$dlmap_width == "") {
        dlmap_width <- 800
      } else {
        dlmap_width <- input$dlmap_width
      }
      if (is.null(input$dlmap_height) || input$dlmap_height == "") {
        dlmap_height <- 600
      } else {
        dlmap_height <- input$dlmap_height
      }
      if (is.null(input$dlmap_scaling) || input$dlmap_scaling == "") {
        dlmap_scaling <- 1
      } else {
        dlmap_scaling <- input$dlmap_scaling
      }
      if (dlmap_width > 10000) {
        showNotification("Map width is too large, setting to 10000.")
        dlmap_width <- 10000
      } else if (dlmap_width < 100) {
        showNotification("Map width is too small, setting to 100.")
        dlmap_width <- 100
      }
      if (dlmap_height > 10000) {
        showNotification("Map height is too large, setting to 10000.")
        dlmap_height <- 10000
      } else if (dlmap_height < 100) {
        showNotification("Map height is too small, setting to 100.")
        dlmap_height <- 100
      }
      if (dlmap_scaling > 10) {
        showNotification("Scaling factor is too large, setting to 10.")
        dlmap_scaling <- 10
      } else if (dlmap_scaling <= 0.1) {
        showNotification("Scaling factor is too small, setting to 0.1.")
        dlmap_scaling <- 0.1
      }
      if (
        ((dlmap_height / dlmap_scaling) < 800) ||
        ((dlmap_height / dlmap_scaling) > 5000) ||
        ((dlmap_width / dlmap_scaling) < 800) ||
        ((dlmap_width / dlmap_scaling) > 5000)
      ) {
        showNotification(
          paste0(
            "Warning: scaling factor may be inappropriate. Please double ",
            "check that your scaling factor makes sense for your chosen width ",
            "and height and that the downloaded image looks as expected. ",
            "If the text and legend are too small comparative to the map, ",
            "try increasing the scaling factor. If they are too large, ",
            "try decreasing the scaling factor."
          )
        )
      }

      if (input$downloadOptions_map == "JPG" ||
          input$downloadOptions_map == "PNG" ||
          input$downloadOptions_map == "TIFF" ) {
        ggsave(filename,
          plot = plot_to_print_map(),
          device = tolower(input$downloadOptions_map),
          width = dlmap_width,
          height = dlmap_height,
          units = "px",
          scaling = dlmap_scaling
        )
      } else {
        ggsave(filename,
          plot = plot_to_print_map(),
          device = tolower(input$downloadOptions_map),
          width = dlmap_width,
          height = dlmap_height,
          units = "px"
        )
      }
    }
  )

  fig_legend <- eventReactive(input$plot_map_bt, {
    species_text <- if (!is.null(input$species) && length(input$species) > 0) {
      paste("of", paste(input$species, collapse = ", "))
    } else if (!is.null(input$family) && length(input$family) > 0) {
      paste("of family", paste(input$family, collapse = ", "))
    } else {
      "for all input cube taxa"
    }

    region_text <- if (!is.null(input$region) && length(input$region) > 0) {
      paste("in", paste(input$region, collapse = ", "))
    } else {
      "in all input cube regions"
    }

    paste(
      input$indicatorsToAnalyse,
      species_text,
      region_text,
      "visualised at",
      input$spatiallevel,
      "level and observed from",
      input$daterange[1],
      "to",
      input$daterange[2]
    )
  })

  output$figure_legend_map_text <-
    renderText({
      fig_legend()
    })

  observeEvent(input$plot_ts_bt, {
    req(input$dataCube)
     output$indicator_background_text_ts <-
      renderUI({
        chosen_ind <- switch(
          input$indicatorsToAnalyse,
          "Observed Species Richness" = obs_richness_bg,
          "Total Occurrences" = total_occ_bg,
          "Pielou's Evenness" = pielou_evenness_bg,
          "Williams' Evenness" = williams_evenness_bg,
          "Cumulative Species Richness" = cum_richness_bg,
          "Density of Occurrences" = occ_density_bg,
          "Abundance-Based Rarity" = ab_rarity_bg,
          "Area-Based Rarity" = area_rarity_bg,
          "Mean Year of Occurrence" = newness_bg
        )
        HTML(chosen_ind)
      })
  })

  observeEvent(input$plot_map_bt, {
    req(input$dataCube)
    output$indicator_background_text_map <-
      renderUI({
        chosen_ind <- switch(
          input$indicatorsToAnalyse,
          "Observed Species Richness" = obs_richness_bg,
          "Total Occurrences" = total_occ_bg,
          "Pielou's Evenness" = pielou_evenness_bg,
          "Williams' Evenness" = williams_evenness_bg,
          "Cumulative Species Richness" = cum_richness_bg,
          "Density of Occurrences" = occ_density_bg,
          "Abundance-Based Rarity" = ab_rarity_bg,
          "Area-Based Rarity" = area_rarity_bg,
          "Mean Year of Occurrence" = newness_bg,
          "Taxonomic Distinctness" = tax_distinct_bg,
          "Species Richness (Estimated by Coverage-Based Rarefaction)" - hill0_bg,
          "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)" - hill1_bg,
          "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)" - hill2_bg,
          "Species Occurrences" = spec_occ_bg,
          "Species Range" = spec_range_bg,
          "Occupancy Turnover" = occ_turnover_bg
        )
        HTML(chosen_ind)
      })
  })

  obs_richness_bg <- paste(
    em("Species Richness"),
    p(
      "Species richness is the total number of species present in a ",
      "sample (Magurran, 1988). It is a fundamental and commonly used ",
      "measure of biodiversity, providing a simple and intuitive ",
      "overview of the status of biodiversity. However, richness is not ",
      "well suited to measuring biodiversity change over time, as it ",
      "only decreases when local extinctions occur and thus lags behind ",
      "abundance for negative trends. While it may act as a leading ",
      "indicator of alien species invasions, it will not indicate ",
      "establishment because it ignores abundance. Nor will it ",
      "necessarily indicate changes in local species composition, which ",
      "can occur without any change in richness. Although richness is ",
      "conceptually simple, it can be measured in different ways."
    ),
    p(strong("Observed Richness")),
    p(
      "Observed richness is calculated by summing the number of unique ",
      "species observed for each year or each cell. Observed richness is ",
      "highly dependent on the comprehensiveness of the dataset it is being ",
      "applied to. If some regions are more intensively, carefully, or ",
      "systematically sampled than others, this will likely result in higher ",
      "observed richness. Observed richness also depends on the relative ",
      "abundance and spatial aggregation of each species, with less abundant ",
      "and less aggregated species less likely to be discovered during surveys ",
      "(Hillebrand et al., 2018), as well as the detectability of each species. ",
    )
  )

  total_occ_bg <- paste(
    p(strong("Total Occurrences")),
    p(
      "The total number of occurrences is calculated by summing the ",
      "occurrences of all species observed for each cell or year. This ",
      "variable provides an overview of the comprehensiveness and ",
      "distribution of data in the cube being analyzed, and may be ",
      "helpful, or even vital, for interpreting the results of ",
      "calculated indicators."
    )
  )

  pielou_evenness_bg <- paste(
    em("Evenness"),
    p(
      "Species evenness is a commonly used indicator that measures how ",
      "uniformly individuals are distributed across species in a region ",
      "or over time. It provides a complement to richness by taking ",
      "relative abundance into account. Although GBIF provides ",
      "information about abundances as individual counts, the majority ",
      "of entries lack this information. Hence, evenness can only be ",
      "calculated using the proportions of observations rather than ",
      "proportions of individuals. Strictly speaking, the evenness ",
      "measures therefore indicate how uniformly species are ",
      "represented in the respective data set rather than the true ",
      "evenness of the ecological community."
    ),
    p(strong("Pielou's Evenness")),
    shiny::withMathJax(
      p(
        "Pielou’s evenness (1966) is a well-known and commonly ",
        "used evenness measure. It is calculated as: ",
        "$$ E = -\\sum_{i=1}^{S} p_i \\ln(p_i) / \\ln(S) $$ ",
        "where S is the number of species and pi is the proportion of occurrences ",
        "represented by species i."
      )
    )
  )

  williams_evenness_bg <- paste(
    em("Evenness"),
    p(
      "Species evenness is a commonly used indicator that measures how ",
      "uniformly individuals are distributed across species in a region ",
      "or over time. It provides a complement to richness by taking ",
      "relative abundance into account. Although GBIF provides ",
      "information about abundances as individual counts, the majority ",
      "of entries lack this information. Hence, evenness can only be ",
      "calculated using the proportions of observations rather than ",
      "proportions of individuals. Strictly speaking, the evenness ",
      "measures therefore indicate how uniformly species are ",
      "represented in the respective data set rather than the true ",
      "evenness of the ecological community."
    ),
    p(strong("Williams' Evenness")),
    shiny::withMathJax(
      p(
        "An analysis of evenness properties by Kvålseth (2015) showed ",
        "that an evenness index introduced by Williams in 1977 in an ",
        "unpublished manuscript has two important properties which ",
        "Pielou’s does not. The properties in question are complex ",
        "mathematical properties known as the Schur-Concavity and ",
        "value validity, but we attempt to describe them here more ",
        "simply. If a measure of evenness is Schur-concave, it means ",
        "that when the distribution of individuals becomes more evenly ",
        "spread across species, the measure of evenness will stay the ",
        "same or increase, but never decrease. Value validity means ",
        "that an evenness index should provide sensible and meaningful ",
        "values across its range for any given distribution of species ",
        "abundances. Kvålseth referred to this evenness measure as E9 ",
        "but we refer to it as Williams’ evenness.",
        "Williams' evenness is calculated as: ",
        "$$ 1 - \\left[ \\frac{\\left( S\\sum_{i=1}^{S} p_i^2 - 1 \\right) }{S - 1}\\right]^{1/2} $$ ",
        "where S is the number of species and pi is the proportion of occurrences ",
        "represented by species i."
      )
    )
  )

  cum_richness_bg <- paste(
    em("Species Richness"),
    p(
      "Species richness is the total number of species present in a ",
      "sample (Magurran, 1988). It is a fundamental and commonly used ",
      "measure of biodiversity, providing a simple and intuitive ",
      "overview of the status of biodiversity. However, richness is not ",
      "well suited to measuring biodiversity change over time, as it ",
      "only decreases when local extinctions occur and thus lags behind ",
      "abundance for negative trends. While it may act as a leading ",
      "indicator of alien species invasions, it will not indicate ",
      "establishment because it ignores abundance. Nor will it ",
      "necessarily indicate changes in local species composition, which ",
      "can occur without any change in richness. Although richness is ",
      "conceptually simple, it can be measured in different ways."
    ),
    p(strong("Cumulative Richness")),
    p(
      "Cumulative richness is calculated by adding the newly observed ",
      "unique species each year to a cumulative sum. This indicator ",
      "provides an estimation of whether and how many new species are ",
      "still being discovered in a region. While an influx of alien ",
      "species could cause an increase in cumulative richness, a fast-",
      "rising trend is likely an indication that the ",
      "dataset is not comprehensive and therefore observed richness ",
      "will provide an underestimate of species richness."
    )
  )

  occ_density_bg <- paste(
    p(strong("Density of Occurrences")),
    p(
      "Density is calculated by summing the total number of occurrences ",
      "per square km for each cell or year. This provides similar ",
      "information to total occurrences, but is adjusted for cell area."
    )
  )

  ab_rarity_bg <- paste(
    em("Rarity"),
    p(
      "Rarity is the scarcity or infrequency of a particular species in ",
      "an area. A rare species might have a small population size, a ",
      "limited distribution, or a unique ecological niche (Maciel, ",
      "2021; Rabinowitz, 1981). Rarity can also be a biodiversity ",
      "indicator when summed over multiple species in an area, and may ",
      "provide important insight for determining conservation ",
      "priorities. When measured over time, rarity may indicate ",
      "potential threats or changes in the environment."
    ),
    p(strong("Abundance-Based Rarity")),
    shiny::withMathJax(
      p(
        "Abundance-based rarity is the inverse of the proportion of total occurrences ",
        "represented by a particular species. The total summed rarity for each grid ",
        "cell or year is calculated (sum the rarity values of each species present ",
        "there). It is calculated as: ",
        "$$ \\sum_{i=1}^{S} 1/p_i$$ ",
        "where S is the number of species and pi is the proportion of occurrences ",
        "represented by species i."
      )
    )
  )

  area_rarity_bg <- paste(
    em("Rarity"),
    p(
      "Rarity is the scarcity or infrequency of a particular species in ",
      "an area. A rare species might have a small population size, a ",
      "limited distribution, or a unique ecological niche (Maciel, ",
      "2021; Rabinowitz, 1981). Rarity can also be a biodiversity ",
      "indicator when summed over multiple species in an area, and may ",
      "provide important insight for determining conservation ",
      "priorities. When measured over time, rarity may indicate ",
      "potential threats or changes in the environment."
    ),
    p(strong("Area-Based Rarity")),
    shiny::withMathJax(
      p(
        "Area-based rarity is the inverse of occupancy frequency (proportion of grid ",
        "cells occupied) for a particular species. The total summed rarity for each ",
        "grid cell or year is calculated (sum the rarity values of each species ",
        "present there). It is calculated as: ",
        "$$ \\sum_{i=1}^{S} N/n_i$$ ",
        "where S is the number of species, N is the total number of occupied grid ",
        "cells, and ni is the number of grid cells occupied by species i."
      )
    )
  )

  newness_bg <- paste(
    p(strong("Mean Year of Occurrence")),
    p(
      "The mean year of occurrence is calculated per cell, giving an ",
      "indication of how recent the data is for each cell. A recent ",
      "mean year is not necessarily an indication of quality, as some ",
      "countries or regions have been conducting comprehensive ",
      "biodiversity monitoring for many years and will therefore ",
      "reflect an older mean year of occurrence, while others may show ",
      "a recent mean year due to e.g., the sudden availability of large ",
      "amounts of citizen science data."
    )
  )

  tax_distinct_bg <- paste(
    p(strong("Taxonomic Distinctness")),
    shiny::withMathJax(
      p(
        "Taxonomic distinctness measures the taxonomic relatedness between ",
        "species, providing a measure of biodiversity that accounts for ",
        "evolutionary relationships. A distance matrix based on pairwise ",
        "taxonomic relationships is calculated for each cell using the taxize ",
        "package (Chamberlain & Szöcs, 2013; Chamberlain et al., 2020), then ",
        "taxonomic distinctness is calculated as the Taxonomic Distinctness ",
        "Index (TDI; Clarke & Warwick, 1999): ",
        "$$ \\displaystyle \\frac{\\sum \\sum_{i<j} \\frac{|R_i - R_j|}{L}}{\\frac{S(S-1)}{2}} $$ ",
        "where S is the number of species, Ri and Rj are the taxonomic ranks ",
        "of species i and j (from the GBIF Taxonomic Backbone), and L is the ",
        "maximum number of taxonomic ranks. The double summation syntax here ",
        "is to explicitly denote iteration over all unique pairs (i,j) with i < j."
      )
    )
  )

  hill0_bg <- paste(
    em("Hill Diversity"),
    shiny::withMathJax(
      p(
        "Hill (1973) introduced the concept of Hill diversity, which assumes ",
        "that the number and relative abundance of species are inseparable ",
        "components of diversity. Hill diversity uses a single equation to ",
        "calculate multiple measures of diversity by varying a single ",
        "parameter ℓ, which changes the emphasis on rare vs common species ",
        "(Roswell et al., 2019). It represents the mean rarity of sampled ",
        "species, and is calculated as: ",
        "$$ \\displaystyle D = \\left( \\sum_{i=1}^{S} p_i r_i^\\ell \\right)^{1/\\ell} $$ ",
        "where D is diversity, S is the number of species, pi is the proportion ",
        "of individuals belonging to species i, ri is the rarity of species i, ",
        "and ℓ determines the rarity scale for the mean. While ℓ can ",
        "theoretically take almost any value, three common measures of diversity ",
        "are special cases: species richness, and modified versions of the ",
        "Shannon and Simpson diversity indices (Roswell et al., 2019). These ",
        "three measures occur when ℓ takes the value of 1, 0 (or near-zero, ",
        "as ℓ cannot actually take the value of 0), or -1, respectively. ",
        "Richness uses an arithmetic scale (the arithmetic mean), thus giving ",
        "rare species a lot of leverage. By contrast, Hill-Shannon diversity ",
        "uses a logarithmic scale (the geometric mean), treating common and ",
        "rare species equally, and Hill-Simpson diversity uses a reciprocal ",
        "scale (the harmonic mean), giving common species higher leverage."
      )
    ),
    p(strong("Species Richness")),
    p(
      "Using the Hill diversity equation, richness becomes simply S, ",
      "the number of species, and is thus identical to richness ",
      "calculated without Hill diversity."
    )
  )

  hill1_bg <- paste(
    em("Hill Diversity"),
    shiny::withMathJax(
      p(
        "Hill (1973) introduced the concept of Hill diversity, which assumes ",
        "that the number and relative abundance of species are inseparable ",
        "components of diversity. Hill diversity uses a single equation to ",
        "calculate multiple measures of diversity by varying a single ",
        "parameter ℓ, which changes the emphasis on rare vs common species ",
        "(Roswell et al., 2019). It represents the mean rarity of sampled ",
        "species, and is calculated as: ",
        "$$ \\displaystyle D = \\left( \\sum_{i=1}^{S} p_i r_i^\\ell \\right)^{1/\\ell} $$ ",
        "where D is diversity, S is the number of species, pi is the proportion ",
        "of individuals belonging to species i, ri is the rarity of species i, ",
        "and ℓ determines the rarity scale for the mean. While ℓ can ",
        "theoretically take almost any value, three common measures of diversity ",
        "are special cases: species richness, and modified versions of the ",
        "Shannon and Simpson diversity indices (Roswell et al., 2019). These ",
        "three measures occur when ℓ takes the value of 1, 0 (or near-zero, ",
        "as ℓ cannot actually take the value of 0), or -1, respectively. ",
        "Richness uses an arithmetic scale (the arithmetic mean), thus giving ",
        "rare species a lot of leverage. By contrast, Hill-Shannon diversity ",
        "uses a logarithmic scale (the geometric mean), treating common and ",
        "rare species equally, and Hill-Simpson diversity uses a reciprocal ",
        "scale (the harmonic mean), giving common species higher leverage."
      )
    ),
    p(strong("Hill-Shannon Diversity")),
    p(
      "Hill-Shannon diversity is actually e (base of the natural log) raised ",
      "to the power of the Shannon index. It is estimated for each year or ",
      "cell count using the iNEXT package, standardized by coverage, as: ",
      "$$ \\displaystyle e^{-\\sum_{i=1}^{S} p_i \\ln(p_i)} $$ ",
      "where S is the number of species and pi is the proportion of occurrences ",
      "represented by species i."
    )
  )

  hill2_bg <- paste(
    em("Hill Diversity"),
    shiny::withMathJax(
    p(
      "Hill (1973) introduced the concept of Hill diversity, which assumes ",
      "that the number and relative abundance of species are inseparable ",
      "components of diversity. Hill diversity uses a single equation to ",
      "calculate multiple measures of diversity by varying a single ",
      "parameter ℓ, which changes the emphasis on rare vs common species ",
      "(Roswell et al., 2019). It represents the mean rarity of sampled ",
      "species, and is calculated as: ",
      "$$ \\displaystyle D = \\left( \\sum_{i=1}^{S} p_i r_i^\\ell \\right)^{1/\\ell} $$ ",
      "where D is diversity, S is the number of species, pi is the proportion ",
      "of individuals belonging to species i, ri is the rarity of species i, ",
      "and ℓ determines the rarity scale for the mean. While ℓ can ",
      "theoretically take almost any value, three common measures of diversity ",
      "are special cases: species richness, and modified versions of the ",
      "Shannon and Simpson diversity indices (Roswell et al., 2019). These ",
      "three measures occur when ℓ takes the value of 1, 0 (or near-zero, ",
      "as ℓ cannot actually take the value of 0), or -1, respectively. ",
      "Richness uses an arithmetic scale (the arithmetic mean), thus giving ",
      "rare species a lot of leverage. By contrast, Hill-Shannon diversity ",
      "uses a logarithmic scale (the geometric mean), treating common and ",
      "rare species equally, and Hill-Simpson diversity uses a reciprocal ",
      "scale (the harmonic mean), giving common species higher leverage."
    ),
    p(strong("Hill-Simpson Diversity")),
    shiny::withMathJax(
      p(
        "Hill-Simpson diversity is the inverse of the Simpson index. ",
        "It is estimated using the iNEXT package for each year or cell, ",
        "standardized by coverage, as:",
        "$$ \\displaystyle \\frac{1}{\\sum_{i=1}^{S} p_i^2} $$",
        "where S is the number of species and pi is the proportion of ",
        "occurrences represented by species i.",
        "Both Hill-Simpson and Hill-Shannon diversity describe a combination ",
        "of richness and evenness that reduce the inadequacies of either ",
        "measure alone."
      )
    )
  )
  )

  ############################ time series tab outputs

  # create time series from imported cube
  plot_to_render_ts <- eventReactive(input$plot_ts_bt, {
    req(r$dataCube1)

    tryCatch(
      {
        withCallingHandlers(
          {
            params <- list(
              data = r$dataCube1,
              cell_size = input$cellsize,
              level = input$spatiallevel,
              first_year = input$daterange[1],
              last_year = input$daterange[2]
            )

            ts_plot <- do.call(
              switch(input$indicatorsToAnalyse,
                "Observed Species Richness" = obs_richness_ts,
                "Total Occurrences" = total_occ_ts,
                "Pielou's Evenness" = pielou_evenness_ts,
                "Williams' Evenness" = williams_evenness_ts,
                "Cumulative Species Richness" = cum_richness_ts,
                "Density of Occurrences" = occ_density_ts,
                "Abundance-Based Rarity" = ab_rarity_ts,
                "Area-Based Rarity" = area_rarity_ts,
                "Mean Year of Occurrence" = newness_ts,
                "Taxonomic Distinctness" = tax_distinct_ts,
                "Species Richness (Estimated by Coverage-Based Rarefaction)" = hill0_ts,
                "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)" = hill1_ts,
                "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)" = hill2_ts,
                "Species Occurrences" = spec_occ_ts,
                "Species Range" = spec_range_ts,
                "Occupancy Turnover" = occ_turnover_ts
              ),
              params
            )

            ts_plot # Return the plot object
          },
          warning = function(w) {
            showNotification(paste("Warning:", conditionMessage(w)), type = "warning")
            invokeRestart("muffleWarning")
          }
        )
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = NULL)
        return(NULL)
      }
    )
  })

  observeEvent(input$plot_ts_bt, {
    output$plot_ts_container <- renderUI({
      plotOutput("plot_ts",
                 width = paste0(reactive({
                   width_val <- input$plot_width
                   if (is.na(width_val) || width_val < 100 || width_val > 2000) {
                     showNotification(
                       "Warning: width is not valid. Setting to default value of 600."
                     )
                     600
                   } else {
                     width_val
                   }
                 })(), "px"),
                 height = paste0(reactive({
                   height_val <- input$plot_height
                   if (is.na(height_val) || height_val < 100 || height_val > 2000) {
                     showNotification(
                       "Warning: height is not valid. Setting to default value of 400."
                     )
                     400
                   } else {
                     height_val
                   }
                 })(), "px"))
    })
    # output time series from imported cube
    output$plot_ts <- renderPlot({
      req(plot_to_render_ts())

      ribboncolour <- if (input$ci_vis_type == "None") NA else input$ribboncolour

      gridlines <- if (input$ts_gridlines == "TRUE") FALSE else TRUE

      if (
        input$title == ""
      ) {
        title <- NULL
      } else {
        title <- input$title
      }

      if (
        is.null(input$ts_x_label) ||
        length(input$ts_x_label) == 0 ||
        input$ts_x_label == ""
      ) {
        xlabel <- NULL
      } else {
        xlabel <- input$ts_x_label
      }

      if (
        is.null(input$ts_y_label) ||
        length(input$ts_y_label) == 0 ||
        input$ts_y_label == ""
      ) {
        ylabel <- NULL
      } else {
        ylabel <- input$ts_y_label
      }

      # Check if subtitle is provided
      if (input$subtitle == "") {
        subtitle <- NULL
      } else {
        subtitle <- input$subtitle
      }

      # Check if caption is provided
      if (input$caption == "") {
        caption <- NULL
      } else {
        caption <- input$caption
      }

      # Check that title, subtitle and caption font size inputs are ok
      if (input$title_size < 8 ||
          input$title_size > 40 ||
          is.na(input$title_size)
          ) {
        showNotification(
          paste0("Title font size is outside reasonable boundaries.",
                 "Resetting to default."),
          type = "error")
        title_size <- NULL
      } else {
        title_size <- input$title_size
      }

      if (input$subtitle_size < 6 ||
          input$subtitle_size > 30 ||
          is.na(input$subtitle_size)
          ) {
        showNotification(
          paste0("Subtitle font size is outside reasonable boundaries.",
                 "Resetting to default."),
          type = "error")
        subtitle_size <- NULL
      } else {
        subtitle_size <- input$subtitle_size
      }

      if (input$caption_size < 4 ||
          input$caption_size > 20 ||
          is.na(input$caption_size)
          ) {
        showNotification(
          paste0("Caption font size is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        caption_size <- NULL
      } else {
        caption_size <- input$caption_size
      }

      if (input$wrap_length < 20 ||
          input$wrap_length > 200 ||
          is.na(input$wrap_length)
      ) {
        showNotification(
          paste0("Title wrap length is outside reasonable boundaries. ",
                 "Resetting to default."),
          type = "error")
        wrap_length <- NULL
      } else {
        wrap_length <- input$wrap_length
      }

      if (input$suppress_y == TRUE) {
        y_axis_text <- element_blank()
      } else {
        y_axis_text <- element_text()
      }

      if (input$suppress_yt == TRUE) {
        y_axis_title <- element_blank()
      } else {
        y_axis_title <- element_text()
      }

      if (input$suppress_x == TRUE) {
        x_axis_text <- element_blank()
      } else {
        x_axis_text <- element_text()
      }

      if (input$suppress_xt == TRUE) {
        x_axis_title <- element_blank()
      } else {
        x_axis_title <- element_text()
      }

      if (input$ts_x_breaks > 100 ||
          input$ts_x_breaks < 2 ||
          is.na(input$ts_x_breaks)
          ) {
        showNotification(
          paste0("Number of X axis breaks outside of reasonable range. ",
                 "Resetting to default."),
          type = "error"
        )
        ts_x_breaks <- 10
      } else {
        ts_x_breaks <- input$ts_x_breaks
      }

      if (input$ts_y_breaks > 100 ||
          input$ts_y_breaks < 2 ||
          is.na(input$ts_y_breaks)
          ) {
        showNotification(
          paste0("Number of X axis breaks outside of reasonable range. ",
                 "Resetting to default."),
          type = "error"
        )
        ts_y_breaks <- 6
      } else {
        ts_y_breaks <- input$ts_y_breaks
      }

      # Confidence intervals for indicator lines or points
      if (input$ci_vis_type == "Ribbon") {
        vis_type <- "ribbon"
      } else if (input$ci_vis_type == "Error Bars") {
        vis_type <- "error_bars"
      } else {
        vis_type <- "error_bars"
        error_alpha <- 0
      }

      error_alpha <- input$error_alpha

      if (input$error_thickness <= 0 ||
          input$error_thickness > 10 ||
          is.na(input$error_thickness)
          ) {
        showNotification(
          paste0("Error bar thickness outside of reasonable range.",
                 "Resetting to default."),
          type = "error"
        )
        error_thickness <- 1
      } else {
        error_thickness <- input$error_thickness
      }

      if (input$error_width <= 0 ||
          input$error_width > 10 ||
          is.na(input$error_width)) {
        showNotification(
          paste0("Error bar width outside of reasonable range.",
                 "Resetting to default."),
          type = "error"
        )
        error_width <- 1
      } else {
        error_width <- input$error_width
      }

      if (input$linewidth <= 0 ||
          input$linewidth > 10 ||
          is.na(input$linewidth)
          ) {
        showNotification(
          paste0("Indicator line width outside of reasonable range.",
                 "Resetting to default."),
          type = "error"
        )
        linewidth <- 1
      } else {
        linewidth <- input$linewidth
      }

      if (input$smooth_linewidth <= 0 ||
          input$smooth_linewidth > 10 ||
          is.na(input$smooth_linewidth)
          ) {
        showNotification(
          paste0("Trend line width outside of reasonable range.",
                 "Resetting to default."),
          type = "error"
        )
        smooth_linewidth <- 1
      } else {
        smooth_linewidth <- input$smooth_linewidth
      }

      if (input$smooth_cilinewidth <= 0 ||
          input$smooth_cilinewidth > 10 ||
          is.na(input$smooth_cilinewidth)) {
        showNotification(
          paste0("Trend envelope edge width outside of reasonable range.",
                 "Resetting to default."),
          type = "error"
        )
        smooth_cilinewidth <- 1
      } else {
        smooth_cilinewidth <- input$smooth_cilinewidth
      }

      if (input$indicatorsToAnalyse == "Cumulative Species Richness") {
        smoothed_trend <- FALSE
      } else {
        smoothed_trend <- input$smoothed_trend
      }

      params <- list(
        x = plot_to_render_ts(),
        title = title,
        suppress_y = FALSE,
        smoothed_trend = input$smoothed_trend,
        x_label = xlabel,
        y_label = ylabel,
        x_expand = c(input$ts_x_expand_left, input$ts_x_expand_right),
        y_expand = c(input$ts_y_expand_bottom, input$ts_y_expand_top),
        x_breaks = ts_x_breaks,
        y_breaks = ts_y_breaks,
        gridoff = gridlines,
        ci_type = vis_type,
        point_line = input$point_line,
        pointsize = input$pointsize,
        linewidth = linewidth,
        linecolour = input$linecolour,
        linealpha = input$linealpha,
        error_width = error_width,
        error_thickness = error_thickness,
        error_alpha = error_alpha,
        ribboncolour = ribboncolour,
        ribbonalpha = input$ribbonalpha,
        smooth_linetype = input$smooth_linetype,
        smooth_linewidth = smooth_linewidth,
        trendlinecolour = input$trendlinecolour,
        trendlinealpha = input$trendlinealpha,
        smooth_cilinewidth = smooth_cilinewidth,
        envelopecolour = input$envelopecolour,
        envelopealpha = input$envelopealpha,
        smooth_cialpha = input$smooth_cialpha,
        wrap_length = wrap_length
      )

      # Plot diversity metric
      ts_plot <- do.call(plot, params)

      # Add other options to plot
      ts_plot <- ts_plot +
        labs(title = title,
             subtitle = subtitle,
             caption = caption) +
        theme(
          axis.title.x = x_axis_title,
          axis.text.x = x_axis_text,
          axis.title.y = y_axis_title,
          axis.text.y = y_axis_text,
          plot.title = element_text(color = input$title_color,
                                    size = title_size,
                                    face = "bold",
                                    hjust = 0),
          plot.subtitle = element_text(color = input$subtitle_color,
                                       size = subtitle_size),
          plot.caption = element_text(color = input$caption_color,
                                      size = caption_size,
                                      face = "italic")
        )
      print(input$indicatorsToAnalyse)
      ts_plot
    })
  })

  plot_to_print_ts <- reactive({
    plot(plot_to_render_ts())
  })

  output$downloadGo_ts <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*", "", .) %>%
        paste0(
          .,
          "_timeSeries.",
          tolower(input$downloadOptions_ts)
        )
    },
    content = function(filename) {
            if (is.null(input$dlts_width) || input$dlts_width == "") {
        dlts_width <- 800
      } else {
        dlts_width <- input$dlts_width
      }
      if (is.null(input$dlts_height) || input$dlts_height == "") {
        dlts_height <- 600
      } else {
        dlts_height <- input$dlts_height
      }
      if (is.null(input$dlts_scaling) || input$dlts_scaling == "") {
        dlts_scaling <- 1
      } else {
        dlts_scaling <- input$dlts_scaling
      }
      if (dlts_width > 10000) {
        showNotification("Map width is too large, setting to 10000.")
        dlts_width <- 10000
      } else if (dlts_width < 100) {
        showNotification("Map width is too small, setting to 100.")
        dlts_width <- 100
      }
      if (dlts_height > 10000) {
        showNotification("Map height is too large, setting to 10000.")
        dlts_height <- 10000
      } else if (dlts_height < 100) {
        showNotification("Map height is too small, setting to 100.")
        dlts_height <- 100
      }
      if (dlts_scaling > 10) {
        showNotification("Scaling factor is too large, setting to 10.")
        dlts_scaling <- 10
      } else if (dlts_scaling <= 0.1) {
        showNotification("Scaling factor is too small, setting to 0.1.")
        dlts_scaling <- 0.1
      }
      if (
        ((dlts_height / dlts_scaling) < 800) ||
        ((dlts_height / dlts_scaling) > 5000) ||
        ((dlts_width / dlts_scaling) < 800) ||
        ((dlts_width / dlts_scaling) > 5000)
      ) {
        showNotification(
          paste0(
            "Warning: scaling factor may be inappropriate. Please double ",
            "check that your scaling factor makes sense for your chosen width ",
            "and height and that the downloaded image looks as expected. ",
            "If the text and legend are too small comparative to the map, ",
            "try increasing the scaling factor. If they are too large, ",
            "try decreasing the scaling factor."
          )
        )
      }

      if (input$downloadOptions_ts == "JPG" ||
          input$downloadOptions_ts == "PNG" ||
          input$downloadOptions_ts == "TIFF" ) {
        ggsave(filename,
          plot = plot_to_print_ts(),
          device = tolower(input$downloadOptions_ts),
          width = dlts_width,
          height = dlts_height,
          units = "px",
          scaling = dlts_scaling
        )
      } else {
        ggsave(filename,
          plot = plot_to_print_ts(),
          device = tolower(input$downloadOptions_ts),
          width = dlts_width,
          height = dlts_height,
          units = "px"
        )
      }
    }
  )


  fig_legend_ts <- eventReactive(input$plot_ts_bt, {
    species_text <- if (!is.null(input$species) && length(input$species) > 0) {
      paste("for", paste(input$species, collapse = ", "))
    } else if (!is.null(input$family) && length(input$family) > 0) {
      paste("for family", paste(input$family, collapse = ", "))
    } else {
      "for all input cube taxa"
    }

    region_text <- if (!is.null(input$region) && length(input$region) > 0) {
      paste("in", paste(input$region, collapse = ", "))
    } else {
      "in all input cube regions"
    }

    paste(
      input$indicatorsToAnalyse,
      species_text,
      region_text,
      "from",
      input$daterange[1],
      "to",
      input$daterange[2]
    )
  })

  output$figure_legend_ts_text <-
    renderText({
      fig_legend_ts()
    })





  output$downloadProcessedCube <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*", "", .) %>%
        paste0(
          .,
          ".",
          "csv"
        )
    },
    content = function(filename) {
      write.csv(unclass(r$dataCube1$data), filename)
    }
  )

  output$downloadMappedCube <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*", "", .) %>%
        paste0(
          .,
          "_mapped_",
          ".",
          "csv"
        )
    },
    content = function(filename) {
      write.csv(unclass(plot_to_render_map()$data), filename)
    }
  )

  output$downloadTimeSeriesData <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*", "", .) %>%
        paste0(
          .,
          "_mapped_",
          ".",
          "csv"
        )
    },
    content = function(filename) {
      write.csv(unclass(plot_to_render_ts()$data), filename)
    }
  )
}

shinyApp(ui = ui, server = server)

