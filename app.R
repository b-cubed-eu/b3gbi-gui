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
      "B³ Indicators"
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
      href = "https://fonts.googleapis.com/css2?family=PT+Sans+Narrow:wght@400",
      ";700&display=swap",
      rel = "stylesheet"
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
          "outputs.<br><br>In the Explore Your Data tab, you will find the ",
          "metadata summarising your data cube. The Plot tab visualizes the ",
          "biodiversity indicators on a map, the Table tab prints the data cube ",
          "data, and in the Report tab, you can view the raw code used to ",
          "produce outputs.<br></p>"
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

          # input$dataCube
          fileInput(
            inputId = "dataCube",
            label = HTML("Upload the data cube")
          )
        ),

        tabPanel(
          "Input filters",

          # the indicators
          selectInput(
            inputId = "indicatorsToAnalyse",
            label = "Biodiversity Indicator", multiple = FALSE,
            choices = as.character(sapply(b3gbi::available_indicators, "[[", 2))
          ),

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
          ),

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
        ),

        tabPanel(
          title = "Visualization Options",
          HTML("<br>"),

          textInput(
            "title",
            label = "Custom Plot Title"
          ),
          numericInput(
            "wrap_length",
            label = "Title Wrap Length (max. characters on a single line)",
            min = 20,
            max = 200,
            step = 5,
            value = 60
          ),
          checkboxInput(
            "ts_options",
            "Show Time Series Visualization Options"
          ),
          conditionalPanel(
            condition = "input.ts_options == true",
            tags$hr(),
            textInput(
              "ts_x_label",
              label = "Custom X-Axis Label",
              value = ""
            ),
            checkboxInput(
              "suppress_y",
              label = "Suppress Y-Axis Values",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.suppress_y == false",
              textInput(
                "ts_y_label",
                label = "Custom Y-Axis Label",
                value = ""
              )
            ),

            checkboxInput(
              "ts_expand",
              label = "Expand Axes"
            ),
            conditionalPanel(
              condition = "input.ts_expand == true",
              tags$hr(),
              numericInput(
                "ts_x_expand_left",
                paste0("Expand X Axis: Left"),
                min = 0,
                max = 1,
                step = 0.01,
                value = 0
              ),
              numericInput(
                "ts_x_expand_right",
                paste0("Expand X Axis: Right"),
                min = 0,
                max = 1,
                step = 0.01,
                value = 0
              ),
              numericInput(
                "ts_y_expand_top",
                paste0("Expand Y Axis: Top"),
                min = 0,
                max = 1,
                step = 0.01,
                value = 0
              ),
              numericInput(
                "ts_y_expand_bottom",
                paste0("Expand Y Axis: Bottom"),
                min = 0,
                max = 1,
                step = 0.01,
                value = 0
              ),
              tags$hr()
            ),
            numericInput(
              "ts_x_breaks",
              paste0("Number of X Axis Breaks (approximate)"),
              min = 0,
              max = 100,
              step = 1,
              value = 10
            ),
            numericInput(
              "ts_y_breaks",
              paste0("Number of Y Axis Breaks (approximate)"),
              min = 0,
              max = 100,
              step = 1,
              value = 6
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
              condition = "input.point_line === 'Points'",
              numericInput(
                "pointsize",
                paste0("Size of Indicator Points"),
                min = 0,
                max = 1,
                step = 0.1,
                value = 2
              )
            ),
            conditionalPanel(
              condition = "input.point_line === 'Line'",
              numericInput(
                "linewidth",
                paste0("Width of Indicator Line"),
                min = 0,
                max = 1,
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
              tags$hr(),
              numericInput(
                "error_width",
                paste0("Width of Error Bars"),
                min = 0,
                max = 10,
                step = 0.1,
                value = 1
              ),
              numericInput(
                "error_thickness",
                paste0("Thickness of Error Bars"),
                min = 0,
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
              ),
              tags$hr()
            ),
            conditionalPanel(
              condition = "input.ci_vis_type === 'Ribbon'",
              tags$hr(),
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
              ),
              tags$hr()
            ),

            checkboxInput(
              "smoothed_trend",
              label = "Plot Smoothed Trend Line"
            ),
            conditionalPanel(
              condition = "input.smoothed_trend == true",
              tags$hr(),
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
                min = 0,
                max = 1,
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
                min = 0,
                max = 1,
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
              ),
              tags$hr()
            ),
            tags$hr()
          ),

          checkboxInput(
            "map_options",
            "Show Map Visualization Options"
          ),

          conditionalPanel(
            condition = "input.map_options == true",
            tags$hr(),
            checkboxInput(
              "custom_map_axes",
              "Custom X and Y Axis Limits"
            ),
            conditionalPanel(
              condition = "input.custom_map_axes == true",
              tags$hr(),
              textInput(
                "xcoord_min",
                "Minimum X Coordinate Value",
                value = ""
              ),
              textInput(
                "xcoord_max",
                "Maximum X Coordinate Value",
                value = ""
              ),
              textInput(
                "ycoord_min",
                "Minimum Y Coordinate Value",
                value = ""
              ),
              textInput(
                "ycoord_max",
                "Maximum Y Coordinate Value",
                value = ""
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
              value = TRUE
            ),
            colourInput(
              "panel_bg",
              "Customize Background Colour for Map",
              value = NULL
            ),
            colourInput(
              "land_fill_colour",
              "Customize Colour for Land Areas Outside of Grid",
              value = NULL
            ),
            selectInput(
              "trans",
              "Apply Scale Transformation to Indicator Values",
              choices = c(
                'None' = "none",
                'Exponential Transformation' = "exp",
                'Log Transformation' = "log",
                'Log10 Transformation' = "log10",
                'Log1p Transformation' = "log1p",
                'Log2 Transformation' = "log2",
                'Square-root Transformation' = "sqrt",
                'Reciprocal Transformation' = "reciprocal"
              ),
              selected = NULL
            ),
            textInput(
              "breaks",
              "Custom Break Points for Legend",
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
            textInput(
              "legend_limits",
              "Custom Legend Scale Limits",
              value = ""
            ),
            numericInput(
              "legend_title_wrap_length",
              "Legend Title Wrap Length (max characters on one line)",
              min = 10,
              max = 100,
              step = 2,
              value = 20
            )
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
          title = "Explore Your Data",
          HTML("<br>"), # Adding line break for spacing
          em(
            "In this tab you can view the metadata summarising ",
            "your data cube."
          ),
          HTML("<br>"),
          HTML("<br>"),
          verbatimTextOutput("metadata"),
          HTML("<br>"),
          HTML("<br>")
        ),

        tabPanel(
          title = "Background",
          HTML("<br>"),
          em(
            "In this tab you can view information on available biodiversity ",
            "indicators."
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
          em("Richness"),
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
          p(strong("Cumulative Species Richness")),
          p(
            "Cumulative richness is calculated by adding the newly observed ",
            "unique species each year to a cumulative sum. This indicator ",
            "provides an estimation of whether and how many new species are ",
            "still being discovered in a region. While an influx of alien ",
            "species could cause an increase in cumulative richness, a fast-",
            "rising trend as shown in Fig. 2 is likely an indication that the ",
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
          p("Pielou (1966)"),
          p(strong("Williams' Evenness")),
          p("Kvålseth (2015)"),
          HTML("<br>"),
          em("Rarity"),
          p(
            "Rarity is the scarcity or infrequency of a particular species in ",
            "an area. A rare species might have a small population size, a ",
            "limited distribution, or a unique ecological niche (Maciel, ",
            "2021; Rabinowitz, 1981). Rarity can also be a biodiversity ",
            "indicator when summed over multiple species in an area, and may ",
            "provide important insight for determining conservation ",
            "priorities. It can be measured in different ways, but we will ",
            "provide workflows to calculate rarity by abundance (using the ",
            "number of occurrences as a proxy) and by area. When measured ",
            "over time, rarity may indicate potential threats or changes in ",
            "the environment."
          ),
          p(strong("Abundance-Based Rarity")),
          p(
            "Abundance-based rarity is the inverse of the proportion of total ",
            "occurrences represented by a particular species. The total ",
            "summed rarity for each grid cell or year is calculated (sum the ",
            "rarity values of each species present there)."
          ),
          p(strong("Area-Based Rarity")),
          p(
            "Area-based rarity is the inverse of occupancy frequency ",
            "(proportion of grid cells occupied) for a particular species. ",
            "The total summed rarity for each grid cell or year is calculated ",
            "(sum the rarity values of each species present there)."
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
          HTML("<br>")  # Adding line break for spacing
        ),

        ############################# Map tab
        tabPanel(
          title = "Map",
          HTML("<br>"),  # Adding line break for spacing
          em(
            "In this tab you can view your selected biodiversity indicator ",
            "projected onto a map. Use the left-hand panel to select the ",
            "indicator, taxa, geographical area, and temporal window of ",
            "interest."
          ),
          HTML("<br>"),  # Adding line break for spacing
          HTML("<br>"),  # Adding line break for spacing
          em(
            "Loading the plots could take a few minutes, depending on the ",
            "options you have selected."
          ),
          HTML("<br>"),  # Adding line break for spacing
          HTML("<br>"),
          actionButton("plot_map_bt", "Plot Map"),
          HTML("<br>"), # Adding line break for spacing
          HTML("<br>"), # Adding line break for spacing
          plotOutput("plot_map"),
          HTML("<br>"), # Adding line break for spacing
          p(strong("What am I looking at?")),
          textOutput("figure_legend_map_text"),
          HTML("<br>"), # Adding line break for spacing
          p(strong("But what does this indicator mean?")),
          p("Please consult the background tab for now"),
          ########### placer
          fluidRow(
            column(
              selectizeInput(
                inputId = "downloadOptions_map",
                label = "Download Formats",
                choices = c(
                  "EPS",
                  "JPEG",
                  "PDF",
                  "PNG",
                  "SVG",
                  "TEX",
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
          HTML("<br>")  # Adding line break for spacing
        ),

        ############################# Time Series tab
        tabPanel(
          title = "Time-series",
          HTML("<br>"), # Adding line break for spacing
          em(
            "In this tab you can view the time-series plot of your selected ",
            "biodiversity indicator. Use the left-hand panel to select the ",
            "indicator, taxa, geographical area, and temporal window of ",
            "interest."
          ),
          HTML("<br>"), # Adding line break for spacing
          HTML("<br>"), # Adding line break for spacing
          em(
            "Loading the plot could take a few minutes, depending on the ",
            "options you have selected."
          ),
          HTML("<br>"),  # Adding line break for spacing
          HTML("<br>"),
          actionButton("plot_ts_bt", "Plot Time Series"),
          HTML("<br>"), # Adding line break for spacing
          HTML("<br>"), # Adding line break for spacing
          plotlyOutput("plot_ts"),
          HTML("<br>"),
          p(strong("What am I looking at?")),
          textOutput("figure_legend_ts_text"),
          HTML("<br>"),
          p(strong("But what does this indicator mean?")),
          p("Please consult the background tab for now"),
          fluidRow(
            column(
              selectizeInput("downloadOptions_ts",
                             "Download Formats",
                             choices = c(
                               "EPS",
                               "JPEG",
                               "PDF",
                               "PNG",
                               "SVG",
                               "TEX",
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
          HTML("<br>")  # Adding line break for spacing
        ),
        #####################
        tabPanel(
          title = "Table",
          HTML("<br>"),  # Adding line break for spacing
          textOutput("table_text"),
          HTML("<br>"), # Adding line break for spacing
          HTML("<br>"), # Adding line break for spacing
          dataTableOutput("table")
        ),
        tabPanel(
          title = "Export",
          HTML("<br>"),  # Adding line break for spacing
          HTML("<div>Download the processed data cube here.</div>"),
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
        tabPanel(
          title = "About",
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
              "B-Cubed (Biodiversity Building Blocks for policy) receives ",
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
    # Load GBIF data cube
    # cube_name <- "data/europe_species_cube.csv"
    # req(input$dataCube$datapath)
    # cube_name <- input$dataCube$datapath

    # Prepare cube
    #  if (!is.null(input$taxaFile$datapath)) {
    #    r$dataCube <- process_cube_old(cube_name, input$taxaFile$datapath)
    #  } else {
    r$dataCube <- process_cube(input$dataCube$datapath)
    #  }

    r$dataCube1 <- r$dataCube
  })

  observeEvent(regionupdate, {
    choices <- regionupdate()
    updateSelectInput(
      inputId = "region",
      choices = choices
    )
  })

  # update input$region options based on user-selected spatial level
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

  # update map resolution options based on country type
  reschoiceupdate <- reactive({
    if (input$countrytype == "tiny_countries") {
      c("50", "110")
    } else {
      c("10", "50", "110")
    }
  })

  observeEvent(reschoiceupdate(), {
    choices <- reschoiceupdate()
    updateSelectInput(
      inputId = "mapres",
      choices = choices,
      selected = "50"
    )
  })

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

  # update country type options based on spatial level and map resolution
  observeEvent(countrytypeupdate(), {
    choices <- countrytypeupdate()
    updateSelectInput(
      inputId = "countrytype",
      choices = choices
    )
  })

  observeEvent(input$dataCube, {
    req(r$dataCube)

    units <- stringr::str_extract(r$dataCube$resolutions, "(?<=[0-9,.]{1,6})[a-z]*$")
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

  all_families <- reactive({
    req(r$dataCube)
    sort(unique(r$dataCube$data$family))
  })

  all_species <- reactive({
    req(r$dataCube)
    sort(unique(r$datCube$data$scientificName))
  })

  observeEvent(all_families(), {
    updateSelectInput(
      session,
      inputId = "family",
      choices = all_families(),
      selected = input$family
    )
  })

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

  ############################ metadata tab outputs

  # output metadata from imported cube
  output$metadata <- renderPrint({
    req(r$dataCube1)
    r$dataCube1
  })

  ############################ table tab outputs

  # output message for table tab
  output$table_text <- renderText(
    paste("In this tab you can view your data cube as a table.", input$table_text)
  )
  # output interactive table from imported cube
  output$table <- renderDataTable({
    req(r$dataCube1)
    print(r$dataCube1$data, n = 0)
  })


  ############################ map tab outputs

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

            params <- list(
              data = r$dataCube1,
              cell_size = input$cellsize,
              level = input$spatiallevel,
              first_year = input$daterange[1],
              last_year = input$daterange[2],
              ne_type = input$countrytype,
              ne_scale = mapres,
              region = region_param
            )

            # Example of utilizing an action if input is missing or NULL leads to avoiding
            # ifelse usage like that
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
                "Mean Year of Occurrence" = newness_map
              ),
              params
            )

            map # Return the map object
          },
          warning = function(w) {
            showNotification(paste("Warning:", conditionMessage(w)), type = "warning")
            invokeRestart("muffleWarning")
          }
        )
      },
      error = function(e) {
        showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = NULL)
        return(NULL)
      }
    )
  })

  # output plot from imported cube
  output$plot_map <- renderPlot({
    req(plot_to_render_map())

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

    if (input$breaks == "") {
      breaks <- NULL
    } else {
      breaks <- c(as.numeric(input$breaks))
    }

    if (input$labels == "") {
      labels <- NULL
    } else {
      labels <- c(input$labels)
    }

    if (input$legend_limits == "") {
      legend_limits <- NULL
    } else {
      legend_limits <- c(as.numeric(input$legend_limits))
    }

    if (input$title == "") {
      title <- NULL
    } else {
      title <- input$title
    }

    if (input$legend_title == "") {
      legend_title <- NULL
    } else {
      legend_title <- input$legend_title
    }

    if (input$trans == "none") {
      trans <- NULL
    } else {
      trans <- input$trans
    }

    params <- list(
      x = plot_to_render_map(),
      title = title,
      title_wrap_length = input$wrap_length,
      # xlims = xlims,
      # ylims = ylims,
      # trans = trans,
      # breaks = breaks,
      # labels = labels,
      Europe_crop_EEA = input$europe_crop_eea,
      crop_to_grid = input$crop_to_grid,
      panel_bg = input$panel_bg,
      land_fill_colour = input$land_fill_colour,
      # legend_title = legend_title,
      # legend_limits = legend_limits,
      legend_title_wrap_length = input$legend_title_wrap_length
    )

    map_plot <- do.call(plot, params)
  })

  plot_to_print_map <- reactive({
    plot(plot_to_render_map())
  })

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
      ggsave(filename,
        plot = plot_to_print_map(),
        device = tolower(input$downloadOptions_map)
      )
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
                "Mean Year of Occurrence" = newness_ts
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

  # output time series from imported cube
  output$plot_ts <- renderPlotly({
    req(plot_to_render_ts())

    ribboncolour <- if (input$ci_vis_type == "None") NA else input$ribboncolour
    vistype <- if (input$ci_vis_type == "None") "ribbon" else input$ci_vis_type
    gridlines <- if (input$ts_gridlines == "TRUE") FALSE else TRUE
    if (
      is.null(input$title) ||
      length(input$title) == 0 ||
      input$title == ""
    ) {
      title <- "auto"
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

    params <- list(
      x = plot_to_render_ts(),
      title = title,
      suppress_y = input$suppress_y,
      smoothed_trend = input$smoothed_trend,
      x_label = xlabel,
      y_label = ylabel,
      x_expand = c(input$ts_x_expand_left, input$ts_x_expand_right),
      y_expand = c(input$ts_y_expand_bottom, input$ts_y_expand_top),
      x_breaks = input$ts_x_breaks,
      y_breaks = input$ts_y_breaks,
      gridoff = gridlines,
      ci_type = input$vistype,
      point_line = input$point_line,
      pointsize = input$pointsize,
      linewidth = input$linewidth,
      linecolour = input$linecolour,
      linealpha = input$linealpha,
      error_width = input$error_width,
      error_thickness = input$error_thickness,
      error_alpha = input$error_alpha,
      ribboncolour = ribboncolour,
      ribbonalpha = input$ribbonalpha,
      smooth_linetype = input$smooth_linetype,
      smooth_linewidth = input$smooth_linewidth,
      trendlinecolour = input$trendlinecolour,
      trendlinealpha = input$trendlinealpha,
      smooth_cilinewidth = input$smooth_cilinewidth,
      envelopecolour = input$envelopecolour,
      envelopealpha = input$envelopealpha,
      smooth_cialpha = input$smooth_cialpha,
      wrap_length = input$wrap_length
    )

    # Plot diversity metric
    ts_plot <- do.call(plot, params)

    ts_plot
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
      ggsave(filename, plot = plot_to_print_ts(), device = tolower(input$downloadOptions_ts))
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

