# This is a push test from Lissa

#install.packages("shiny")
#install.packages("shinyWidgets")
#install.packages("devtools")
#devtools::install_github("shawndove/b3gbi")
#install.packages("plotly")

library(plotly)
library(shiny)
library(shinyWidgets)
library(b3gbi)
library(DT)


#shinyWidgetsGallery()

library(lubridate)
library(shinyjs)
library(jsonlite)

# Hello, can you see this?
# Test yani

#test



###############################################################################################################
#####   UI     ################################################################################################
###########################   UI     ##########################################################################
##################################################   UI     ###################################################
#########################################################################   UI     ############################
###############################################################################################################


ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs

  # Style
  tags$head(
    tags$title("B³ Indicators"),
    tags$link(rel="icon", type="image/png", size="32x32", href="B3_logomark.png"),
    tags$meta(name="viewport", content="width=device-width"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  ),

  # input = text fields, action buttons

  # Application title

  titlePanel(title = span(img(src = "B3_logomark.png", height = 50),
                          "B-Cubed: General Biodiversity Indicators",
                          style="color:#000")),

  (
    div(
      HTML("<h3>Welcome to the B-Cubed: Biodiversity Indicators Shiny app!</h3>"),
      HTML("<p>Start by uploading your data cube using the file browser in the left-hand panel. You can also use this panel to choose the biodiversity indicator(s), taxa, geographical area, and temporal window of interest for your data. Use the tabs to visualize the outputs.</p>"),
      HTML("<p>In the Metadata tab, you will find the metadata associated with the data analysis options you selected. The Plot tab visualizes the biodiversity indicators on a map, the Table tab prints the data cube data, and in the Report tab, you can view the raw code used to produce outputs.</p>")
    )
  ),


  sidebarLayout(


#######################################################
####################### Inputs ########################
#######################################################

    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Input data",
          # input$dataCube
          fileInput(inputId = "dataCube",
                    label = HTML("Upload the data cube")
                    ),

        ),
        tabPanel(
          "Options",

          # the indicators
          selectInput(
            inputId = "indicatorsToAnalyse",
            label = "What indicators do you want to analyse?", multiple = FALSE,
            choices = as.character(sapply(b3gbi::available_indicators, "[[", 2)),
            selected = "Observed Species Richness",
          ),

          # Spatial level
          selectInput('spatiallevel',
                      'Spatial level',
                      c("continent", "country","world"),
                      selected = "continent"
                      ),

          # Spatial resolution
          numericInput('cellsize',
                    'Spatial resolution in kilometers',
                    value = 10),

          # Date range
          sliderInput("daterange",
                      "Date range:",
                      min = 1100,
                      max = year(Sys.Date()),
                      value=c(1100, year(Sys.Date())),
                      sep = ""
                      ),

          # Select by family name if available
          disabled(
            selectInput( ## select taxa from the database
              inputId = "family",
              label = "Subset by family",
              choices = NULL ,
              multiple = T
              )
          ),

        ),
      ),
    ),

#######################################################
####################### Outputs #######################
#######################################################

    # output = tables, plots, texts
    mainPanel(
      tabsetPanel(


##################### Metadata tab

        tabPanel(title = "Metadata",
                 ## output$metadata
                 textOutput("metadata"),

        ),

############################# Map tab

        tabPanel(
                 title = "Map",
                 em("In this tab you can view your selected biodiversity indicator projected onto a map. Use the left-hand panel to select the indicator, taxa, geographical area, and temporal window of interest."),
                 HTML("<br>"),  # Adding line break for spacing
                 #the maps
                 em("Loading the plots will take a minute or forever. Calm yourself!"),
                 plotOutput("plot_map"),
                 p(strong("Figure legend : What the heck am I looking at?")),
                 p(strong("But what is this indicator?")),
                 p(strong("And what does my plot say?")),
                 fluidRow(
                   column(
                     selectizeInput(inputId = "downloadOptions_map",
                                    label = "Download Formats",
                                    choices = c("EPS",
                                                "JPEG",
                                                "PDF",
                                                "PNG",
                                                "SVG",
                                                "TEX",
                                                "TIFF")
                                                ),
                     width = 6),
                   column(
                     downloadButton("downloadGo_map"),
                     width = 4,
                     style="padding:18px;"
                     )
                   ),
             ),
############################# Time Series tab


        tabPanel(title = "Time-series",
                 em("In this tab you can view the time-series plot of your selected biodiversity indicator. Use the left-hand panel to select the indicator, taxa, geographical area, and temporal window of interest."),
                 HTML("<br>"),  # Adding line break for spacing
                 #the time series
                 em("Loading the plots will take a minute or forever. Calm yourself!"),
                 plotlyOutput("plot_ts"),
                 p(strong("Figure legend : What the heck am I looking at?")),
                 p(strong("But what is this index?")),
                 p(strong("And what does my plot say?")),
                 fluidRow(
                   column(
                     selectizeInput("downloadOptions_ts",
                                    "Download Formats",
                                    choices = c("EPS",
                                                "JPEG",
                                                "PDF",
                                                "PNG",
                                                "SVG",
                                                "TEX",
                                                "TIFF")
                                    ),
                     width = 6),
                   column(
                     downloadButton("downloadGo_ts"),
                     width = 4,
                     style="padding:18px;"
                     )
                   ),
                 ),

##################### Table tab

        tabPanel(title = "Table",
                 textOutput("table_text"),
                 HTML("<br>"),  # Adding line break for spacing
                 HTML("<br>"),  # Adding line break for spacing
                 DTOutput("table")
        ),

##################### Export tab

        tabPanel(title = "Export",
                 HTML("<div>Download the processed cube data here.</div>"),
                 HTML("<br>"),  # Adding line break for spacing
                 HTML("<br>"),  # Adding line break for spacing
                 downloadButton("downloadProcessedCube",
                                label = "Processed Cube"),
                 downloadButton("downloadMappedCube",
                                label = "Mapped Cube")
        ),

##################### Report tab

        tabPanel(title = "Report",
                 textOutput("report_text")
        )
      )
    )
  ))



###############################################################################################################
#####   SERVER     ############################################################################################
###########################   SERVER     ######################################################################
##################################################   SERVER     ###############################################
#########################################################################   SERVER     ########################
###############################################################################################################


server <-function(input, output, session){

  options(shiny.maxRequestSize=500*1024^2)


############################ GENERAL Reactives and observers

  # update input$scientificname options based on the imported DataCube ---_
  observeEvent(input$taxaFile, {
    freezeReactiveValue(input, "scientificname")
    updateSelectInput(session = session, inputId = "scientificname",
                      #choices = sort(unique(dataCube()$data$scientificName))
                      choices = sort(unique(read.csv(input$taxaFile$datapath)$scientificName))
                      )
  })


  dataCube <- reactive({
    # Load GBIF data cube
    # cube_name <- "data/europe_species_cube.csv"
    req(input$dataCube$datapath)
    cube_name <- input$dataCube$datapath

    # Prepare cube
    if (!is.null(input$taxaFile$datapath)) {
     process_cube(cube_name, input$taxaFile$datapath)
    } else {
     process_cube(cube_name)
    }

  })

############################ metadata tab outputs

  output$metadata <- renderText(
    paste("In this tab you will be able to view the metadata associated with the options you have selected to visualise the biodiversity indicator(s).")
  )


############################ Maps tab outputs

  plot_to_render_map <- reactive({
    req(dataCube())

    params <- list(data = dataCube(),
                   cell_size = input$cellsize,
                   level = input$spatiallevel,
                   first_year = input$daterange[1],
                   last_year = input$daterange[2])

    if(input$indicatorsToAnalyse == "Observed Species Richness"){
      do.call(obs_richness_map, params)
    } else if (input$indicatorsToAnalyse == "Total Occurrences"){
      do.call(total_occ_map, params)
    } else if (input$indicatorsToAnalyse == "Pielou's Evenness"){
      do.call(pielou_evenness_map, params)
    } else if (input$indicatorsToAnalyse == "Williams' Evenness"){
      do.call(williams_evenness_map, params)
    } else if (input$indicatorsToAnalyse == "Cumulative Species Richness"){
      NULL
    } else if (input$indicatorsToAnalyse == "Density of Occurrences"){
      do.call(occ_density_map, params)
    } else if (input$indicatorsToAnalyse == "Abundance-Based Rarity"){
      do.call(ab_rarity_map, params)
    } else if (input$indicatorsToAnalyse == "Area-Based Rarity"){
      do.call(area_rarity_map, params)
    } else if (input$indicatorsToAnalyse == "Mean Year of Occurrence"){
      do.call(newness_map, params)
    }



  })


  output$plot_map <- renderPlot({
    req(plot_to_render_map())

    # Plot diversity metric
    plot(plot_to_render_map(),
         title = paste(input$indicatorsToAnalyse, ": Insects in Europe"))
  })


  plot_to_print_map <- reactive({
    plot(plot_to_render_map())
  })

  output$downloadGo_map <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*","",.) %>%
        paste0(.,
               "_map.",
               tolower(input$downloadOptions_map))},
    content = function(filename) {
      ggsave(filename,
             plot = plot_to_print_map(),
             device = tolower(input$downloadOptions_map))
    }
  )

#  output$map_text <- renderText(
#    paste("In this tab you can view your selected biodiversity indicator projected onto a map. Use the left-hand panel to select the indicator, taxa, geographical area, and temporal window of interest.", input$text_ts)
#  )



############################ time-series tab outputs

  plot_to_render_ts <- reactive({
    req(dataCube())

    params <- list(data = dataCube(),
                   cell_size = input$cellsize,
                   level = input$spatiallevel,
                   first_year = input$daterange[1],
                   last_year = input$daterange[2])

    if(input$indicatorsToAnalyse == "Observed Species Richness"){
      do.call(obs_richness_ts, params)
    } else if (input$indicatorsToAnalyse == "Total Occurrences"){
      do.call(total_occ_ts, params)
    } else if (input$indicatorsToAnalyse == "Pielou's Evenness"){
      do.call(pielou_evenness_ts, params)
    } else if (input$indicatorsToAnalyse == "Williams' Evenness"){
      do.call(williams_evenness_ts, params)
    } else if (input$indicatorsToAnalyse == "Cumulative Species Richness"){
      do.call(cum_richness_ts, params)
    } else if (input$indicatorsToAnalyse == "Density of Occurrences"){
      do.call(occ_density_ts, params)
    } else if (input$indicatorsToAnalyse == "Abundance-Based Rarity"){
      do.call(ab_rarity_ts, params)
    } else if (input$indicatorsToAnalyse == "Area-Based Rarity"){
      do.call(area_rarity_ts, params)
    } else if (input$indicatorsToAnalyse == "Mean Year of Occurrence"){
      do.call(newness_ts, params)
    }



  })

  output$plot_ts <- renderPlotly({
    req(plot_to_render_ts())
    # Plot diversity metric
    plot(plot_to_render_ts(),
         title = paste(input$indicatorsToAnalyse, ": Insects in Europe"))
  })

  plot_to_print_ts <- reactive({
    plot(plot_to_render_ts())
  })

  output$downloadGo_ts <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*","",.) %>%
        paste0(.,
               "_timeSeries.",
               tolower(input$downloadOptions_ts))},
    content = function(filename) {
      ggsave(filename, plot = plot_to_print_ts(), device = tolower(input$downloadOptions_ts))
    }
  )

#  output$timeSeries_text <- renderText(
#    paste("In this tab you can view the time-series plot of your selected biodiversity indicator. Use the left-hand panel to select the indicator, taxa, geographical area, and temporal window of interest.", input$text_ts)
#  )

############################ table tab outputs



  output$table <- renderDT({

    req(dataCube())

    dataCube()$data

  })

  output$table_text <- renderText(
    paste("In this tab you can view your data cube as a table.", input$table_text)
  )


  ############################ metadata tab outputs

#  output$plot_text <- renderText(
#    paste("In this tab you can view your selected biodiversity indicator projected onto a map. Use the left-hand panel to select the indicator, taxa, geographical area, and temporal window of interest.", input$plot_text)
#  )


#  output$table_text <- renderText(
#    paste("In this tab you can view your data cube as a table.", input$table_text)
#  )

  output$report_text <- renderText(
    paste("In this tab you can view a report summarising the code that was used to plot biodversity indicators from your data cube.", input$report_text)
  )

  #  plot_to_render <- reactive({
  #    req(dataCube())
  #
  #    obs_richness_map(dataCube())
  #
  #  })





  output$downloadProcessedCube <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*","",.) %>%
        paste0(.,
               ".",
               "json")},
    content = function(filename) {
      toexport = toJSON(unclass(dataCube()),
                        digits=NA,
                        pretty=T,
                        flatten=T,
                        auto_unbox=T)
      write(toexport,
            filename)
    }
  )
  output$downloadMappedCube <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*","",.) %>%
        paste0(.,
               "_mapped_",
               ".",
               "json")},
    content = function(filename) {
      toexport = toJSON(unclass(plot_to_render()),
                        digits=NA,
                        pretty=T,
                        flatten=T,
                        auto_unbox=T)
      write(toexport,
            filename)
    }
  )



}

shinyApp(ui = ui, server = server)
