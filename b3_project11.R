# This is a push test from Lissa

#install.packages("shiny")
#install.packages("shinyWidgets")
#install.packages("devtools")
#devtools::install_github("shawndove/b3gbi")

library(shiny)
library(shinyWidgets)
library(b3gbi)
library(DT)
library(lubridate)

# Hello, can you see this?
# Test yani

#test

ui <- fluidPage(
  # Style
  tags$head(
    tags$title("B³ Indicators"),
    tags$link(rel="icon", type="image/png", size="32x32", href="B3_logomark.png"),
    tags$meta(name="viewport", content="width=device-width"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  # input = text fields, action buttons

  # Application title
  titlePanel(title = span(img(src = "B3_logomark.png", height = 50),
                          "B-Cubed: General Biodiversity Indicators",
                          style="color:#000")),

  sidebarLayout(
    sidebarPanel(
      # input$dataCube
      fileInput(inputId = "dataCube",
                label = "Upload the data cube",
                ),
      # input$taxaFile
      fileInput(inputId = "taxaFile", label = "Upload the taxa information"),
      # Spatial level
      selectInput('spatiallevel', 'Spatial level', c("continent", "country","world")),
      # Spatial resolution
      selectInput('cellsize', 'Spatial resolution in kilometers', c(10,100)),
      # Date range
      sliderInput("daterange",
                  "Date range:",
                  min = 1900,
                  max = year(Sys.Date()),
                  value=c(1900, year(Sys.Date())),
                  sep = ""),
      # Taxa selection
      selectInput( ## select taxa from the database
        inputId = "scientificname",
        label = "Scientific name:",
        choices = NULL ,
        multiple = T 
      )
      
      # shinyWidgetsGallery()

    ),
    # output = tables, plots, texts
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Metadata",
                 ## output$metadata
                 textOutput("metadata")
        ),

        tabPanel(title = "Plot",
                 ## output$plot
                 textOutput("plot_text"),
                 HTML("<br>"),  # Adding line break for spacing
                 plotOutput("plot"),
                 textOutput("dataCube_path"),
                 textOutput("taxaFile_path"),
                 fluidRow(
                   column(
                     selectizeInput("downloadOptions",
                                    "Download Formats",
                                    choices = c("EPS",
                                                "JPEG",
                                                "PDF",
                                                "PNG",
                                                "SVG",
                                                "TEX",
                                                "TIFF")),
                     width = 6),
                   column(
                     downloadButton("downloadGo"),
                     width = 4,
                     style="padding:18px;"
                   )
        )),
        tabPanel(title = "Table",
                 textOutput("table_text"),
                 HTML("<br>"),  # Adding line break for spacing
                 HTML("<br>"),  # Adding line break for spacing
                 DTOutput("table")
        ),
        tabPanel(title = "Report",
                 textOutput("report_text")
        )
      ),
    )
  )

  # shinyWidgetsGallery()



)

server <-function(input, output, session){

  options(shiny.maxRequestSize=500*1024^2)

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
  
  
  output$table <- renderDT({
    
    req(dataCube())
    
    dataCube()$data

  })


  output$metadata <- renderText(
    paste("In this tab you will be able to view the metadata associated with the options you have selected to visualise the biodiversity indicator(s).", input$metadata)
  )
  output$plot_text <- renderText(
    paste("In this tab you can view your selected biodiversity indicator projected onto a map. Use the left-hand panel to select the indicator, taxa, geographical area, and temporal window of interest.", input$plot_text)
  )
  output$table_text <- renderText(
    paste("In this tab you can view your data cube as a table.", input$table_text)
  )
  output$report_text <- renderText(
    paste("In this tab you can view a report summarising the code that was used to plot biodversity indicators from your data cube.", input$report_text)
  )

  plot_to_render <- reactive({
    req(dataCube())
    obs_richness_map(dataCube())

  })

  output$plot <- renderPlot({
    req(plot_to_render())
    # Plot diversity metric
    plot(plot_to_render(), title = "Observed Species Richness: Insects in Europe")
  })

  plot_to_print <- reactive({
    plot(plot_to_render())
  })

  output$downloadGo <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*","",.) %>%
        paste0(.,
               ".",
               tolower(input$downloadOptions))},
    content = function(filename) {
      ggsave(filename, plot = plot_to_print(), device = tolower(input$downloadOptions))
    }
  )

}

shinyApp(ui = ui, server = server)
