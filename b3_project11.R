#install.packages("shiny")
#install.packages("shinyWidgets")
#install.packages("devtools")
#devtools::install_github("shawndove/b3gbi")

library(shiny)
library(shinyWidgets)
library(b3gbi)
library(DT)
# Hello, can you see this?

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
  titlePanel("B-Cubed: Biodiversity Indicators"),

  sidebarLayout(
    sidebarPanel(
      # input$dataCube
      fileInput(inputId = "dataCube",
                label = "Upload the data cube",
                ),
      # input$taxaFile
      fileInput(inputId = "taxaFile", label = "Upload the taxa information"),
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
                     width = 8),
                   column(
                     downloadButton("downloadGo"),
                     width = 4
                   )
        )),
        tabPanel(title = "Table",
                 DTOutput("table")
        ),
        tabPanel(title = "Report",
                 textOutput("name")
        )
      ),
    )
  )

  # shinyWidgetsGallery()



)

server <-function(input, output){

  options(shiny.maxRequestSize=500*1024^2)

  dataCube <- reactive({
    # Load GBIF data cube
    # cube_name <- "data/europe_species_cube.csv"
    cube_name <- input$dataCube$datapath

    # Prepare cube
    if (!is.null(input$taxaFile$datapath)) {
      process_cube(cube_name, input$taxaFile$datapath)
    } else {
      process_cube(cube_name)
    }
  })

  output$table <- renderDT({
    dataCube()$data
  })


  output$metadata <- renderText(
    paste("Hello,", input$metadata)
  )

  plot_to_render <- reactive({
    obs_richness_map(dataCube())
  })

  output$plot <- renderPlot({
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
