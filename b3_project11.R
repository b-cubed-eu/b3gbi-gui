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
library(shinyjs)
library(jsonlite)

# Hello, can you see this?
# Test yani

#test

ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  # Style
  tags$head(
    tags$title("B³ Indicators"),
    tags$link(rel="icon", type="image/png", size="32x32", href="B3_logomark.png"),
    tags$meta(name="viewport", content="width=device-width"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(href="https://fonts.googleapis.com/css2?family=PT+Sans+Narrow:wght@400;700&display=swap",
              rel="stylesheet")
  ),

  # input = text fields, action buttons
  # Application title


  titlePanel(title = span(img(src = "B3_logomark.png", height = 50),
                          "B-Cubed: General Biodiversity Indicators",
                          style="color:#000")),

  (
    div(
      HTML("<p><span style='font-size: 18px;'>Welcome to the B-Cubed: Biodiversity Indicators Shiny app!</span><br><br>Start by uploading your data cube using the file browser in the left-hand panel. You can also use this panel to choose the biodiversity indicator(s), taxa, geographical area, and temporal window of interest for your data. Use the tabs to visualize the outputs.<br><br>In the Metadata tab, you will find the metadata associated with the data analysis options you selected. The Plot tab visualizes the biodiversity indicators on a map, the Table tab prints the data cube data, and in the Report tab, you can view the raw code used to produce outputs.</p>"),
      style = "font-size: 16px; color: #555;"
    )
  ),


  sidebarLayout(
    sidebarPanel(
      # input$dataCube
      fileInput(inputId = "dataCube",
                label = HTML("Upload the data cube"),
                ),
      # input$taxaFile


      fileInput(inputId = "taxaFile", label = HTML("Upload the taxa information<br><span style='font-style: italic;'>Note: taxa information is already integrated into some data cubes</span>"),
      ),

      # Spatial level
      selectInput('spatiallevel', 'Spatial level', c("continent", "country","world"), selected = "continent"),
      # Spatial resolution
      textInput('cellsize', 'Spatial resolution in kilometers'),
      # Date range
      sliderInput("daterange",
                  "Date range:",
                  min = 1100,
                  max = year(Sys.Date()),
                  value=c(1100, year(Sys.Date())),
                  sep = ""),
      # Select by family name if available
      disabled(selectInput( ## select taxa from the database
        inputId = "family",
        label = "Subset by family",
        choices = NULL ,
        multiple = T
      ))
    ),
    # output = tables, plots, texts
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Metadata",
                 ## output$metadata
                 textOutput("metadata"),

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
                                                "TIFF"),
                                    selected = "PNG"),
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
        tabPanel(title = "Export",
                 HTML("<div>Download the processed cube data here.</div>"),
                 HTML("<br>"),  # Adding line break for spacing
                 HTML("<br>"),  # Adding line break for spacing
                 downloadButton("downloadProcessedCube",
                                label = "Processed Cube"),
                 downloadButton("downloadMappedCube",
                                label = "Mapped Cube")
        ),
        tabPanel(title = "Report",
                 textOutput("report_text")
        )
      )
    )
  ))

  # shinyWidgetsGallery()



server <-function(input, output, session){

  options(shiny.maxRequestSize=500*1024^2)

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

  ## left out for now, this just for filtering datacube according to the family name
  #   if("year" %in% colnames(dataCube()$data)){ ## "year" will be replaced by "family" as soon as the code is fix
  #
  #     enable(id = "family")
  #
  #     # Update value options in input$family if the column "family" is available
  #     observe({
  #       freezeReactiveValue(input, "family")
  #
  #       updateSelectInput(session = session, inputId = "family",
  #                         #choices = sort(unique(dataCube()$data$scientificName))
  #                         choices = sort(unique(dataCube()$data$year))
  #       )
  #     })
  #
  #   }
  #
  #
  #   df <- as.data.frame/(dataCube()$data
  #                        )
  # if(isTruthy(input$family)){
  #
  #   df
  #
  # } else{
  #  df %>% filter(year %in% input$family)
  # }

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

        obs_richness_map(dataCube(),
                     cell_size = as.numeric(input$cellsize),
                     level = input$spatiallevel,
                     first_year = input$daterange[1],
                     last_year =  input$daterange[2]
                       )

  })

  output$plot <- renderPlot({
    req(plot_to_render())
    # Plot diversity metric
    plot(plot_to_render(),
         title = "Observed Species Richness: Insects in Europe")
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
      ggsave(filename,
             plot = plot_to_print(),
             device = tolower(input$downloadOptions))
    }
  )
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
