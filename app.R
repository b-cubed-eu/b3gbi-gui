ui <- fluidPage(

  sidebarLayout(

    #######################################################
    ####################### Inputs ########################
    #######################################################

    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Data cube",
          # input$dataCube
          fileInput(inputId = "dataCube",
                    label = HTML("Upload the data cube")
          )
        )
      )
    ),
    tabPanel(title = "Table",
             textOutput("table_text"),
             HTML("<br>"),  # Adding line break for spacing
             HTML("<br>"),  # Adding line break for spacing
             dataTableOutput("table")
    )
  )
)

server <-function(input, output, session){

  # options(shiny.maxRequestSize=500*1024^2)

  ################################ GENERAL reactives and observers


  # update input$scientificname options based on the imported DataCube ---_
  # observeEvent(input$taxaFile, {
  #   freezeReactiveValue(input, "scientificname")
  #   updateSelectInput(session = session, inputId = "scientificname",
  #                     #choices = sort(unique(dataCube()$data$scientificName))
  #                     choices = sort(unique(read.csv(input$taxaFile$datapath)$scientificName))
  #                     )
  # })


  dataCube1 <- eventReactive(input$dataCube, {
    # Load GBIF data cube
    # cube_name <- "data/europe_species_cube.csv"
    # req(input$dataCube$datapath)
    cube_name <- input$dataCube

    # Prepare cube
    process_cube(cube_name)

  })
  # this is not working the way we want it to
  #output$dataCubePrint <- renderPrint({dataCube()[1:6]})
  output$table <- renderDataTable({

    req(dataCube1())

    dataCube1()$data

  })

}

shinyApp(ui, server)

