# This is a push test from Lissa

#install.packages("shiny")
#install.packages("shinyWidgets")
#install.packages("devtools")
#devtools::install_github("shawndove/b3gbi", force = TRUE)
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
      HTML("<p><span style='font-size: 18px;'>Welcome to the B-Cubed: Biodiversity Indicators Shiny app!</span><br><br>The B-Cubed: Biodiversity Indicators Shiny app uses the R package <a href='https://github.com/b-cubed-eu/b3gbi' style='color: blue; text-decoration: none;'>b3gbi</a> to calculate and visualise widely used biodiversity indicators from a data cube; either one created using <a href='https://www.gbif.org/' style='color: blue; text-decoration: none;'>GBIF</a> or one created from your own data.<br><br>Start by uploading your data cube using the file browser in the left-hand panel. You can also use this panel to choose the biodiversity indicator(s), taxa, geographical area, and temporal window of interest for your data. Use the tabs to visualize the outputs.<br><br>In the Explore Your Data tab, you will find the metadata summarising your data cube. The Plot tab visualizes the biodiversity indicators on a map, the Table tab prints the data cube data, and in the Report tab, you can view the raw code used to produce outputs.</p>"),
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
          fileInput(inputId = "dataCube",
                    label = HTML("Upload the data cube")
          ),
        ),
        tabPanel(
          "Input filters",
          # the indicators
          selectInput(
            inputId = "indicatorsToAnalyse",
            label = "Biodiversity Indicator", multiple = FALSE,
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
                       value = 100),

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
        )
    ),


#######################################################
####################### Outputs #######################
#######################################################

    # output = tables, plots, texts
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Explore Your Data",
                 ## output$metadata
                 textOutput("metadata"),
                 # this was not working the way we want it to (will fix later)
                 #textOutput("dataCubePrint"),
                 p("DOI: I10.15468/dl.p4z2v5"), #This should change with every cube (for now it is static)


        ),
        tabPanel(
          title = "Background",
          em("In this tab you can view all available biodiversity indicators."),
          h3("Biodiversity Indicators"),
          HTML("<br>"),

          em("Occurrences"),
          p(strong("Total Occurrences")),
          p("The total number of occurrences is calculated by summing the occurrences of all species observed for each cell or year. This variable provides an overview of the comprehensiveness and distribution of data in the cube being analysed, and may be helpful, or even vital, for interpreting the results of calculated indicators."),
          p(strong("Density of Occurrences")),
          p("Density is calculated by summing the total number of occurrences per square kilometre for each cell or year. This provides similar information to total occurrences, but is adjusted for cell area."),
          HTML("<br>"),

          em("Richness"),
          p("Species richness is the total number of species present in a sample (Magurran, 1988). It is a fundamental and commonly used measure of biodiversity, providing a simple and intuitive overview of the status of biodiversity. However, richness is not well suited to measuring biodiversity change over time, as it only decreases when local extinctions occur and thus lags behind abundance for negative trends. While it may act as a leading indicator of alien species invasions, it will not indicate establishment because it ignores abundance. Nor will it necessarily indicate changes in local species composition, which can occur without any change in richness. Although richness is conceptually simple, it can be measured in different ways."),
          p(strong("Cumulative Species Richness")),
          p("Cumulative richness is calculated by adding the newly observed unique species each year to a cumulative sum. This indicator provides an estimation of whether and how many new species are still being discovered in a region. While an influx of alien species could cause an increase in cumulative richness, a fast-rising trend as shown in Fig. 2 is likely an indication that the dataset is not comprehensive and therefore observed richness will provide an underestimate of species richness."),
          HTML("<br>"),

          em("Evennes"),
          p("Species evenness is a commonly used indicator that measures how uniformly individuals are distributed across species in a region or over time. It provides a complement to richness by taking relative abundance into account. Although GBIF provides information about abundances as individual counts, the majority of entries lack this information. Hence, evenness can only be calculated using the proportions of observations rather than proportions of individuals. Strictly speaking, the evenness measures therefore indicate how uniformly species are represented in the respective data set rather than the true evenness of the ecological community."),
          p(strong("Pielou's Evenness")),
          p("Pielou (1966)"),
          p(strong("Williams' Evenness")),
          p("Kvålseth (2015)"),
          HTML("<br>"),

          em("Rarity"),
          p("Rarity is the scarcity or infrequency of a particular species in an area. A rare species might have a small population size, a limited distribution, or a unique ecological niche (Maciel, 2021; Rabinowitz, 1981). Rarity can also be a biodiversity indicator when summed over multiple species in an area, and may provide important insight for determining conservation priorities. It can be measured in different ways, but we will provide workflows to calculate rarity by abundance (using number of occurrences as a proxy) and by area. When measured over time rarity may indicate potential threats or changes in the environment."),
          p(strong("Abundance-Based Rarity")),
          p("Abundance-based rarity is the inverse of the proportion of total occurrences represented by a particular species. The total summed rarity for each grid cell or year is calculated (sum the rarity values of each species present there)."),
          p(strong("Area-Based Rarity")),
          p("Area-based rarity is the inverse of occupancy frequency (proportion of grid cells occupied) for a particular species. The total summed rarity for each grid cell or year is calculated (sum the rarity values of each species present there)."),
          HTML("<br>"),

          p(strong("Mean Year of Occurrence")),
          p("The mean year of occurrence is calculated per cell, giving an indication of how recent the data is for each cell. A recent mean year is not necessarily an indication of quality, as some countries or regions have been conducting comprehensive biodiversity monitoring for many years and will therefore reflect an older mean year of occurrence, while others may show a recent mean year due to e.g. the sudden availability of large amounts of citizen science data."),
          HTML("<br>")# Adding line break for spacing
          ),

############################# Map tab

        tabPanel(
                 title = "Map",
                 em("In this tab you can view your selected biodiversity indicator projected onto a map. Use the left-hand panel to select the indicator, taxa, geographical area, and temporal window of interest."),
                 HTML("<br>"),  # Adding line break for spacing
                 #the maps
                 em("Loading the plots will take a minute or forever. Calm yourself!"),
                 plotOutput("plot_map"),
                 HTML("<br>"),  # Adding line break for spacing
                 p(strong("What the heck am I looking at?")),
                 textOutput("figure_legend_map_text"),
                 HTML("<br>"),  # Adding line break for spacing
                 p(strong("But what does this indicator mean?")),
                 p("Sounds like a you problem. Kidding. Please consult the background tab for now"),
                 ########### placer
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
                 HTML("<br>"),
                 p(strong("What the heck am I looking at?")),
                 textOutput("figure_legend_ts_text"),
                 HTML("<br>"),
                 p(strong("But what does this indicator mean?")),
                 p("Sounds like a you problem. Kidding. Please consult the background tab for now"),
                 HTML("<br>"),
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
#####################
        tabPanel(title = "Table",
                 textOutput("table_text"),
                 HTML("<br>"),  # Adding line break for spacing
                 HTML("<br>"),  # Adding line break for spacing
                 DTOutput("table")
        ),
        tabPanel(title = "Export",
                 HTML("<div>Download the processed data cube here.</div>"),
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


###############################################################################################################
#####   SERVER     ############################################################################################
###########################   SERVER     ######################################################################
##################################################   SERVER     ###############################################
#########################################################################   SERVER     ########################
###############################################################################################################


server <-function(input, output, session){

  options(shiny.maxRequestSize=500*1024^2)

################################ GENERAL reactives and observers


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
  # this is not working the way we want it to
  #output$dataCubePrint <- renderPrint({dataCube()[1:6]})
  output$table <- renderDT({

    req(dataCube())

    dataCube()$data

  })


output$metadata <- renderText(
  paste("In this tab you will be able to view the metadata summarising your data cube.", input$metadata)
)


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

#  output$plot <- renderPlot({
#    req(plot_to_render())
#    # Plot diversity metric
#    plot(plot_to_render(), title = "Observed Species Richness: Insects in Europe")
#  })

  output$plot_map <- renderPlot({
    req(plot_to_render_map())

    # Plot diversity metric
    plot(plot_to_render_map(),
         title = paste(input$indicatorsToAnalyse))
  })

#  plot_to_print <- reactive({
#    plot(plot_to_render())
#  })

#  output$downloadGo <- downloadHandler(
#    filename = function() {
#      input$dataCube$name %>%
#        gsub("\\..*","",.) %>%
#        paste0(.,
#               ".",
#               tolower(input$downloadOptions))},
#    content = function(filename) {
#      ggsave(filename, plot = plot_to_print(), device = tolower(input$downloadOptions))
#    }
#  )

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



output$figure_legend_map_text <- renderText({
    paste(input$indicatorsToAnalyse,
            " of taxa in region, visualised at ",
            as.character(input$spatiallevel),
            " level and observed from ",
            as.character(input$daterange[1]),
            " to ",
            as.character(input$daterange[2]))
})





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

  output$figure_legend_ts_text <- renderText({
    paste(input$indicatorsToAnalyse,
          " of taxa in region  from ",
          as.character(input$daterange[1]),
          " to ",
          as.character(input$daterange[2]))
  })


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
