# Server Initialization Module
#
# This module provides a centralized initialization function for all
# server-side modules. It brings together data, taxonomy, and visualization
# server modules into a single interface.
#

#' Get Indicator Background HTML
#'
#' @description
#' Returns HTML content describing the selected indicator
#'
#' @param indicator Character. Selected indicator name
#'
#' @return HTML content for indicator background
#'
#' @keywords internal
get_indicator_background <- function(indicator) {
  switch(indicator,
    "Observed Species Richness" = HTML(
      paste(
        em("Species Richness"),
        p("Species richness is the total number of species present in a sample (Magurran, 1988)."),
        p(strong("Observed Richness")),
        p("Observed richness is calculated by summing the number of unique species observed for each year or each cell.")
      )
    ),
    "Total Occurrences" = HTML(
      paste(
        p(strong("Total Occurrences")),
        p("The total number of occurrences is calculated by summing the occurrences of all species observed for each cell or year.")
      )
    ),
    "Pielou's Evenness" = HTML(
      paste(
        em("Evenness"),
        p("Species evenness measures how uniformly individuals are distributed across species."),
        p(strong("Pielou's Evenness")),
        p("Pielou's evenness (1966) is a well-known and commonly used evenness measure.")
      )
    ),
    "Williams' Evenness" = HTML(
      paste(
        em("Evenness"),
        p("Species evenness measures how uniformly individuals are distributed across species."),
        p(strong("Williams' Evenness")),
        p("Williams' evenness has important mathematical properties which Pielou's does not.")
      )
    ),
    "Cumulative Species Richness" = HTML(
      paste(
        em("Species Richness"),
        p("Species richness is the total number of species present in a sample."),
        p(strong("Cumulative Richness")),
        p("Cumulative richness is calculated by adding the newly observed unique species each year to a cumulative sum.")
      )
    ),
    "Density of Occurrences" = HTML(
      paste(
        p(strong("Density of Occurrences")),
        p("Density is calculated by summing the total number of occurrences per square km for each cell or year.")
      )
    ),
    "Abundance-Based Rarity" = HTML(
      paste(
        em("Rarity"),
        p("Rarity is the scarcity or infrequency of a particular species in an area."),
        p(strong("Abundance-Based Rarity")),
        p("Abundance-based rarity is the inverse of the proportion of total occurrences represented by a particular species.")
      )
    ),
    "Area-Based Rarity" = HTML(
      paste(
        em("Rarity"),
        p("Rarity is the scarcity or infrequency of a particular species in an area."),
        p(strong("Area-Based Rarity")),
        p("Area-based rarity is the inverse of occupancy frequency for a particular species.")
      )
    ),
    "Mean Year of Occurrence" = HTML(
      paste(
        p(strong("Mean Year of Occurrence")),
        p("The mean year of occurrence is calculated per cell, giving an indication of how recent the data is for each cell.")
      )
    ),
    "Taxonomic Distinctness" = HTML(
      paste(
        p(strong("Taxonomic Distinctness")),
        p("Taxonomic distinctness measures the taxonomic relatedness between species.")
      )
    ),
    "Species Richness (Estimated by Coverage-Based Rarefaction)" = HTML(
      paste(
        em("Hill Diversity"),
        p("Hill diversity calculates multiple measures of diversity by varying a single parameter."),
        p(strong("Species Richness")),
        p("Using the Hill diversity equation, richness becomes simply S, the number of species.")
      )
    ),
    "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)" = HTML(
      paste(
        em("Hill Diversity"),
        p("Hill diversity calculates multiple measures of diversity by varying a single parameter."),
        p(strong("Hill-Shannon Diversity")),
        p("Hill-Shannon diversity is e raised to the power of the Shannon index.")
      )
    ),
    "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)" = HTML(
      paste(
        em("Hill Diversity"),
        p("Hill diversity calculates multiple measures of diversity by varying a single parameter."),
        p(strong("Hill-Simpson Diversity")),
        p("Hill-Simpson diversity is the inverse of the Simpson index.")
      )
    ),
    "Species Occurrences" = HTML(
      paste(
        p(strong("Species Occurrences")),
        p("Species occurrences are considered an essential biodiversity variable (EBV).")
      )
    ),
    "Species Range" = HTML(
      paste(
        p(strong("Species Range")),
        p("Species range is the area over which a species is found.")
      )
    ),
    "Occupancy Turnover" = HTML(
      paste(
        p(strong("Occupancy Turnover")),
        p("Occupancy turnover measures the change in species composition over time.")
      )
    ),
    HTML("<p>Select an indicator to view background information.</p>")
  )
}

#' Initialize All Server Modules
#'
#' @description
#' Main initialization function that sets up all server-side reactive
#' logic for the B-Cubed Shiny application. This is the primary entry point
#' for Track G server logic.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param continents_data Continents spatial data (from app.R global scope)
#'
#' @return List containing:
#'   \itemize{
#'     \item r - ReactiveValues object (dataCube, dataCube1)
#'     \item shapefile_path - Reactive for shapefile path
#'     \item plot_to_print_map - Reactive for map plot
#'     \item plot_to_print_ts - Reactive for time series plot
#'     \item parsed_inputs - List of parsed input reactives
#'     \item iv - InputValidator object
#'   }
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   server_modules <- initialize_server_modules(
#'     input, output, session,
#'     continents_data = continents
#'   )
#' }
#' }
#'
#' @export
initialize_server_modules <- function(input, output, session,
                                       continents_data = NULL) {

  # Create reactiveValues for data storage
  r <- reactiveValues(
    dataCube = NULL,
    dataCube1 = NULL
  )

  # Initialize input validator
  iv <- initialize_input_validator(input)

  # Initialize data server modules
  message("Initializing data server modules...")
  data_module <- initialize_data_server(
    input, output, session,
    r = r,
    continents_data = continents_data
  )

  # Initialize taxonomy server modules
  message("Initializing taxonomy server modules...")
  server_taxonomy_filtering(input, output, session, r)

  # Initialize visualization server modules
  message("Initializing visualization server modules...")

  # UI dependencies
  server_viz_ui_dependencies(input, output, session)

  # Parsed inputs
  parsed_inputs <- server_parsed_inputs(input, output, session)

  # Map plot
  map_plot_module <- server_map_plot(input, output, session, r, parsed_inputs)

  # Time series plot
  ts_plot_module <- server_timeseries_plot(input, output, session, r, parsed_inputs)

  # Data table output - show cube data
  output$table <- DT::renderDT({
    req(r$dataCube1)
    DT::datatable(r$dataCube1$data,
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

  # Metadata output for Summary tab - show cube metadata
  output$metadata <- renderPrint({
    req(r$dataCube1)
    r$dataCube1
  })

  # Figure legend outputs
  fig_legend_map <- eventReactive(input$plot_map_bt, {
    req(r$dataCube1)
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

  output$figure_legend_map_text <- renderText({
    req(r$dataCube1)
    fig_legend_map()
  })

  fig_legend_ts <- eventReactive(input$plot_ts_bt, {
    req(r$dataCube1)
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

  output$figure_legend_ts_text <- renderText({
    req(r$dataCube1)
    fig_legend_ts()
  })

  # Indicator background text outputs
  output$indicator_background_text_map <- renderUI({
    req(input$indicatorsToAnalyse)
    get_indicator_background(input$indicatorsToAnalyse)
  })

  output$indicator_background_text_ts <- renderUI({
    req(input$indicatorsToAnalyse)
    get_indicator_background(input$indicatorsToAnalyse)
  })

  # Download handlers
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
      dlmap_width <- if (is.null(input$dlmap_width) || input$dlmap_width == "") 2000 else input$dlmap_width
      dlmap_height <- if (is.null(input$dlmap_height) || input$dlmap_height == "") 2000 else input$dlmap_height
      dlmap_scaling <- if (is.null(input$dlmap_scaling) || input$dlmap_scaling == "") 1 else input$dlmap_scaling

      if (dlmap_width > 10000) dlmap_width <- 10000
      if (dlmap_width < 100) dlmap_width <- 100
      if (dlmap_height > 10000) dlmap_height <- 10000
      if (dlmap_height < 100) dlmap_height <- 100
      if (dlmap_scaling > 10) dlmap_scaling <- 10
      if (dlmap_scaling <= 0.1) dlmap_scaling <- 0.1

      if (input$downloadOptions_map == "JPG" ||
          input$downloadOptions_map == "PNG" ||
          input$downloadOptions_map == "TIFF") {
        ggsave(filename,
               plot = map_plot_module$plot_to_print_map(),
               device = tolower(input$downloadOptions_map),
               width = dlmap_width,
               height = dlmap_height,
               units = "px",
               scaling = dlmap_scaling
        )
      } else {
        ggsave(filename,
               plot = map_plot_module$plot_to_print_map(),
               device = tolower(input$downloadOptions_map),
               width = dlmap_width,
               height = dlmap_height,
               units = "px"
        )
      }
    }
  )

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
      dlts_width <- if (is.null(input$dlts_width) || input$dlts_width == "") 2000 else input$dlts_width
      dlts_height <- if (is.null(input$dlts_height) || input$dlts_height == "") 2000 else input$dlts_height
      dlts_scaling <- if (is.null(input$dlts_scaling) || input$dlts_scaling == "") 1 else input$dlts_scaling

      if (dlts_width > 10000) dlts_width <- 10000
      if (dlts_width < 100) dlts_width <- 100
      if (dlts_height > 10000) dlts_height <- 10000
      if (dlts_height < 100) dlts_height <- 100
      if (dlts_scaling > 10) dlts_scaling <- 10
      if (dlts_scaling <= 0.1) dlts_scaling <- 0.1

      if (input$downloadOptions_ts == "JPG" ||
          input$downloadOptions_ts == "PNG" ||
          input$downloadOptions_ts == "TIFF") {
        ggsave(filename,
               plot = ts_plot_module$plot_to_print_ts(),
               device = tolower(input$downloadOptions_ts),
               width = dlts_width,
               height = dlts_height,
               units = "px",
               scaling = dlts_scaling
        )
      } else {
        ggsave(filename,
               plot = ts_plot_module$plot_to_print_ts(),
               device = tolower(input$downloadOptions_ts),
               width = dlts_width,
               height = dlts_height,
               units = "px"
        )
      }
    }
  )

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
      map_result <- map_plot_module$plot_to_print_map()
      if (!is.null(map_result)) {
        write.csv(unclass(map_result$data), filename)
      }
    }
  )

  output$downloadTimeSeriesData <- downloadHandler(
    filename = function() {
      input$dataCube$name %>%
        gsub("\\..*", "", .) %>%
        paste0(
          .,
          "_timeseries_",
          ".",
          "csv"
        )
    },
    content = function(filename) {
      ts_result <- ts_plot_module$plot_to_print_ts()
      if (!is.null(ts_result)) {
        write.csv(unclass(ts_result$data), filename)
      }
    }
  )

  # Create output renderers (must be called to register outputs)
  message("DEBUG: About to call render_map()...")
  map_plot_module$render_map()
  message("DEBUG: About to call render_ts()...")
  ts_plot_module$render_ts()

  message("All server modules initialized successfully!")

  # Return list of all reactive objects
  list(
    r = r,
    shapefile_path = data_module$shapefile_path,
    plot_to_print_map = map_plot_module$plot_to_print_map,
    plot_to_print_ts = ts_plot_module$plot_to_print_ts,
    parsed_inputs = parsed_inputs,
    iv = iv
  )
}

#' Initialize Input Validator
#'
#' @description
#' Sets up shinyvalidate InputValidator with validation rules for
#' plot dimensions and other critical inputs.
#'
#' @param input Shiny input object
#'
#' @return InputValidator object
#'
#' @keywords internal
initialize_input_validator <- function(input) {
  iv <- shinyvalidate::InputValidator$new()

  # Add validation rules for plot dimensions
  iv$add_rule("plot_height", shinyvalidate::sv_required())
  iv$add_rule("plot_height", function(x) {
    if (x < 100 || x > 2000) {
      return("Height must be between 100 and 2000.")
    }
  })

  iv$add_rule("plot_width", shinyvalidate::sv_required())
  iv$add_rule("plot_width", function(x) {
    if (x < 100 || x > 2000) {
      return("Width must be between 100 and 2000.")
    }
  })

  # Start displaying errors in the UI
  iv$enable()

  iv
}
