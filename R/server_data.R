# R/server_data.R
# Server-side data management functions
#
# This module provides Shiny server functions that wrap the business logic
# from data_loading.R, shapefile.R, and region_logic.R with proper Shiny
# reactivity, error handling, and notifications.

#' Server Data Loading
#'
#' @description
#' Handles data cube loading in Shiny server context. Wraps the load_data_cube()
#' function with Shiny-specific error handling and notifications.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param r ReactiveValues object containing dataCube and dataCube1
#'
#' @return None (called for side effects)
#'
#' @examples
#' \dontrun{
#' # In server function:
#' r <- initialize_data_state()
#' server_data_loading(input, output, session, r)
#' }
#'
#' @export
server_data_loading <- function(input, output, session, r) {
  observeEvent(input$dataCube, {
    req(input$dataCube)

    tryCatch({
      file_path <- input$dataCube$datapath

      # Check file size before loading
      check_file_size(file_path, threshold_mb = 100)

      # Show progress notification
      withProgress(
        message = "Loading data cube...",
        detail = "Processing file with b3gbi",
        value = 0.3,
        {
          # Load the data cube using existing module
          cube <- load_data_cube(file_path, validate = TRUE)

          setProgress(value = 0.8, message = "Finalizing...")

          # Store in reactive values using data access module
          set_data_cube(r, cube)

          # Get metadata for notification
          meta <- get_cube_metadata(cube)

          setProgress(value = 1)
        }
      )

      # Success notification
      showNotification(
        paste0(
          "Data loaded successfully!\n",
          format(meta$n_rows, big.mark = ","), " observations, ",
          meta$n_species, " species"
        ),
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      # Clear any partial data
      clear_data_cubes(r)

      # Show error notification
      showNotification(
        paste("Error loading data:", conditionMessage(e)),
        type = "error",
        duration = 10
      )

      # Log error for debugging
      warning("Data loading failed: ", conditionMessage(e))
    })
  })
}


#' Server Shapefile Handling
#'
#' @description
#' Creates a reactive expression for shapefile path extraction from uploaded
#' ZIP files. Wraps load_shapefile_from_zip() with Shiny-specific error handling.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return A reactive expression that returns the shapefile path or NULL
#'
#' @examples
#' \dontrun{
#' # In server function:
#' shapefile_path <- server_shapefile_handling(input, output, session)
#' 
#' # Use the reactive:
#' observe({
#'   shp_path <- shapefile_path()
#'   if (!is.null(shp_path)) {
#'     shp_data <- read_shapefile_safe(shp_path)
#'   }
#' })
#' }
#'
#' @export
server_shapefile_handling <- function(input, output, session) {
  reactive({
    req(input$shapefile_zip)

    tryCatch({
      zip_path <- input$shapefile_zip$datapath

      # Show progress
      withProgress(
        message = "Processing shapefile...",
        detail = "Extracting from ZIP archive",
        value = 0.3,
        {
          # Use existing shapefile module
          shp_path <- load_shapefile_from_zip(zip_path)

          # Validate components
          validate_shapefile_components(shp_path)

          setProgress(value = 1, message = "Complete!")
        }
      )

      # Success notification
      showNotification(
        "Shapefile loaded successfully",
        type = "message",
        duration = 3
      )

      shp_path

    }, error = function(e) {
      showNotification(
        paste("Error processing shapefile:", conditionMessage(e)),
        type = "error",
        duration = 10
      )

      # Log error
      warning("Shapefile processing failed: ", conditionMessage(e))

      NULL
    })
  })
}


#' Server Region Updates
#'
#' @description
#' Handles region selection updates based on spatial level, country type, and
#' map resolution. Uses region_logic.R functions for the actual business logic.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param continents_data Character vector of continent names (loaded globally)
#' @param data_env Environment containing spatial data objects. Default is .GlobalEnv
#'
#' @return None (called for side effects)
#'
#' @examples
#' \dontrun{
#' # In server function (assuming continents is loaded globally):
#' server_region_updates(input, output, session, continents)
#' }
#'
#' @export
server_region_updates <- function(input, output, session,
                                   continents_data = NULL,
                                   data_env = .GlobalEnv) {

  # Create reactive for region options using region_logic module
  region_options <- reactive({
    # Validate inputs exist
    req(input$spatiallevel)

    # Use the region_logic module to get options
    choices <- get_region_options(
      spatiallevel = input$spatiallevel,
      countrytype = input$countrytype,
      mapres = input$mapres,
      continents = continents_data,
      data_env = data_env
    )

    choices
  })

  # Update region selection when inputs change
  observeEvent({
    input$spatiallevel
    input$countrytype
    input$mapres
  }, {
    # Skip if spatial level doesn't require region selection
    if (input$spatiallevel %in% c("cube", "world")) {
      updateSelectizeInput(
        session,
        inputId = "region",
        choices = character(0),
        selected = ""
      )
      return()
    }

    # Get region options
    choices <- region_options()

    if (is.null(choices)) {
      # No valid choices for this combination
      updateSelectizeInput(
        session,
        inputId = "region",
        choices = character(0),
        selected = "",
        options = list(
          placeholder = "No regions available for this selection"
        )
      )
      return()
    }

    # Sort choices for better UX
    choices <- sort(unique(choices))

    # Preserve current selection if still valid
    current_selection <- input$region
    if (!is.null(current_selection) && current_selection %in% choices) {
      selected <- current_selection
    } else {
      selected <- NULL
    }

    # Update the input
    updateSelectizeInput(
      session,
      inputId = "region",
      choices = choices,
      selected = selected
    )
  }, ignoreNULL = FALSE)

  # Return the reactive in case it's needed for other operations
  invisible(region_options)
}


#' Server Map Resolution Updates
#'
#' @description
#' Handles map resolution dropdown updates based on country type selection.
#' Uses get_resolution_choices() from region_logic.R.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return None (called for side effects)
#'
#' @examples
#' \dontrun{
#' # In server function:
#' server_map_resolution_updates(input, output, session)
#' }
#'
#' @export
server_map_resolution_updates <- function(input, output, session) {

  # Create reactive for resolution choices
  resolution_choices <- reactive({
    req(input$countrytype)
    get_resolution_choices(input$countrytype)
  })

  # Update resolution dropdown when country type changes
  observeEvent(resolution_choices(), {
    choices <- resolution_choices()

    # Get current selection
    current <- input$mapres

    # If current selection is invalid for new choices, select default
    if (!is.null(current) && current %in% choices) {
      selected <- current
    } else {
      # Select middle option or first
      selected <- choices[min(2, length(choices))]
    }

    updateSelectInput(
      session,
      inputId = "mapres",
      choices = choices,
      selected = selected
    )
  })

  invisible(resolution_choices)
}


#' Server Custom Region Toggle
#'
#' @description
#' Handles resetting region selections when custom region mode is toggled off.
#' This restores default values for spatial level, country type, and clears
#' region selection.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return None (called for side effects)
#'
#' @examples
#' \dontrun{
#' # In server function:
#' server_custom_region_toggle(input, output, session)
#' }
#'
#' @export
server_custom_region_toggle <- function(input, output, session) {
  observeEvent(input$customregion, {
    if (isFALSE(input$customregion)) {
      # Reset to defaults
      updateSelectInput(
        session,
        inputId = "countrytype",
        selected = "countries"
      )

      updateSelectInput(
        session,
        inputId = "spatiallevel",
        selected = "cube"
      )

      updateSelectizeInput(
        session,
        inputId = "region",
        choices = character(0),
        selected = ""
      )
    }
  }, ignoreNULL = TRUE)
}


#' Initialize Data Server Module
#'
#' @description
#' Convenience function to initialize all data-related server observers and
#' reactives in one call. This is the main entry point for Track G server logic.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param r ReactiveValues object (will be created if NULL)
#' @param continents_data Character vector of continent names
#' @param data_env Environment containing spatial data objects
#'
#' @return List containing:
#'   \itemize{
#'     \item r: The reactiveValues object
#'     \item shapefile_path: Reactive for shapefile path
#'     \item region_options: Reactive for region options
#' }
#'
#' @examples
#' \dontrun{
#' # In server function:
#' data_module <- initialize_data_server(
#'   input, output, session,
#'   continents_data = continents
#' )
#' 
#' # Access reactive values:
#' data_module$r$dataCube
#' data_module$shapefile_path()
#' }
#'
#' @export
initialize_data_server <- function(input, output, session,
                                    r = NULL,
                                    continents_data = NULL,
                                    data_env = .GlobalEnv) {

  # Initialize reactive values if not provided
  if (is.null(r)) {
    r <- initialize_data_state()
  }

  # Set up data loading
  server_data_loading(input, output, session, r)

  # Set up shapefile handling
  shapefile_path <- server_shapefile_handling(input, output, session)

  # Set up region updates
  region_options <- server_region_updates(
    input, output, session,
    continents_data = continents_data,
    data_env = data_env
  )

  # Set up resolution updates
  server_map_resolution_updates(input, output, session)

  # Set up custom region toggle
  server_custom_region_toggle(input, output, session)

  # Return references for external use
  list(
    r = r,
    shapefile_path = shapefile_path,
    region_options = region_options
  )
}
