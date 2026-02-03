# Server Initialization Module
#
# This module provides a centralized initialization function for all
# server-side modules. It brings together data, taxonomy, and visualization
# server modules into a single interface.
#

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
  map_plot_module <- server_map_plot(input, output, session, r)

  # Time series plot
  ts_plot_module <- server_timeseries_plot(input, output, session, r)

  message("All server modules initialized successfully!")

  # Return list of all reactive objects
  list(
    r = r,
    shapefile_path = data_module$shapefile_path,
    plot_to_print_map = map_plot_module$plot_reactive,
    plot_to_print_ts = ts_plot_module$plot_reactive,
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
