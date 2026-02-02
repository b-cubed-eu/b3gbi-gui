#' Data Access Layer for B3GBI Shiny App
#'
#' @description
#' Provides standardized getter and setter functions for accessing and managing
#' data within the Shiny application. This abstraction layer simplifies data
#' access across different modules.
#'
#' @name data_access
NULL

#' Get Original Data Cube
#'
#' @description
#' Retrieves the original unfiltered data cube from the reactive values store.
#'
#' @param r ReactiveValues object containing app state.
#'
#' @return The original processed_cube object, or NULL if not loaded.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In server context
#' r <- reactiveValues(dataCube = NULL)
#' # ... load data ...
#' cube <- get_data_cube(r)
#' }
get_data_cube <- function(r) {
  if (!inherits(r, "ReactiveValues")) {
    stop("r must be a ReactiveValues object")
  }
  
  r$dataCube
}


#' Get Filtered Data Cube
#'
#' @description
#' Retrieves the filtered data cube (after taxonomic filtering has been applied).
#' If no filtering has been applied, returns the original cube.
#'
#' @param r ReactiveValues object containing app state.
#'
#' @return The filtered processed_cube object, or NULL if not loaded.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- get_filtered_data_cube(r)
#' }
get_filtered_data_cube <- function(r) {
  if (!inherits(r, "ReactiveValues")) {
    stop("r must be a ReactiveValues object")
  }
  
  # Return filtered cube if available, otherwise original
  r$dataCube1 %||% r$dataCube
}


#' Get Data Cube Data Frame
#'
#' @description
#' Retrieves just the data frame from the cube, with optional filtering.
#' This is a convenience function for accessing raw data.
#'
#' @param r ReactiveValues object containing app state.
#' @param filtered Logical. Whether to return filtered data (TRUE) or
#'   original data (FALSE). Default is TRUE.
#'
#' @return A data frame containing the cube data, or NULL if not loaded.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get filtered data
#' data <- get_cube_data(r)
#'
#' # Get original unfiltered data
#' data <- get_cube_data(r, filtered = FALSE)
#' }
get_cube_data <- function(r, filtered = TRUE) {
  cube <- if (filtered) get_filtered_data_cube(r) else get_data_cube(r)
  
  if (is.null(cube)) {
    return(NULL)
  }
  
  cube$data
}


#' Set Data Cube
#'
#' @description
#' Stores a processed data cube in the reactive values store.
#' Automatically creates a copy for filtering.
#'
#' @param r ReactiveValues object containing app state.
#' @param cube Object of class 'processed_cube'.
#'
#' @return Invisible NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("data.csv")
#' set_data_cube(r, cube)
#' }
set_data_cube <- function(r, cube) {
  if (!inherits(r, "ReactiveValues")) {
    stop("r must be a ReactiveValues object")
  }
  
  if (!inherits(cube, "processed_cube")) {
    stop("cube must be a processed_cube object")
  }
  
  r$dataCube <- cube
  r$dataCube1 <- cube  # Create copy for filtering
  
  invisible(NULL)
}


#' Update Filtered Data
#'
#' @description
#' Updates the filtered data cube with new filtered data.
#'
#' @param r ReactiveValues object containing app state.
#' @param filtered_data Data frame containing filtered data.
#'
#' @return Invisible NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # After filtering
#' filtered <- filter_by_taxonomy(r$dataCube, families = "Pinaceae")
#' update_filtered_data(r, filtered$data)
#' }
update_filtered_data <- function(r, filtered_data) {
  if (!inherits(r, "ReactiveValues")) {
    stop("r must be a ReactiveValues object")
  }
  
  if (!is.data.frame(filtered_data)) {
    stop("filtered_data must be a data frame")
  }
  
  if (is.null(r$dataCube1)) {
    stop("Original data cube not set. Call set_data_cube() first.")
  }
  
  r$dataCube1$data <- filtered_data
  
  invisible(NULL)
}


#' Clear Data Cubes
#'
#' @description
#' Clears all data from the reactive values store. Useful for reset operations
#' or when data loading fails.
#'
#' @param r ReactiveValues object containing app state.
#' @param keep_original Logical. Whether to keep the original data cube
#'   and only clear filtered data. Default is FALSE (clear everything).
#'
#' @return Invisible NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear everything
#' clear_data_cubes(r)
#'
#' # Clear only filtered data, keep original
#' clear_data_cubes(r, keep_original = TRUE)
#' }
clear_data_cubes <- function(r, keep_original = FALSE) {
  if (!inherits(r, "ReactiveValues")) {
    stop("r must be a ReactiveValues object")
  }
  
  r$dataCube1 <- NULL
  
  if (!keep_original) {
    r$dataCube <- NULL
  }
  
  invisible(NULL)
}


#' Check if Data is Loaded
#'
#' @description
#' Checks whether a data cube has been loaded.
#'
#' @param r ReactiveValues object containing app state.
#' @param check_filtered Logical. Whether to check for filtered data (TRUE)
#'   or just original data (FALSE). Default is FALSE.
#'
#' @return Logical indicating whether data is available.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' if (is_data_loaded(r)) {
#'   # Perform operations requiring data
#' }
#' }
is_data_loaded <- function(r, check_filtered = FALSE) {
  if (!inherits(r, "ReactiveValues")) {
    return(FALSE)
  }
  
  if (check_filtered) {
    return(!is.null(r$dataCube1))
  } else {
    return(!is.null(r$dataCube))
  }
}


#' Get Data Loading Status
#'
#' @description
#' Returns detailed information about the current data loading state.
#'
#' @param r ReactiveValues object containing app state.
#'
#' @return A list containing:
#'   \itemize{
#'     \item loaded: Logical indicating if data is loaded
#'     \item has_filtered: Logical indicating if filtered data exists
#'     \item n_rows_original: Number of rows in original data
#'     \item n_rows_filtered: Number of rows in filtered data
#'     \item is_filtered: Logical indicating if filtering has been applied
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' status <- get_data_status(r)
#' if (status$loaded) {
#'   print(paste("Loaded", status$n_rows_original, "rows"))
#' }
#' }
get_data_status <- function(r) {
  if (!inherits(r, "ReactiveValues")) {
    stop("r must be a ReactiveValues object")
  }
  
  loaded <- !is.null(r$dataCube)
  has_filtered <- !is.null(r$dataCube1)
  
  n_rows_original <- if (loaded) nrow(r$dataCube$data) else 0
  n_rows_filtered <- if (has_filtered) nrow(r$dataCube1$data) else 0
  
  list(
    loaded = loaded,
    has_filtered = has_filtered,
    n_rows_original = n_rows_original,
    n_rows_filtered = n_rows_filtered,
    is_filtered = has_filtered && n_rows_filtered != n_rows_original
  )
}


#' Initialize Data State
#'
#' @description
#' Initializes the reactive values for data storage. Should be called once
#' at app startup.
#'
#' @return A ReactiveValues object with initialized data fields.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In server function
#' r <- initialize_data_state()
#' }
initialize_data_state <- function() {
  shiny::reactiveValues(
    dataCube = NULL,    # Original loaded data
    dataCube1 = NULL    # Filtered/processed data
  )
}


# Helper function for null default values
`%||%` <- function(x, y) if (is.null(x)) y else x
