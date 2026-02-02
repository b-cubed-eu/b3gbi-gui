#' Load and Process GBIF Data Cube
#'
#' @description
#' Loads a data cube from a CSV file and processes it using the b3gbi package.
#' This function wraps b3gbi::process_cube() with additional validation and
#' error handling suitable for Shiny applications.
#'
#' @param file_path Character string. Path to the CSV file containing the data cube.
#' @param validate Logical. Whether to validate the data cube after loading.
#'   Default is TRUE.
#'
#' @return An object of class 'processed_cube' containing the data cube and metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load a data cube
#' cube <- load_data_cube("path/to/cube.csv")
#' 
#' # Access the data
#' head(cube$data)
#' 
#' # Access metadata
#' cube$metadata
#' }
#'
#' @seealso
#' \code{\link[b3gbi]{process_cube}} for the underlying processing function
#' \code{\link{validate_data_cube}} for validation details
#'
#' @importFrom b3gbi process_cube
#'
load_data_cube <- function(file_path, validate = TRUE) {
  # Input validation
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("file_path must be a single character string")
  }
  
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path, 
         "\nPlease check the path and try again.")
  }
  
  # Check file extension
  ext <- tolower(tools::file_ext(file_path))
  if (!ext %in% c("csv", "txt")) {
    warning("Unexpected file extension: .", ext, 
            "\nExpected .csv or .txt. Attempting to load anyway.")
  }
  
  # Load and process the cube
  tryCatch({
    cube <- b3gbi::process_cube(file_path)
    
    if (validate) {
      validate_data_cube(cube)
    }
    
    cube
  }, error = function(e) {
    stop("Failed to load data cube: ", conditionMessage(e),
         "\nCommon causes:",
         "\n  - File is corrupted or not a valid CSV",
         "\n  - Required columns are missing",
         "\n  - File is locked by another program")
  })
}


#' Validate Data Cube Structure
#'
#' @description
#' Validates that a loaded data cube has the expected structure and required columns.
#' This function checks for the presence of essential columns needed by the app.
#'
#' @param cube Object of class 'processed_cube' from b3gbi::process_cube()
#'
#' @return Invisible TRUE if validation passes, otherwise throws an error.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' cube <- b3gbi::process_cube("cube.csv")
#' validate_data_cube(cube)  # Returns TRUE if valid
#' }
validate_data_cube <- function(cube) {
  # Check class
  if (!inherits(cube, "processed_cube")) {
    stop("Invalid data cube: expected class 'processed_cube', got '", 
         class(cube)[1], "'")
  }
  
  # Check data exists
  if (is.null(cube$data) || !is.data.frame(cube$data)) {
    stop("Invalid data cube: $data is missing or not a data frame")
  }
  
  # Required columns for basic functionality
  required_cols <- c("scientificName", "family", "year")
  missing_cols <- setdiff(required_cols, names(cube$data))
  
  if (length(missing_cols) > 0) {
    stop("Data cube missing required columns: ", 
         paste(missing_cols, collapse = ", "),
         "\nExpected columns: ", paste(required_cols, collapse = ", "))
  }
  
  # Check for empty data
  if (nrow(cube$data) == 0) {
    warning("Data cube contains 0 rows")
  }
  
  # Check resolution info exists
  if (is.null(cube$resolutions)) {
    warning("Data cube missing resolution information")
  }
  
  invisible(TRUE)
}


#' Get Data Cube Metadata Summary
#'
#' @description
#' Extracts and summarizes key metadata from a processed data cube for display
#' in the UI or logging.
#'
#' @param cube Object of class 'processed_cube'
#'
#' @return A list containing summary statistics:
#'   \itemize{
#'     \item n_rows: Number of observations
#'     \item n_species: Number of unique species
#'     \item n_families: Number of unique families
#'     \item year_range: Vector of min and max years
#'     \item resolution: Grid resolution
#'     \item columns: Names of all columns in the data
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("cube.csv")
#' meta <- get_cube_metadata(cube)
#' print(meta)
#' }
get_cube_metadata <- function(cube) {
  if (!inherits(cube, "processed_cube") || is.null(cube$data)) {
    stop("Invalid data cube provided")
  }
  
  data <- cube$data
  
  list(
    n_rows = nrow(data),
    n_species = length(unique(data$scientificName)),
    n_families = length(unique(data$family)),
    year_range = if ("year" %in% names(data)) {
      range(data$year, na.rm = TRUE)
    } else {
      c(NA, NA)
    },
    resolution = cube$resolutions %||% NA,
    columns = names(data)
  )
}


#' Check File Size and Warn if Large
#'
#' @description
#' Checks if a file exceeds a size threshold and warns the user if it might
#' cause performance issues.
#'
#' @param file_path Character string. Path to the file.
#' @param threshold_mb Numeric. Threshold in MB. Default is 100.
#'
#' @return Invisible file size in bytes.
#'
#' @keywords internal
#'
check_file_size <- function(file_path, threshold_mb = 100) {
  if (!file.exists(file_path)) {
    return(invisible(0))
  }
  
  size_bytes <- file.size(file_path)
  size_mb <- size_bytes / (1024^2)
  
  if (size_mb > threshold_mb) {
    warning(sprintf("Large file detected (%.1f MB). This may take longer to process.",
                    size_mb))
  }
  
  invisible(size_bytes)
}


# Helper function for null default values
`%||%` <- function(x, y) if (is.null(x)) y else x
