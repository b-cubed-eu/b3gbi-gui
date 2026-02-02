#' Load Shapefile from ZIP Archive
#'
#' @description
#' Extracts and loads a shapefile from a ZIP archive. This function handles
#' the common workflow of uploading shapefiles as ZIP archives in Shiny apps.
#'
#' @param zip_path Character string. Path to the ZIP file containing the shapefile.
#' @param temp_dir Character string. Temporary directory for extraction.
#'   Default is \code{tempdir()}.
#'
#' @return Character string path to the extracted .shp file, or NULL if extraction fails.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load shapefile from uploaded ZIP
#' shp_path <- load_shapefile_from_zip("/path/to/upload.zip")
#' if (!is.null(shp_path)) {
#'   shp_data <- sf::read_sf(shp_path)
#' }
#' }
#'
#' @importFrom utils unzip
#'
load_shapefile_from_zip <- function(zip_path, temp_dir = tempdir()) {
  # Input validation
  if (!is.character(zip_path) || length(zip_path) != 1) {
    stop("zip_path must be a single character string")
  }
  
  if (!file.exists(zip_path)) {
    stop("ZIP file not found: ", zip_path)
  }
  
  if (!grepl("\\.zip$", zip_path, ignore.case = TRUE)) {
    warning("File does not have .zip extension: ", zip_path)
  }
  
  # Extract ZIP file
  tryCatch({
    unzip(zip_path, exdir = temp_dir, overwrite = TRUE)
  }, error = function(e) {
    stop("Failed to extract ZIP file: ", conditionMessage(e),
         "\nCommon causes:",
         "\n  - File is corrupted",
         "\n  - File is not a valid ZIP archive",
         "\n  - Insufficient disk space")
  })
  
  # Find .shp file in extracted contents
  shp_files <- list.files(
    temp_dir,
    pattern = "\\.shp$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(shp_files) == 0) {
    stop("No .shp file found in ZIP archive.",
         "\nExpected shapefile components: .shp, .shx, .dbf")
  }
  
  if (length(shp_files) > 1) {
    warning("Multiple .shp files found. Using first one: ", basename(shp_files[1]))
  }
  
  shp_files[1]
}


#' Validate Shapefile Components
#'
#' @description
#' Checks that all required shapefile components are present.
#' A valid shapefile requires at minimum: .shp, .shx, and .dbf files.
#'
#' @param shp_path Character string. Path to the .shp file.
#'
#' @return Invisible TRUE if all components are present, otherwise throws error.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' shp_path <- load_shapefile_from_zip("upload.zip")
#' validate_shapefile_components(shp_path)
#' }
validate_shapefile_components <- function(shp_path) {
  if (!is.character(shp_path) || length(shp_path) != 1) {
    stop("shp_path must be a single character string")
  }
  
  if (!file.exists(shp_path)) {
    stop("Shapefile not found: ", shp_path)
  }
  
  # Get base name without extension
  base_path <- sub("\\.shp$", "", shp_path, ignore.case = TRUE)
  
  # Required components
  required_exts <- c(".shp", ".shx", ".dbf")
  missing <- character(0)
  
  for (ext in required_exts) {
    component <- paste0(base_path, ext)
    if (!file.exists(component)) {
      missing <- c(missing, ext)
    }
  }
  
  if (length(missing) > 0) {
    stop("Shapefile missing required components: ", paste(missing, collapse = ", "),
         "\nRequired: .shp, .shx, .dbf",
         "\nOptional but recommended: .prj (projection)")
  }
  
  # Warn about missing projection
  prj_file <- paste0(base_path, ".prj")
  if (!file.exists(prj_file)) {
    warning("Shapefile missing .prj file. CRS (coordinate system) information may be unavailable.")
  }
  
  invisible(TRUE)
}


#' Read Shapefile with Error Handling
#'
#' @description
#' Reads a shapefile into an sf object with comprehensive error handling
#' and validation.
#'
#' @param shp_path Character string. Path to the .shp file.
#' @param crs Character string or numeric. Optional CRS to assign if missing.
#'   Default is NULL (use shapefile's CRS if available).
#'
#' @return An sf object containing the shapefile data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' shp_path <- load_shapefile_from_zip("upload.zip")
#' shp_data <- read_shapefile_safe(shp_path)
#' 
#' # With explicit CRS
#' shp_data <- read_shapefile_safe(shp_path, crs = "EPSG:4326")
#' }
#'
#' @importFrom sf read_sf st_crs
#'
read_shapefile_safe <- function(shp_path, crs = NULL) {
  # Validate components first
  validate_shapefile_components(shp_path)
  
  # Read the shapefile
  tryCatch({
    shp_data <- sf::read_sf(shp_path)
    
    # Check if CRS is missing and one was provided
    if (!is.null(crs)) {
      current_crs <- sf::st_crs(shp_data)
      if (is.na(current_crs)) {
        sf::st_crs(shp_data) <- crs
        message("Assigned CRS to shapefile: ", crs)
      }
    }
    
    # Validate geometry
    if (all(sf::st_is_empty(shp_data$geometry))) {
      warning("Shapefile contains only empty geometries")
    }
    
    shp_data
  }, error = function(e) {
    stop("Failed to read shapefile: ", conditionMessage(e),
         "\nCommon causes:",
         "\n  - Shapefile is corrupted",
         "\n  - Unsupported geometry type",
         "\n  - Encoding issues in attribute table")
  })
}


#' Check Shapefile Validity for Spatial Operations
#'
#' @description
#' Performs additional checks to ensure a shapefile is suitable for
#' spatial intersection operations with biodiversity data.
#'
#' @param shp_data An sf object.
#' @param check_validity Logical. Whether to check and attempt to fix invalid geometries.
#'   Default is TRUE.
#'
#' @return The validated sf object, potentially with fixed geometries.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' shp_data <- read_shapefile_safe(shp_path)
#' validated_shp <- check_shapefile_validity(shp_data)
#' }
#'
#' @importFrom sf st_is_valid st_make_valid
#'
check_shapefile_validity <- function(shp_data, check_validity = TRUE) {
  if (!inherits(shp_data, "sf")) {
    stop("Input must be an sf object")
  }
  
  # Check for empty geometries
  empty_count <- sum(sf::st_is_empty(shp_data))
  if (empty_count > 0) {
    warning(sprintf("Shapefile contains %d empty geometries", empty_count))
  }
  
  # Check validity and attempt repair
  if (check_validity) {
    invalid_count <- sum(!sf::st_is_valid(shp_data))
    if (invalid_count > 0) {
      warning(sprintf("Shapefile contains %d invalid geometries. Attempting to fix...", 
                      invalid_count))
      shp_data <- sf::st_make_valid(shp_data)
      
      # Check again
      still_invalid <- sum(!sf::st_is_valid(shp_data))
      if (still_invalid > 0) {
        warning(sprintf("Could not fix %d geometries", still_invalid))
      } else {
        message("All geometries fixed successfully")
      }
    }
  }
  
  shp_data
}


#' Clean Up Temporary Shapefile Files
#'
#' @description
#' Removes temporary shapefile files extracted during the session.
#' Should be called when shapefile processing is complete or on session end.
#'
#' @param temp_dir Character string. Directory where files were extracted.
#'   Default is \code{tempdir()}.
#' @param keep_shp Character string. Optional specific .shp file to keep.
#'
#' @return Invisible NULL.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Clean all temporary shapefiles
#' cleanup_shapefiles()
#' 
#' # Clean but keep specific file
#' cleanup_shapefiles(keep_shp = "/path/to/important.shp")
#' }
cleanup_shapefiles <- function(temp_dir = tempdir(), keep_shp = NULL) {
  shp_extensions <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".sbn", ".sbx")
  
  for (ext in shp_extensions) {
    files <- list.files(temp_dir, pattern = paste0("\\", ext, "$"), 
                        full.names = TRUE, ignore.case = TRUE)
    
    for (file in files) {
      if (is.null(keep_shp) || !identical(file, keep_shp)) {
        try(unlink(file), silent = TRUE)
      }
    }
  }
  
  invisible(NULL)
}
