#' Region Selection Logic for B3GBI Shiny App
#'
#' @description
#' Provides lookup tables and functions for determining available region options
#' based on spatial level, country type, and map resolution selections.
#'
#' @name region_logic
NULL

# Lookup table for spatial data sources
# Maps country type and resolution to the appropriate data object name
.SPATIAL_DATA_MAP <- list(
  countries = list(
    "10" = "rne_countries_10",
    "50" = "rne_countries_50",
    "110" = "rne_countries_110"
  ),
  map_units = list(
    "10" = "rne_mapunits_10",
    "50" = "rne_mapunits_50",
    "110" = "rne_mapunits_110"
  ),
  sovereignty = list(
    "10" = "rne_sovereignties_10",
    "50" = "rne_sovereignties_50",
    "110" = "rne_sovereignties_110"
  ),
  tiny_countries = list(
    "50" = "rne_tiny_countries_50",
    "110" = "rne_tiny_countries_110"
  )
)

# Valid fields for each spatial level
.SPATIAL_FIELDS <- list(
  continent = NULL,  # Uses continents vector directly
  country = "ADMIN",
  sovereignty = "SOVEREIGNT",
  geounit = "GEOUNIT"
)

# Available spatial levels
.SPATIAL_LEVELS <- c("cube", "world", "continent", "country", "sovereignty", "geounit")

#' Get Country Options
#'
#' @description
#' Returns available country/region options based on country type, map resolution,
#' and field name. This is a core function for populating the region dropdown.
#'
#' @param countrytype Character. Type of country data: "countries",
#'   "map_units", "sovereignty", or "tiny_countries".
#' @param mapres Character or numeric. Map resolution: "10", "50", or "110".
#' @param field Character. Field to extract: "ADMIN", "SOVEREIGNT", or "GEOUNIT".
#' @param data_env Environment. Environment containing the rnaturalearth data objects.
#'   Defaults to the global environment.
#'
#' @return Character vector of region names, or NULL if invalid combination.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get country names at 50m resolution
#' options <- get_country_options("countries", "50", "ADMIN")
#'
#' # Get sovereignty names at 110m resolution
#' options <- get_country_options("sovereignty", "110", "SOVEREIGNT")
#'
#' # Invalid combination returns NULL
#' options <- get_country_options("tiny_countries", "10", "ADMIN")  # NULL
#' }
get_country_options <- function(countrytype, mapres, field,
                                 data_env = .GlobalEnv) {
  # Input validation
  if (!is.character(countrytype) || length(countrytype) != 1) {
    warning("countrytype must be a single character string")
    return(NULL)
  }
  
  if (!is.character(mapres) && !is.numeric(mapres)) {
    warning("mapres must be character or numeric")
    return(NULL)
  }
  
  mapres <- as.character(mapres)
  
  # Look up data source
  data_sources <- .SPATIAL_DATA_MAP[[countrytype]]
  if (is.null(data_sources)) {
    warning("Invalid countrytype: ", countrytype)
    return(NULL)
  }
  
  data_obj_name <- data_sources[[mapres]]
  if (is.null(data_obj_name)) {
    # Invalid resolution for this country type (e.g., tiny_countries at 10m)
    return(NULL)
  }
  
  # Get data object from environment
  if (!exists(data_obj_name, envir = data_env)) {
    warning("Spatial data not found: ", data_obj_name)
    return(NULL)
  }
  
  data_obj <- get(data_obj_name, envir = data_env)
  
  # Extract field
  if (!field %in% names(data_obj)) {
    warning("Field not found in spatial data: ", field)
    return(NULL)
  }
  
  result <- data_obj[[field]]
  
  # Remove NAs and sort
  result <- result[!is.na(result)]
  sort(unique(result))
}


#' Get Resolution Choices
#'
#' @description
#' Returns available map resolution options based on country type.
#' Tiny countries don't have 10m resolution data.
#'
#' @param countrytype Character. Type of country data.
#'
#' @return Named character vector of resolution choices.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # For regular countries - all 3 resolutions
#' choices <- get_resolution_choices("countries")
#' # Returns: c("Small (110m)" = "110", "Medium (50m)" = "50", "Large (10m)" = "10")
#'
#' # For tiny countries - only 50m and 110m
#' choices <- get_resolution_choices("tiny_countries")
#' # Returns: c("Small (110m)" = "110", "Medium (50m)" = "50")
#' }
get_resolution_choices <- function(countrytype) {
  if (countrytype == "tiny_countries") {
    c(
      "Small (110m)" = "110",
      "Medium (50m)" = "50"
    )
  } else {
    c(
      "Small (110m)" = "110",
      "Medium (50m)" = "50",
      "Large (10m)" = "10"
    )
  }
}


#' Get Country Type Choices
#'
#' @description
#' Returns available country type options based on spatial level and map resolution.
#' Some combinations don't support all country types.
#'
#' @param spatiallevel Character. Spatial level: "continent", "world", "cube",
#'   "country", "sovereignty", or "geounit".
#' @param mapres Character or numeric. Map resolution: "10", "50", or "110".
#'
#' @return Character vector of country types, or NULL if spatial level doesn't
#'   use country types.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # For country level at 10m resolution
#' choices <- get_country_type_choices("country", "10")
#' # Returns: c("countries", "map_units", "sovereignty")
#'
#' # For country level at 50m resolution
#' choices <- get_country_type_choices("country", "50")
#' # Returns: c("countries", "map_units", "sovereignty", "tiny_countries")
#'
#' # For continent level - returns NULL (no country types)
#' choices <- get_country_type_choices("continent", "50")
#' # Returns: NULL
#' }
get_country_type_choices <- function(spatiallevel, mapres) {
  # Input validation
  if (!is.character(spatiallevel) || length(spatiallevel) != 1) {
    warning("spatiallevel must be a single character string")
    return(NULL)
  }
  
  mapres <- as.character(mapres)
  
  # Spatial levels that don't use country types
  if (spatiallevel %in% c("continent", "world", "cube")) {
    return(NULL)
  }
  
  # Valid country types depend on resolution
  if (mapres == "10") {
    # 10m resolution doesn't have tiny_countries
    c("countries", "map_units", "sovereignty")
  } else {
    # 50m and 110m have all country types
    c("countries", "map_units", "sovereignty", "tiny_countries")
  }
}


#' Get Region Options
#'
#' @description
#' Main function for determining available region options based on all spatial
#' parameters. Handles the complex conditional logic for region selection.
#'
#' @param spatiallevel Character. Spatial level selection.
#' @param countrytype Character. Country type selection (if applicable).
#' @param mapres Character or numeric. Map resolution.
#' @param continents Character vector. Pre-loaded continents data.
#' @param data_env Environment. Environment containing spatial data objects.
#'
#' @return Character vector of region options, or NULL if not applicable.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get continent options
#' options <- get_region_options("continent", NULL, NULL, continents_data)
#'
#' # Get country options
#' options <- get_region_options("country", "countries", "50",
#'                               continents_data, .GlobalEnv)
#'
#' # Cube level - returns NULL
#' options <- get_region_options("cube", NULL, NULL, continents_data)
#' # Returns: NULL
#' }
get_region_options <- function(spatiallevel, countrytype = NULL,
                                mapres = NULL, continents = NULL,
                                data_env = .GlobalEnv) {
  # Input validation
  if (!is.character(spatiallevel) || length(spatiallevel) != 1) {
    warning("spatiallevel must be a single character string")
    return(NULL)
  }
  
  switch(spatiallevel,
    "continent" = {
      if (is.null(continents)) {
        warning("continents data not provided")
        return(NULL)
      }
      sort(unique(continents))
    },
    "country" = {
      if (is.null(countrytype) || is.null(mapres)) {
        warning("countrytype and mapres required for country level")
        return(NULL)
      }
      get_country_options(countrytype, mapres, "ADMIN", data_env)
    },
    "sovereignty" = {
      if (is.null(countrytype) || is.null(mapres)) {
        warning("countrytype and mapres required for sovereignty level")
        return(NULL)
      }
      get_country_options(countrytype, mapres, "SOVEREIGNT", data_env)
    },
    "geounit" = {
      if (is.null(countrytype) || is.null(mapres)) {
        warning("countrytype and mapres required for geounit level")
        return(NULL)
      }
      get_country_options(countrytype, mapres, "GEOUNIT", data_env)
    },
    # Default for "cube", "world", or unknown
    NULL
  )
}


#' Validate Spatial Selection
#'
#' @description
#' Validates that a spatial level, country type, and resolution combination
#' is valid. Returns diagnostic information about why a combination might
#' be invalid.
#'
#' @param spatiallevel Character. Spatial level.
#' @param countrytype Character. Country type.
#' @param mapres Character or numeric. Map resolution.
#'
#' @return List with validation results:
#'   \itemize{
#'     \item valid: Logical indicating if combination is valid
#'     \item message: Character string with validation message
#'     \item requires_countrytype: Logical indicating if countrytype is needed
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Valid combination
#' result <- validate_spatial_selection("country", "countries", "50")
#' # Returns: list(valid = TRUE, ...)
#'
#' # Invalid - tiny_countries at 10m
#' result <- validate_spatial_selection("country", "tiny_countries", "10")
#' # Returns: list(valid = FALSE, message = "tiny_countries not available at 10m resolution")
#' }
validate_spatial_selection <- function(spatiallevel, countrytype = NULL,
                                       mapres = NULL) {
  result <- list(
    valid = TRUE,
    message = "Valid selection",
    requires_countrytype = FALSE
  )
  
  # Check if countrytype is required
  if (spatiallevel %in% c("country", "sovereignty", "geounit")) {
    result$requires_countrytype <- TRUE
    
    if (is.null(countrytype) || is.null(mapres)) {
      result$valid <- FALSE
      result$message <- "countrytype and mapres are required for this spatial level"
      return(result)
    }
    
    # Check if tiny_countries is valid at this resolution
    if (countrytype == "tiny_countries" && mapres == "10") {
      result$valid <- FALSE
      result$message <- "tiny_countries not available at 10m resolution"
      return(result)
    }
    
    # Check if country type is valid
    valid_types <- get_country_type_choices(spatiallevel, mapres)
    if (!is.null(valid_types) && !countrytype %in% valid_types) {
      result$valid <- FALSE
      result$message <- paste("Invalid countrytype for", spatiallevel, "at", mapres, "resolution")
      return(result)
    }
  }
  
  result
}


#' Get Default Resolution
#'
#' @description
#' Returns the default resolution for a given country type.
#'
#' @param countrytype Character. Type of country data.
#'
#' @return Character. Default resolution ("50" for most, "110" for tiny_countries).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_default_resolution("countries")  # Returns: "50"
#' get_default_resolution("tiny_countries")  # Returns: "110"
#' }
get_default_resolution <- function(countrytype) {
  if (countrytype == "tiny_countries") {
    "110"
  } else {
    "50"
  }
}


#' Get Spatial Level Description
#'
#' @description
#' Returns a human-readable description of a spatial level.
#'
#' @param spatiallevel Character. Spatial level.
#'
#' @return Character. Description of the spatial level.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_spatial_level_description("country")  # "Countries"
#' get_spatial_level_description("continent")  # "Continents"
#' }
get_spatial_level_description <- function(spatiallevel) {
  descriptions <- c(
    cube = "Full data cube",
    world = "World",
    continent = "Continents",
    country = "Countries",
    sovereignty = "Sovereignties",
    geounit = "Geographic units"
  )
  
  descriptions[[spatiallevel]] %||% spatiallevel
}


# Helper function for null default values
`%||%` <- function(x, y) if (is.null(x)) y else x
