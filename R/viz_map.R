#' Map Visualization Module for B3GBI Shiny App
#'
#' @description
#' Functions for creating and customizing biodiversity indicator maps using b3gbi.
#' Handles map generation, parameter processing, and plotting.
#'
#' @name viz_map
NULL

#' Calculate Indicator Map
#'
#' @description
#' Calculates a biodiversity indicator map from data cube using specified parameters.
#' Wraps b3gbi indicator functions with input validation and error handling.
#'
#' @param data Object of class 'processed_cube'.
#' @param indicator Character. Indicator name to calculate.
#' @param cell_size Numeric. Spatial resolution in km or degrees.
#' @param spatiallevel Character. Spatial level ("cube", "world", "continent", etc.).
#' @param first_year Numeric. Start year for analysis.
#' @param last_year Numeric. End year for analysis.
#' @param countrytype Character. Country type ("countries", "map_units", etc.).
#' @param mapres Character. Map resolution ("small", "medium", "large").
#' @param region Character vector. Selected region(s).
#' @param output_crs Character. Output CRS (e.g., "EPSG: 4326") or NULL.
#' @param shapefile_path Character. Path to shapefile or NULL.
#' @param invert Logical. Whether to invert shapefile selection.
#' @param include_land Logical. Whether to include land areas.
#' @param include_ocean Logical. Whether to include ocean areas.
#' @param species Character. Species name for species-specific indicators.
#'
#' @return Indicator map object from b3gbi.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("data.csv")
#' map <- calc_indicator_map(
#'   data = cube,
#'   indicator = "obs_richness",
#'   cell_size = 10,
#'   spatiallevel = "country",
#'   first_year = 2000,
#'   last_year = 2020,
#'   countrytype = "countries",
#'   mapres = "medium",
#'   region = "Germany"
#' )
#' }
calc_indicator_map <- function(data, indicator, cell_size, spatiallevel,
                                first_year, last_year, countrytype, mapres,
                                region = NULL, output_crs = NULL,
                                shapefile_path = NULL, invert = FALSE,
                                include_land = TRUE, include_ocean = TRUE,
                                species = NULL) {
  # Input validation
  if (!inherits(data, "processed_cube")) {
    stop("Invalid data cube provided")
  }

  # Get the appropriate indicator function
  indicator_fn <- get_indicator_function(indicator, "map")

  if (is.null(indicator_fn)) {
    stop(paste("Indicator", indicator, "is not available for mapping"))
  }

  # Build parameter list
  params <- list(
    data = data,
    cell_size = cell_size,
    level = spatiallevel,
    first_year = first_year,
    last_year = last_year,
    ne_type = countrytype,
    ne_scale = mapres,
    region = region,
    output_crs = output_crs,
    shapefile_crs = NULL,
    shapefile_path = shapefile_path,
    invert = invert,
    include_land = include_land,
    include_ocean = include_ocean
  )

  # Add species if provided
  if (!is.null(species)) {
    params$species <- species
  }

  # Call the indicator function
  tryCatch({
    do.call(indicator_fn, params)
  }, error = function(e) {
    stop(paste("Failed to calculate indicator map:", conditionMessage(e)))
  })
}


#' Get Indicator Function
#'
#' @description
#' Returns the appropriate b3gbi function for calculating an indicator.
#'
#' @param indicator Character. Indicator name.
#' @param type Character. Type of indicator: "map" or "ts" (time series).
#'
#' @return Function object or NULL if not available.
#'
#' @keywords internal
get_indicator_function <- function(indicator, type = "map") {
  # Map of indicator display names to function names in b3gbi
  # Store as strings to avoid evaluation errors during sourcing
  indicator_map <- list(
    map = list(
      "Observed Species Richness" = "obs_richness_map",
      "Total Occurrences" = "total_occ_map",
      "Pielou's Evenness" = "pielou_evenness_map",
      "Williams' Evenness" = "williams_evenness_map",
      "Cumulative Species Richness" = "cum_richness_map",
      "Density of Occurrences" = "occ_density_map",
      "Abundance-Based Rarity" = "ab_rarity_map",
      "Area-Based Rarity" = "area_rarity_map",
      "Mean Year of Occurrence" = "newness_map",
      "Taxonomic Distinctness" = "tax_distinct_map",
      "Species Richness (Estimated by Coverage-Based Rarefaction)" = "hill0_map",
      "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)" = "hill1_map",
      "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)" = "hill2_map",
      "Species Occurrences" = "spec_occ_map",
      "Species Range" = "spec_range_map",
      "Occupancy Turnover" = "occ_turnover_map"
    ),
    ts = list(
      "Observed Species Richness" = "obs_richness_ts",
      "Total Occurrences" = "total_occ_ts",
      "Pielou's Evenness" = "pielou_evenness_ts",
      "Williams' Evenness" = "williams_evenness_ts",
      "Cumulative Species Richness" = "cum_richness_ts",
      "Density of Occurrences" = "occ_density_ts",
      "Abundance-Based Rarity" = "ab_rarity_ts",
      "Area-Based Rarity" = "area_rarity_ts",
      "Mean Year of Occurrence" = "newness_ts",
      "Taxonomic Distinctness" = "tax_distinct_ts",
      "Species Richness (Estimated by Coverage-Based Rarefaction)" = "hill0_ts",
      "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)" = "hill1_ts",
      "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)" = "hill2_ts",
      "Species Occurrences" = "spec_occ_ts",
      "Species Range" = "spec_range_ts",
      "Occupancy Turnover" = "occ_turnover_ts"
    )
  )

  fn_name <- indicator_map[[type]][[indicator]]

  if (is.null(fn_name)) {
    fn_name <- paste0(indicator, "_", type)
  }

  # Look up the function in b3gbi namespace
  fn <- tryCatch({
    get(fn_name, envir = asNamespace("b3gbi"))
  }, error = function(e) {
    NULL
  })

  fn
}


#' Create Map Plot
#'
#' @description
#' Creates a ggplot map from calculated indicator result.
#'
#' @param indicator_map Result from calc_indicator_map().
#' @param params List of plotting parameters from create_map_plot_params().
#'
#' @return ggplot object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' map_result <- calc_indicator_map(...)
#' params <- create_map_plot_params(map_result, input, parsed)
#' plot <- create_map_plot(map_result, params)
#' }
create_map_plot <- function(indicator_map, params) {
  if (is.null(indicator_map)) {
    stop("No indicator map to plot")
  }
  
  tryCatch({
    do.call(plot, c(list(x = indicator_map), params))
  }, error = function(e) {
    stop(paste("Failed to create map plot:", conditionMessage(e)))
  })
}


#' Validate Map Inputs
#'
#' @description
#' Validates inputs specific to map generation before calculation.
#'
#' @param indicator Character. Selected indicator.
#' @param species Character vector. Selected species.
#' @param mapres Character. Map resolution.
#'
#' @return List with valid (logical) and message (character).
#'
#' @export
validate_map_inputs <- function(indicator, species = NULL, mapres = NULL) {
  result <- list(valid = TRUE, message = "Valid inputs")
  
  # Check species-specific indicators
  species_specific <- c("Species Occurrences", "Species Range")
  
  if (indicator %in% species_specific) {
    if (is.null(species) || length(species) == 0) {
      result$valid <- FALSE
      result$message <- paste0(
        "Please select a single species using the 'Subset by family' ",
        "and 'Subset by species' filters."
      )
      return(result)
    }
    
    if (length(species) > 1) {
      result$valid <- FALSE
      result$message <- "Please select only one species for this indicator"
      return(result)
    }
  }
  
  # Validate map resolution
  if (!is.null(mapres) && !mapres %in% c("10", "50", "110")) {
    result$valid <- FALSE
    result$message <- "Map resolution is not properly selected"
    return(result)
  }
  
  result
}


#' Prepare CRS String
#'
#' @description
#' Converts EPSG code input to proper CRS string.
#'
#' @param epsg_code Character or numeric. EPSG code.
#' @param use_custom Logical. Whether to use custom CRS.
#'
#' @return Character CRS string or NULL.
#'
#' @export
prepare_crs <- function(epsg_code, use_custom = FALSE) {
  if (!use_custom || is.null(epsg_code) || epsg_code == "") {
    return(NULL)
  }
  
  code <- suppressWarnings(as.numeric(epsg_code))
  
  if (is.na(code)) {
    warning("Invalid EPSG code provided")
    return(NULL)
  }
  
  paste0("EPSG:", code)
}


#' Check Indicator Availability
#'
#' @description
#' Checks if an indicator is available for a given visualization type.
#'
#' @param indicator Character. Indicator name.
#' @param type Character. "map" or "ts".
#'
#' @return Logical.
#'
#' @export
is_indicator_available <- function(indicator, type = "map") {
  !is.null(get_indicator_function(indicator, type))
}


#' Get Available Indicators
#'
#' @description
#' Returns list of available indicators by type.
#'
#' @param type Character. "map" or "ts".
#'
#' @return Character vector of indicator names.
#'
#' @export
get_available_indicators <- function(type = "map") {
  indicators <- list(
    map = c(
      "Observed Richness",
      "Total Occurrences",
      "Newness",
      "Evenness",
      "Spec. Profile",
      "Species Occurrences",
      "Species Range",
      "Hill Diversity",
      "Hill Evenness",
      "Density",
      "Whorl"
    ),
    ts = c(
      "Observed Richness",
      "Total Occurrences",
      "Newness",
      "Evenness",
      "Spec. Profile",
      "Species Occurrences",
      "Hill Diversity",
      "Hill Evenness",
      "Density",
      "Whorl"
    )
  )
  
  indicators[[type]]
}
