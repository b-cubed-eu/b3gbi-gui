#' Map Visualization Configuration and Utilities
#'
#' @description
#' Provides configuration objects, validation functions, and utilities for
#' creating biodiversity indicator maps with b3gbi.
#'
#' @name viz_map_config
NULL

# Map resolution mapping
MAP_RESOLUTION_MAP <- c(
  "110" = "small",
  "50" = "medium", 
  "10" = "large"
)

# Default color schemes
DEFAULT_COLORS <- list(
  ocean = "#92c5f0",
  land = "grey85",
  grid = "black"
)

# Font size validation ranges
FONT_SIZE_RANGES <- list(
  title = c(min = 8, max = 40),
  subtitle = c(min = 6, max = 30),
  caption = c(min = 4, max = 20),
  wrap_length = c(min = 20, max = 200),
  legend_wrap = c(min = 10, max = 100),
  axis = c(min = 0, max = 30)
)

#' Validate Font Size
#'
#' @description
#' Validates and clamps font size values to acceptable ranges.
#'
#' @param size Numeric. Input font size.
#' @param min_size Numeric. Minimum allowed size.
#' @param max_size Numeric. Maximum allowed size.
#' @param name Character. Name of the parameter for error messages.
#'
#' @return Numeric. Validated font size (clamped to range).
#'
#' @export
validate_font_size <- function(size, min_size, max_size, name = "Font size") {
  if (is.null(size) || !is.numeric(size) || length(size) != 1) {
    warning(paste(name, "must be a single numeric value. Using default."))
    return(median(c(min_size, max_size)))
  }
  
  if (size < min_size) {
    warning(paste(name, "too small. Using minimum:", min_size))
    return(min_size)
  }
  
  if (size > max_size) {
    warning(paste(name, "too large. Using maximum:", max_size))
    return(max_size)
  }
  
  size
}


#' Parse Coordinate Limits
#'
#' @description
#' Parses text input for coordinate limits and validates them.
#'
#' @param min_val Character or numeric. Minimum coordinate value.
#' @param max_val Character or numeric. Maximum coordinate value.
#' @param axis Character. Axis name ("X" or "Y") for error messages.
#'
#' @return List with xlims or ylims vector, or NULL if invalid/empty.
#'
#' @export
parse_coordinates <- function(min_val, max_val, axis = "X") {
  # Handle empty inputs
  if ((is.null(min_val) || min_val == "") && 
      (is.null(max_val) || max_val == "")) {
    return(NULL)
  }
  
  # Convert to numeric
  min_num <- suppressWarnings(as.numeric(min_val))
  max_num <- suppressWarnings(as.numeric(max_val))
  
  # Check for conversion errors
  if (!is.null(min_val) && min_val != "" && is.na(min_num)) {
    warning(paste("Invalid", axis, "minimum coordinate:", min_val))
    return(NULL)
  }
  
  if (!is.null(max_val) && max_val != "" && is.na(max_num)) {
    warning(paste("Invalid", axis, "maximum coordinate:", max_val))
    return(NULL)
  }
  
  # Build limits vector
  limits <- c(
    ifelse(is.na(min_num), NA, min_num),
    ifelse(is.na(max_num), NA, max_num)
  )
  
  # Validate range
  if (!any(is.na(limits)) && limits[1] >= limits[2]) {
    warning(paste(axis, "minimum must be less than maximum"))
    return(NULL)
  }
  
  # Return NULL if both are NA
  if (all(is.na(limits))) {
    return(NULL)
  }
  
  limits
}


#' Parse Legend Limits
#'
#' @description
#' Parses text input for legend scale limits.
#'
#' @param min_val Character or numeric. Minimum legend value.
#' @param max_val Character or numeric. Maximum legend value.
#'
#' @return Numeric vector of length 2, or NULL if invalid.
#'
#' @export
parse_legend_limits <- function(min_val, max_val) {
  # Handle empty inputs
  if ((is.null(min_val) || min_val == "") && 
      (is.null(max_val) || max_val == "")) {
    return(NULL)
  }
  
  # Convert to numeric
  min_num <- suppressWarnings(as.numeric(min_val))
  max_num <- suppressWarnings(as.numeric(max_val))
  
  # Check for valid conversion
  if ((!is.null(min_val) && min_val != "" && is.na(min_num)) ||
      (!is.null(max_val) && max_val != "" && is.na(max_num))) {
    warning("Invalid legend limits provided")
    return(NULL)
  }
  
  # If only one provided, return NULL (need both)
  if (xor(is.na(min_num), is.na(max_num))) {
    warning("Both legend min and max must be provided")
    return(NULL)
  }
  
  # Validate range
  if (min_num >= max_num) {
    warning("Legend minimum must be less than maximum")
    return(NULL)
  }
  
  c(min_num, max_num)
}


#' Parse Break Points from Text Input
#'
#' @description
#' Converts comma-separated text into numeric break points.
#'
#' @param input_text Character. Comma-separated numeric values.
#' @param sort Logical. Whether to sort the breaks. Default is TRUE.
#'
#' @return Numeric vector of break points, or NULL if empty/invalid.
#'
#' @export
parse_break_points <- function(input_text, sort = TRUE) {
  if (is.null(input_text) || input_text == "") {
    return(NULL)
  }
  
  # Split by comma with optional whitespace
  parts <- unlist(strsplit(input_text, ",\\s*"))
  
  # Convert to numeric
  breaks <- suppressWarnings(as.numeric(parts))
  
  # Remove NAs
  breaks <- breaks[!is.na(breaks)]
  
  if (length(breaks) == 0) {
    return(NULL)
  }
  
  if (sort) {
    breaks <- sort(breaks)
  }
  
  breaks
}


#' Parse Labels from Text Input
#'
#' @description
#' Converts comma-separated text into character labels.
#'
#' @param input_text Character. Comma-separated labels.
#'
#' @return Character vector of labels, or NULL if empty.
#'
#' @export
parse_labels <- function(input_text) {
  if (is.null(input_text) || input_text == "") {
    return(NULL)
  }
  
  unlist(strsplit(input_text, ",\\s*"))
}


#' Validate Breaks and Labels
#'
#' @description
#' Ensures breaks and labels are compatible (same length if both provided).
#'
#' @param breaks Numeric vector. Break points.
#' @param labels Character vector. Labels for breaks.
#' @param warn Logical. Whether to issue warnings. Default is TRUE.
#'
#' @return List with validated breaks and labels (NULL if incompatible).
#'
#' @export
validate_breaks_labels <- function(breaks, labels, warn = TRUE) {
  # If either is NULL, return as-is
  if (is.null(breaks) || is.null(labels)) {
    return(list(breaks = breaks, labels = labels))
  }
  
  # Check length compatibility
  if (length(labels) != length(breaks)) {
    if (warn) {
      warning(sprintf(
        "Number of labels (%d) does not match number of breaks (%d). Using default labels.",
        length(labels), length(breaks)
      ))
    }
    return(list(breaks = breaks, labels = NULL))
  }
  
  list(breaks = breaks, labels = labels)
}


#' Get Map Resolution String
#'
#' @description
#' Converts numeric resolution code to b3gbi resolution string.
#'
#' @param mapres Character or numeric. Resolution code ("10", "50", "110").
#'
#' @return Character. Resolution string ("small", "medium", "large").
#'
#' @export
get_map_resolution <- function(mapres) {
  res <- MAP_RESOLUTION_MAP[as.character(mapres)]
  
  if (is.na(res)) {
    warning("Invalid map resolution. Using 'medium'.")
    return("medium")
  }
  
  res
}


#' Get Color Value with Default
#'
#' @description
#' Returns color value or NULL if empty, with optional default.
#'
#' @param color_input Character. Input color value.
#' @param default Character. Default color if input is empty.
#' @param allow_null Logical. Whether to return NULL for empty input.
#'
#' @return Character color value, or NULL/default.
#'
#' @export
get_color_or_default <- function(color_input, default = NULL, allow_null = TRUE) {
  if (is.null(color_input) || color_input == "") {
    if (allow_null) {
      return(default)
    } else {
      return(default %||% "grey50")
    }
  }
  
  color_input
}


#' Create Map Visualization Parameters
#'
#' @description
#' Builds a parameter list for b3gbi map plotting functions from UI inputs.
#'
#' @param indicator_result Result from indicator calculation function.
#' @param input List. Shiny input values.
#' @param parsed_inputs List. Pre-parsed inputs (breaks, labels, limits, etc.).
#'
#' @return List of parameters ready for plot() function.
#'
#' @export
create_map_plot_params <- function(indicator_result, input, parsed_inputs) {
  list(
    x = indicator_result,
    title = input$title %||% "auto",
    subtitle = if (input$subtitle == "") NULL else input$subtitle,
    caption = if (input$caption == "") NULL else input$caption,
    title_size = validate_font_size(input$title_size, 8, 40, "Title"),
    subtitle_size = validate_font_size(input$subtitle_size, 6, 30, "Subtitle"),
    caption_size = validate_font_size(input$caption_size, 4, 20, "Caption"),
    title_wrap_length = validate_font_size(input$title_wrap_length, 20, 200, "Title wrap"),
    subtitle_wrap_length = validate_font_size(input$subtitle_wrap_length, 20, 200, "Subtitle wrap"),
    caption_wrap_length = validate_font_size(input$caption_wrap_length, 20, 200, "Caption wrap"),
    title_colour = input$title_color %||% "black",
    subtitle_colour = input$subtitle_color %||% "black",
    caption_colour = input$caption_color %||% "black",
    xlims = parsed_inputs$xlims,
    ylims = parsed_inputs$ylims,
    breaks = parsed_inputs$breaks,
    labels = parsed_inputs$labels,
    trans = if (input$trans_yesno) input$trans else NULL,
    bcpower = input$bcpower,
    legend_title = if (input$legend_title == "") NULL else input$legend_title,
    legend_limits = parsed_inputs$legend_limits,
    legend_title_wrap_length = validate_font_size(input$legend_title_wrap_length, 10, 100, "Legend wrap"),
    crop_to_grid = input$crop_to_grid,
    crop_by_region = input$crop_by_region,
    ocean_fill_colour = get_color_or_default(input$ocean_fill_colour, DEFAULT_COLORS$ocean),
    land_fill_colour = get_color_or_default(input$land_fill_colour, DEFAULT_COLORS$land),
    visible_gridlines = input$gridlines,
    visible_panel_gridlines = input$panel_gridlines,
    scale = get_map_resolution(input$mapres)
  )
}


# Helper function for null default values
`%||%` <- function(x, y) if (is.null(x)) y else x
