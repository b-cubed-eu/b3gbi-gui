#' Time Series Visualization Module for B3GBI Shiny App
#'
#' @description
#' Functions for creating and customizing biodiversity indicator time series
#' plots using b3gbi.
#'
#' @name viz_timeseries
NULL

#' Calculate Indicator Time Series
#'
#' @description
#' Calculates a biodiversity indicator time series from data cube.
#'
#' @param data Object of class 'processed_cube'.
#' @param indicator Character. Indicator name to calculate.
#' @param cell_size Numeric. Spatial resolution.
#' @param spatiallevel Character. Spatial level.
#' @param first_year Numeric. Start year.
#' @param last_year Numeric. End year.
#' @param countrytype Character. Country type.
#' @param region Character vector. Selected region(s).
#' @param species Character. Species name for species-specific indicators.
#'
#' @return Indicator time series object from b3gbi.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cube <- load_data_cube("data.csv")
#' ts <- calc_indicator_ts(
#'   data = cube,
#'   indicator = "obs_richness",
#'   cell_size = 10,
#'   first_year = 2000,
#'   last_year = 2020
#' )
#' }
calc_indicator_ts <- function(data, indicator, cell_size, spatiallevel,
                               first_year, last_year, countrytype,
                               region = NULL, species = NULL) {
  # Input validation
  if (!inherits(data, "processed_cube")) {
    stop("Invalid data cube provided")
  }
  
  # Get indicator function
  indicator_fn <- get_indicator_function(indicator, "ts")
  
  if (is.null(indicator_fn)) {
    stop(paste("Indicator", indicator, "is not available for time series"))
  }
  
  # Build parameters
  params <- list(
    data = data,
    cell_size = cell_size,
    level = spatiallevel,
    first_year = first_year,
    last_year = last_year,
    ne_type = countrytype,
    region = region
  )
  
  # Add species if provided
  if (!is.null(species)) {
    params$species <- species
  }
  
  # Call function
  tryCatch({
    do.call(indicator_fn, params)
  }, error = function(e) {
    stop(paste("Failed to calculate time series:", conditionMessage(e)))
  })
}


#' Create Time Series Plot
#'
#' @description
#' Creates a ggplot time series from calculated indicator result.
#'
#' @param indicator_ts Result from calc_indicator_ts().
#' @param params List of plotting parameters.
#'
#' @return ggplot object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ts_result <- calc_indicator_ts(...)
#' params <- create_ts_plot_params(...)
#' plot <- create_ts_plot(ts_result, params)
#' }
create_ts_plot <- function(indicator_ts, params) {
  if (is.null(indicator_ts)) {
    stop("No time series to plot")
  }
  
  tryCatch({
    do.call(plot, c(list(x = indicator_ts), params))
  }, error = function(e) {
    stop(paste("Failed to create time series plot:", conditionMessage(e)))
  })
}


#' Create Time Series Plot Parameters
#'
#' @description
#' Builds parameter list for time series plotting from UI inputs.
#'
#' @param input List. Shiny input values.
#'
#' @return List of parameters for plot() function.
#'
#' @export
create_ts_plot_params <- function(input) {
  list(
    title = if (input$title == "") "auto" else input$title,
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
    x_label = if (input$ts_x_label == "") NULL else input$ts_x_label,
    y_label = if (input$ts_y_label == "") NULL else input$ts_y_label,
    suppress_x = input$suppress_x,
    suppress_xt = input$suppress_xt,
    suppress_y = input$suppress_y,
    suppress_yt = input$suppress_yt,
    expand_x = c(input$ts_x_expand_left, input$ts_x_expand_right),
    expand_y = c(input$ts_y_expand_bottom, input$ts_y_expand_top),
    n_x_breaks = input$ts_x_breaks,
    n_y_breaks = input$ts_y_breaks,
    gridlines = input$ts_gridlines,
    point = input$point_line == "point",
    point_size = input$pointsize,
    line = input$point_line == "line",
    line_size = input$linewidth,
    line_colour = input$linecolour,
    line_alpha = input$linealpha,
    ci_type = switch(input$ci_vis_type,
                    "Error Bars" = "errorbar",
                    "Ribbon" = "ribbon",
                    "None" = NULL),
    error_bar_width = input$error_width,
    error_bar_thickness = input$error_thickness,
    error_bar_alpha = input$error_alpha,
    ribbon_colour = input$ribboncolour,
    ribbon_alpha = input$ribbonalpha,
    smoothed_trend = input$smoothed_trend,
    trend_line_size = input$smooth_linewidth,
    trend_line_type = input$smooth_linetype,
    trend_line_colour = input$trendlinecolour,
    trend_line_alpha = input$trendlinealpha,
    trend_envelope_colour = input$envelopecolour,
    trend_envelope_alpha = input$envelopealpha,
    trend_ci_size = input$smooth_cilinewidth,
    trend_ci_alpha = input$smooth_cialpha
  )
}


#' Validate Time Series Inputs
#'
#' @description
#' Validates inputs specific to time series generation.
#'
#' @param indicator Character. Selected indicator.
#' @param species Character vector. Selected species.
#'
#' @return List with valid (logical) and message (character).
#'
#' @export
validate_ts_inputs <- function(indicator, species = NULL) {
  result <- list(valid = TRUE, message = "Valid inputs")
  
  # Check species-specific indicators
  if (indicator %in% c("Species Occurrences", "Species Range")) {
    if (is.null(species) || length(species) == 0) {
      result$valid <- FALSE
      result$message <- "Please select a species for this indicator"
      return(result)
    }
    
    if (length(species) > 1) {
      result$valid <- FALSE
      result$message <- "Please select only one species"
      return(result)
    }
  }
  
  result
}


#' Parse Expansion Values
#'
#' @description
#' Converts expansion input values to proper format.
#'
#' @param left Numeric. Left expansion.
#' @param right Numeric. Right expansion.
#' @param top Numeric. Top expansion.
#' @param bottom Numeric. Bottom expansion.
#' @param use_custom Logical. Whether to use custom expansion.
#'
#' @return List with expand_x and expand_y vectors.
#'
#' @export
parse_expansion <- function(left, right, top, bottom, use_custom = FALSE) {
  if (!use_custom) {
    return(list(
      expand_x = c(0, 0),
      expand_y = c(0, 0)
    ))
  }
  
  list(
    expand_x = c(left %||% 0, right %||% 0),
    expand_y = c(bottom %||% 0, top %||% 0)
  )
}


#' Get Confidence Interval Type
#'
#' @description
#' Converts UI CI type selection to plot parameter.
#'
#' @param ci_type Character. CI visualization type.
#'
#' @return Character or NULL.
#'
#' @export
get_ci_type <- function(ci_type) {
  switch(ci_type,
         "Error Bars" = "errorbar",
         "Ribbon" = "ribbon",
         "None" = NULL,
         NULL)
}


# Reuse helper from viz_map_config
`%||%` <- function(x, y) if (is.null(x)) y else x
