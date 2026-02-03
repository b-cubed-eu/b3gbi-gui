# Tests for viz_timeseries.R module

test_that("validate_ts_inputs validates correctly", {
  # Valid inputs
  result <- validate_ts_inputs("Observed Richness", NULL)
  expect_true(result$valid)
  expect_equal(result$message, "Valid inputs")
  
  # Species-specific without species
  result <- validate_ts_inputs("Species Occurrences", NULL)
  expect_false(result$valid)
  expect_match(result$message, "select a species")
  
  result <- validate_ts_inputs("Species Range", NULL)
  expect_false(result$valid)
  
  # With one species
  result <- validate_ts_inputs("Species Occurrences", "Pinus sylvestris")
  expect_true(result$valid)
  
  # With multiple species
  result <- validate_ts_inputs("Species Occurrences", c("A", "B"))
  expect_false(result$valid)
  expect_match(result$message, "only one species")
  
  # Non-species indicator with species (should be valid)
  result <- validate_ts_inputs("Observed Richness", "Pinus sylvestris")
  expect_true(result$valid)
})

test_that("parse_expansion handles all cases", {
  # Not using custom
  result <- parse_expansion(0.1, 0.1, 0.1, 0.1, use_custom = FALSE)
  expect_equal(result$expand_x, c(0, 0))
  expect_equal(result$expand_y, c(0, 0))
  
  # Using custom with values
  result <- parse_expansion(0.1, 0.2, 0.3, 0.4, use_custom = TRUE)
  expect_equal(result$expand_x, c(0.1, 0.2))
  expect_equal(result$expand_y, c(0.4, 0.3))  # Note: bottom comes first
  
  # Using custom with NULL values
  result <- parse_expansion(NULL, NULL, NULL, NULL, use_custom = TRUE)
  expect_equal(result$expand_x, c(0, 0))
  expect_equal(result$expand_y, c(0, 0))
  
  # Mixed values
  result <- parse_expansion(0.1, NULL, 0.3, NULL, use_custom = TRUE)
  expect_equal(result$expand_x, c(0.1, 0))
  expect_equal(result$expand_y, c(0, 0.3))
})

test_that("get_ci_type maps correctly", {
  expect_equal(get_ci_type("Error Bars"), "errorbar")
  expect_equal(get_ci_type("Ribbon"), "ribbon")
  expect_null(get_ci_type("None"))
  expect_null(get_ci_type(NULL))
  expect_null(get_ci_type("Invalid"))
})

test_that("create_ts_plot_params builds correct structure", {
  # Mock input list
  input <- list(
    title = "TS Title",
    subtitle = "TS Subtitle",
    caption = "TS Caption",
    title_size = 14,
    subtitle_size = 12,
    caption_size = 10,
    title_wrap_length = 80,
    subtitle_wrap_length = 80,
    caption_wrap_length = 80,
    title_color = "black",
    subtitle_color = "grey",
    caption_color = "black",
    ts_x_label = "Year",
    ts_y_label = "Value",
    suppress_x = FALSE,
    suppress_xt = FALSE,
    suppress_y = FALSE,
    suppress_yt = FALSE,
    ts_x_expand_left = 0.05,
    ts_x_expand_right = 0.05,
    ts_y_expand_top = 0.05,
    ts_y_expand_bottom = 0.05,
    ts_x_breaks = 10,
    ts_y_breaks = 5,
    ts_gridlines = TRUE,
    point_line = "point",
    pointsize = 2,
    linewidth = 1,
    linecolour = "blue",
    linealpha = 1,
    ci_vis_type = "Ribbon",
    error_width = 0.2,
    error_thickness = 1,
    error_alpha = 1,
    ribboncolour = "grey",
    ribbonalpha = 0.3,
    smoothed_trend = TRUE,
    smooth_linewidth = 1,
    smooth_linetype = "solid",
    trendlinecolour = "red",
    trendlinealpha = 0.8,
    envelopecolour = "lightblue",
    envelopealpha = 0.2,
    smooth_cilinewidth = 1,
    smooth_cialpha = 0.5
  )
  
  params <- create_ts_plot_params(input)
  
  expect_type(params, "list")
  expect_equal(params$title, "TS Title")
  expect_equal(params$subtitle, "TS Subtitle")
  expect_equal(params$caption, "TS Caption")
  expect_equal(params$x_label, "Year")
  expect_equal(params$y_label, "Value")
  expect_equal(params$point, TRUE)
  expect_equal(params$line, FALSE)
  expect_equal(params$ci_type, "ribbon")
  expect_equal(params$smoothed_trend, TRUE)
  expect_equal(params$trend_line_colour, "red")
})

test_that("create_ts_plot_params handles empty strings", {
  input <- list(
    title = "",
    subtitle = "",
    caption = "",
    ts_x_label = "",
    ts_y_label = "",
    # Add other required fields with default values
    title_size = 14,
    subtitle_size = 12,
    caption_size = 10,
    title_wrap_length = 80,
    subtitle_wrap_length = 80,
    caption_wrap_length = 80,
    title_color = "black",
    subtitle_color = "grey",
    caption_color = "black",
    suppress_x = FALSE,
    suppress_xt = FALSE,
    suppress_y = FALSE,
    suppress_yt = FALSE,
    ts_x_expand_left = 0,
    ts_x_expand_right = 0,
    ts_y_expand_top = 0,
    ts_y_expand_bottom = 0,
    ts_x_breaks = 10,
    ts_y_breaks = 5,
    ts_gridlines = TRUE,
    point_line = "point",
    pointsize = 2,
    linewidth = 1,
    linecolour = "blue",
    linealpha = 1,
    ci_vis_type = "None",
    error_width = 0.2,
    error_thickness = 1,
    error_alpha = 1,
    ribboncolour = "grey",
    ribbonalpha = 0.3,
    smoothed_trend = FALSE,
    smooth_linewidth = 1,
    smooth_linetype = "solid",
    trendlinecolour = "blue",
    trendlinealpha = 0.5,
    envelopecolour = "lightblue",
    envelopealpha = 0.2,
    smooth_cilinewidth = 1,
    smooth_cialpha = 1
  )
  
  params <- create_ts_plot_params(input)
  
  # Empty strings should become NULL for optional fields
  expect_equal(params$title, "auto")
  expect_null(params$subtitle)
  expect_null(params$caption)
  expect_null(params$x_label)
  expect_null(params$y_label)
  # CI type should be NULL for "None"
  expect_null(params$ci_type)
})

test_that("calc_indicator_ts validates data cube", {
  # Invalid cube
  expect_error(
    calc_indicator_ts("not a cube", "Observed Richness", 10, "cube", 2000, 2020, "countries"),
    "Invalid data cube"
  )
  
  expect_error(
    calc_indicator_ts(NULL, "Observed Richness", 10, "cube", 2000, 2020, "countries"),
    "Invalid data cube"
  )
  
  # Valid cube but invalid indicator
  cube <- structure(list(data = data.frame(), resolutions = "1km"), class = "processed_cube")
  expect_error(
    calc_indicator_ts(cube, "NonExistentIndicator", 10, "cube", 2000, 2020, "countries"),
    "not available for time series"
  )
})

test_that("create_ts_plot validates inputs", {
  # NULL indicator ts
  expect_error(
    create_ts_plot(NULL, list()),
    "No time series to plot"
  )
  
  # Empty params should still work
  # Full testing requires actual b3gbi objects
})
