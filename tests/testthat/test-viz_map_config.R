# Tests for viz_map_config.R module

test_that("validate_font_size returns correct values", {
  # Valid values pass through
  expect_equal(validate_font_size(15, 8, 40), 15)
  expect_equal(validate_font_size(8, 8, 40), 8)
  expect_equal(validate_font_size(40, 8, 40), 40)
  
  # Too small - clamped to min
  expect_warning(
    result <- validate_font_size(5, 8, 40, "Test"),
    "too small"
  )
  expect_equal(result, 8)
  
  # Too large - clamped to max
  expect_warning(
    result <- validate_font_size(50, 8, 40, "Test"),
    "too large"
  )
  expect_equal(result, 40)
  
  # NULL - returns median
  expect_warning(
    result <- validate_font_size(NULL, 8, 40),
    "must be a single numeric"
  )
  expect_equal(result, 24)
  
  # Non-numeric - returns median
  expect_warning(
    result <- validate_font_size("abc", 8, 40),
    "must be a single numeric"
  )
  expect_equal(result, 24)
})

test_that("parse_coordinates handles valid inputs", {
  # Both values provided
  result <- parse_coordinates("10", "20", "X")
  expect_equal(result, c(10, 20))
  
  # Only min
  result <- parse_coordinates("10", "", "X")
  expect_equal(result, c(10, NA))
  
  # Only max
  result <- parse_coordinates("", "20", "Y")
  expect_equal(result, c(NA, 20))
  
  # Numeric inputs
  result <- parse_coordinates(10, 20, "X")
  expect_equal(result, c(10, 20))
})

test_that("parse_coordinates handles invalid inputs", {
  # Both empty
  expect_null(parse_coordinates("", ""))
  expect_null(parse_coordinates(NULL, NULL))
  
  # Invalid min
  expect_warning(
    result <- parse_coordinates("abc", "20", "X"),
    "Invalid X minimum"
  )
  expect_null(result)
  
  # Invalid max
  expect_warning(
    result <- parse_coordinates("10", "xyz", "Y"),
    "Invalid Y maximum"
  )
  expect_null(result)
  
  # Min >= Max
  expect_warning(
    result <- parse_coordinates("20", "10", "X"),
    "minimum must be less than maximum"
  )
  expect_null(result)
})

test_that("parse_legend_limits handles valid inputs", {
  # Both values
  result <- parse_legend_limits("0", "100")
  expect_equal(result, c(0, 100))
  
  # Decimal values
  result <- parse_legend_limits("0.5", "99.5")
  expect_equal(result, c(0.5, 99.5))
  
  # Negative values
  result <- parse_legend_limits("-10", "10")
  expect_equal(result, c(-10, 10))
})

test_that("parse_legend_limits handles invalid inputs", {
  # Both empty
  expect_null(parse_legend_limits("", ""))
  
  # Only one provided
  expect_warning(
    result <- parse_legend_limits("0", ""),
    "Both legend min and max must be provided"
  )
  expect_null(result)
  
  expect_warning(
    result <- parse_legend_limits("", "100"),
    "Both legend min and max must be provided"
  )
  expect_null(result)
  
  # Invalid conversion
  expect_warning(
    result <- parse_legend_limits("abc", "100"),
    "Invalid legend limits"
  )
  expect_null(result)
  
  # Min >= Max
  expect_warning(
    result <- parse_legend_limits("100", "0"),
    "minimum must be less than maximum"
  )
  expect_null(result)
})

test_that("parse_break_points works correctly", {
  # Standard comma-separated
  result <- parse_break_points("1, 2, 3, 4, 5")
  expect_equal(result, c(1, 2, 3, 4, 5))
  
  # No spaces
  result <- parse_break_points("10,20,30")
  expect_equal(result, c(10, 20, 30))
  
  # Mixed spaces
  result <- parse_break_points("1,  2,3 ,4")
  expect_equal(result, c(1, 2, 3, 4))
  
  # Single value
  result <- parse_break_points("5")
  expect_equal(result, 5)
  
  # Decimal values
  result <- parse_break_points("0.1, 0.5, 1.0")
  expect_equal(result, c(0.1, 0.5, 1.0))
  
  # Negative values
  result <- parse_break_points("-10, 0, 10")
  expect_equal(result, c(-10, 0, 10))
  
  # Empty
  expect_null(parse_break_points(""))
  expect_null(parse_break_points(NULL))
  
  # Invalid - returns NULL
  expect_null(parse_break_points("abc, def"))
  
  # Partially invalid - returns valid numbers only
  result <- parse_break_points("1, abc, 3")
  expect_equal(result, c(1, 3))
})

test_that("parse_break_points respects sort parameter", {
  result_sorted <- parse_break_points("5, 1, 3", sort = TRUE)
  expect_equal(result_sorted, c(1, 3, 5))
  
  result_unsorted <- parse_break_points("5, 1, 3", sort = FALSE)
  expect_equal(result_unsorted, c(5, 1, 3))
})

test_that("parse_labels works correctly", {
  result <- parse_labels("Low, Medium, High")
  expect_equal(result, c("Low", "Medium", "High"))
  
  result <- parse_labels("A,B,C")
  expect_equal(result, c("A", "B", "C"))
  
  # Empty
  expect_null(parse_labels(""))
  expect_null(parse_labels(NULL))
  
  # Preserves spaces within labels
  result <- parse_labels("Very Low, Very High")
  expect_equal(result, c("Very Low", "Very High"))
})

test_that("validate_breaks_labels validates correctly", {
  # Both NULL
  result <- validate_breaks_labels(NULL, NULL)
  expect_null(result$breaks)
  expect_null(result$labels)
  
  # Only breaks
  result <- validate_breaks_labels(c(1, 2, 3), NULL)
  expect_equal(result$breaks, c(1, 2, 3))
  expect_null(result$labels)
  
  # Only labels
  result <- validate_breaks_labels(NULL, c("A", "B"))
  expect_null(result$breaks)
  expect_equal(result$labels, c("A", "B"))
  
  # Matching lengths
  result <- validate_breaks_labels(c(1, 2), c("A", "B"))
  expect_equal(result$breaks, c(1, 2))
  expect_equal(result$labels, c("A", "B"))
  
  # Mismatched lengths - labels set to NULL
  expect_warning(
    result <- validate_breaks_labels(c(1, 2, 3), c("A", "B")),
    "Number of labels"
  )
  expect_equal(result$breaks, c(1, 2, 3))
  expect_null(result$labels)
})

test_that("get_map_resolution maps correctly", {
  expect_equal(get_map_resolution("110"), "small")
  expect_equal(get_map_resolution("50"), "medium")
  expect_equal(get_map_resolution("10"), "large")
  
  # Numeric input
  expect_equal(get_map_resolution(110), "small")
  
  # Invalid - returns medium with warning
  expect_warning(
    result <- get_map_resolution("999"),
    "Invalid map resolution"
  )
  expect_equal(result, "medium")
})

test_that("get_color_or_default works correctly", {
  # Valid color
  expect_equal(get_color_or_default("red"), "red")
  expect_equal(get_color_or_default("#FF0000"), "#FF0000")
  
  # Empty - returns NULL
  expect_null(get_color_or_default(""))
  expect_null(get_color_or_default(NULL))
  
  # Empty with default - returns default
  expect_equal(get_color_or_default("", "blue"), "blue")
  expect_equal(get_color_or_default(NULL, "blue"), "blue")
  
  # Empty with default and allow_null=FALSE
  expect_equal(get_color_or_default("", "blue", allow_null = FALSE), "blue")
  
  # Empty without default and allow_null=FALSE - returns fallback
  expect_equal(get_color_or_default("", NULL, allow_null = FALSE), "grey50")
})

test_that("create_map_plot_params builds correct structure", {
  # Mock inputs
  input <- list(
    title = "Test Title",
    subtitle = "Test Subtitle",
    caption = "",
    title_size = 14,
    subtitle_size = 12,
    caption_size = 10,
    title_wrap_length = 80,
    subtitle_wrap_length = 80,
    caption_wrap_length = 80,
    title_color = "black",
    subtitle_color = "grey",
    caption_color = "black",
    legend_title = "Legend",
    legend_title_wrap_length = 20,
    trans_yesno = FALSE,
    trans = "log",
    bcpower = NULL,
    ocean_fill_colour = "#92c5f0",
    land_fill_colour = "grey85",
    gridlines = TRUE,
    panel_gridlines = FALSE,
    crop_to_grid = FALSE,
    crop_by_region = FALSE,
    mapres = "50"
  )
  
  parsed <- list(
    xlims = c(0, 10),
    ylims = c(0, 20),
    breaks = c(1, 2, 3),
    labels = c("Low", "Med", "High"),
    legend_limits = c(0, 100)
  )
  
  params <- create_map_plot_params("mock_result", input, parsed)
  
  expect_type(params, "list")
  expect_equal(params$title, "Test Title")
  expect_equal(params$subtitle, "Test Subtitle")
  expect_null(params$caption)  # Empty string becomes NULL
  expect_equal(params$xlims, c(0, 10))
  expect_equal(params$ylims, c(0, 20))
  expect_null(params$trans)  # trans_yesno is FALSE
})

# Helper function test
`%||%` <- function(x, y) if (is.null(x)) y else x

test_that("%||% helper works correctly", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% 10, 0)  # 0 is not NULL
  expect_equal(NA %||% "default", NA)  # NA is not NULL
  expect_equal(c() %||% "default", "default")  # Empty vector is not NULL
})
