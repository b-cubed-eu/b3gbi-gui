# Tests for viz_map.R module

# Mock processed cube for testing
setup_mock_cube <- function() {
  structure(list(
    data = data.frame(
      scientificName = c("Pinus sylvestris", "Quercus robur"),
      family = c("Pinaceae", "Fagaceae"),
      year = c(2020, 2021),
      x = c(1, 2),
      y = c(3, 4),
      stringsAsFactors = FALSE
    ),
    resolutions = "1km"
  ), class = "processed_cube")
}

test_that("validate_map_inputs validates correctly", {
  # Valid inputs
  result <- validate_map_inputs("Observed Richness", NULL, "50")
  expect_true(result$valid)
  expect_equal(result$message, "Valid inputs")
  
  # Species-specific indicator without species
  result <- validate_map_inputs("Species Occurrences", NULL, "50")
  expect_false(result$valid)
  expect_match(result$message, "select a single species")
  
  # Species-specific with one species
  result <- validate_map_inputs("Species Occurrences", "Pinus sylvestris", "50")
  expect_true(result$valid)
  
  # Species-specific with multiple species
  result <- validate_map_inputs("Species Occurrences", c("A", "B"), "50")
  expect_false(result$valid)
  expect_match(result$message, "only one species")
  
  # Invalid map resolution
  result <- validate_map_inputs("Observed Richness", NULL, "999")
  expect_false(result$valid)
  expect_match(result$message, "resolution is not properly selected")
})

test_that("validate_map_inputs handles all species-specific indicators", {
  species_indicators <- c("Species Occurrences", "Species Range")
  
  for (indicator in species_indicators) {
    result <- validate_map_inputs(indicator, NULL, "50")
    expect_false(result$valid, info = paste(indicator, "should require species"))
  }
})

test_that("get_indicator_function returns correct functions", {
  # Note: These tests may fail if b3gbi functions aren't available
  # They serve as documentation of expected behavior
  
  skip_if_not_installed("b3gbi")
  
  # Test known map indicators
  fn <- get_indicator_function("Observed Richness", "map")
  expect_type(fn, "closure")
  
  fn <- get_indicator_function("Total Occurrences", "map")
  expect_type(fn, "closure")
  
  # Test known ts indicators
  fn <- get_indicator_function("Observed Richness", "ts")
  expect_type(fn, "closure")
  
  # Unknown indicator returns NULL
  fn <- get_indicator_function("NonExistent", "map")
  expect_null(fn)
  
  # Unknown type returns NULL
  fn <- get_indicator_function("Observed Richness", "invalid_type")
  expect_null(fn)
})

test_that("is_indicator_available returns correct values", {
  skip_if_not_installed("b3gbi")
  
  expect_true(is_indicator_available("Observed Richness", "map"))
  expect_true(is_indicator_available("Total Occurrences", "ts"))
  expect_false(is_indicator_available("NonExistent", "map"))
  expect_false(is_indicator_available("Observed Richness", "invalid_type"))
})

test_that("get_available_indicators returns character vector", {
  skip_if_not_installed("b3gbi")
  
  map_indicators <- get_available_indicators("map")
  expect_type(map_indicators, "character")
  expect_gt(length(map_indicators), 0)
  expect_true("Observed Richness" %in% map_indicators)
  
  ts_indicators <- get_available_indicators("ts")
  expect_type(ts_indicators, "character")
  expect_gt(length(ts_indicators), 0)
  
  # Both should have similar indicators (except Species Range for ts)
  expect_true("Observed Richness" %in% ts_indicators)
})

test_that("prepare_crs handles EPSG codes correctly", {
  # Valid EPSG code
  result <- prepare_crs("4326", use_custom = TRUE)
  expect_equal(result, "EPSG:4326")
  
  # Numeric input
  result <- prepare_crs(4326, use_custom = TRUE)
  expect_equal(result, "EPSG:4326")
  
  # Not using custom
  expect_null(prepare_crs("4326", use_custom = FALSE))
  
  # Empty input
  expect_null(prepare_crs("", use_custom = TRUE))
  expect_null(prepare_crs(NULL, use_custom = TRUE))
  
  # Invalid code
  expect_warning(
    result <- prepare_crs("invalid", use_custom = TRUE),
    "Invalid EPSG code"
  )
  expect_null(result)
})

test_that("calc_indicator_map validates data cube", {
  # Invalid cube
  expect_error(
    calc_indicator_map("not a cube", "Observed Richness", 10, "cube", 2000, 2020, "countries", "medium"),
    "Invalid data cube"
  )
  
  expect_error(
    calc_indicator_map(NULL, "Observed Richness", 10, "cube", 2000, 2020, "countries", "medium"),
    "Invalid data cube"
  )
  
  # Valid cube but invalid indicator (should error when trying to call)
  cube <- setup_mock_cube()
  expect_error(
    calc_indicator_map(cube, "NonExistentIndicator", 10, "cube", 2000, 2020, "countries", "medium"),
    "not available for mapping"
  )
})

test_that("create_map_plot validates inputs", {
  # NULL indicator map
  expect_error(
    create_map_plot(NULL, list()),
    "No indicator map to plot"
  )
  
  # Empty params should still work (plot function will use defaults)
  # We can't fully test without actual b3gbi plot function
})
