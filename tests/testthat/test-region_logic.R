# Tests for region_logic.R module

# Mock data for testing
setup_mock_spatial_data <- function() {
  list(
    rne_countries_50 = data.frame(
      ADMIN = c("Germany", "France", "Spain"),
      SOVEREIGNT = c("Germany", "France", "Spain"),
      GEOUNIT = c("Germany", "France", "Spain"),
      stringsAsFactors = FALSE
    ),
    rne_countries_110 = data.frame(
      ADMIN = c("Germany", "France", "Spain", "Italy"),
      SOVEREIGNT = c("Germany", "France", "Spain", "Italy"),
      GEOUNIT = c("Germany", "France", "Spain", "Italy"),
      stringsAsFactors = FALSE
    ),
    rne_tiny_countries_50 = data.frame(
      ADMIN = c("Monaco", "Liechtenstein"),
      stringsAsFactors = FALSE
    ),
    rne_tiny_countries_110 = data.frame(
      ADMIN = c("Monaco", "Liechtenstein", "Andorra"),
      stringsAsFactors = FALSE
    ),
    continents = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
  )
}

test_that("get_country_options returns correct options", {
  mock_env <- list2env(setup_mock_spatial_data())
  
  # Test countries at 50m resolution
  result <- get_country_options("countries", "50", "ADMIN", mock_env)
  expect_type(result, "character")
  expect_equal(length(result), 3)
  expect_true("Germany" %in% result)
  expect_equal(result, sort(result))  # Should be sorted
  
  # Test different field
  result <- get_country_options("countries", "50", "SOVEREIGNT", mock_env)
  expect_equal(length(result), 3)
  
  # Test at 110m resolution (more countries)
  result <- get_country_options("countries", "110", "ADMIN", mock_env)
  expect_equal(length(result), 4)
  expect_true("Italy" %in% result)
})

test_that("get_country_options handles tiny_countries correctly", {
  mock_env <- list2env(setup_mock_spatial_data())
  
  # Valid - tiny_countries at 50m
  result <- get_country_options("tiny_countries", "50", "ADMIN", mock_env)
  expect_equal(length(result), 2)
  expect_true("Monaco" %in% result)
  
  # Valid - tiny_countries at 110m (more countries)
  result <- get_country_options("tiny_countries", "110", "ADMIN", mock_env)
  expect_equal(length(result), 3)
  expect_true("Andorra" %in% result)
  
  # Invalid - tiny_countries at 10m (not available)
  result <- get_country_options("tiny_countries", "10", "ADMIN", mock_env)
  expect_null(result)
})

test_that("get_country_options validates inputs", {
  mock_env <- list2env(setup_mock_spatial_data())
  
  # Invalid countrytype
  expect_warning(
    result <- get_country_options("invalid_type", "50", "ADMIN", mock_env),
    "Invalid countrytype"
  )
  expect_null(result)
  
  # Non-existent data
  expect_warning(
    result <- get_country_options("countries", "10", "ADMIN", mock_env),
    "Spatial data not found"
  )
  expect_null(result)
  
  # Invalid field
  expect_warning(
    result <- get_country_options("countries", "50", "INVALID_FIELD", mock_env),
    "Field not found"
  )
  expect_null(result)
  
  # Bad input types
  expect_warning(
    result <- get_country_options(123, "50", "ADMIN", mock_env),
    "countrytype must be a single character string"
  )
  expect_null(result)
  
  expect_warning(
    result <- get_country_options(c("a", "b"), "50", "ADMIN", mock_env),
    "countrytype must be a single character string"
  )
  expect_null(result)
})

test_that("get_country_options removes NAs and duplicates", {
  mock_env <- list2env(list(
    rne_countries_50 = data.frame(
      ADMIN = c("Germany", NA, "Germany", "France"),  # Has NA and duplicate
      stringsAsFactors = FALSE
    )
  ))
  
  result <- get_country_options("countries", "50", "ADMIN", mock_env)
  expect_equal(length(result), 2)
  expect_false(any(is.na(result)))
  expect_equal(result, c("France", "Germany"))
})

test_that("get_resolution_choices returns correct options", {
  # Regular countries - all 3 resolutions
  result <- get_resolution_choices("countries")
  expect_type(result, "character")
  expect_equal(length(result), 3)
  expect_equal(result["Small (110m)"], "110")
  expect_equal(result["Medium (50m)"], "50")
  expect_equal(result["Large (10m)"], "10")
  
  # Map units - same as countries
  result <- get_resolution_choices("map_units")
  expect_equal(length(result), 3)
  
  # Sovereignty - same as countries
  result <- get_resolution_choices("sovereignty")
  expect_equal(length(result), 3)
  
  # Tiny countries - only 2 resolutions (no 10m)
  result <- get_resolution_choices("tiny_countries")
  expect_equal(length(result), 2)
  expect_false("10" %in% result)
  expect_true("110" %in% result)
  expect_true("50" %in% result)
})

test_that("get_country_type_choices returns correct options", {
  # Levels that don't use country types
  expect_null(get_country_type_choices("continent", "50"))
  expect_null(get_country_type_choices("world", "50"))
  expect_null(get_country_type_choices("cube", "50"))
  
  # At 10m resolution - no tiny_countries
  result <- get_country_type_choices("country", "10")
  expect_equal(length(result), 3)
  expect_true("countries" %in% result)
  expect_false("tiny_countries" %in% result)
  
  # At 50m resolution - all types
  result <- get_country_type_choices("country", "50")
  expect_equal(length(result), 4)
  expect_true("tiny_countries" %in% result)
  
  # At 110m resolution - all types
  result <- get_country_type_choices("country", "110")
  expect_equal(length(result), 4)
  
  # Sovereignty level
  result <- get_country_type_choices("sovereignty", "50")
  expect_equal(length(result), 4)
  
  # Geounit level
  result <- get_country_type_choices("geounit", "50")
  expect_equal(length(result), 4)
})

test_that("get_country_type_choices validates input", {
  expect_warning(
    result <- get_country_type_choices(123, "50"),
    "spatiallevel must be a single character string"
  )
  expect_null(result)
  
  expect_warning(
    result <- get_country_type_choices(c("a", "b"), "50"),
    "spatiallevel must be a single character string"
  )
  expect_null(result)
})

test_that("get_region_options handles all spatial levels", {
  mock_env <- list2env(setup_mock_spatial_data())
  continents <- mock_env$continents
  
  # Continent level
  result <- get_region_options("continent", NULL, NULL, continents, mock_env)
  expect_type(result, "character")
  expect_equal(length(result), 6)
  expect_true("Europe" %in% result)
  
  # Country level
  result <- get_region_options("country", "countries", "50", continents, mock_env)
  expect_equal(length(result), 3)
  
  # Sovereignty level
  result <- get_region_options("sovereignty", "countries", "50", continents, mock_env)
  expect_equal(length(result), 3)
  
  # Geounit level
  result <- get_region_options("geounit", "countries", "50", continents, mock_env)
  expect_equal(length(result), 3)
  
  # Cube level - returns NULL
  expect_null(get_region_options("cube", NULL, NULL, continents, mock_env))
  
  # World level - returns NULL
  expect_null(get_region_options("world", NULL, NULL, continents, mock_env))
})

test_that("get_region_options validates inputs", {
  mock_env <- list2env(setup_mock_spatial_data())
  continents <- mock_env$continents
  
  # Missing required parameters for country level
  expect_warning(
    result <- get_region_options("country", NULL, "50", continents, mock_env),
    "countrytype and mapres required"
  )
  expect_null(result)
  
  expect_warning(
    result <- get_region_options("country", "countries", NULL, continents, mock_env),
    "countrytype and mapres required"
  )
  expect_null(result)
  
  # Bad spatiallevel type
  expect_warning(
    result <- get_region_options(123, NULL, NULL, continents, mock_env),
    "spatiallevel must be a single character string"
  )
  expect_null(result)
  
  # Missing continents for continent level
  expect_warning(
    result <- get_region_options("continent", NULL, NULL, NULL, mock_env),
    "continents data not provided"
  )
  expect_null(result)
})

test_that("validate_spatial_selection validates combinations", {
  # Valid combinations
  result <- validate_spatial_selection("country", "countries", "50")
  expect_true(result$valid)
  expect_true(result$requires_countrytype)
  
  result <- validate_spatial_selection("continent", NULL, NULL)
  expect_true(result$valid)
  expect_false(result$requires_countrytype)
  
  # Invalid - tiny_countries at 10m
  result <- validate_spatial_selection("country", "tiny_countries", "10")
  expect_false(result$valid)
  expect_match(result$message, "tiny_countries not available at 10m")
  
  # Invalid - missing required parameters
  result <- validate_spatial_selection("country", NULL, "50")
  expect_false(result$valid)
  expect_match(result$message, "countrytype and mapres are required")
  
  result <- validate_spatial_selection("sovereignty", "countries", NULL)
  expect_false(result$valid)
  expect_match(result$message, "countrytype and mapres are required")
  
  # Invalid country type for level
  result <- validate_spatial_selection("country", "invalid_type", "50")
  expect_false(result$valid)
  expect_match(result$message, "Invalid countrytype")
})

test_that("get_default_resolution returns sensible defaults", {
  expect_equal(get_default_resolution("countries"), "50")
  expect_equal(get_default_resolution("map_units"), "50")
  expect_equal(get_default_resolution("sovereignty"), "50")
  expect_equal(get_default_resolution("tiny_countries"), "110")
})

test_that("get_spatial_level_description returns correct labels", {
  expect_equal(get_spatial_level_description("cube"), "Full data cube")
  expect_equal(get_spatial_level_description("world"), "World")
  expect_equal(get_spatial_level_description("continent"), "Continents")
  expect_equal(get_spatial_level_description("country"), "Countries")
  expect_equal(get_spatial_level_description("sovereignty"), "Sovereignties")
  expect_equal(get_spatial_level_description("geounit"), "Geographic units")
  
  # Unknown level returns as-is
  expect_equal(get_spatial_level_description("unknown"), "unknown")
})
