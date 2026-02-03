# Integration Tests: Filtering Workflow
#
# These tests verify end-to-end filtering functionality including:
# - Region/custom region filtering
# - Date range filtering
# - Family/species filtering
# - Spatial filtering with shapefiles
# - Taxonomy filtering

# Helper to check if we can run integration tests
skip_if_no_integration <- function() {
  skip_if_no_shinytest2()
  skip_if_not(interactive(), "Skipping integration tests in non-interactive mode")
  skip_on_ci_with_display()
}

# Helper to upload test data with variety for filtering tests
upload_test_cube_for_filtering <- function(app, n_rows = 100) {
  # Create diverse test data
  species <- c("Pinus sylvestris", "Quercus robur", "Picea abies", 
               "Fagus sylvatica", "Acer pseudoplatanus")
  families <- c("Pinaceae", "Fagaceae", "Pinaceae", "Fagaceae", "Sapindaceae")
  
  test_data <- data.frame(
    scientificName = sample(species, n_rows, replace = TRUE),
    family = sample(families, n_rows, replace = TRUE),
    year = sample(2000:2023, n_rows, replace = TRUE),
    x = runif(n_rows, -10, 10),
    y = runif(n_rows, -10, 10),
    stringsAsFactors = FALSE
  )
  
  temp_csv <- tempfile(pattern = "filter_test_", fileext = ".csv")
  write.csv(test_data, temp_csv, row.names = FALSE)
  on.exit(unlink(temp_csv), add = TRUE)
  
  app$upload_file(dataCube = temp_csv)
  wait_for_idle(app, timeout = 5)
  
  invisible(app)
}

test_that("filtering workflow: date range filter updates correctly", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Upload test data
    upload_test_cube_for_filtering(app)
    
    # Navigate to Analysis tab
    navigate_to_tab(app, "input_tabs", "Analysis")
    wait_for_idle(app, timeout = 3)
    
    # Get initial date range
    values <- app$get_values()
    initial_range <- values$input$daterange
    
    # Update date range
    new_range <- c(2010, 2020)
    app$set_inputs(daterange = new_range)
    wait_for_idle(app, timeout = 2)
    
    # Verify the change was applied
    values <- app$get_values()
    expect_equal(
      values$input$daterange,
      new_range,
      info = "Date range should update correctly"
    )
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("filtering workflow: custom region filtering enables correctly", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Upload test data
    upload_test_cube_for_filtering(app)
    
    # Navigate to Analysis tab
    navigate_to_tab(app, "input_tabs", "Analysis")
    wait_for_idle(app, timeout = 3)
    
    # Enable custom region
    app$set_inputs(customregion = TRUE)
    wait_for_idle(app, timeout = 2)
    
    # Verify spatial level input appears
    expect_true(
      element_exists(app, "#spatiallevel") || 
      element_exists(app, "[data-id='spatiallevel']"),
      "Spatial level dropdown should appear"
    )
    
    # Verify country type input appears
    expect_true(
      element_exists(app, "#countrytype") ||
      element_exists(app, "[data-id='countrytype']"),
      "Country type dropdown should appear"
    )
    
    # Verify region selector appears
    expect_true(
      element_exists(app, "#region") ||
      element_exists(app, "[data-id='region']"),
      "Region selector should appear"
    )
    
    # Test changing spatial level
    app$set_inputs(spatiallevel = "continent")
    wait_for_idle(app, timeout = 2)
    
    values <- app$get_values()
    expect_equal(
      values$input$spatiallevel,
      "continent",
      info = "Spatial level should update to continent"
    )
    
    # Test changing country type
    app$set_inputs(countrytype = "sovereignty")
    wait_for_idle(app, timeout = 2)
    
    values <- app$get_values()
    expect_equal(
      values$input$countrytype,
      "sovereignty",
      info = "Country type should update"
    )
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("filtering workflow: family and species dropdowns populate", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Upload test data with multiple families and species
    upload_test_cube_for_filtering(app, n_rows = 50)
    
    # Navigate to Analysis tab
    navigate_to_tab(app, "input_tabs", "Analysis")
    wait_for_idle(app, timeout = 5)
    
    # Check that family dropdown exists
    expect_true(
      element_exists(app, "#family") ||
      element_exists(app, "[data-id='family']"),
      "Family dropdown should exist"
    )
    
    # Check that species dropdown exists
    expect_true(
      element_exists(app, "#species") ||
      element_exists(app, "[data-id='species']"),
      "Species dropdown should exist"
    )
    
    # Note: The actual population of dropdowns depends on server_taxonomy_filtering.R
    # which updates these based on the loaded data
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("filtering workflow: indicator selection works", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Navigate to Analysis tab
    navigate_to_tab(app, "input_tabs", "Analysis")
    wait_for_idle(app, timeout = 2)
    
    # Check that indicator dropdown exists
    expect_true(
      element_exists(app, "#indicatorsToAnalyse") ||
      element_exists(app, "[data-id='indicatorsToAnalyse']"),
      "Indicator dropdown should exist"
    )
    
    # Get available indicators (should be populated from b3gbi)
    values <- app$get_values()
    
    # Try to select first available indicator if dropdown is populated
    if (!is.null(values$input$indicatorsToAnalyse)) {
      # Already has a value
      expect_true(
        nchar(values$input$indicatorsToAnalyse) > 0,
        "Indicator should have a default value"
      )
    }
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("filtering workflow: map resolution selection", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Navigate to Analysis tab
    navigate_to_tab(app, "input_tabs", "Analysis")
    wait_for_idle(app, timeout = 2)
    
    # Check that map resolution dropdown exists
    expect_true(
      element_exists(app, "#mapres") ||
      element_exists(app, "[data-id='mapres']"),
      "Map resolution dropdown should exist"
    )
    
    # Test changing map resolution
    app$set_inputs(mapres = "110")
    wait_for_idle(app, timeout = 2)
    
    values <- app$get_values()
    expect_equal(
      values$input$mapres,
      "110",
      info = "Map resolution should update"
    )
    
    # Test another resolution
    app$set_inputs(mapres = "10")
    wait_for_idle(app, timeout = 2)
    
    values <- app$get_values()
    expect_equal(
      values$input$mapres,
      "10",
      info = "Map resolution should update to 10m"
    )
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("filtering workflow: spatial resolution (cell size)", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Navigate to Analysis tab
    navigate_to_tab(app, "input_tabs", "Analysis")
    wait_for_idle(app, timeout = 2)
    
    # Check that cell size input exists
    expect_true(
      element_exists(app, "#cellsize") ||
      element_exists(app, "[data-id='cellsize']"),
      "Cell size input should exist"
    )
    
    # Test changing cell size
    app$set_inputs(cellsize = 5)
    wait_for_idle(app, timeout = 1)
    
    values <- app$get_values()
    expect_equal(
      values$input$cellsize,
      5,
      info = "Cell size should update"
    )
    
    # Test another cell size
    app$set_inputs(cellsize = 25)
    wait_for_idle(app, timeout = 1)
    
    values <- app$get_values()
    expect_equal(
      values$input$cellsize,
      25,
      info = "Cell size should update to 25"
    )
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("filtering workflow: include land/ocean options", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Navigate to Analysis tab
    navigate_to_tab(app, "input_tabs", "Analysis")
    wait_for_idle(app, timeout = 2)
    
    # Check that land/ocean checkboxes exist
    expect_true(
      element_exists(app, "#include_land") ||
      element_exists(app, "[data-id='include_land']"),
      "Include land checkbox should exist"
    )
    
    expect_true(
      element_exists(app, "#include_ocean") ||
      element_exists(app, "[data-id='include_ocean']"),
      "Include ocean checkbox should exist"
    )
    
    # Test toggling options
    app$set_inputs(include_land = FALSE)
    wait_for_idle(app, timeout = 1)
    
    values <- app$get_values()
    expect_false(
      values$input$include_land,
      info = "Include land should be toggleable"
    )
    
    # Re-enable
    app$set_inputs(include_land = TRUE)
    wait_for_idle(app, timeout = 1)
    
    values <- app$get_values()
    expect_true(
      values$input$include_land,
      info = "Include land should be re-enableable"
    )
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("filtering workflow: combined filters work together", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Upload test data
    upload_test_cube_for_filtering(app)
    
    # Navigate to Analysis tab
    navigate_to_tab(app, "input_tabs", "Analysis")
    wait_for_idle(app, timeout = 3)
    
    # Apply multiple filters
    app$set_inputs(
      daterange = c(2010, 2020),
      mapres = "110",
      cellsize = 10,
      include_land = TRUE,
      include_ocean = FALSE
    )
    wait_for_idle(app, timeout = 3)
    
    # Verify all filters were applied
    values <- app$get_values()
    
    expect_equal(values$input$daterange, c(2010, 2020))
    expect_equal(values$input$mapres, "110")
    expect_equal(values$input$cellsize, 10)
    expect_true(values$input$include_land)
    expect_false(values$input$include_ocean)
    
    # App should remain stable with combined filters
    expect_true(app_is_ready(app))
    
  }, finally = {
    cleanup_app(app)
  })
})
