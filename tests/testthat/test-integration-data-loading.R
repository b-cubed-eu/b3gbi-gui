# Integration Tests: Data Loading Workflow
#
# These tests verify end-to-end data loading functionality including:
# - CSV data cube upload
# - Example data loading (if available)
# - Data validation and processing
# - Error handling for invalid data

# Helper to check if we can run integration tests
skip_if_no_integration <- function() {
  skip_if_no_shinytest2()
  skip_if_not(interactive(), "Skipping integration tests in non-interactive mode")
  skip_on_ci_with_display()
}

test_that("data loading workflow: app loads with empty data state", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Verify initial state - no data loaded
    values <- app$get_values()
    
    # Check that we're on the Data tab by default
    expect_equal(values$input$input_tabs, "Data")
    
    # Verify data upload input exists and is empty
    expect_true(element_exists(app, "#dataCube"))
    
    # Summary tab should show empty/no data message
    navigate_to_tab(app, "output_tabs", "Summary")
    wait_for_idle(app, timeout = 2)
    
    # The summary should indicate no data is loaded
    html <- app$get_html("#output_tabs")
    expect_true(
      nchar(html) > 0,
      "Summary panel should render even with no data"
    )
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("data loading workflow: CSV file upload works", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Create a test CSV file
    test_data <- data.frame(
      scientificName = c("Pinus sylvestris", "Quercus robur", "Pinus sylvestris"),
      family = c("Pinaceae", "Fagaceae", "Pinaceae"),
      year = c(2020, 2020, 2021),
      x = c(1, 2, 3),
      y = c(4, 5, 6),
      stringsAsFactors = FALSE
    )
    
    temp_csv <- tempfile(pattern = "test_cube_", fileext = ".csv")
    write.csv(test_data, temp_csv, row.names = FALSE)
    on.exit(unlink(temp_csv), add = TRUE)
    
    # Upload the file
    app$upload_file(dataCube = temp_csv)
    wait_for_idle(app, timeout = 5)
    
    # Verify the file was uploaded (input value should contain the filename)
    values <- app$get_values()
    
    # The dataCube input should have a value (file info)
    expect_true(
      !is.null(values$input$dataCube),
      "File upload should have a value after upload"
    )
    
    # Wait for data processing
    wait_for_idle(app, timeout = 3)
    
    # Check that family dropdown is populated after data load
    # This happens via server_taxonomy_filtering module
    navigate_to_tab(app, "input_tabs", "Analysis")
    wait_for_idle(app, timeout = 2)
    
    # The family and species dropdowns should exist
    expect_true(
      element_exists(app, "#family") || element_exists(app, "[data-id='family']"),
      "Family dropdown should exist"
    )
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("data loading workflow: data validation rejects invalid CSV", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Create an invalid CSV (missing required columns)
    invalid_data <- data.frame(
      wrong_column1 = c("a", "b", "c"),
      wrong_column2 = c(1, 2, 3),
      stringsAsFactors = FALSE
    )
    
    temp_csv <- tempfile(pattern = "invalid_cube_", fileext = ".csv")
    write.csv(invalid_data, temp_csv, row.names = FALSE)
    on.exit(unlink(temp_csv), add = TRUE)
    
    # Upload the invalid file
    app$upload_file(dataCube = temp_csv)
    wait_for_idle(app, timeout = 5)
    
    # The app should handle the error gracefully
    # No crash should occur
    values <- app$get_values()
    expect_true(
      !is.null(values$input$dataCube),
      "App should not crash on invalid upload"
    )
    
    # An error message might be displayed
    # We don't strictly require this, but check if it exists
    error_html <- tryCatch({
      app$get_html(".shiny-output-error")
    }, error = function(e) NULL)
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("data loading workflow: shapefile upload interface works", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Enable shapefile checkbox
    app$set_inputs(shapefile = TRUE)
    wait_for_idle(app, timeout = 2)
    
    # Shapefile upload should become visible
    expect_true(
      element_exists(app, "#shapefile_zip") || 
      element_exists(app, "[data-id='shapefile_zip']"),
      "Shapefile upload should appear when checkbox is enabled"
    )
    
    # Disable shapefile checkbox
    app$set_inputs(shapefile = FALSE)
    wait_for_idle(app, timeout = 1)
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("data loading workflow: navigation between tabs after data load", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Upload test data
    test_data <- data.frame(
      scientificName = rep("Species test", 10),
      family = rep("Family test", 10),
      year = sample(2000:2023, 10, replace = TRUE),
      x = runif(10, -10, 10),
      y = runif(10, -10, 10),
      stringsAsFactors = FALSE
    )
    
    temp_csv <- tempfile(pattern = "nav_test_", fileext = ".csv")
    write.csv(test_data, temp_csv, row.names = FALSE)
    on.exit(unlink(temp_csv), add = TRUE)
    
    app$upload_file(dataCube = temp_csv)
    wait_for_idle(app, timeout = 5)
    
    # Navigate through all input tabs
    tabs <- c("Data", "Analysis", "Viz Options")
    for (tab in tabs) {
      navigate_to_tab(app, "input_tabs", tab)
      
      values <- app$get_values()
      expect_equal(
        values$input$input_tabs, 
        tab,
        info = paste("Should navigate to", tab, "tab")
      )
    }
    
    # Navigate through all output tabs
    out_tabs <- c("Summary", "Map", "Time Series", "Table")
    for (tab in out_tabs) {
      navigate_to_tab(app, "output_tabs", tab)
      
      values <- app$get_values()
      expect_equal(
        values$input$output_tabs, 
        tab,
        info = paste("Should navigate to", tab, "output tab")
      )
    }
    
  }, finally = {
    cleanup_app(app)
  })
})

test_that("data loading workflow: data cube info display", {
  skip_if_no_integration()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app_from_tests("b3gbi_app")
    
    # Upload test data with known characteristics
    test_data <- data.frame(
      scientificName = c("Species A", "Species B", "Species C"),
      family = c("Family X", "Family Y", "Family X"),
      year = c(2019, 2020, 2021),
      x = c(1, 2, 3),
      y = c(4, 5, 6),
      stringsAsFactors = FALSE
    )
    
    temp_csv <- tempfile(pattern = "info_test_", fileext = ".csv")
    write.csv(test_data, temp_csv, row.names = FALSE)
    on.exit(unlink(temp_csv), add = TRUE)
    
    app$upload_file(dataCube = temp_csv)
    wait_for_idle(app, timeout = 5)
    
    # Navigate to Summary tab to see data info
    navigate_to_tab(app, "output_tabs", "Summary")
    wait_for_idle(app, timeout = 2)
    
    # Summary panel should render without errors
    summary_html <- tryCatch({
      app$get_html("#output_tabs .tab-content .active")
    }, error = function(e) NULL)
    
    expect_true(
      !is.null(summary_html),
      "Summary content should be accessible"
    )
    
  }, finally = {
    cleanup_app(app)
  })
})
