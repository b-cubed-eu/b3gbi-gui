# shinytest2 Main Test File
#
# This file sets up and initializes shinytest2 for the b3gbiGUI package.
# It provides basic infrastructure tests and example usage patterns.
#

# Ensure required packages are available
skip_if_no_shinytest2 <- function() {
  if (!requireNamespace("shinytest2", quietly = TRUE)) {
    skip("shinytest2 package not available. Install with: install.packages('shinytest2')")
  }
}

# Basic infrastructure test: Verify shinytest2 is working
test_that("shinytest2 infrastructure is set up correctly", {
  skip_if_no_shinytest2()
  
  # Test that we can create a test driver
  expect_true(requireNamespace("shinytest2", quietly = TRUE))
  expect_true(requireNamespace("shiny", quietly = TRUE))
})

# Test: App can be found and loaded
test_that("b3gbiGUI app exists and has required structure", {
  skip_if_no_shinytest2()
  skip_if_not(interactive(), "Skipping in non-interactive mode")
  
  # Find app directory
  root <- rprojroot::find_root(rprojroot::is_r_package)
  app_r_path <- file.path(root, "app.R")
  
  # Check app.R exists
  expect_true(file.exists(app_r_path), "app.R should exist in package root")
  
  # Check R directory with modules exists
  r_dir <- file.path(root, "R")
  expect_true(dir.exists(r_dir), "R/ directory with modules should exist")
  
  # Check data directory exists
  data_dir <- file.path(root, "data")
  expect_true(dir.exists(data_dir), "data/ directory should exist")
  
  # Check example data exists
  expect_true(
    file.exists(file.path(data_dir, "example_cube_1.rda")) ||
    file.exists(file.path(data_dir, "europe_species_cube.csv")),
    "Example data should exist"
  )
})

# Test: Basic app startup (when interactive or with display)
test_that("b3gbiGUI app starts without errors", {
  skip_if_no_shinytest2()
  skip_if_not(interactive(), "Skipping app startup test in non-interactive mode")
  skip_on_ci_with_display()
  
  # Setup app
  app <- NULL
  
  # Use tryCatch to ensure cleanup
  tryCatch({
    app <- setup_test_app(timeout = 20 * 1000)
    
    # Basic smoke test
    expect_true(app_is_ready(app), "App should load and be ready")
    
    # Verify we can get app state
    expect_no_error(app$get_values())
    
  }, finally = {
    cleanup_app(app)
  })
})

# Test: UI elements are present
test_that("b3gbiGUI has expected UI structure", {
  skip_if_no_shinytest2()
  skip_if_not(interactive(), "Skipping UI test in non-interactive mode")
  skip_on_ci_with_display()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app()
    
    # Wait for app to be ready
    expect_true(run_smoke_test(app), "Smoke test should pass")
    
    # Check input tabs exist
    expect_true(element_exists(app, "#input_tabs"))
    
    # Check output tabs exist  
    expect_true(element_exists(app, "#output_tabs"))
    
    # Check data upload input exists
    expect_true(element_exists(app, "[data-id='dataCube']") ||
                element_exists(app, "#dataCube"))
    
  }, finally = {
    cleanup_app(app)
  })
})

# Test: Data tab interactions
test_that("data tab has expected inputs", {
  skip_if_no_shinytest2()
  skip_if_not(interactive(), "Skipping data tab test in non-interactive mode")
  skip_on_ci_with_display()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app()
    
    # Navigate to Data tab (should be default)
    navigate_to_tab(app, "input_tabs", "Data")
    
    # Verify data inputs exist
    values <- app$get_values()
    
    # Check that dataCube input exists in the input list
    expect_true(
      "dataCube" %in% names(values$input) || 
      exists("dataCube", envir = values$input),
      "dataCube input should exist"
    )
    
  }, finally = {
    cleanup_app(app)
  })
})

# Test: Analysis tab inputs
test_that("analysis tab has expected inputs", {
  skip_if_no_shinytest2()
  skip_if_not(interactive(), "Skipping analysis tab test in non-interactive mode")
  skip_on_ci_with_display()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app()
    
    # Navigate to Analysis tab
    navigate_to_tab(app, "input_tabs", "Analysis")
    
    # Verify key inputs exist
    expect_true(element_exists(app, "#indicatorsToAnalyse") ||
                element_exists(app, "[data-id='indicatorsToAnalyse']"),
                "Indicator selection should exist")
    
    expect_true(element_exists(app, "#daterange") ||
                element_exists(app, "[data-id='daterange']"),
                "Date range slider should exist")
    
  }, finally = {
    cleanup_app(app)
  })
})

# Test: Output tabs exist
test_that("output tabs have expected structure", {
  skip_if_no_shinytest2()
  skip_if_not(interactive(), "Skipping output tabs test in non-interactive mode")
  skip_on_ci_with_display()
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app()
    
    # Check all output tabs exist by looking for their content areas
    tab_names <- c("Summary", "Map", "Time Series", "Table")
    
    for (tab in tab_names) {
      navigate_to_tab(app, "output_tabs", tab)
      
      # Verify we're on the right tab
      values <- app$get_values()
      expect_equal(values$input$output_tabs, tab,
                   info = paste("Should be on", tab, "tab"))
    }
    
  }, finally = {
    cleanup_app(app)
  })
})

# Test: Snapshot of initial app state (for regression testing)
test_that("b3gbiGUI initial state snapshot", {
  skip_if_no_shinytest2()
  skip_if_not(interactive(), "Skipping snapshot test in non-interactive mode")
  skip_on_ci_with_display()
  
  # This test creates a snapshot that can be compared across runs
  # to detect unexpected UI changes
  
  app <- NULL
  
  tryCatch({
    app <- setup_test_app(variant = shinytest2::platform_variant())
    
    # Take snapshot of initial state
    app$expect_values(
      name = "initial_state",
      input = c("input_tabs", "output_tabs", "gen_options", 
                "map_options", "ts_options"),
      output = FALSE,  # Don't snapshot outputs initially
      export = FALSE
    )
    
  }, finally = {
    cleanup_app(app)
  })
})
