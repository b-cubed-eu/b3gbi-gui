# shinytest2 Helper Functions for b3gbiGUI Integration Testing
# 
# This file provides utility functions for integration testing with shinytest2.
# These helpers are loaded automatically by testthat before running tests.
#

# Skip tests if shinytest2 is not available
skip_if_no_shinytest2 <- function() {
  if (!requireNamespace("shinytest2", quietly = TRUE)) {
    skip("shinytest2 package not available")
  }
}

# Skip tests on CI if needed (shiny apps need a display)
skip_on_ci_with_display <- function() {
  if (identical(Sys.getenv("CI"), "true") && 
      !identical(Sys.getenv("SHINYTEST2_DISPLAY"), "true")) {
    skip("Skipping shinytest2 tests on CI without display")
  }
}

# Create a test Shiny app driver with default options
#' @param app_dir Directory containing the Shiny app (defaults to package root)
#' @param variant Optional variant for snapshots (e.g., "mac", "linux", "windows")
#' @param ... Additional arguments passed to AppDriver$new()
#' @return AppDriver object
setup_test_app <- function(app_dir = system.file("app", package = "b3gbiGUI"), 
                           variant = NULL, ...) {
  skip_if_no_shinytest2()
  
  # If running in development, use root directory
  if (app_dir == "" || !dir.exists(app_dir)) {
    app_dir <- rprojroot::find_root(rprojroot::is_r_package)
  }
  
  # Check app.R exists
  if (!file.exists(file.path(app_dir, "app.R"))) {
    stop("app.R not found in: ", app_dir)
  }
  
  shinytest2::AppDriver$new(
    app_dir = app_dir,
    variant = variant,
    timeout = 30 * 1000,  # 30 second timeout
    wait = TRUE,
    ...
  )
}

# Alternative: setup app from apps directory in tests
setup_test_app_from_tests <- function(app_name = "b3gbi_app", 
                                      variant = NULL, ...) {
  skip_if_no_shinytest2()
  
  app_dir <- file.path("apps", app_name)
  
  if (!dir.exists(app_dir)) {
    stop("Test app not found: ", app_dir)
  }
  
  shinytest2::AppDriver$new(
    app_dir = app_dir,
    variant = variant,
    timeout = 30 * 1000,
    wait = TRUE,
    ...
  )
}

# Test data paths
get_test_data_path <- function(filename = NULL) {
  root <- rprojroot::find_root(rprojroot::is_r_package)
  data_dir <- file.path(root, "data")
  
  if (is.null(filename)) {
    return(data_dir)
  }
  
  file.path(data_dir, filename)
}

# Get path to example cube data for testing
get_example_cube_path <- function(which = 1) {
  get_test_data_path(sprintf("example_cube_%d.rda", which))
}

# Load example cube data for use in tests
load_example_cube <- function(which = 1) {
  path <- get_example_cube_path(which)
  if (!file.exists(path)) {
    stop("Example cube not found: ", path)
  }
  
  env <- new.env()
  load(path, envir = env)
  
  # Return the first object found (should be the cube)
  obj_names <- ls(env)
  if (length(obj_names) == 0) {
    stop("No objects found in ", path)
  }
  
  get(obj_names[1], envir = env)
}

# Create a minimal test cube suitable for testing
#' @param n_rows Number of rows in the test cube
#' @param n_species Number of unique species
#' @param year_range Range of years to include
#' @return processed_cube object
make_test_cube <- function(n_rows = 100, 
                          n_species = 10,
                          year_range = c(2000, 2023)) {
  
  species <- paste0("Species_", 1:n_species)
  families <- paste0("Family_", sample(1:5, n_species, replace = TRUE))
  
  data <- data.frame(
    scientificName = sample(species, n_rows, replace = TRUE),
    family = sample(families, n_rows, replace = TRUE),
    year = sample(year_range[1]:year_range[2], n_rows, replace = TRUE),
    x = runif(n_rows, -10, 10),
    y = runif(n_rows, -10, 10),
    stringsAsFactors = FALSE
  )
  
  # Create structure similar to b3gbi processed cube
  cube <- structure(
    list(
      data = data,
      resolutions = "1km"
    ),
    class = "processed_cube"
  )
  
  cube
}

# Wait for a condition with timeout
#' @param app AppDriver object
#' @param condition Function that returns TRUE when condition is met
#' @param timeout Maximum time to wait in seconds
#' @param interval Check interval in seconds
#' @return TRUE if condition was met, FALSE if timed out
wait_for_condition <- function(app, condition, timeout = 10, interval = 0.5) {
  start_time <- Sys.time()
  
  while (as.numeric(difftime(Sys.time(), start_time, units = "secs")) < timeout) {
    if (condition(app)) {
      return(TRUE)
    }
    Sys.sleep(interval)
  }
  
  FALSE
}

# Wait for output to be rendered
#' @param app AppDriver object
#' @param output_id Output ID to wait for
#' @param timeout Maximum wait time in seconds
wait_for_output <- function(app, output_id, timeout = 10) {
  wait_for_condition(
    app,
    function(a) {
      tryCatch({
        val <- a$get_value(output = output_id)
        !is.null(val) && length(val) > 0
      }, error = function(e) FALSE)
    },
    timeout = timeout
  )
}

# Wait for Shiny to be idle (no reactive updates pending)
#' @param app AppDriver object
#' @param timeout Maximum wait time in seconds
#' @param duration How long to wait after becoming idle (in ms)
wait_for_idle <- function(app, timeout = 10, duration = 500) {
  app$wait_for_idle(timeout = timeout * 1000, duration = duration)
}

# Utility: Take a snapshot with descriptive name
#' @param app AppDriver object
#' @param name Descriptive name for the snapshot
#' @param ... Additional arguments passed to expect_values()
take_snapshot <- function(app, name, ...) {
  app$expect_values(name = name, ...)
}

# Clean up app driver after test
cleanup_app <- function(app) {
  if (!is.null(app)) {
    tryCatch({
      app$stop()
    }, error = function(e) {
      # Ignore cleanup errors
    })
  }
}

# Integration test helper: Simulate data cube upload
#' @param app AppDriver object
#' @param cube_path Path to cube file to upload
#' @param wait Whether to wait for processing
simulate_cube_upload <- function(app, cube_path = NULL, wait = TRUE) {
  if (is.null(cube_path)) {
    # Use a minimal test cube
    cube_path <- tempfile(pattern = "test_cube_", fileext = ".csv")
    cube <- make_test_cube(n_rows = 50)
    write.csv(cube$data, cube_path, row.names = FALSE)
    on.exit(unlink(cube_path))
  }
  
  # Upload the file
  app$upload_file(dataCube = cube_path)
  
  if (wait) {
    wait_for_idle(app, timeout = 5)
  }
  
  invisible(app)
}

# Integration test helper: Set common analysis inputs
#' @param app AppDriver object
#' @param indicator Indicator to select
#' @param year_range Year range as c(min, max)
set_analysis_inputs <- function(app, indicator = NULL, year_range = NULL) {
  
  if (!is.null(indicator)) {
    app$set_inputs(indicatorsToAnalyse = indicator)
  }
  
  if (!is.null(year_range)) {
    app$set_inputs(daterange = year_range)
  }
  
  wait_for_idle(app, timeout = 3)
  invisible(app)
}

# Integration test helper: Navigate to a specific tab
#' @param app AppDriver object
#' @param tab_id Tab ID to navigate to (input_tabs or output_tabs)
#' @param panel_name Panel name to select	navigate_to_tab <- function(app, tab_id = "input_tabs", panel_name) {
  app$set_inputs(!!tab_id := panel_name)
  wait_for_idle(app, timeout = 2)
  invisible(app)
}

# Check if an element exists in the UI
#' @param app AppDriver object
#' @param selector CSS selector for the element
element_exists <- function(app, selector) {
  result <- tryCatch({
    app$get_html(selector)
    TRUE
  }, error = function(e) FALSE)
  
  result
}

# Get UI element text safely
#' @param app AppDriver object
#' @param selector CSS selector
#' @return Text content or NULL if not found
get_element_text <- function(app, selector) {
  tryCatch({
    html <- app$get_html(selector)
    if (length(html) > 0) {
      # Extract text from HTML
      xml2::xml_text(xml2::read_html(html))
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

# Test if app is ready for interaction
#' @param app AppDriver object
#' @return TRUE if app is ready
app_is_ready <- function(app) {
  tryCatch({
    # Try to get the title element as a basic check
    html <- app$get_html("title")
    grepl("B-Cubed", html) || grepl("Biodiversity", html)
  }, error = function(e) FALSE)
}

# Run a basic smoke test on the app
#' @param app AppDriver object
#' @return TRUE if smoke test passed
run_smoke_test <- function(app) {
  # Check app loads
  if (!app_is_ready(app)) {
    message("App failed to load properly")
    return(FALSE)
  }
  
  # Check basic elements exist
  checks <- list(
    title = element_exists(app, ".navbar-header") || 
            element_exists(app, "title"),
    sidebar = element_exists(app, ".sidebar-panel") || 
              element_exists(app, ".col-sm-3"),
    main_panel = element_exists(app, ".main-panel") || 
                 element_exists(app, ".col-sm-9")
  )
  
  if (!all(unlist(checks))) {
    message("Some UI elements missing: ", 
            paste(names(checks)[!unlist(checks)], collapse = ", "))
    return(FALSE)
  }
  
  TRUE
}

# Teardown: Clean up any test apps
withr::defer({
  # Clean up any temporary test data files
  temp_pattern <- "^test_cube_"
  temp_files <- list.files(tempdir(), pattern = temp_pattern, full.names = TRUE)
  if (length(temp_files) > 0) {
    unlink(temp_files)
  }
}, envir = parent.frame())
