# Test helper functions for b3gbiGUI

# Set test environment options
options(testthat.progress.max_fails = 100)
options(testthat.progress.show_failures = TRUE)

# Helper function to create temporary test data
create_test_cube <- function(n_rows = 100) {
  data.frame(
    scientificName = rep(paste0("Species ", 1:10), each = n_rows / 10),
    family = rep(paste0("Family ", 1:5), each = n_rows / 5),
    year = sample(2000:2023, n_rows, replace = TRUE),
    x = runif(n_rows, -10, 10),
    y = runif(n_rows, -10, 10),
    stringsAsFactors = FALSE
  )
}

# Helper to check if required packages are available
skip_if_missing_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      skip(paste(pkg, "not available"))
    }
  }
}

# Clean up temporary files after tests
cleanup_temp_files <- function() {
  temp_files <- list.files(tempdir(), pattern = "^test_", full.names = TRUE)
  if (length(temp_files) > 0) {
    unlink(temp_files, recursive = TRUE)
  }
}

# Run cleanup after all tests
teardown({
  cleanup_temp_files()
})
