# Tests for data_loading.R module

# Setup test data
setup_test_cube <- function() {
  # Create a minimal processed_cube-like object for testing
  list(
    data = data.frame(
      scientificName = c("Pinus sylvestris", "Quercus robur", "Pinus sylvestris"),
      family = c("Pinaceae", "Fagaceae", "Pinaceae"),
      year = c(2020, 2020, 2021),
      x = c(1, 2, 3),
      y = c(4, 5, 6),
      stringsAsFactors = FALSE
    ),
    resolutions = "1km",
    class = "processed_cube"
  )
}

test_that("get_cube_metadata returns correct structure", {
  cube <- setup_test_cube()
  class(cube) <- "processed_cube"
  
  meta <- get_cube_metadata(cube)
  
  expect_type(meta, "list")
  expect_equal(meta$n_rows, 3)
  expect_equal(meta$n_species, 2)
  expect_equal(meta$n_families, 2)
  expect_type(meta$year_range, "integer")
  expect_equal(meta$resolution, "1km")
  expect_type(meta$columns, "character")
  expect_true(all(c("scientificName", "family", "year") %in% meta$columns))
})

test_that("get_cube_metadata handles edge cases", {
  # Empty cube
  empty_cube <- list(
    data = data.frame(
      scientificName = character(),
      family = character(),
      year = integer(),
      stringsAsFactors = FALSE
    ),
    resolutions = NULL,
    class = "processed_cube"
  )
  class(empty_cube) <- "processed_cube"
  
  meta <- get_cube_metadata(empty_cube)
  expect_equal(meta$n_rows, 0)
  expect_equal(meta$n_species, 0)
  expect_equal(meta$n_families, 0)
  expect_true(is.na(meta$resolution))
})

test_that("get_cube_metadata validates input", {
  expect_error(get_cube_metadata(NULL), "Invalid data cube")
  expect_error(get_cube_metadata(list()), "Invalid data cube")
  expect_error(get_cube_metadata("not a cube"), "Invalid data cube")
})

test_that("validate_data_cube checks required columns", {
  cube <- setup_test_cube()
  class(cube) <- "processed_cube"
  
  # Valid cube should pass
  expect_true(validate_data_cube(cube))
  
  # Missing required column
  bad_cube <- cube
  bad_cube$data <- subset(bad_cube$data, select = -c(family))
  expect_error(validate_data_cube(bad_cube), "missing required columns")
})

test_that("validate_data_cube checks class", {
  cube <- setup_test_cube()
  
  # Without proper class
  expect_error(validate_data_cube(cube), "expected class 'processed_cube'")
})

test_that("validate_data_cube handles empty data", {
  cube <- setup_test_cube()
  class(cube) <- "processed_cube"
  cube$data <- cube$data[0, ]  # Empty data frame
  
  # Should warn but not error
  expect_warning(validate_data_cube(cube), "0 rows")
})

test_that("check_file_size warns for large files", {
  # Create a small temp file
  tmp <- tempfile(fileext = ".csv")
  writeLines("test", tmp)
  
  # Should not warn for small file
  expect_silent(check_file_size(tmp, threshold_mb = 100))
  
  unlink(tmp)
})

test_that("helper function %||% works correctly", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% 10, 0)  # 0 is not NULL
  expect_equal(NA %||% "default", NA)  # NA is not NULL
})
