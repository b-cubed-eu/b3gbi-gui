# Tests for data_access.R module

test_that("get_data_cube returns correct data", {
  r <- shiny::reactiveValues(dataCube = list(test = "original"))
  
  result <- get_data_cube(r)
  expect_equal(result$test, "original")
})

test_that("get_data_cube validates input", {
  expect_error(get_data_cube(NULL), "must be a ReactiveValues object")
  expect_error(get_data_cube(list()), "must be a ReactiveValues object")
  expect_error(get_data_cube("not reactive"), "must be a ReactiveValues object")
})

test_that("get_filtered_data_cube prefers filtered data", {
  r <- shiny::reactiveValues(
    dataCube = list(test = "original"),
    dataCube1 = list(test = "filtered")
  )
  
  result <- get_filtered_data_cube(r)
  expect_equal(result$test, "filtered")
})

test_that("get_filtered_data_cube falls back to original", {
  r <- shiny::reactiveValues(
    dataCube = list(test = "original"),
    dataCube1 = NULL
  )
  
  result <- get_filtered_data_cube(r)
  expect_equal(result$test, "original")
})

test_that("get_cube_data returns data frame", {
  r <- shiny::reactiveValues(
    dataCube = list(data = data.frame(x = 1:3, y = 4:6))
  )
  
  result <- get_cube_data(r, filtered = FALSE)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("get_cube_data returns NULL when no data", {
  r <- shiny::reactiveValues(dataCube = NULL)
  
  expect_null(get_cube_data(r))
})

test_that("set_data_cube stores data correctly", {
  r <- shiny::reactiveValues()
  cube <- structure(list(data = data.frame(x = 1)), class = "processed_cube")
  
  set_data_cube(r, cube)
  
  expect_identical(r$dataCube, cube)
  expect_identical(r$dataCube1, cube)  # Should also set filtered copy
})

test_that("set_data_cube validates input", {
  r <- shiny::reactiveValues()
  
  expect_error(set_data_cube(NULL, list()), "must be a ReactiveValues object")
  expect_error(set_data_cube(r, NULL), "must be a processed_cube object")
  expect_error(set_data_cube(r, "not a cube"), "must be a processed_cube object")
})

test_that("update_filtered_data updates correctly", {
  r <- shiny::reactiveValues(
    dataCube1 = structure(
      list(data = data.frame(x = 1:3)),
      class = "processed_cube"
    )
  )
  
  new_data <- data.frame(x = 4:6)
  update_filtered_data(r, new_data)
  
  expect_equal(r$dataCube1$data, new_data)
})

test_that("update_filtered_data validates input", {
  r <- shiny::reactiveValues()
  
  expect_error(update_filtered_data(NULL, data.frame()), "must be a ReactiveValues object")
  expect_error(update_filtered_data(r, NULL), "must be a data frame")
  expect_error(update_filtered_data(r, "not data"), "must be a data frame")
  expect_error(update_filtered_data(r, data.frame()), "Original data cube not set")
})

test_that("clear_data_cubes clears all data", {
  r <- shiny::reactiveValues(
    dataCube = list(test = "original"),
    dataCube1 = list(test = "filtered")
  )
  
  clear_data_cubes(r)
  
  expect_null(r$dataCube)
  expect_null(r$dataCube1)
})

test_that("clear_data_cubes can keep original", {
  r <- shiny::reactiveValues(
    dataCube = list(test = "original"),
    dataCube1 = list(test = "filtered")
  )
  
  clear_data_cubes(r, keep_original = TRUE)
  
  expect_equal(r$dataCube$test, "original")
  expect_null(r$dataCube1)
})

test_that("clear_data_cubes validates input", {
  expect_error(clear_data_cubes(NULL), "must be a ReactiveValues object")
})

test_that("is_data_loaded detects loaded data", {
  # No data
  r <- shiny::reactiveValues()
  expect_false(is_data_loaded(r))
  expect_false(is_data_loaded(r, check_filtered = TRUE))
  
  # Original only
  r$dataCube <- list(test = "data")
  expect_true(is_data_loaded(r))
  expect_false(is_data_loaded(r, check_filtered = TRUE))
  
  # Both
  r$dataCube1 <- list(test = "filtered")
  expect_true(is_data_loaded(r))
  expect_true(is_data_loaded(r, check_filtered = TRUE))
})

test_that("is_data_loaded handles invalid input", {
  expect_false(is_data_loaded(NULL))
  expect_false(is_data_loaded("not reactive"))
})

test_that("get_data_status returns complete status", {
  r <- shiny::reactiveValues(
    dataCube = structure(
      list(data = data.frame(x = 1:10)),
      class = "processed_cube"
    ),
    dataCube1 = structure(
      list(data = data.frame(x = 1:5)),
      class = "processed_cube"
    )
  )
  
  status <- get_data_status(r)
  
  expect_type(status, "list")
  expect_named(status, c("loaded", "has_filtered", "n_rows_original", "n_rows_filtered", "is_filtered"))
  
  expect_true(status$loaded)
  expect_true(status$has_filtered)
  expect_equal(status$n_rows_original, 10)
  expect_equal(status$n_rows_filtered, 5)
  expect_true(status$is_filtered)
})

test_that("get_data_status detects unfiltered data", {
  r <- shiny::reactiveValues(
    dataCube = structure(
      list(data = data.frame(x = 1:10)),
      class = "processed_cube"
    ),
    dataCube1 = structure(
      list(data = data.frame(x = 1:10)),  # Same as original
      class = "processed_cube"
    )
  )
  
  status <- get_data_status(r)
  expect_false(status$is_filtered)  # Same row count = not filtered
})

test_that("get_data_status handles empty state", {
  r <- shiny::reactiveValues()
  
  status <- get_data_status(r)
  
  expect_false(status$loaded)
  expect_false(status$has_filtered)
  expect_equal(status$n_rows_original, 0)
  expect_equal(status$n_rows_filtered, 0)
  expect_false(status$is_filtered)
})

test_that("get_data_status validates input", {
  expect_error(get_data_status(NULL), "must be a ReactiveValues object")
  expect_error(get_data_status(list()), "must be a ReactiveValues object")
})

test_that("initialize_data_state creates proper structure", {
  r <- initialize_data_state()
  
  expect_true(inherits(r, "ReactiveValues"))
  expect_null(r$dataCube)
  expect_null(r$dataCube1)
})
