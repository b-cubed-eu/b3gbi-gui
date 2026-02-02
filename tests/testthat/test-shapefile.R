# Tests for shapefile.R module

# Helper to create a minimal shapefile for testing
setup_test_shapefile <- function(temp_dir = tempdir()) {
  # We can't easily create real shapefiles in tests without sf
  # So we'll test the validation and utility functions
  
  # Create a fake .shp file path for testing
  shp_path <- file.path(temp_dir, "test.shp")
  shx_path <- file.path(temp_dir, "test.shx")
  dbf_path <- file.path(temp_dir, "test.dbf")
  
  # Create empty files
  file.create(shp_path)
  file.create(shx_path)
  file.create(dbf_path)
  
  list(shp = shp_path, shx = shx_path, dbf = dbf_path, temp_dir = temp_dir)
}

# Clean up test files
cleanup_test_shapefile <- function(paths) {
  unlink(c(paths$shp, paths$shx, paths$dbf))
}

test_that("load_shapefile_from_zip validates inputs", {
  # Non-existent file
  expect_error(
    load_shapefile_from_zip("/nonexistent/path/file.zip"),
    "ZIP file not found"
  )
  
  # Non-character input
  expect_error(
    load_shapefile_from_zip(123),
    "must be a single character string"
  )
  
  # Multiple strings
  expect_error(
    load_shapefile_from_zip(c("file1.zip", "file2.zip")),
    "must be a single character string"
  )
})

test_that("load_shapefile_from_zip handles invalid ZIP", {
  # Create a fake zip file (not actually a zip)
  tmp <- tempfile(fileext = ".zip")
  writeLines("not a zip file", tmp)
  
  expect_error(
    load_shapefile_from_zip(tmp),
    "Failed to extract ZIP file"
  )
  
  unlink(tmp)
})

test_that("validate_shapefile_components checks files exist", {
  temp_dir <- tempdir()
  
  # Test with non-existent file
  expect_error(
    validate_shapefile_components(file.path(temp_dir, "nonexistent.shp")),
    "Shapefile not found"
  )
  
  # Create minimal shapefile set
  paths <- setup_test_shapefile(temp_dir)
  
  # Should pass with all required components
  expect_true(validate_shapefile_components(paths$shp))
  
  # Test with missing component
  unlink(paths$shx)
  expect_error(
    validate_shapefile_components(paths$shp),
    "missing required components"
  )
  
  cleanup_test_shapefile(paths)
})

test_that("validate_shapefile_components warns about missing projection", {
  paths <- setup_test_shapefile()
  
  # Should warn about missing .prj
  expect_warning(
    validate_shapefile_components(paths$shp),
    "missing .prj file"
  )
  
  cleanup_test_shapefile(paths)
})

test_that("validate_shapefile_components validates input types", {
  expect_error(
    validate_shapefile_components(123),
    "must be a single character string"
  )
  
  expect_error(
    validate_shapefile_components(c("file1.shp", "file2.shp")),
    "must be a single character string"
  )
})

test_that("cleanup_shapefiles removes temporary files", {
  paths <- setup_test_shapefile()
  
  # Verify files exist
  expect_true(file.exists(paths$shp))
  expect_true(file.exists(paths$shx))
  expect_true(file.exists(paths$dbf))
  
  # Clean up
  cleanup_shapefiles(paths$temp_dir, keep_shp = NULL)
  
  # Files should be gone
  expect_false(file.exists(paths$shx))
  expect_false(file.exists(paths$dbf))
  # .shp should also be gone since we didn't keep it
  expect_false(file.exists(paths$shp))
})

test_that("cleanup_shapefiles respects keep_shp parameter", {
  paths <- setup_test_shapefile()
  
  # Clean up but keep the .shp file
  cleanup_shapefiles(paths$temp_dir, keep_shp = paths$shp)
  
  # .shp should still exist
  expect_true(file.exists(paths$shp))
  # But others should be gone
  expect_false(file.exists(paths$shx))
  expect_false(file.exists(paths$dbf))
  
  # Clean up the remaining file
  unlink(paths$shp)
})

test_that("cleanup_shapefiles handles non-existent directories gracefully", {
  # Should not error on non-existent directory
  expect_silent(cleanup_shapefiles("/nonexistent/directory"))
})
