# Tests for taxonomy_filter.R module

# Setup test data
setup_test_cube <- function() {
  structure(list(
    data = data.frame(
      scientificName = c("Pinus sylvestris", "Pinus nigra", "Quercus robur",
                        "Quercus petraea", "Betula pendula"),
      family = c("Pinaceae", "Pinaceae", "Fagaceae", "Fagaceae", "Betulaceae"),
      year = c(2020, 2021, 2020, 2021, 2020),
      x = 1:5,
      y = 6:10,
      stringsAsFactors = FALSE
    ),
    resolutions = "1km",
    first_year = 2020,
    last_year = 2021
  ), class = "processed_cube")
}

test_that("extract_taxonomic_groups returns correct groups", {
  cube <- setup_test_cube()
  
  result <- extract_taxonomic_groups(cube)
  
  expect_type(result, "list")
  expect_named(result, c("families", "species"))
  
  # Check families
  expect_type(result$families, "character")
  expect_equal(length(result$families), 3)
  expect_true(all(c("Betulaceae", "Fagaceae", "Pinaceae") %in% result$families))
  expect_equal(result$families, sort(result$families))  # Should be sorted
  
  # Check species
  expect_type(result$species, "character")
  expect_equal(length(result$species), 5)
  expect_true("Pinus sylvestris" %in% result$species)
})

test_that("extract_taxonomic_groups validates input", {
  expect_error(extract_taxonomic_groups(NULL), "Invalid data cube")
  expect_error(extract_taxonomic_groups("not a cube"), "Invalid data cube")
  expect_error(extract_taxonomic_groups(list(data = data.frame())), "Invalid data cube")
})

test_that("get_species_by_family returns correct species", {
  cube <- setup_test_cube()
  
  # Single family
  result <- get_species_by_family(cube, "Pinaceae")
  expect_type(result, "character")
  expect_equal(length(result), 2)
  expect_true(all(c("Pinus sylvestris", "Pinus nigra") %in% result))
  
  # Multiple families
  result <- get_species_by_family(cube, c("Pinaceae", "Betulaceae"))
  expect_equal(length(result), 3)
  expect_true("Betula pendula" %in% result)
  
  # Empty or NULL families
  result <- get_species_by_family(cube, NULL)
  expect_equal(length(result), 0)
  expect_equal(get_species_by_family(cube, character(0)), character(0))
})

test_that("get_species_by_family handles sorting", {
  cube <- setup_test_cube()
  
  # Default is sorted
  result_sorted <- get_species_by_family(cube, "Fagaceae", sort = TRUE)
  expect_equal(result_sorted, sort(result_sorted))
  
  # Unsorted (though with 2 items, order might coincidentally be sorted)
  result_unsorted <- get_species_by_family(cube, "Fagaceae", sort = FALSE)
  expect_type(result_unsorted, "character")
  expect_equal(length(result_unsorted), 2)
})

test_that("filter_by_taxonomy filters by family", {
  cube <- setup_test_cube()
  
  # Filter by single family
  result <- filter_by_taxonomy(cube, families = "Pinaceae")
  expect_s3_class(result, "processed_cube")
  expect_equal(nrow(result$data), 2)
  expect_true(all(result$data$family == "Pinaceae"))
  
  # Filter by multiple families
  result <- filter_by_taxonomy(cube, families = c("Pinaceae", "Betulaceae"))
  expect_equal(nrow(result$data), 3)
})

test_that("filter_by_taxonomy filters by species", {
  cube <- setup_test_cube()
  
  # Filter by species
  result <- filter_by_taxonomy(cube, species = "Pinus sylvestris")
  expect_equal(nrow(result$data), 1)
  expect_equal(result$data$scientificName, "Pinus sylvestris")
  
  # Multiple species
  result <- filter_by_taxonomy(cube, species = c("Pinus sylvestris", "Betula pendula"))
  expect_equal(nrow(result$data), 2)
})

test_that("filter_by_taxonomy combines family and species filters", {
  cube <- setup_test_cube()
  
  # Both filters - intersection
  result <- filter_by_taxonomy(
    cube,
    families = "Fagaceae",
    species = c("Quercus robur", "Pinus sylvestris")  # Only Quercus is in Fagaceae
  )
  expect_equal(nrow(result$data), 1)
  expect_equal(result$data$scientificName, "Quercus robur")
})

test_that("filter_by_taxonomy handles empty filters", {
  cube <- setup_test_cube()
  
  # No filters - returns original
  result <- filter_by_taxonomy(cube)
  expect_equal(nrow(result$data), 5)
  
  # Empty vectors
  result <- filter_by_taxonomy(cube, families = character(0))
  expect_equal(nrow(result$data), 5)
})

test_that("filter_by_taxonomy can return data frame only", {
  cube <- setup_test_cube()
  
  result <- filter_by_taxonomy(cube, families = "Pinaceae", preserve_cube = FALSE)
  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "processed_cube"))
  expect_equal(nrow(result), 2)
})

test_that("get_taxonomic_summary calculates correct statistics", {
  cube <- setup_test_cube()
  
  stats <- get_taxonomic_summary(cube)
  
  expect_type(stats, "list")
  expect_equal(stats$n_families, 3)
  expect_equal(stats$n_species, 5)
  expect_equal(stats$n_observations, 5)
  
  # Check top families structure
  expect_s3_class(stats$top_families, "data.frame")
  expect_named(stats$top_families, c("family", "n"))
  
  # Check top species structure
  expect_s3_class(stats$top_species, "data.frame")
  expect_named(stats$top_species, c("scientificName", "n"))
})

test_that("get_taxonomic_summary handles empty data", {
  cube <- setup_test_cube()
  empty_data <- cube$data[0, ]
  
  stats <- get_taxonomic_summary(cube, filtered_data = empty_data)
  
  expect_equal(stats$n_families, 0)
  expect_equal(stats$n_species, 0)
  expect_equal(stats$n_observations, 0)
})

test_that("get_taxonomic_summary handles NULL filtered_data", {
  cube <- setup_test_cube()
  
  # Should use cube$data when filtered_data is NULL
  stats <- get_taxonomic_summary(cube, filtered_data = NULL)
  expect_equal(stats$n_observations, 5)
})

test_that("validate_taxonomic_selections warns on invalid selections", {
  cube <- setup_test_cube()
  
  # Valid selections
  expect_silent(validate_taxonomic_selections(cube, "Pinaceae", "Pinus sylvestris"))
  
  # Invalid family
  expect_warning(
    validate_taxonomic_selections(cube, "Invalidaceae"),
    "Invalid family selections"
  )
  
  # Invalid species
  expect_warning(
    validate_taxonomic_selections(cube, species = "Invalidus nonexistus"),
    "Invalid species selections"
  )
  
  # Both invalid
  expect_warning(
    validate_taxonomic_selections(cube, "BadFamily", "BadSpecies"),
    "Invalid family selections"
  )
})

test_that("validate_taxonomic_selections handles NULL inputs", {
  cube <- setup_test_cube()
  
  # NULL inputs should not error
  expect_true(validate_taxonomic_selections(cube, NULL, NULL))
  expect_true(validate_taxonomic_selections(cube, character(0), character(0)))
})

test_that("reset_taxonomic_filters returns cube unchanged", {
  cube <- setup_test_cube()
  
  result <- reset_taxonomic_filters(cube)
  expect_identical(result, cube)
})

test_that("reset_taxonomic_filters validates input", {
  expect_error(reset_taxonomic_filters(NULL), "Invalid data cube")
  expect_error(reset_taxonomic_filters("not a cube"), "Invalid data cube")
})
