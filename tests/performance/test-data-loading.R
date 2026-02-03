# Data Loading Performance Tests
# Track H.3 - Performance Testing Infrastructure
#
# This script benchmarks data loading operations including cube loading
# with various file sizes, shapefile processing, and memory profiling.

# Source performance utilities
source("tests/performance/helper-performance.R")

# Source all modules
module_files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
for (file in module_files) {
  source(file)
}

#' Run All Data Loading Tests
#'
#' @description
#' Main function that runs all data loading performance tests.
#'
#' @param save_results Logical. Whether to save results
#' @param iterations Integer. Number of iterations per test
#' @param compare_baseline Logical. Whether to compare with pre-modularization
#'
#' @return List of test results
run_data_loading_tests <- function(save_results = TRUE,
                                   iterations = 3,
                                   compare_baseline = FALSE) {
  cat("=== Starting Data Loading Performance Tests ===\n\n")

  all_results <- list()

  # 1. Benchmark data cube loading
  cat("1. Benchmarking data cube loading...\n")
  all_results <- c(all_results, benchmark_cube_loading(iterations))

  # 2. Benchmark shapefile processing
  cat("2. Benchmarking shapefile processing...\n")
  all_results <- c(all_results, benchmark_shapefile_loading(iterations))

  # 3. Memory profiling during operations
  cat("3. Running memory profiling...\n")
  all_results <- c(all_results, profile_memory_usage(iterations))

  # 4. Test with various file sizes
  cat("4. Testing with various file sizes...\n")
  all_results <- c(all_results, test_various_file_sizes(iterations))

  # 5. Compare pre/post modularization if requested
  if (compare_baseline) {
    cat("5. Comparing modularization performance...\n")
    all_results <- c(all_results, compare_modularization_impact(iterations))
  }

  # Save results
  if (save_results) {
    save_benchmark_results(all_results, tag = "data_loading")
  }

  cat("\n=== Data Loading Tests Complete ===\n")
  all_results
}


#' Benchmark Data Cube Loading
#'
#' @description
#' Benchmarks loading data cubes from CSV files with different sizes.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of benchmark results
benchmark_cube_loading <- function(iterations = 3) {
  results <- list()
  temp_dir <- tempdir()

  # Define test sizes
  test_sizes <- list(
    tiny = list(n_rows = 100, desc = "100 rows"),
    small = list(n_rows = 1000, desc = "1K rows"),
    medium = list(n_rows = 10000, desc = "10K rows"),
    large = list(n_rows = 50000, desc = "50K rows"),
    xlarge = list(n_rows = 100000, desc = "100K rows")
  )

  for (size_name in names(test_sizes)) {
    size <- test_sizes[[size_name]]

    # Generate test data
    test_data <- generate_sample_cube(
      n_rows = size$n_rows,
      n_species = min(100, size$n_rows / 10),
      n_families = min(20, size$n_rows / 50)
    )

    temp_file <- file.path(temp_dir, paste0("cube_", size_name, ".csv"))
    utils::write.csv(test_data, temp_file, row.names = FALSE)

    # Benchmark CSV read
    result <- comprehensive_benchmark({
      data <- utils::read.csv(temp_file, stringsAsFactors = FALSE)
      # Basic validation
      stopifnot("scientificName" %in% names(data))
      stopifnot("family" %in% names(data))
      stopifnot("year" %in% names(data))
    }, name = paste0("cube_load_csv_", size_name, "_", size$desc),
    iterations = iterations)

    results[[length(results) + 1]] <- result

    # Benchmark with file size check
    result <- comprehensive_benchmark({
      check_file_size(temp_file, threshold_mb = 100)
      data <- utils::read.csv(temp_file, stringsAsFactors = FALSE)
    }, name = paste0("cube_load_with_check_", size_name, "_", size$desc),
    iterations = iterations)

    results[[length(results) + 1]] <- result

    # Get actual file size
    file_size <- file.size(temp_file)
    file_size_mb <- round(file_size / (1024^2), 2)

    cat(sprintf("  - %s: %s (%.2f MB)\n", size_name, size$desc, file_size_mb))

    # Cleanup
    unlink(temp_file)
  }

  results
}


#' Benchmark Shapefile Loading
#'
#' @description
#' Benchmarks shapefile processing operations.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of benchmark results
benchmark_shapefile_loading <- function(iterations = 3) {
  results <- list()

  # Check if sf package available
  if (!requireNamespace("sf", quietly = TRUE)) {
    cat("  - Skipping (sf package not available)\n")
    return(results)
  }

  temp_dir <- tempdir()

  # Create test shapefiles of different sizes
  test_sizes <- c(10, 100, 500)

  for (n_features in test_sizes) {
    # Generate test shapefile
    test_sf <- sf::st_sf(
      id = 1:n_features,
      name = paste0("region_", 1:n_features),
      area = runif(n_features, 1, 1000),
      geometry = sf::st_sfc(
        lapply(1:n_features, function(i) {
          # Create simple polygons
          offset <- i * 2
          sf::st_polygon(list(matrix(c(
            0 + offset, 0 + offset,
            1 + offset, 0 + offset,
            1 + offset, 1 + offset,
            0 + offset, 1 + offset,
            0 + offset, 0 + offset
          ), ncol = 2, byrow = TRUE)))
        })
      )
    )

    temp_shp <- file.path(temp_dir, paste0("test_", n_features, ".shp"))
    sf::write_sf(test_sf, temp_shp)

    # Benchmark read
    result <- comprehensive_benchmark({
      data <- sf::read_sf(temp_shp)
    }, name = paste0("shapefile_read_", n_features, "features"),
    iterations = iterations)

    results[[length(results) + 1]] <- result

    # Benchmark with validation
    result <- comprehensive_benchmark({
      validate_shapefile_components(temp_shp)
      data <- sf::read_sf(temp_shp)
    }, name = paste0("shapefile_read_validate_", n_features, "features"),
    iterations = iterations)

    results[[length(results) + 1]] <- result

    # Benchmark geometry validation
    result <- comprehensive_benchmark({
      data <- sf::read_sf(temp_shp)
      invalid <- sum(!sf::st_is_valid(data))
    }, name = paste0("shapefile_geom_validate_", n_features, "features"),
    iterations = iterations)

    results[[length(results) + 1]] <- result

    cat(sprintf("  - Tested %d features\n", n_features))
  }

  # Test ZIP extraction
  zip_file <- file.path(temp_dir, "test_shapefile.zip")

  # Create a ZIP with shapefile
  shp_files <- list.files(temp_dir, pattern = "test_10\\.", full.names = TRUE)
  utils::zip(zip_file, shp_files, flags = "-j")

  result <- comprehensive_benchmark({
    load_shapefile_from_zip(zip_file)
  }, name = "shapefile_zip_extraction",
  iterations = max(1, iterations - 1))

  results[[length(results) + 1]] <- result

  cat("  - Tested ZIP extraction\n")

  # Cleanup
  cleanup_shapefiles(temp_dir)
  unlink(zip_file)

  results
}


#' Profile Memory Usage
#'
#' @description
#' Profiles memory usage during data operations.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of memory profile results
profile_memory_usage <- function(iterations = 3) {
  results <- list()

  # Test 1: Memory during data loading
  temp_file <- tempfile(fileext = ".csv")
  test_data <- generate_sample_cube(n_rows = 50000)
  utils::write.csv(test_data, temp_file, row.names = FALSE)

  mem_profile <- memory_profile({
    data <- utils::read.csv(temp_file, stringsAsFactors = FALSE)
    result <- aggregate(y ~ family, data = data, FUN = mean)
    rm(data)
    gc()
  }, name = "memory_data_load_and_agg")

  results[[length(results) + 1]] <- list(
    name = mem_profile$name,
    timing = list(elapsed = 0, user = 0, system = 0),
    memory = mem_profile,
    combined_score = mem_profile$memory_used_mb,
    timestamp = Sys.time()
  )

  # Test 2: Memory during filtering operations
  mem_profile <- memory_profile({
    data <- generate_sample_cube(n_rows = 50000)
    filtered <- data[data$family == "Family_1", ]
    aggregated <- aggregate(y ~ year, data = filtered, FUN = sum)
    rm(data, filtered)
    gc()
  }, name = "memory_filter_operations")

  results[[length(results) + 1]] <- list(
    name = mem_profile$name,
    timing = list(elapsed = 0, user = 0, system = 0),
    memory = mem_profile,
    combined_score = mem_profile$memory_used_mb,
    timestamp = Sys.time()
  )

  # Test 3: Memory leak detection (multiple operations)
  mem_before <- sum(gc(reset = TRUE)[, 2])

  for (i in 1:10) {
    data <- generate_sample_cube(n_rows = 10000)
    result <- aggregate(y ~ family, data = data, FUN = mean)
    rm(data, result)
  }

  gc_result <- gc(reset = FALSE)
  mem_after <- sum(gc_result[, 2])
  mem_diff <- mem_after - mem_before

  results[[length(results) + 1]] <- list(
    name = "memory_leak_detection",
    timing = list(elapsed = 0, user = 0, system = 0),
    memory = list(
      name = "memory_leak_detection",
      baseline_mb = mem_before,
      peak_mb = mem_after,
      memory_used_mb = max(0, mem_diff),
      n_cells_alloc = gc_result[1, 1],
      n_vcells_alloc = gc_result[2, 1],
      timestamp = Sys.time()
    ),
    combined_score = max(0, mem_diff),
    timestamp = Sys.time()
  )

  cat(sprintf("  - Memory leak check: %.2f MB change after 10 iterations\n",
              max(0, mem_diff)))

  unlink(temp_file)
  results
}


#' Test Various File Sizes
#'
#' @description
#' Tests loading performance with files of various sizes.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of test results
test_various_file_sizes <- function(iterations = 3) {
  results <- list()
  temp_dir <- tempdir()

  # Create files of increasing sizes
  file_specs <- list(
    list(rows = 1000, cols = 5, desc = "small_5col"),
    list(rows = 1000, cols = 20, desc = "small_20col"),
    list(rows = 10000, cols = 5, desc = "medium_5col"),
    list(rows = 10000, cols = 20, desc = "medium_20col"),
    list(rows = 50000, cols = 5, desc = "large_5col"),
    list(rows = 50000, cols = 20, desc = "large_20col")
  )

  for (spec in file_specs) {
    # Generate data with specified columns
    base_data <- generate_sample_cube(n_rows = spec$rows)

    # Add extra columns if needed
    if (spec$cols > 5) {
      for (i in 6:spec$cols) {
        base_data[[paste0("extra_col_", i)]] <- runif(spec$rows)
      }
    }

    temp_file <- file.path(temp_dir, paste0("test_", spec$desc, ".csv"))
    utils::write.csv(base_data, temp_file, row.names = FALSE)

    file_size_mb <- round(file.size(temp_file) / (1024^2), 2)

    # Benchmark loading
    result <- comprehensive_benchmark({
      data <- utils::read.csv(temp_file, stringsAsFactors = FALSE)
      # Access all columns to ensure full load
      for (col in names(data)) {
        tmp <- data[[col]]
      }
    }, name = paste0("filesize_", spec$desc, "_", file_size_mb, "MB"),
    iterations = iterations)

    results[[length(results) + 1]] <- result

    cat(sprintf("  - %s: %d rows x %d cols (%.2f MB)\n",
                spec$desc, spec$rows, spec$cols, file_size_mb))

    unlink(temp_file)
  }

  results
}


#' Compare Modularization Impact
#'
#' @description
#' Compares data loading performance before and after modularization.
#' Simulates both approaches to measure improvement/regression.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of comparison results
compare_modularization_impact <- function(iterations = 3) {
  results <- list()
  temp_dir <- tempdir()

  # Generate test data
  test_data <- generate_sample_cube(n_rows = 10000)
  temp_file <- file.path(temp_dir, "mod_comparison.csv")
  utils::write.csv(test_data, temp_file, row.names = FALSE)

  # "Pre-modularization" approach (monolithic loading)
  result_legacy <- comprehensive_benchmark({
    # Direct read without module functions
    data <- utils::read.csv(temp_file, stringsAsFactors = FALSE)

    # Manual validation (no helper functions)
    required_cols <- c("scientificName", "family", "year")
    for (col in required_cols) {
      if (!(col %in% names(data))) {
        stop(paste("Missing column:", col))
      }
    }

    # Manual metadata extraction
    metadata <- list(
      n_rows = nrow(data),
      n_species = length(unique(data$scientificName)),
      n_families = length(unique(data$family))
    )

    list(data = data, metadata = metadata)
  }, name = "loading_pre_modular", iterations = iterations)

  results[[length(results) + 1]] <- result_legacy

  # "Post-modularization" approach (using module functions)
  result_modular <- comprehensive_benchmark({
    # Use modular functions
    cube <- load_data_cube(temp_file, validate = TRUE)
    meta <- get_cube_metadata(cube)

    list(cube = cube, metadata = meta)
  }, name = "loading_post_modular", iterations = iterations)

  results[[length(results) + 1]] <- result_modular

  # Calculate improvement
  time_diff <- result_legacy$timing$elapsed - result_modular$timing$elapsed
  time_pct <- (time_diff / result_legacy$timing$elapsed) * 100

  mem_diff <- result_legacy$memory$memory_used_mb - result_modular$memory$memory_used_mb
  mem_pct <- (mem_diff / result_legacy$memory$memory_used_mb) * 100

  cat(sprintf("  - Time difference: %.3fs (%.1f%% %s)\n",
              abs(time_diff), abs(time_pct),
              ifelse(time_diff > 0, "faster", "slower")))
  cat(sprintf("  - Memory difference: %.2fMB (%.1f%% %s)\n",
              abs(mem_diff), abs(mem_pct),
              ifelse(mem_diff > 0, "less", "more")))

  # Cleanup
  unlink(temp_file)

  results
}


#' Print Data Loading Summary
#'
#' @description
#' Prints a formatted summary of data loading test results.
#'
#' @param results List of test results
print_data_loading_summary <- function(results) {
  cat("\n=== Data Loading Performance Summary ===\n\n")

  # Group by test type
  test_types <- list()

  for (result in results) {
    if (grepl("^cube_load", result$name)) {
      type <- "Cube Loading"
    } else if (grepl("^shapefile", result$name)) {
      type <- "Shapefile Processing"
    } else if (grepl("^memory", result$name)) {
      type <- "Memory Profiling"
    } else if (grepl("^filesize", result$name)) {
      type <- "File Size Tests"
    } else if (grepl("^loading", result$name)) {
      type <- "Modularization Comparison"
    } else {
      type <- "Other"
    }

    if (is.null(test_types[[type]])) {
      test_types[[type]] <- list()
    }
    test_types[[type]][[length(test_types[[type]]) + 1]] <- result
  }

  # Print each type
  for (type_name in names(test_types)) {
    cat(type_name, "\n")
    cat(paste(rep("=", nchar(type_name)), collapse = ""), "\n")

    for (result in test_types[[type_name]]) {
      time_str <- sprintf("%.3fs", result$timing$elapsed)
      mem_str <- sprintf("%.2fMB", result$memory$memory_used_mb)

      cat(sprintf("  %-45s Time: %8s  Memory: %8s\n",
                  result$name, time_str, mem_str))
    }

    cat("\n")
  }
}


# Run tests if called directly
if (interactive() || sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)

  iterations <- 3
  save_results <- TRUE
  compare_baseline <- FALSE

  for (i in seq_along(args)) {
    if (args[i] == "--iterations" && i < length(args)) {
      iterations <- as.integer(args[i + 1])
    }
    if (args[i] == "--no-save") {
      save_results <- FALSE
    }
    if (args[i] == "--compare-baseline") {
      compare_baseline <- TRUE
    }
  }

  results <- run_data_loading_tests(
    save_results = save_results,
    iterations = iterations,
    compare_baseline = compare_baseline
  )

  print_data_loading_summary(results)
}
