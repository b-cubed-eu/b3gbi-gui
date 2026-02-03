# Main Performance Benchmark Script
# Track H.3 - Performance Testing Infrastructure
#
# This script runs comprehensive performance benchmarks for the b3gbiGUI
# application including module loading, data processing, and visualization.

# Source performance utilities
source("tests/performance/helper-performance.R")

# Source all R modules
module_files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
for (file in module_files) {
  source(file)
}

#' Run All Performance Benchmarks
#'
#' @description
#' Main function that runs all performance benchmarks and saves results.
#'
#' @param save_results Logical. Whether to save results to disk
#' @param compare_to_baseline Logical. Whether to compare against baseline
#' @param baseline_file Character. Path to baseline file
#' @param iterations Integer. Number of iterations for timing tests
#'
#' @return List of all benchmark results
#'
#' @examples
#' \dontrun{
#' # Run all benchmarks
#' results <- run_all_benchmarks()
#'
#' # Run with baseline comparison
#' results <- run_all_benchmarks(
#'   compare_to_baseline = TRUE,
#'   baseline_file = "tests/performance/results/baseline.json"
#' )
#' }
run_all_benchmarks <- function(save_results = TRUE,
                            compare_to_baseline = FALSE,
                            baseline_file = NULL,
                            iterations = 3) {
  cat("=== Starting Performance Benchmarks ===\n")
  cat("Iterations per test:", iterations, "\n\n")

  all_results <- list()

  # 1. Module Loading Benchmarks
  cat("1. Running module loading benchmarks...\n")
  all_results <- c(all_results, benchmark_module_loading(iterations))

  # 2. Data Processing Benchmarks
  cat("2. Running data processing benchmarks...\n")
  all_results <- c(all_results, benchmark_data_processing(iterations))

  # 3. Data Loading Benchmarks
  cat("3. Running data loading benchmarks...\n")
  all_results <- c(all_results, benchmark_data_loading(iterations))

  # 4. Shapefile Processing Benchmarks
  cat("4. Running shapefile processing benchmarks...\n")
  all_results <- c(all_results, benchmark_shapefile_processing(iterations))

  # 5. Visualization Benchmarks (if data available)
  cat("5. Running visualization benchmarks...\n")
  viz_results <- benchmark_visualization(iterations)
  if (!is.null(viz_results)) {
    all_results <- c(all_results, viz_results)
  }

  # Save results
  if (save_results) {
    save_benchmark_results(all_results, tag = format(Sys.time(), "%Y%m%d"))
  }

  # Compare to baseline
  if (compare_to_baseline && !is.null(baseline_file)) {
    cat("\n=== Comparing to Baseline ===\n")
    baseline <- load_baseline(baseline_file)
    comparison <- compare_benchmarks(all_results, baseline)
    detect_regressions(comparison, fail_on_regression = FALSE)
  }

  cat("\n=== Benchmarks Complete ===\n")
  all_results
}


#' Benchmark Module Loading
#'
#' @description
#' Times the loading of all R modules in the application.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of benchmark results
benchmark_module_loading <- function(iterations = 3) {
  results <- list()

  # Benchmark individual module loading
  module_files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)

  for (mod_file in module_files) {
    mod_name <- basename(mod_file)

    result <- time_benchmark({
      source(mod_file)
    }, name = paste0("load_module_", sub("\\.R$", "", mod_name)),
    iterations = iterations)

    results[[length(results) + 1]] <- list(
      name = result$name,
      timing = result,
      memory = NULL,
      combined_score = result$elapsed,
      timestamp = Sys.time()
    )
  }

  # Benchmark total module loading
  total_result <- time_benchmark({
    for (file in module_files) {
      source(file)
    }
  }, name = "load_all_modules", iterations = iterations)

  results[[length(results) + 1]] <- list(
    name = total_result$name,
    timing = total_result,
    memory = NULL,
    combined_score = total_result$elapsed,
    timestamp = Sys.time()
  )

  cat("  - Benchmarked", length(module_files), "modules\n")
  results
}


#' Benchmark Data Processing
#'
#' @description
#' Benchmarks data processing operations with different cube sizes.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of benchmark results
benchmark_data_processing <- function(iterations = 3) {
  results <- list()

  # Define test sizes
  sizes <- list(
    small = list(n_rows = 1000, n_species = 10, n_families = 5),
    medium = list(n_rows = 10000, n_species = 50, n_families = 10),
    large = list(n_rows = 50000, n_species = 100, n_families = 20),
    xlarge = list(n_rows = 100000, n_species = 200, n_families = 30)
  )

  for (size_name in names(sizes)) {
    size <- sizes[[size_name]]

    # Generate test data
    test_data <- generate_sample_cube(
      n_rows = size$n_rows,
      n_species = size$n_species,
      n_families = size$n_families
    )

    # Benchmark data validation
    result <- comprehensive_benchmark({
      # Simulate data cube validation
      required_cols <- c("scientificName", "family", "year")
      missing <- setdiff(required_cols, names(test_data))
      if (length(missing) > 0) {
        stop("Missing columns: ", paste(missing, collapse = ", "))
      }
      # Basic operations
      unique(test_data$scientificName)
      unique(test_data$family)
      range(test_data$year)
    }, name = paste0("data_validation_", size_name, "_", size$n_rows, "rows"),
    iterations = iterations)

    results[[length(results) + 1]] <- result

    # Benchmark aggregation
    result <- comprehensive_benchmark({
      # Simulate aggregation operations
      aggregate(count ~ scientificName + year, data = test_data, FUN = length)
    }, name = paste0("data_aggregation_", size_name, "_", size$n_rows, "rows"),
    iterations = iterations)

    results[[length(results) + 1]] <- result

    # Benchmark filtering
    result <- comprehensive_benchmark({
      # Simulate filtering operations
      subset(test_data, family == "Family_1")
    }, name = paste0("data_filtering_", size_name, "_", size$n_rows, "rows"),
    iterations = iterations)

    results[[length(results) + 1]] <- result
  }

  cat("  - Benchmarked", length(sizes), "data sizes (", paste(names(sizes), collapse = ", "), ")\n")
  results
}


#' Benchmark Data Loading
#'
#' @description
#' Benchmarks data cube loading from CSV files of various sizes.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of benchmark results
benchmark_data_loading <- function(iterations = 3) {
  results <- list()

  # Create temporary test files of different sizes
  temp_dir <- tempdir()

  sizes <- c(1000, 10000, 50000)

  for (n_rows in sizes) {
    # Generate test data
    test_data <- generate_sample_cube(n_rows = n_rows)
    temp_file <- file.path(temp_dir, paste0("test_cube_", n_rows, ".csv"))
    utils::write.csv(test_data, temp_file, row.names = FALSE)

    # Benchmark CSV reading
    result <- comprehensive_benchmark({
      read.csv(temp_file, stringsAsFactors = FALSE)
    }, name = paste0("csv_read_", n_rows, "rows"), iterations = iterations)

    results[[length(results) + 1]] <- result

    # Benchmark file size check
    result <- comprehensive_benchmark({
      check_file_size(temp_file, threshold_mb = 100)
    }, name = paste0("file_size_check_", n_rows, "rows"), iterations = iterations)

    results[[length(results) + 1]] <- result

    # Cleanup
    unlink(temp_file)
  }

  cat("  - Benchmarked", length(sizes), "file sizes (", paste(sizes, collapse = ", "), "rows)\n")
  results
}


#' Benchmark Shapefile Processing
#'
#' @description
#' Benchmarks shapefile loading and processing operations.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of benchmark results
benchmark_shapefile_processing <- function(iterations = 3) {
  results <- list()

  # Skip if sf package not available
  if (!requireNamespace("sf", quietly = TRUE)) {
    cat("  - Skipping shapefile benchmarks (sf package not available)\n")
    return(results)
  }

  # Create temporary test shapefile
  temp_dir <- tempdir()

  # Generate simple test shapefile
  n_features <- 100
  test_sf <- sf::st_sf(
    id = 1:n_features,
    name = paste0("region_", 1:n_features),
    geometry = sf::st_sfc(
      lapply(1:n_features, function(i) {
        sf::st_polygon(list(matrix(c(
          0, 0, 1, 0, 1, 1, 0, 1, 0, 0
        ) + c(i, i), ncol = 2, byrow = TRUE)))
      })
    )
  )

  temp_shp <- file.path(temp_dir, "test_shapefile.shp")
  sf::write_sf(test_sf, temp_shp)

  # Benchmark shapefile reading
  result <- comprehensive_benchmark({
    sf::read_sf(temp_shp)
  }, name = "shapefile_read", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Benchmark shapefile validation
  result <- comprehensive_benchmark({
    validate_shapefile_components(temp_shp)
  }, name = "shapefile_validation", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Cleanup
  cleanup_shapefiles(temp_dir)

  cat("  - Benchmarked", length(results), "shapefile operations\n")
  results
}


#' Benchmark Visualization Generation
#'
#' @description
#' Benchmarks plot generation operations (requires sample data).
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of benchmark results or NULL if data unavailable
benchmark_visualization <- function(iterations = 3) {
  results <- list()

  # Skip if b3gbi package not available or no sample data
  if (!requireNamespace("b3gbi", quietly = TRUE)) {
    cat("  - Skipping visualization benchmarks (b3gbi not available)\n")
    return(NULL)
  }

  # Check for sample data files
  sample_files <- list.files("data", pattern = "example.*\\.rda$", full.names = TRUE)

  if (length(sample_files) == 0) {
    cat("  - Skipping visualization benchmarks (no sample data found)\n")
    return(NULL)
  }

  # Load first available sample
  env <- new.env()
  load(sample_files[1], envir = env)
  sample_objects <- ls(env)

  if (length(sample_objects) > 0) {
    sample_data <- env[[sample_objects[1]]]

    # Benchmark plot generation (if data is suitable)
    if (inherits(sample_data, "indicator_ts") || inherits(sample_data, "indicator_map")) {
      result <- comprehensive_benchmark({
        tryCatch({
          b3gbi::plot(sample_data)
        }, error = function(e) NULL)
      }, name = "viz_plot_generation", iterations = max(1, iterations - 1))

      results[[length(results) + 1]] <- result

      cat("  - Benchmarked plot generation\n")
    } else {
      cat("  - Sample data not suitable for visualization benchmark\n")
    }
  }

  if (length(results) == 0) NULL else results
}


#' Print Benchmark Summary
#'
#' @description
#' Prints a formatted summary of benchmark results.
#'
#' @param results List of benchmark results
print_benchmark_summary <- function(results) {
  cat("\n=== Performance Benchmark Summary ===\n\n")

  # Group by category
  categories <- list()

  for (result in results) {
    if (grepl("^load_module_", result$name)) {
      cat <- "Module Loading"
    } else if (grepl("^data_", result$name)) {
      cat <- "Data Processing"
    } else if (grepl("^csv_", result$name)) {
      cat <- "Data Loading"
    } else if (grepl("^shapefile_", result$name)) {
      cat <- "Shapefile Processing"
    } else if (grepl("^viz_", result$name)) {
      cat <- "Visualization"
    } else {
      cat <- "Other"
    }

    if (is.null(categories[[cat]])) {
      categories[[cat]] <- list()
    }
    categories[[cat]][[length(categories[[cat]]) + 1]] <- result
  }

  # Print each category
  for (cat_name in names(categories)) {
    cat(cat_name, "\n")
    cat(paste(rep("-", nchar(cat_name)), collapse = ""), "\n")

    cat_results <- categories[[cat_name]]

    for (result in cat_results) {
      time_str <- sprintf("%.3fs", result$timing$elapsed)
      mem_str <- if (!is.null(result$memory)) {
        sprintf("%.1fMB", result$memory$memory_used_mb)
      } else {
        "N/A"
      }

      cat(sprintf("  %-40s %10s %10s\n", result$name, time_str, mem_str))
    }

    cat("\n")
  }
}


# Run benchmarks if called directly
if (interactive() || sys.nframe() == 0) {
  # Check for command line arguments
  args <- commandArgs(trailingOnly = TRUE)

  iterations <- 3
  save_results <- TRUE
  baseline_file <- NULL

  # Parse arguments
  for (i in seq_along(args)) {
    if (args[i] == "--iterations" && i < length(args)) {
      iterations <- as.integer(args[i + 1])
    }
    if (args[i] == "--baseline" && i < length(args)) {
      baseline_file <- args[i + 1]
    }
    if (args[i] == "--no-save") {
      save_results <- FALSE
    }
  }

  # Run benchmarks
  results <- run_all_benchmarks(
    save_results = save_results,
    compare_to_baseline = !is.null(baseline_file),
    baseline_file = baseline_file,
    iterations = iterations
  )

  # Print summary
  print_benchmark_summary(results)
}
