# Performance Testing Utilities for b3gbiGUI
# Track H.3 - Performance Testing Infrastructure
#
# This file provides helper functions for performance testing including timing,
# memory profiling, result storage, and regression detection.

#' Time a Function Execution
#'
#' @description
#' Wrapper around system.time() that provides detailed timing information
#' and stores results in a standardized format.
#'
#' @param expr Expression to time
#' @param name Character. Name of the benchmark
#' @param iterations Integer. Number of iterations to run. Default 1.
#'
#' @return List containing timing results
#'
#' @examples
#' \dontrun{
#' result <- time_benchmark({
#'   sum(1:1000000)
#' }, name = "sum_test")
#' }
time_benchmark <- function(expr, name, iterations = 1) {
  times <- numeric(iterations)

  for (i in seq_len(iterations)) {
    gc(reset = TRUE)
    timing <- system.time(expr)
    times[i] <- timing["elapsed"]
  }

  list(
    name = name,
    iterations = iterations,
    user = timing["user.self"],
    system = timing["sys.self"],
    elapsed = mean(times),
    min_time = min(times),
    max_time = max(times),
    sd_time = stats::sd(times),
    timestamp = Sys.time()
  )
}


#' Profile Memory Usage
#'
#' @description
#' Tracks memory usage before, during, and after function execution using
#' R's memory profiling capabilities.
#'
#' @param expr Expression to profile
#' @param name Character. Name of the profile
#'
#' @return List containing memory statistics
#'
#' @examples
#' \dontrun{
#' result <- memory_profile({
#'   data <- rnorm(1000000)
#' }, name = "memory_test")
#' }
memory_profile <- function(expr, name) {
  # Force garbage collection and get baseline
  gc(reset = TRUE)
  baseline <- gc(reset = FALSE)

  # Execute expression
  expr_result <- force(expr)

  # Get post-execution memory stats
  gc_result <- gc(reset = FALSE)

  # Calculate memory used (in MB)
  memory_used <- sum(gc_result[, 2] - baseline[, 2])

  list(
    name = name,
    baseline_mb = sum(baseline[, 2]),
    peak_mb = sum(gc_result[, 2]),
    memory_used_mb = memory_used,
    n_cells_alloc = gc_result[1, 1],
    n_vcells_alloc = gc_result[2, 1],
    timestamp = Sys.time()
  )
}


#' Comprehensive Benchmark
#'
#' @description
#' Combines timing and memory profiling for a complete performance snapshot.
#'
#' @param expr Expression to benchmark
#' @param name Character. Name of the benchmark
#' @param iterations Integer. Number of iterations. Default 3.
#'
#' @return List with both timing and memory results
#'
#' @examples
#' \dontrun{
#' result <- comprehensive_benchmark({
#'   load_data_cube("data.csv")
#' }, name = "data_loading", iterations = 3)
#' }
comprehensive_benchmark <- function(expr, name, iterations = 3) {
  # Memory profile (single iteration to avoid interference)
  mem_result <- memory_profile(expr, name)

  # Timing benchmark
  time_result <- time_benchmark(expr, name, iterations)

  list(
    name = name,
    timing = time_result,
    memory = mem_result,
    combined_score = time_result$elapsed + (mem_result$memory_used_mb / 100),
    timestamp = Sys.time()
  )
}


#' Save Benchmark Results
#'
#' @description
#' Saves benchmark results to JSON and CSV files for tracking and comparison.
#'
#' @param results List. Benchmark results to save
#' @param output_dir Character. Directory to save results. Default "tests/performance/results"
#' @param tag Character. Optional tag for the benchmark run
#'
#' @return Invisible path to saved files
#'
#' @examples
#' \dontrun{
#' result <- comprehensive_benchmark(...)
#' save_benchmark_results(result, tag = "v1.0.0")
#' }
save_benchmark_results <- function(results, output_dir = "tests/performance/results", tag = NULL) {
  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Generate filename with timestamp and optional tag
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  tag_suffix <- if (!is.null(tag)) paste0("_", tag) else ""
  base_name <- paste0("benchmark_", timestamp, tag_suffix)

  # Save as JSON
  json_file <- file.path(output_dir, paste0(base_name, ".json"))
  jsonlite::write_json(results, json_file, pretty = TRUE, auto_unbox = TRUE)

  # Flatten for CSV
  flat_results <- flatten_benchmark_results(results)
  csv_file <- file.path(output_dir, paste0(base_name, ".csv"))
  utils::write.csv(flat_results, csv_file, row.names = FALSE)

  message("Benchmark results saved to:")
  message("  JSON: ", json_file)
  message("  CSV:  ", csv_file)

  invisible(c(json_file, csv_file))
}


#' Flatten Benchmark Results for CSV Export
#'
#' @keywords internal
flatten_benchmark_results <- function(results) {
  if (!is.list(results)) {
    return(data.frame())
  }

  rows <- list()

  for (result in results) {
    if (is.null(result) || !is.list(result)) next

    row <- list(
      name = result$name %||% NA,
      timestamp = result$timestamp %||% NA,
      elapsed_time = result$timing$elapsed %||% NA,
      min_time = result$timing$min_time %||% NA,
      max_time = result$timing$max_time %||% NA,
      sd_time = result$timing$sd_time %||% NA,
      memory_used_mb = result$memory$memory_used_mb %||% NA,
      peak_memory_mb = result$memory$peak_mb %||% NA,
      combined_score = result$combined_score %||% NA
    )

    rows[[length(rows) + 1]] <- row
  }

  do.call(rbind, lapply(rows, data.frame))
}


#' Load Baseline Results
#'
#' @description
#' Loads previously saved benchmark results to use as baseline for comparison.
#'
#' @param baseline_file Character. Path to JSON baseline file
#'
#' @return List of baseline results
#'
#' @examples
#' \dontrun{
#' baseline <- load_baseline("tests/performance/results/baseline.json")
#' }
load_baseline <- function(baseline_file) {
  if (!file.exists(baseline_file)) {
    warning("Baseline file not found: ", baseline_file)
    return(NULL)
  }

  jsonlite::fromJSON(baseline_file, simplifyVector = FALSE)
}


#' Compare Benchmark Results
#'
#' @description
#' Compares current benchmark results against a baseline and reports differences.
#'
#' @param current List. Current benchmark results
#' @param baseline List. Baseline results to compare against
#' @param threshold Numeric. Threshold for regression warning (default 1.2 = 20% slower)
#'
#' @return Data frame with comparison results
#'
#' @examples
#' \dontrun{
#' comparison <- compare_benchmarks(current_results, baseline_results)
#' print(comparison)
#' }
compare_benchmarks <- function(current, baseline, threshold = 1.2) {
  if (is.null(baseline)) {
    warning("No baseline provided for comparison")
    return(NULL)
  }

  comparisons <- list()

  for (curr in current) {
    name <- curr$name

    # Find matching baseline
    base_match <- NULL
    for (base in baseline) {
      if (base$name == name) {
        base_match <- base
        break
      }
    }

    if (is.null(base_match)) {
      warning("No baseline found for: ", name)
      next
    }

    # Calculate changes
    time_change <- curr$timing$elapsed / base_match$timing$elapsed
    mem_change <- curr$memory$memory_used_mb / base_match$memory$memory_used_mb
    score_change <- curr$combined_score / base_match$combined_score

    comparison <- list(
      name = name,
      current_time = curr$timing$elapsed,
      baseline_time = base_match$timing$elapsed,
      time_change_ratio = time_change,
      time_change_pct = (time_change - 1) * 100,
      current_memory = curr$memory$memory_used_mb,
      baseline_memory = base_match$memory$memory_used_mb,
      memory_change_ratio = mem_change,
      memory_change_pct = (mem_change - 1) * 100,
      score_change_ratio = score_change,
      status = if (time_change > threshold) "REGRESSION" else if (time_change < 0.8) "IMPROVED" else "OK"
    )

    comparisons[[length(comparisons) + 1]] <- comparison
  }

  result_df <- do.call(rbind, lapply(comparisons, data.frame))

  # Print summary
  cat("\n=== Performance Comparison Summary ===\n")
  regressions <- result_df[result_df$status == "REGRESSION", ]
  improvements <- result_df[result_df$status == "IMPROVED", ]

  cat("Regressions: ", nrow(regressions), "\n")
  cat("Improvements: ", nrow(improvements), "\n")
  cat("Unchanged: ", nrow(result_df[result_df$status == "OK", ]), "\n\n")

  if (nrow(regressions) > 0) {
    cat("WARNING: Performance regressions detected!\n")
    print(regressions[, c("name", "time_change_pct", "memory_change_pct")])
  }

  result_df
}


#' Detect Performance Regressions
#'
#' @description
#' Automatically detects if any benchmarks have regressed beyond the threshold.
#'
#' @param comparison_df Data frame from compare_benchmarks()
#' @param fail_on_regression Logical. Whether to stop on regression detection
#'
#' @return Logical indicating if regressions were found
#'
#' @examples
#' \dontrun{
#' if (detect_regressions(comparison_df, fail_on_regression = TRUE)) {
#'   # Handle regressions
#' }
#' }
detect_regressions <- function(comparison_df, fail_on_regression = FALSE) {
  if (is.null(comparison_df) || !"status" %in% names(comparison_df)) {
    return(FALSE)
  }

  regressions <- comparison_df[comparison_df$status == "REGRESSION", ]
  has_regressions <- nrow(regressions) > 0

  if (has_regressions && fail_on_regression) {
    stop(
      "Performance regressions detected in ", nrow(regressions),
      " benchmark(s). See comparison results for details."
    )
  }

  has_regressions
}


#' Generate Sample Data Cube
#'
#' @description
#' Generates a synthetic data cube of specified size for performance testing.
#'
#' @param n_rows Integer. Number of rows to generate
#' @param n_species Integer. Number of unique species
#' @param n_families Integer. Number of unique families
#' @param year_range Integer vector of length 2. Min and max years
#'
#' @return Data frame simulating a data cube
#'
#' @examples
#' \dontrun{
#' small_cube <- generate_sample_cube(1000, n_species = 10)
#' large_cube <- generate_sample_cube(100000, n_species = 100)
#' }
generate_sample_cube <- function(n_rows = 1000,
                                 n_species = 50,
                                 n_families = 10,
                                 year_range = c(2000, 2023)) {
  data.frame(
    scientificName = sample(
      paste0("Species_", 1:n_species),
      n_rows,
      replace = TRUE
    ),
    family = sample(
      paste0("Family_", 1:n_families),
      n_rows,
      replace = TRUE
    ),
    year = sample(year_range[1]:year_range[2], n_rows, replace = TRUE),
    x = runif(n_rows, -180, 180),
    y = runif(n_rows, -90, 90),
    stringsAsFactors = FALSE
  )
}


#' Create Progress Reporter
#'
#' @description
#' Creates a simple progress reporting function for long-running benchmarks.
#'
#' @param total Integer. Total number of items to process
#' @param message Character. Prefix message
#'
#' @return Function that accepts current progress
#'
#' @examples
#' \dontrun{
#' reporter <- create_progress_reporter(10, "Benchmarking")
#' for (i in 1:10) {
#'   reporter(i)
#'   Sys.sleep(0.1)
#' }
#' }
create_progress_reporter <- function(total, message = "Progress") {
  function(current) {
    pct <- round((current / total) * 100)
    cat(sprintf("\r%s: %d/%d (%d%%)", message, current, total, pct))
    if (current >= total) cat("\n")
    utils::flush.console()
  }
}


#' Run Rprof Profiling
#'
#' @description
#' Runs Rprof on an expression and returns profiling summary.
#'
#' @param expr Expression to profile
#' @param interval Numeric. Sampling interval in seconds
#'
#' @return Data frame with profiling summary
#'
#' @examples
#' \dontrun{
#' profile <- run_rprof_profile({
#'   # Some slow operation
#'   Sys.sleep(0.1)
#' })
#' head(profile)
#' }
run_rprof_profile <- function(expr, interval = 0.02) {
  temp_file <- tempfile()

  Rprof(temp_file, interval = interval)
  result <- force(expr)
  Rprof(NULL)

  # Read and summarize profile
  if (file.exists(temp_file) && file.size(temp_file) > 0) {
    profile_summary <- summaryRprof(temp_file)
    profile_summary
  } else {
    NULL
  }
}


# Helper function
`%||%` <- function(x, y) if (is.null(x)) y else x
