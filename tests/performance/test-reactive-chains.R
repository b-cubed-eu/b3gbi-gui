# Reactive Chain Performance Tests
# Track H.3 - Performance Testing Infrastructure
#
# This script tests reactive chain performance including dependency chains,
# reactive execution time, observer triggers, and different input combinations.

# Source performance utilities
source("tests/performance/helper-performance.R")

# Source all modules
module_files <- list.files(path = "R", pattern = "\\.R$", full.names = TRUE)
for (file in module_files) {
  source(file)
}

#' Run All Reactive Chain Tests
#'
#' @description
#' Main function that tests all reactive chain performance scenarios.
#'
#' @param save_results Logical. Whether to save results
#' @param iterations Integer. Number of iterations per test
#'
#' @return List of test results
run_reactive_tests <- function(save_results = TRUE, iterations = 5) {
  cat("=== Starting Reactive Chain Performance Tests ===\n\n")

  all_results <- list()

  # 1. Test reactive dependency chains
  cat("1. Testing reactive dependency chains...\n")
  all_results <- c(all_results, test_reactive_dependencies(iterations))

  # 2. Profile reactive execution
  cat("2. Profiling reactive execution time...\n")
  all_results <- c(all_results, profile_reactive_execution(iterations))

  # 3. Test observer triggers
  cat("3. Testing observer triggers...\n")
  all_results <- c(all_results, test_observer_triggers(iterations))

  # 4. Test input combinations
  cat("4. Testing different input combinations...\n")
  all_results <- c(all_results, test_input_combinations(iterations))

  # 5. Test reactive isolation
  cat("5. Testing reactive isolation...\n")
  all_results <- c(all_results, test_reactive_isolation(iterations))

  # Save results
  if (save_results) {
    save_benchmark_results(all_results, tag = "reactive_chains")
  }

  cat("\n=== Reactive Chain Tests Complete ===\n")
  all_results
}


#' Test Reactive Dependencies
#'
#' @description
#' Tests performance of reactive dependency chains in the application.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of test results
test_reactive_dependencies <- function(iterations = 5) {
  results <- list()

  # Simulate reactive chain: data -> filter -> aggregate -> display

  # Create reactive values like in the real app
  r <- shiny::reactiveValues(
    dataCube = NULL,
    dataCube1 = NULL
  )

  # Test 1: Simple reactive chain (1 dependency)
  result <- comprehensive_benchmark({
    # Simulate a simple reactive chain
    shiny::reactive({
      r$dataCube
    })
  }, name = "reactive_chain_simple_1dep", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 2: Medium reactive chain (3 dependencies)
  result <- comprehensive_benchmark({
    # Simulate a medium reactive chain
    shiny::reactive({
      data <- r$dataCube
      if (is.null(data)) return(NULL)

      filtered <- shiny::reactive({
        if (is.null(data)) return(NULL)
        data
      })

      aggregated <- shiny::reactive({
        if (is.null(filtered)) return(NULL)
        filtered
      })

      aggregated
    })
  }, name = "reactive_chain_medium_3dep", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 3: Complex reactive chain (5+ dependencies)
  result <- comprehensive_benchmark({
    # Simulate a complex reactive chain
    shiny::reactive({
      level1 <- shiny::reactive({ r$dataCube })
      level2 <- shiny::reactive({ level1() })
      level3 <- shiny::reactive({ level2() })
      level4 <- shiny::reactive({ level3() })
      level5 <- shiny::reactive({ level4() })
      level5
    })
  }, name = "reactive_chain_complex_5dep", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 4: Cross-module dependencies
  result <- comprehensive_benchmark({
    # Simulate cross-module reactive pattern
    module_a <- shiny::reactiveValues(data = NULL)
    module_b <- shiny::reactiveValues(processed = NULL)

    # Module A produces data
    producer <- shiny::reactive({
      module_a$data <- generate_sample_cube(1000)
      module_a$data
    })

    # Module B consumes data
    consumer <- shiny::reactive({
      req(module_a$data)
      module_b$processed <- module_a$data
      module_b$processed
    })

    list(producer = producer, consumer = consumer)
  }, name = "reactive_cross_module_deps", iterations = iterations)

  results[[length(results) + 1]] <- result

  cat("  - Tested", length(results), "reactive dependency patterns\n")
  results
}


#' Profile Reactive Execution
#'
#' @description
#' Profiles the execution time of reactive expressions.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of profile results
profile_reactive_execution <- function(iterations = 5) {
  results <- list()

  # Generate test data
  test_data <- generate_sample_cube(n_rows = 10000)

  # Test 1: Reactive with small computation
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(data = test_data)

    reactive_small <- shiny::reactive({
      sum(r$data$x)
    })

    # Force evaluation
    reactive_small()
  }, name = "reactive_exec_small_compute", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 2: Reactive with medium computation
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(data = test_data)

    reactive_medium <- shiny::reactive({
      aggregate(y ~ family, data = r$data, FUN = mean)
    })

    reactive_medium()
  }, name = "reactive_exec_medium_compute", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 3: Reactive with heavy computation
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(data = test_data)

    reactive_heavy <- shiny::reactive({
      # Simulate complex computation
      result <- aggregate(y ~ family + scientificName, data = r$data, FUN = mean)
      result$rank <- rank(result$y)
      result[order(result$rank, decreasing = TRUE), ]
    })

    reactive_heavy()
  }, name = "reactive_exec_heavy_compute", iterations = max(1, iterations - 2))

  results[[length(results) + 1]] <- result

  # Test 4: Reactive with cached results
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(data = test_data, cache = NULL)

    reactive_cached <- shiny::reactive({
      if (!is.null(r$cache)) {
        return(r$cache)
      }
      r$cache <- mean(r$data$y)
      r$cache
    })

    # First call (computes)
    reactive_cached()
    # Second call (cached)
    reactive_cached()
  }, name = "reactive_exec_cached", iterations = iterations)

  results[[length(results) + 1]] <- result

  cat("  - Profiled", length(results), "reactive execution patterns\n")
  results
}


#' Test Observer Triggers
#'
#' @description
#' Tests performance of observer trigger patterns.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of test results
test_observer_triggers <- function(iterations = 5) {
  results <- list()

  # Generate test data
  test_data <- generate_sample_cube(n_rows = 1000)

  # Test 1: Single observer
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(counter = 0)

    # Simulate observer
    obs <- shiny::observe({
      r$counter <- r$counter + 1
    })

    # Trigger multiple times
    for (i in 1:10) {
      r$counter <- i
    }

    obs$destroy()
  }, name = "observer_single_trigger", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 2: Multiple observers on same reactive
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(data = test_data)

    obs_count <- 0

    # Create multiple observers
    obs1 <- shiny::observe({
      obs_count <<- obs_count + 1
      r$data$x
    })

    obs2 <- shiny::observe({
      obs_count <<- obs_count + 1
      r$data$y
    })

    obs3 <- shiny::observe({
      obs_count <<- obs_count + 1
      r$data$family
    })

    # Trigger all observers
    r$data <- generate_sample_cube(n_rows = 1000)

    obs1$destroy()
    obs2$destroy()
    obs3$destroy()
  }, name = "observer_multiple_triggers", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 3: Observer with priority
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(value = 0)

    obs_high <- shiny::observe({
      r$value <- r$value + 1
    }, priority = 10)

    obs_low <- shiny::observe({
      r$value <- r$value * 2
    }, priority = 1)

    # Trigger
    for (i in 1:5) {
      r$value <- i
    }

    obs_high$destroy()
    obs_low$destroy()
  }, name = "observer_priority_triggers", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 4: Observer chain reaction
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(
      input = 0,
      intermediate = 0,
      output = 0
    )

    obs1 <- shiny::observe({
      r$intermediate <- r$input * 2
    })

    obs2 <- shiny::observe({
      r$output <- r$intermediate + 1
    })

    # Chain reaction
    for (i in 1:5) {
      r$input <- i
    }

    obs1$destroy()
    obs2$destroy()
  }, name = "observer_chain_reaction", iterations = iterations)

  results[[length(results) + 1]] <- result

  cat("  - Tested", length(results), "observer trigger patterns\n")
  results
}


#' Test Input Combinations
#'
#' @description
#' Tests performance with different input combinations and change patterns.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of test results
test_input_combinations <- function(iterations = 5) {
  results <- list()

  # Simulate different input patterns

  # Test 1: Rapid input changes
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(inputs = list())

    # Simulate rapid changes
    for (i in 1:20) {
      r$inputs[[paste0("input_", i)]] <- i
    }

    # Access all
    values <- shiny::reactive({
      unlist(r$inputs)
    })

    values()
  }, name = "input_rapid_changes", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 2: Batch input updates
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues()

    # Batch update
    shiny::isolate({
      for (i in 1:20) {
        r[[paste0("input_", i)]] <- i
      }
    })

    # Single reactive read
    all_inputs <- shiny::reactive({
      result <- list()
      for (i in 1:20) {
        result[[i]] <- r[[paste0("input_", i)]]
      }
      result
    })

    all_inputs()
  }, name = "input_batch_updates", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 3: Cascading input dependencies
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(
      region = "Europe",
      country_type = "countries",
      resolution = "50"
    )

    # Reactive chain based on inputs
    region_options <- shiny::reactive({
      if (r$region == "World") {
        return(NULL)
      }
      paste(r$region, r$country_type, r$resolution)
    })

    # Multiple input changes
    for (i in 1:5) {
      r$region <- c("Europe", "Asia", "Africa", "Americas", "Oceania")[i]
      region_options()
    }
  }, name = "input_cascading_deps", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 4: Complex input validation
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(
      numeric_input = 50,
      text_input = "test",
      select_input = "option1"
    )

    validated <- shiny::reactive({
      # Simulate input validation
      if (r$numeric_input < 0 || r$numeric_input > 100) {
        return(NULL)
      }
      if (nchar(r$text_input) == 0) {
        return(NULL)
      }
      list(
        num = r$numeric_input,
        text = r$text_input,
        select = r$select_input
      )
    })

    # Test various inputs
    for (i in 1:10) {
      r$numeric_input <- sample(0:100, 1)
      r$text_input <- paste0("test_", i)
      validated()
    }
  }, name = "input_complex_validation", iterations = max(1, iterations - 2))

  results[[length(results) + 1]] <- result

  cat("  - Tested", length(results), "input combination patterns\n")
  results
}


#' Test Reactive Isolation
#'
#' @description
#' Tests performance impact of shiny::isolate() usage patterns.
#'
#' @param iterations Integer. Number of iterations
#'
#' @return List of test results
test_reactive_isolation <- function(iterations = 5) {
  results <- list()

  test_data <- generate_sample_cube(n_rows = 1000)

  # Test 1: Without isolation
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(data = test_data)

    no_isolate <- shiny::reactive({
      sum(r$data$x) + sum(r$data$y)
    })

    # Multiple reads
    for (i in 1:10) {
      no_isolate()
    }
  }, name = "reactive_no_isolation", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 2: With isolation
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(data = test_data)

    with_isolate <- shiny::reactive({
      shiny::isolate({
        sum(r$data$x) + sum(r$data$y)
      })
    })

    # Multiple reads
    for (i in 1:10) {
      with_isolate()
    }
  }, name = "reactive_with_isolation", iterations = iterations)

  results[[length(results) + 1]] <- result

  # Test 3: Selective isolation
  result <- comprehensive_benchmark({
    r <- shiny::reactiveValues(
      data = test_data,
      threshold = 5
    )

    selective <- shiny::reactive({
      # Non-isolated: depends on threshold
      thresh <- r$threshold

      # Isolated: doesn't trigger on data changes
      data_sum <- shiny::isolate({
        sum(r$data$x)
      })

      data_sum > thresh
    })

    # Test changes
    for (i in 1:5) {
      r$threshold <- i
      selective()
    }
  }, name = "reactive_selective_isolation", iterations = iterations)

  results[[length(results) + 1]] <- result

  cat("  - Tested", length(results), "isolation patterns\n")
  results
}


#' Print Reactive Test Summary
#'
#' @description
#' Prints a formatted summary of reactive chain test results.
#'
#' @param results List of test results
print_reactive_summary <- function(results) {
  cat("\n=== Reactive Chain Performance Summary ===\n\n")

  for (result in results) {
    time_str <- sprintf("%.4fs", result$timing$elapsed)
    mem_str <- if (!is.null(result$memory)) {
      sprintf("%.2fMB", result$memory$memory_used_mb)
    } else {
      "N/A"
    }

    cat(sprintf("%-35s Time: %10s  Memory: %10s\n",
                result$name, time_str, mem_str))
  }

  cat("\n")
}


# Run tests if called directly
if (interactive() || sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)

  iterations <- 5
  save_results <- TRUE

  for (i in seq_along(args)) {
    if (args[i] == "--iterations" && i < length(args)) {
      iterations <- as.integer(args[i + 1])
    }
    if (args[i] == "--no-save") {
      save_results <- FALSE
    }
  }

  results <- run_reactive_tests(save_results = save_results, iterations = iterations)
  print_reactive_summary(results)
}
