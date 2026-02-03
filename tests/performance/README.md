# Performance Testing Documentation
# Track H.3 - Performance Testing Infrastructure

## Overview

This directory contains performance testing infrastructure for the b3gbiGUI application. The tests measure timing, memory usage, and detect performance regressions.

## Directory Structure

```
tests/performance/
├── helper-performance.R     # Performance utilities and helper functions
├── benchmark.R             # Main benchmark script
├── test-reactive-chains.R  # Reactive chain performance tests
├── test-data-loading.R     # Data loading performance tests
├── README.md              # This file
└── results/               # Benchmark results (auto-created)
    ├── benchmark_YYYYMMDD_HHMMSS.json
    └── benchmark_YYYYMMDD_HHMMSS.csv
```

## Running Performance Tests

### Run All Benchmarks

```bash
# Run from project root
Rscript tests/performance/benchmark.R
```

### Run Specific Test Suites

```bash
# Reactive chain tests
Rscript tests/performance/test-reactive-chains.R

# Data loading tests
Rscript tests/performance/test-data-loading.R
```

### Command Line Options

```bash
# Specify iterations
Rscript tests/performance/benchmark.R --iterations 5

# Compare to baseline
Rscript tests/performance/benchmark.R --baseline tests/performance/results/baseline.json

# Don't save results
Rscript tests/performance/benchmark.R --no-save

# Data loading with modularization comparison
Rscript tests/performance/test-data-loading.R --compare-baseline
```

### Run from R Console

```r
# Source the test files
source("tests/performance/benchmark.R")
source("tests/performance/test-reactive-chains.R")
source("tests/performance/test-data-loading.R")

# Run benchmarks with options
results <- run_all_benchmarks(
  save_results = TRUE,
  compare_to_baseline = TRUE,
  baseline_file = "tests/performance/results/baseline.json",
  iterations = 3
)

# Run specific tests
reactive_results <- run_reactive_tests(iterations = 5)
data_results <- run_data_loading_tests(compare_baseline = TRUE)

# Print summaries
print_benchmark_summary(results)
print_reactive_summary(reactive_results)
print_data_loading_summary(data_results)
```

## Interpreting Results

### Benchmark Output Format

Results are saved in two formats:

1. **JSON** (`benchmark_YYYYMMDD_HHMMSS.json`):
   - Complete structured data
   - Suitable for programmatic analysis
   - Contains all timing and memory details

2. **CSV** (`benchmark_YYYYMMDD_HHMMSS.csv`):
   - Flattened table format
   - Easy to import into spreadsheets
   - Contains key metrics only

### Key Metrics

Each benchmark reports:

| Metric | Description | Unit |
|--------|-------------|------|
| `elapsed` | Wall-clock time | seconds |
| `user` | CPU user time | seconds |
| `system` | CPU system time | seconds |
| `min_time` | Minimum time across iterations | seconds |
| `max_time` | Maximum time across iterations | seconds |
| `sd_time` | Standard deviation of times | seconds |
| `memory_used_mb` | Memory consumed | megabytes |
| `peak_memory_mb` | Peak memory usage | megabytes |
| `combined_score` | Time + normalized memory | composite |

### Console Output

```
=== Performance Benchmark Summary ===

Module Loading
--------------
  load_module_data_loading                  0.023s       N/A
  load_module_shapefile                     0.015s       N/A
  ...

Data Processing
---------------
  data_validation_small_1000rows            0.045s     2.5MB
  data_aggregation_medium_10000rows           0.234s     8.3MB
  ...
```

## Performance Baselines

### Creating a Baseline

1. Run benchmarks on a stable version:
```bash
Rscript tests/performance/benchmark.R --iterations 10
```

2. Copy the best results as baseline:
```bash
cp tests/performance/results/benchmark_20250203_120000.json \
   tests/performance/results/baseline.json
```

### Baseline Storage

Store baselines by version or milestone:
```
tests/performance/results/
├── baseline_v1.0.0.json
├── baseline_v1.1.0.json
└── baseline.json -> baseline_v1.1.0.json  (symlink to current)
```

### Baseline Comparison

When comparing to baseline, the system reports:

- **Time change**: Percentage difference in execution time
- **Memory change**: Percentage difference in memory usage
- **Status**: OK / IMPROVED / REGRESSION

Example:
```
=== Performance Comparison Summary ===

Regressions: 1
Improvements: 2
Unchanged: 5

WARNING: Performance regressions detected!
         name               time_change_pct  memory_change_pct
1 data_loading_large         35.2            12.1
```

## Regression Testing Procedures

### Automated Regression Detection

Run benchmarks with automatic regression detection:

```r
results <- run_all_benchmarks(
  compare_to_baseline = TRUE,
  baseline_file = "tests/performance/results/baseline.json"
)

# Will stop with error if regressions detected
comparison <- compare_benchmarks(results, baseline)
detect_regressions(comparison, fail_on_regression = TRUE)
```

### CI/CD Integration

Add to CI pipeline (`.github/workflows`):

```yaml
- name: Run Performance Tests
  run: |
    Rscript tests/performance/benchmark.R \
      --iterations 3 \
      --baseline tests/performance/results/baseline.json
```

### Regression Thresholds

Default threshold is **1.2** (20% slower triggers warning):

```r
# Custom threshold
compare_benchmarks(current, baseline, threshold = 1.3)  # 30% threshold
```

### What to Do When Regressions Are Detected

1. **Verify**: Run tests multiple times to rule out noise
2. **Profile**: Use `Rprof()` to identify bottlenecks:
   ```r
   profile <- run_rprof_profile({
     slow_operation()
   })
   ```
3. **Bisect**: Use git bisect to find the offending commit
4. **Fix**: Optimize the slow code path
5. **Update**: Update baseline if change is acceptable:
   ```bash
   # After reviewing and accepting the change
   cp results/current.json results/baseline.json
   ```

## Test Categories

### 1. Module Loading (`benchmark.R`)

Tests source file loading performance:
- Individual module loading
- Total startup time
- Dependency resolution

### 2. Data Processing (`benchmark.R`)

Tests data cube operations:
- Validation (various sizes: 1K, 10K, 50K, 100K rows)
- Aggregation operations
- Filtering operations

### 3. Data Loading (`test-data-loading.R`)

Tests file I/O performance:
- CSV reading (various file sizes)
- File size validation
- Memory profiling during load

### 4. Shapefile Processing (`test-data-loading.R`)

Tests spatial data operations:
- Shapefile reading (10, 100, 500 features)
- Component validation
- Geometry validation
- ZIP extraction

### 5. Reactive Chains (`test-reactive-chains.R`)

Tests Shiny reactivity performance:
- Simple dependency chains (1 level)
- Medium chains (3 levels)
- Complex chains (5+ levels)
- Cross-module dependencies
- Observer triggers
- Input combination handling
- Isolation patterns

## Performance Tips

### Reducing Test Time

For faster development testing:

```r
# Reduce iterations
results <- run_all_benchmarks(iterations = 1)

# Run specific tests only
results <- benchmark_module_loading(iterations = 1)
```

### Accurate Measurements

For release testing:

```r
# Increase iterations for statistical significance
results <- run_all_benchmarks(iterations = 10)

# Close other applications
# Use dedicated testing environment
# Run multiple times and average
```

### Profiling Hotspots

Use `Rprof` for detailed profiling:

```r
# Profile a specific operation
profile <- run_rprof_profile({
  load_data_cube("large_file.csv")
}, interval = 0.01)

# View top functions
head(profile$by.total, 20)
```

## Troubleshooting

### Tests Failing

1. Check package availability:
   ```r
   requireNamespace("shiny", quietly = TRUE)
   requireNamespace("sf", quietly = TRUE)
   ```

2. Verify file paths:
   ```r
   file.exists("R/data_loading.R")
   ```

3. Check disk space for temp files

### Inconsistent Results

- Close background applications
- Run during low system load
- Increase iteration count
- Use dedicated testing machine

### Memory Issues

For large data tests:

```r
# Reduce test sizes
results <- benchmark_data_loading(iterations = 1)
# Modify test_sizes in function
```

## Contributing

### Adding New Benchmarks

1. Add benchmark function to appropriate test file
2. Follow naming convention: `benchmark_<category>_<description>`
3. Use `comprehensive_benchmark()` for consistency
4. Add to `run_*_tests()` main function
5. Update this README with new test description

### Example:

```r
benchmark_new_feature <- function(iterations = 3) {
  results <- list()

  result <- comprehensive_benchmark({
    # Test code here
    new_feature_operation()
  }, name = "new_feature_test", iterations = iterations)

  results[[1]] <- result
  results
}
```

## References

- R `system.time()` documentation
- R `Rprof()` profiling documentation
- Shiny reactivity documentation
- b3gbi package documentation
