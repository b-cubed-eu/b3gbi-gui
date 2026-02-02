# Basic smoke test - ensure package loads
test_that("package loads successfully", {
  expect_true(requireNamespace("b3gbiGUI", quietly = TRUE))
})

# Test that required dependencies are available
test_that("required dependencies are installed", {
  required_pkgs <- c("shiny", "b3gbi", "DT", "ggplot2")
  for (pkg in required_pkgs) {
    expect_true(requireNamespace(pkg, quietly = TRUE),
                info = paste(pkg, "should be installed"))
  }
})
