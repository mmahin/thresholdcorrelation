library(testthat)
library(thresholdcorrelation)

# Load data from demo.csv
demo_data <- read.csv(file.path('tests', 'testthat', 'demo.csv'))
x <- demo_data$temp
y <- demo_data$humidity

# Test tcorr function
test_that("tcorr returns expected output", {
  res <- tcorr(x, y, type = "HH", support = 0.1, grid_size = 5)
  expect_is(res, "list")
  expect_named(res, c("HH", "thresholds"))
  expect_is(res$thresholds, "list")
})

# Test tables function
test_that("tables returns a correlation table", {
  res <- tcorr(x, y)
  tbl <- tables(res, "HH")
  expect_is(tbl, "numeric")
})

# Test thresholds function
test_that("thresholds returns a list of thresholds", {
  res <- tcorr(x, y)
  thresh <- thresholds(res)
  expect_is(thresh, "list")
  expect_named(thresh, c("x", "y"))
})

# Test statistics function
test_that("statistics returns summary statistics", {
  res <- tcorr(x, y)
  stats <- statistics(res, "HH")
  expect_is(stats, "list")
  expect_named(stats, c("min", "max", "avg"))
})

# Test display function (a bit trickier to test since it's a plotting function)
test_that("display doesn't throw an error", {
  res <- tcorr(x, y)
  expect_silent(display(res, "HH"))
})