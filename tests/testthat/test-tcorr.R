# # Load the libraries
# library(testthat)
# library(thresholdcorrelation)
#
# # Test tcorr function
# test_that("tcorr returns expected output", {
#   file_path <- system.file("data", "demo.csv", package = "thresholdcorrelation")
#   demo_data <- read.csv(file_path)
#   x <- demo_data$temp
#   y <- demo_data$humidity
#   res <- tcorr(x, y, type = "HH", support = 0.1, grid_size = 5)
#   expect_type(res, "list")
#   expect_type(res$thresholds, "list")
# })
#
# # Test tables function
# test_that("tables returns a correlation table", {
#   file_path <- system.file("data", "demo.csv", package = "thresholdcorrelation")
#   demo_data <- read.csv(file_path)
#   x <- demo_data$temp
#   y <- demo_data$humidity
#   res <- tcorr(x, y)
#   tbl <- tables(res, "HH")
#   expect_type(tbl, "double")
# })
#
# # Test thresholds function
# test_that("thresholds returns a list of thresholds", {
#   file_path <- system.file("data", "demo.csv", package = "thresholdcorrelation")
#   demo_data <- read.csv(file_path)
#   x <- demo_data$temp
#   y <- demo_data$humidity
#   res <- tcorr(x, y)
#   thresh <- thresholds(res)
#   expect_type(thresh, "list")
#   expect_named(thresh, c("x", "y"))
# })
#
# # Test statistics function
# test_that("statistics returns summary statistics", {
#   file_path <- system.file("data", "demo.csv", package = "thresholdcorrelation")
#   demo_data <- read.csv(file_path)
#   x <- demo_data$temp
#   y <- demo_data$humidity
#   res <- tcorr(x, y)
#   stats <- statistics(res, "HH")
#   expect_type(stats, "list")
#   expect_named(stats, c("min", "max", "avg"))
# })
#
# # Test display function (a bit trickier to test since it's a plotting function)
# test_that("display doesn't throw an error", {
#   file_path <- system.file("data", "demo.csv", package = "thresholdcorrelation")
#   demo_data <- read.csv(file_path)
#   x <- demo_data$temp
#   y <- demo_data$humidity
#   res <- tcorr(x, y)
#   expect_silent(display(res, "HH"))
# })
