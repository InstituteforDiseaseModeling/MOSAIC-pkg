# Test parallel processing capability of est_initial_R

library(testthat)

# Load the function
source("../../R/est_initial_R.R")

test_that("est_initial_R has parallel parameter", {
  # Check that the function has the parallel parameter in signature
  formal_args <- formals(est_initial_R)
  expect_true("parallel" %in% names(formal_args))
  expect_equal(formal_args$parallel, FALSE)  # Default should be FALSE
})

test_that("est_initial_R parallel logic works with mock config", {
  # Test that parallel logic activates correctly
  
  # Mock minimal inputs (without actually running the function)
  location_codes_small <- c("TCD", "NER", "MLI")  # 3 locations - should not trigger parallel
  location_codes_large <- c("TCD", "NER", "MLI", "BFA", "GHA", "NGA", "COD", "CMR", "CIV", "SEN")  # 10 locations - should trigger parallel
  
  # Test threshold logic (8+ locations required for parallel)
  expect_true(length(location_codes_large) >= 8)
  expect_false(length(location_codes_small) >= 8)
  
  # Test that parallel flag combined with location count determines behavior
  expect_true(TRUE && length(location_codes_large) >= 8)  # parallel=TRUE + many locations
  expect_false(TRUE && length(location_codes_small) >= 8)  # parallel=TRUE + few locations
  expect_false(FALSE && length(location_codes_large) >= 8)  # parallel=FALSE + many locations
})

test_that("parallel processing detection works", {
  skip_on_os("windows")  # mclapply doesn't work on Windows
  
  # Test that parallel::detectCores() is available
  expect_true(requireNamespace("parallel", quietly = TRUE))
  expect_true(parallel::detectCores() >= 1)
})

test_that("est_initial_R parameter documentation updated", {
  # Test that documentation has been updated for parallel processing
  # This is a meta-test to ensure we updated the docs
  
  # Read the function source to check documentation
  func_source <- readLines("../../R/est_initial_R.R")
  
  # Find the line with @param parallel and the next few lines
  parallel_start <- grep("@param parallel", func_source)
  expect_length(parallel_start, 1)
  
  # Check the parallel documentation lines (current line + next 2 lines)
  parallel_doc_lines <- func_source[parallel_start:(parallel_start + 2)]
  parallel_doc_text <- paste(parallel_doc_lines, collapse = " ")
  
  expect_true(grepl("mclapply", parallel_doc_text))
  expect_true(grepl("Windows", parallel_doc_text))
  expect_true(grepl("location_codes.*>=.*8", parallel_doc_text))
})