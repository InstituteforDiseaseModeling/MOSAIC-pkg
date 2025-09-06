# test-plot_model_likelihood.R

# Load the function
source("../../R/plot_model_likelihood.R")

test_that("plot_model_likelihood creates valid plot", {
    
    # Create mock results data
    set.seed(123)
    n <- 50
    results <- data.frame(
        sim = rep(1:10, each = 5),
        iter = rep(1:5, 10),
        likelihood = c(rnorm(40, mean = -1000, sd = 100), rep(NA, 10))  # Include some NAs
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_likelihood_plot")
    
    # Run function (suppress output)
    p <- suppressMessages(
        plot_model_likelihood(results, output_dir = test_dir, verbose = FALSE)
    )
    
    # Check that plot was created
    expect_s3_class(p, "ggplot")
    
    # Check that file was saved
    expect_true(file.exists(file.path(test_dir, "calibration_likelihood_curve.pdf")))
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_likelihood handles all NA likelihood", {
    
    # Create mock results with all NA likelihoods
    results <- data.frame(
        likelihood = rep(NA, 10),
        sim = 1:10
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_likelihood_na")
    
    # Run function - should handle gracefully with warning
    expect_warning({
        p <- plot_model_likelihood(results, output_dir = test_dir, verbose = FALSE)
    }, "No simulations with valid likelihood values found")
    
    # Should return NULL
    expect_null(p)
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_likelihood works without sim/iter columns", {
    
    # Create mock results without sim/iter columns
    set.seed(123)
    results <- data.frame(
        likelihood = rnorm(30, mean = -1000, sd = 100)
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_likelihood_no_sim")
    
    # Run function (should still work)
    expect_no_error({
        p <- suppressMessages(
            plot_model_likelihood(results, output_dir = test_dir, verbose = FALSE)
        )
    })
    
    expect_s3_class(p, "ggplot")
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_likelihood validates inputs", {
    
    # Test with non-dataframe
    expect_error(
        plot_model_likelihood(list(a = 1), output_dir = "test"),
        "results must be a data frame"
    )
    
    # Test without likelihood column
    results_no_ll <- data.frame(phi_1 = runif(10))
    expect_error(
        plot_model_likelihood(results_no_ll, output_dir = "test"),
        "results must contain a 'likelihood' column"
    )
    
    # Test without output_dir
    results <- data.frame(likelihood = rnorm(10))
    expect_error(
        plot_model_likelihood(results),
        "output_dir is required"
    )
})

test_that("plot_model_likelihood creates output directory if needed", {
    
    # Create mock results
    set.seed(123)
    results <- data.frame(
        likelihood = rnorm(20, mean = -1000, sd = 100),
        sim = 1:20
    )
    
    # Use non-existent directory
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "new_likelihood_dir")
    
    # Ensure directory doesn't exist
    unlink(test_dir, recursive = TRUE)
    expect_false(dir.exists(test_dir))
    
    # Run function
    p <- suppressMessages(
        plot_model_likelihood(results, output_dir = test_dir, verbose = FALSE)
    )
    
    # Check that directory was created
    expect_true(dir.exists(test_dir))
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_likelihood identifies best simulation correctly", {
    
    # Create mock results with known best
    results <- data.frame(
        sim = 1:5,
        iter = rep(1, 5),
        likelihood = c(-1000, -900, -1200, -800, -950)  # Best is sim 4 with -800
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_best_sim")
    
    # Capture messages to check if correct best is identified
    msgs <- capture_messages({
        p <- plot_model_likelihood(results, output_dir = test_dir, verbose = TRUE)
    })
    
    # Check that the correct best likelihood is reported
    expect_match(paste(msgs, collapse = " "), "Best likelihood: -800", fixed = TRUE)
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})