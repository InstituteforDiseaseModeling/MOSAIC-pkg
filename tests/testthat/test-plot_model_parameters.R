# test-plot_model_parameters.R

# Load the function
source("../../R/plot_model_parameters.R")

test_that("plot_model_parameters infers n_sim and n_iter correctly", {
    
    # Create mock results data with sim and iter columns
    set.seed(123)
    results <- data.frame(
        sim = rep(1:5, each = 3),
        iter = rep(1:3, 5),
        likelihood = rnorm(15, mean = -1000, sd = 100),
        phi_1 = runif(15, 0.5, 0.9),
        omega_1 = runif(15, 0.01, 0.1),
        beta_j0_env_ETH = runif(15, 0.1, 0.5)
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_plot_params")
    
    # Run function (suppress output)
    plots <- suppressMessages(
        plot_model_parameters(results, output_dir = test_dir, verbose = FALSE)
    )
    
    # Check that plots were created
    expect_type(plots, "list")
    expect_true("global" %in% names(plots))
    expect_true("location" %in% names(plots))
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_parameters works without sim/iter columns", {
    
    # Create mock results without sim/iter columns
    set.seed(123)
    results <- data.frame(
        likelihood = rnorm(20, mean = -1000, sd = 100),
        phi_1 = runif(20, 0.5, 0.9),
        omega_1 = runif(20, 0.01, 0.1),
        beta_j0_env_ETH = runif(20, 0.1, 0.5)
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_plot_params_no_sim")
    
    # Run function (should still work)
    expect_no_error({
        plots <- suppressMessages(
            plot_model_parameters(results, output_dir = test_dir, verbose = FALSE)
        )
    })
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_parameters handles multiple locations", {
    
    # Create mock results with multiple locations
    set.seed(123)
    n <- 30
    results <- data.frame(
        sim = rep(1:10, each = 3),
        iter = rep(1:3, 10),
        likelihood = rnorm(n, mean = -1000, sd = 100),
        phi_1 = runif(n, 0.5, 0.9),
        beta_j0_env_ETH = runif(n, 0.1, 0.5),
        beta_j0_env_KEN = runif(n, 0.1, 0.5),
        beta_j0_env_UGA = runif(n, 0.1, 0.5)
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_plot_params_multi")
    
    # Run function
    plots <- suppressMessages(
        plot_model_parameters(results, output_dir = test_dir, verbose = FALSE)
    )
    
    # Check that location plots were created for each location
    expect_true("location" %in% names(plots))
    expect_equal(length(plots$location), 3)  # ETH, KEN, UGA
    expect_true(all(c("ETH", "KEN", "UGA") %in% names(plots$location)))
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_parameters handles NA likelihood values", {
    
    # Create mock results with some NA likelihoods (failed simulations)
    set.seed(123)
    results <- data.frame(
        sim = 1:10,
        iter = rep(1, 10),
        likelihood = c(rnorm(7, mean = -1000, sd = 100), rep(NA, 3)),
        phi_1 = runif(10, 0.5, 0.9),
        omega_1 = runif(10, 0.01, 0.1)
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_plot_params_na")
    
    # Run function (should handle NAs gracefully)
    expect_no_error({
        plots <- suppressMessages(
            plot_model_parameters(results, output_dir = test_dir, verbose = FALSE)
        )
    })
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_parameters creates output directory if needed", {
    
    # Create mock results
    set.seed(123)
    results <- data.frame(
        likelihood = rnorm(10, mean = -1000, sd = 100),
        phi_1 = runif(10, 0.5, 0.9)
    )
    
    # Use non-existent directory
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "new_dir_that_doesnt_exist")
    
    # Ensure directory doesn't exist
    unlink(test_dir, recursive = TRUE)
    expect_false(dir.exists(test_dir))
    
    # Run function
    plots <- suppressMessages(
        plot_model_parameters(results, output_dir = test_dir, verbose = FALSE)
    )
    
    # Check that directory was created
    expect_true(dir.exists(test_dir))
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_parameters validates inputs", {
    
    # Test with non-dataframe
    expect_error(
        plot_model_parameters(list(a = 1), output_dir = "test"),
        "results must be a data frame"
    )
    
    # Test without likelihood column
    results_no_ll <- data.frame(phi_1 = runif(10))
    expect_error(
        plot_model_parameters(results_no_ll, output_dir = "test"),
        "results must contain a 'likelihood' column"
    )
    
    # Test without output_dir
    results <- data.frame(likelihood = rnorm(10))
    expect_error(
        plot_model_parameters(results),
        "output_dir is required"
    )
})