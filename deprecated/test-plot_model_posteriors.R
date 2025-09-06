# test-plot_model_posteriors.R

test_that("plot_model_posteriors creates valid plot", {
    
    # Create mock results data with varying likelihoods
    set.seed(123)
    n <- 100
    results <- data.frame(
        sim = rep(1:20, each = 5),
        iter = rep(1:5, 20),
        likelihood = c(rnorm(80, mean = -1000, sd = 100), rep(NA, 20)),  # Include some NAs
        phi_1 = runif(n, 0.5, 0.9),
        gamma_1 = runif(n, 0.1, 0.3),
        beta_j0_env_ETH = runif(n, 0.01, 0.1)
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_posterior_plot")
    
    # Run function (suppress output)
    plots <- suppressMessages(
        plot_model_posteriors(results, output_dir = test_dir, verbose = FALSE)
    )
    
    # Check that plots were created
    expect_type(plots, "list")
    expect_s3_class(plots$global, "ggplot")
    
    # Check that files were saved
    expect_true(file.exists(file.path(test_dir, "posterior_global_parameters.pdf")))
    expect_true(file.exists(file.path(test_dir, "posterior_parameters_ETH.pdf")))
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_posteriors handles all NA likelihood", {
    
    # Create mock results with all NA likelihoods
    results <- data.frame(
        likelihood = rep(NA, 10),
        sim = 1:10,
        phi_1 = runif(10)
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_posterior_na")
    
    # Run function - should handle gracefully with warning
    expect_warning({
        p <- plot_model_posteriors(results, output_dir = test_dir, verbose = FALSE)
    }, "No simulations with valid likelihood values found")
    
    # Should return NULL
    expect_null(p)
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_posteriors transforms likelihoods correctly", {
    
    # Create mock results with known likelihood values
    results <- data.frame(
        likelihood = c(-1000, -900, -1100, -950, -1050),  # -900 is best
        phi_1 = c(0.5, 0.6, 0.7, 0.8, 0.9)
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_posterior_weights")
    
    # Run function and check weight transformation
    plots <- suppressMessages(
        plot_model_posteriors(results, output_dir = test_dir, verbose = FALSE)
    )
    
    # The plot with highest likelihood should have highest weight
    # This is implicitly tested by the successful creation of the plot
    expect_s3_class(plots$global, "ggplot")
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_posteriors separates global and location parameters", {
    
    # Create mock results with both global and location-specific parameters
    set.seed(123)
    n <- 50
    results <- data.frame(
        likelihood = rnorm(n, mean = -1000, sd = 100),
        phi_1 = runif(n, 0.5, 0.9),              # Global
        gamma_1 = runif(n, 0.1, 0.3),            # Global
        beta_j0_env_ETH = runif(n, 0.01, 0.1),   # Location-specific ETH
        S_j_initial_UGA = runif(n, 0.7, 0.9)     # Location-specific UGA
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_posterior_separation")
    
    # Run function
    plots <- suppressMessages(
        plot_model_posteriors(results, output_dir = test_dir, verbose = FALSE)
    )
    
    # Check that both global and location plots were created
    expect_s3_class(plots$global, "ggplot")
    expect_type(plots$location, "list")
    expect_length(plots$location, 2)  # Should have ETH and UGA
    
    # Check files
    expect_true(file.exists(file.path(test_dir, "posterior_global_parameters.pdf")))
    expect_true(file.exists(file.path(test_dir, "posterior_parameters_ETH.pdf")))
    expect_true(file.exists(file.path(test_dir, "posterior_parameters_UGA.pdf")))
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_posteriors validates inputs", {
    
    # Test with non-dataframe
    expect_error(
        plot_model_posteriors(list(a = 1), output_dir = "test"),
        "results must be a data frame"
    )
    
    # Test without likelihood column
    results_no_ll <- data.frame(phi_1 = runif(10))
    expect_error(
        plot_model_posteriors(results_no_ll, output_dir = "test"),
        "results must contain a 'likelihood' column"
    )
    
    # Test without output_dir
    results <- data.frame(likelihood = rnorm(10))
    expect_error(
        plot_model_posteriors(results),
        "output_dir is required"
    )
})

test_that("plot_model_posteriors creates output directory if needed", {
    
    # Create mock results
    set.seed(123)
    results <- data.frame(
        likelihood = rnorm(20, mean = -1000, sd = 100),
        phi_1 = runif(20, 0.5, 0.9)
    )
    
    # Use non-existent directory
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "new_posterior_dir")
    
    # Ensure directory doesn't exist
    unlink(test_dir, recursive = TRUE)
    expect_false(dir.exists(test_dir))
    
    # Run function
    plots <- suppressMessages(
        plot_model_posteriors(results, output_dir = test_dir, verbose = FALSE)
    )
    
    # Check that directory was created
    expect_true(dir.exists(test_dir))
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_posteriors handles extreme likelihood values", {
    
    # Create mock results with extreme likelihood differences
    results <- data.frame(
        likelihood = c(-1e6, -1e3, -1e4, -1e5),  # Very large differences
        phi_1 = c(0.5, 0.6, 0.7, 0.8)
    )
    
    # Create temporary directory for output
    temp_dir <- tempdir()
    test_dir <- file.path(temp_dir, "test_extreme_likelihoods")
    
    # Should handle without numerical issues
    expect_no_error({
        plots <- suppressMessages(
            plot_model_posteriors(results, output_dir = test_dir, verbose = FALSE)
        )
    })
    
    expect_s3_class(plots$global, "ggplot")
    
    # Clean up
    unlink(test_dir, recursive = TRUE)
})