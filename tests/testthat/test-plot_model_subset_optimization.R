# test-plot_model_subset_optimization.R

# ---- minimal valid mosaic_subset_optimization fixture -----------------------
# Mirrors the structure returned by optimize_ensemble_subset(): an
# evaluation_table with one row per candidate N plus the scalar selection
# fields. Only the fields consumed by plot_model_subset_optimization() are
# populated (the plot does not touch the optimized ensemble or weights).
.mk_subset_opt <- function() {
    ns <- 30:50
    score <- -(2 - 0.02 * (ns - 30) + 0.0005 * (ns - 40)^2)  # concave: argmax interior
    optimal_n <- ns[which.max(score)]
    structure(
        list(
            evaluation_table = data.frame(
                n           = as.integer(ns),
                r2_cases    = 0.5 + 0.002 * (ns - 30),
                r2_deaths   = 0.2 + 0.001 * (ns - 30),
                bias_cases  = 1.0 + 0.01 * (ns - 40),
                bias_deaths = 0.9 + 0.005 * (ns - 40),
                mae_cases   = 10 - 0.05 * (ns - 30),
                mae_deaths  = 2  - 0.01 * (ns - 30),
                ess         = 5 + 0.5 * (ns - 30),
                score       = score,
                stringsAsFactors = FALSE
            ),
            optimal_n         = as.integer(optimal_n),
            optimal_score     = max(score),
            diagnostics_n     = 50L,
            diagnostics_score = score[length(score)],
            objective         = "mae",
            central_method    = c(cases = "mean", deaths = "mean"),
            stability_flag    = FALSE
        ),
        class = "mosaic_subset_optimization"
    )
}

test_that("plot_model_subset_optimization writes pdf + png and returns a ggplot", {
    subset_opt <- .mk_subset_opt()
    test_dir <- file.path(tempdir(), "test_subset_opt_plot")
    unlink(test_dir, recursive = TRUE)

    p <- suppressMessages(
        plot_model_subset_optimization(subset_opt, output_dir = test_dir, verbose = FALSE)
    )

    expect_s3_class(p, "ggplot")
    expect_true(file.exists(file.path(test_dir, "subset_optimization_diagnostic.pdf")))
    expect_true(file.exists(file.path(test_dir, "subset_optimization_diagnostic.png")))

    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_subset_optimization returns the ggplot invisibly", {
    subset_opt <- .mk_subset_opt()
    test_dir <- file.path(tempdir(), "test_subset_opt_invisible")
    unlink(test_dir, recursive = TRUE)

    expect_invisible(
        suppressMessages(
            plot_model_subset_optimization(subset_opt, output_dir = test_dir, verbose = FALSE)
        )
    )

    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_subset_optimization honours file_prefix", {
    subset_opt <- .mk_subset_opt()
    test_dir <- file.path(tempdir(), "test_subset_opt_prefix")
    unlink(test_dir, recursive = TRUE)

    suppressMessages(
        plot_model_subset_optimization(subset_opt, output_dir = test_dir,
                                       file_prefix = "myprefix", verbose = FALSE)
    )

    expect_true(file.exists(file.path(test_dir, "myprefix_diagnostic.pdf")))
    expect_true(file.exists(file.path(test_dir, "myprefix_diagnostic.png")))

    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_subset_optimization creates output_dir if missing", {
    subset_opt <- .mk_subset_opt()
    test_dir <- file.path(tempdir(), "test_subset_opt_newdir")
    unlink(test_dir, recursive = TRUE)
    expect_false(dir.exists(test_dir))

    suppressMessages(
        plot_model_subset_optimization(subset_opt, output_dir = test_dir, verbose = FALSE)
    )

    expect_true(dir.exists(test_dir))
    unlink(test_dir, recursive = TRUE)
})

test_that("plot_model_subset_optimization validates inputs", {
    # Non-mosaic_subset_optimization input
    expect_error(
        plot_model_subset_optimization(list(a = 1), output_dir = tempdir()),
        "must be a mosaic_subset_optimization object"
    )

    # Missing output_dir
    subset_opt <- .mk_subset_opt()
    expect_error(
        plot_model_subset_optimization(subset_opt),
        "output_dir is required"
    )

    # Evaluation table missing a required column
    bad <- .mk_subset_opt()
    bad$evaluation_table$score <- NULL
    expect_error(
        plot_model_subset_optimization(bad, output_dir = tempdir()),
        "missing required columns"
    )
})
