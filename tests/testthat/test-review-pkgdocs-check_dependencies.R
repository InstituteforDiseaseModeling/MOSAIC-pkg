# Regression tests for check_dependencies() control flow (deep review,
# packaging-env-03 / packaging-env-04). Python is never initialised: the
# environment paths, the attach step and reticulate are all mocked.

skip_if_not_installed("reticulate")
skip_if_not_installed("yaml")

# A fake r-mosaic env whose "python" answers every `pip show` with nothing, so
# tensorflow and keras both look missing.
local_fake_env <- function(env = parent.frame()) {
     root <- withr::local_tempdir(.local_envir = env)
     dir.create(file.path(root, "bin"))
     exec <- file.path(root, "bin", "python")
     writeLines(c("#!/bin/sh", "exit 0"), exec)
     Sys.chmod(exec, "0755")
     list(env = root, exec = exec, norm = exec)
}

test_that("a missing tensorflow/keras is reported in the summary and leaks no global", {
     skip_on_os("windows")
     paths <- local_fake_env()
     if (exists("suitability_working", envir = globalenv(), inherits = FALSE)) {
          rm("suitability_working", envir = globalenv())
     }
     withr::defer(if (exists("suitability_working", envir = globalenv(), inherits = FALSE))
          rm("suitability_working", envir = globalenv()))

     local_mocked_bindings(
          get_python_paths  = function() paths,
          attach_mosaic_env = function(...) invisible(TRUE)
     )
     local_mocked_bindings(
          import    = function(module, ...) stop("no module named ", module),
          py_config = function() list(python = paths$exec),
          .package  = "reticulate"
     )

     out <- paste(cli::cli_fmt(check_dependencies()), collapse = "\n")

     expect_match(out, "tensorflow \\[suitability\\] not found in pip")
     expect_match(out, "Suitability estimation: Limited")
     expect_no_match(out, "TensorFlow/Keras available")
     expect_no_match(out, "MOSAIC is ready for use!")
     expect_false(exists("suitability_working", envir = globalenv(), inherits = FALSE))
})

test_that("an attach failure stops check_dependencies() instead of falling through", {
     skip_on_os("windows")
     paths <- local_fake_env()
     reached <- FALSE

     local_mocked_bindings(
          get_python_paths  = function() paths,
          attach_mosaic_env = function(...) stop("Python already initialized with a different environment")
     )
     local_mocked_bindings(
          import    = function(module, ...) { reached <<- TRUE; stop("unreachable") },
          py_config = function() { reached <<- TRUE; list(python = "/usr/bin/python3") },
          .package  = "reticulate"
     )

     out <- NULL
     expect_no_error(out <- paste(cli::cli_fmt(res <- check_dependencies()), collapse = "\n"))
     expect_null(res)
     expect_match(out, "Failed to attach Python environment: Python already initialized")
     expect_false(reached)
})
