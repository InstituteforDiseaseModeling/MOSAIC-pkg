# alpha_1 is PINNED by default (v0.91.13). Two independent sites carry the
# default -- mosaic_control_defaults()$sampling (run_MOSAIC.R) and
# default_sample_args (sample_parameters.R) -- and they must not drift apart.
# That drift is the failure this file exists to catch: production ran with 40
# free per-location alpha_1 for months while the disease-modeler memory
# recorded them as pinned, because only one site was ever consulted.
#
# WHY pinned: alpha_1 is collinear with log(beta_j0_tot) in the endemic regime
# and with any coupling multiplier at invasion, so the 40 per-location draws
# buy nothing -- the 250k-draw continental posterior moved alpha_1 by 0.057
# prior SD, inside the 0.146 random-subset null. Pinning removes 40 free
# dimensions and restores cross-country comparability of beta_j0_tot.
# The VALUE (0.27) is deliberately low and is NOT changed by this: MOSAIC's
# patches are whole countries, i.e. weakly-coupled aggregates, so strong
# sub-linear mixing is the intended national-scale behaviour. Published
# values of 0.90-0.98 come from community/city-scale measles models and are
# not the right comparison class.

test_that("mosaic_control_defaults() pins alpha_1 and alpha_2", {
  s <- MOSAIC:::mosaic_control_defaults()$sampling
  expect_false(s$sample_alpha_1)
  expect_false(s$sample_alpha_2)
})

test_that("sample_parameters() and mosaic_control_defaults() agree on alpha_1", {
  # Guard BEFORE reading: R/ is absent under an installed R CMD check.
  f <- test_path("..", "..", "R", "sample_parameters.R")
  skip_if_not(file.exists(f), "package source R/ not available (installed check)")
  src <- readLines(f, warn = FALSE)
  line <- grep("^\\s*sample_alpha_1\\s*=", src, value = TRUE)
  skip_if(length(line) == 0, "sample_alpha_1 default not found")
  expect_match(line[1], "sample_alpha_1\\s*=\\s*FALSE")

  # and the two sites must carry the SAME value
  expect_equal(
    grepl("FALSE", line[1]),
    isFALSE(MOSAIC:::mosaic_control_defaults()$sampling$sample_alpha_1)
  )
})

test_that("the pinned alpha_1 stays engine-valid: in (0, 1], scalar or length-nL", {
  a <- as.numeric(unlist(MOSAIC::config_default$alpha_1))
  expect_true(all(a > 0 & a <= 1))            # engine invariant, params.py:446-449
  expect_true(length(a) == 1L ||
              length(a) == length(MOSAIC::config_default$location_name))
  # The dual-mode validator must accept the shipped form.
  expect_silent(MOSAIC:::.sim_scalar(a[1], "alpha_1", lower = 0, upper = 1))
})
