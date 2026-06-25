test_that("calc_mobility_flux returns config-order outputs with the N*tau identity", {
  cfg <- jsonlite::fromJSON(
    system.file("extdata", "config_default.json", package = "MOSAIC"),
    simplifyVector = TRUE)

  mf <- calc_mobility_flux(cfg)

  J <- length(cfg$location_name)
  expect_equal(length(mf$location_name), J)
  expect_identical(mf$location_name, cfg$location_name)   # no re-sort (F3)
  expect_equal(dim(mf$flux), c(J, J))
  expect_equal(dim(mf$pi),   c(J, J))
  expect_equal(dim(mf$D),    c(J, J))

  # Diagonals NA (no self-travel) — DM#1.
  expect_true(all(is.na(diag(mf$flux))))
  expect_true(all(is.na(diag(mf$pi))))

  # Axis labels aligned element-wise to values, in config order (F3).
  expect_identical(rownames(mf$flux), cfg$location_name)
  expect_identical(colnames(mf$flux), cfg$location_name)
  expect_identical(rownames(mf$pi),   cfg$location_name)
  expect_identical(rownames(mf$D),    cfg$location_name)

  # Flux identity: rowSums(M) == N * tau (total daily travelers per origin).
  rs       <- rowSums(mf$flux, na.rm = TRUE)
  expected <- mf$N * mf$tau
  expect_equal(unname(rs), unname(expected), tolerance = 1e-8)
})

test_that("calc_mobility_flux preserves a non-alphabetical config order (F3)", {
  cfg <- jsonlite::fromJSON(
    system.file("extdata", "config_default.json", package = "MOSAIC"),
    simplifyVector = TRUE)

  # Reverse the location order to a deliberately non-alphabetical config and
  # confirm every axis follows config order (the F3 silent-mislabel guard).
  J  <- length(cfg$location_name)
  ord <- rev(seq_len(J))
  cfg$location_name <- cfg$location_name[ord]
  cfg$longitude     <- cfg$longitude[ord]
  cfg$latitude      <- cfg$latitude[ord]
  cfg$N_j_initial   <- cfg$N_j_initial[ord]
  cfg$tau_i         <- cfg$tau_i[ord]

  mf <- calc_mobility_flux(cfg)
  expect_identical(mf$location_name, cfg$location_name)
  expect_identical(rownames(mf$flux), cfg$location_name)
  expect_equal(unname(mf$N), as.numeric(cfg$N_j_initial))

  # The identity still holds under reordering.
  expect_equal(unname(rowSums(mf$flux, na.rm = TRUE)),
               unname(mf$N * mf$tau), tolerance = 1e-8)
})

test_that("calc_mobility_flux errors on missing mobility fields", {
  cfg <- jsonlite::fromJSON(
    system.file("extdata", "config_default.json", package = "MOSAIC"),
    simplifyVector = TRUE)
  cfg$mobility_omega <- NULL
  expect_error(calc_mobility_flux(cfg), "missing required mobility field")
})
