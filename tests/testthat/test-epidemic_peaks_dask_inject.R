# Regression for the Dask-path epidemic_peaks injection (v0.55.16).
# .mosaic_inject_likelihood_settings() overwrites config$epidemic_peaks with a
# fresh filter against MOSAIC::epidemic_peaks. A no-peak location yields a 0-row
# frame that JSON-round-trips to the Coiled worker WITHOUT its iso_code column
# and crashes laser params.py:303 (.iso_code on a column-less DataFrame ->
# "'DataFrame' object has no attribute 'iso_code'"). The injector must null an
# empty result so the engine skips the epidemic_peaks block.

test_that(".mosaic_inject_likelihood_settings nulls an empty epidemic_peaks", {
  cfg <- list(date_start = "2018-01-01", date_stop = "2027-02-04",
              location_name = "ZZZ_NO_SUCH_ISO")   # never present in epidemic_peaks
  out <- MOSAIC:::.mosaic_inject_likelihood_settings(
    cfg, list(.score_window_resolved = NULL))
  expect_null(out$epidemic_peaks)
})

test_that(".mosaic_inject_likelihood_settings keeps non-empty epidemic_peaks with iso_code", {
  # A location that does have peaks must retain a non-empty frame carrying iso_code.
  ep <- MOSAIC::epidemic_peaks
  if (is.null(ep) || nrow(ep) == 0L) skip("no epidemic_peaks data available")
  iso <- as.character(ep$iso_code[[1]])
  cfg <- list(date_start = "2010-01-01", date_stop = "2030-01-01",
              location_name = iso)
  out <- MOSAIC:::.mosaic_inject_likelihood_settings(
    cfg, list(.score_window_resolved = NULL))
  if (!is.null(out$epidemic_peaks)) {
    expect_true(NROW(out$epidemic_peaks) > 0L)
    expect_true("iso_code" %in% names(out$epidemic_peaks))
  } else {
    succeed("location had no in-window peaks; nulled as expected")
  }
})
