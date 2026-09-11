# =============================================================================
# test-presets.R
#
# Nine of this file's ten tests covered mosaic_dask_presets() -- Coiled VM
# sizing ladders, scheduler memory floors, worker/sim tier selection. All of it
# went with the Dask/Coiled backend, and none of it describes a property of the
# surviving local path, so none of it is reproduced here.
# =============================================================================

test_that("mosaic_io_presets() returns the documented shapes", {
  expect_identical(mosaic_io_presets("debug")$format, "csv")
  expect_identical(mosaic_io_presets("default")$compression, "zstd")
  expect_identical(mosaic_io_presets("default")$compression_level, 3L)
  expect_identical(mosaic_io_presets("archive")$compression_level, 9L)
  expect_error(mosaic_io_presets("nope"))
})
