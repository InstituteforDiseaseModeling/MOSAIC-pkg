test_that("mosaic_dask_presets() backward-compatible single-arg call returns a valid spec", {
  spec <- mosaic_dask_presets(125)
  expect_type(spec, "list")
  expect_identical(spec$type, "coiled")
  expect_identical(spec$n_workers, 125L)
  expect_true(is.character(spec$scheduler_vm_types))
  expect_length(spec$scheduler_vm_types, 1L)
  expect_true(all(c("Standard_D2s_v6", "Standard_D2ds_v6",
                    "Standard_D2ads_v6", "Standard_D4s_v6") %in% spec$vm_types))
  expect_true(spec$wait_for_workers >= 1L)
  expect_true(spec$timeout >= 1800L)
})

test_that("mosaic_dask_presets() scheduler floor is 32 GiB on Ev6, never the old 8 GiB D2s (OOM regression)", {
  # The gather-everything Dask path holds the whole batch on the scheduler,
  # so an 8 GiB (Standard_D2s_v6) scheduler is indefensible. Smallest tier
  # must be Standard_E4s_v6 (32 GiB) on the memory-optimized Ev6 family.
  for (nw in c(1L, 10L, 50L)) {
    spec <- mosaic_dask_presets(nw)
    expect_identical(spec$scheduler_vm_types, "Standard_E4s_v6",
                     info = sprintf("n_workers = %d", nw))
    expect_false("Standard_D2s_v6" %in% spec$scheduler_vm_types)
  }
})

test_that("mosaic_dask_presets() worker-count scheduler ladder graduates as documented", {
  expect_identical(mosaic_dask_presets(50)$scheduler_vm_types,  "Standard_E4s_v6")
  expect_identical(mosaic_dask_presets(51)$scheduler_vm_types,  "Standard_E8s_v6")
  expect_identical(mosaic_dask_presets(100)$scheduler_vm_types, "Standard_E8s_v6")
  expect_identical(mosaic_dask_presets(101)$scheduler_vm_types, "Standard_E16s_v6")
  expect_identical(mosaic_dask_presets(200)$scheduler_vm_types, "Standard_E16s_v6")
  expect_identical(mosaic_dask_presets(201)$scheduler_vm_types, "Standard_E32s_v6")
  expect_identical(mosaic_dask_presets(350)$scheduler_vm_types, "Standard_E32s_v6")
  expect_identical(mosaic_dask_presets(351)$scheduler_vm_types, "Standard_E64s_v6")
  expect_identical(mosaic_dask_presets(500)$scheduler_vm_types, "Standard_E64s_v6")
})

test_that("mosaic_dask_presets() sizes the scheduler up for many sims on few workers (OOM fix)", {
  # 40 workers alone would give tier 1 (32 GiB); a 30K-sim batch must lift
  # the scheduler to the 50K-sim tier (128 GiB) so it does not OOM on gather.
  base <- mosaic_dask_presets(40)
  expect_identical(base$scheduler_vm_types, "Standard_E4s_v6")   # 32 GiB

  big  <- mosaic_dask_presets(40, n_sims = 30000)
  expect_identical(big$scheduler_vm_types, "Standard_E16s_v6")  # 128 GiB
})

test_that("mosaic_dask_presets() n_sims ladder maps to the documented tiers", {
  # Hold n_workers at its floor tier so n_sims drives the result.
  expect_identical(mosaic_dask_presets(1, n_sims = 10000)$scheduler_vm_types,  "Standard_E4s_v6")
  expect_identical(mosaic_dask_presets(1, n_sims = 10001)$scheduler_vm_types,  "Standard_E8s_v6")
  expect_identical(mosaic_dask_presets(1, n_sims = 25000)$scheduler_vm_types,  "Standard_E8s_v6")
  expect_identical(mosaic_dask_presets(1, n_sims = 25001)$scheduler_vm_types,  "Standard_E16s_v6")
  expect_identical(mosaic_dask_presets(1, n_sims = 50000)$scheduler_vm_types,  "Standard_E16s_v6")
  expect_identical(mosaic_dask_presets(1, n_sims = 50001)$scheduler_vm_types,  "Standard_E32s_v6")
  expect_identical(mosaic_dask_presets(1, n_sims = 100000)$scheduler_vm_types, "Standard_E32s_v6")
  expect_identical(mosaic_dask_presets(1, n_sims = 100001)$scheduler_vm_types, "Standard_E64s_v6")
})

test_that("mosaic_dask_presets() takes the LARGER of the worker and sim tiers", {
  # Big worker count, tiny n_sims: worker tier wins (not lowered by sims).
  spec <- mosaic_dask_presets(200, n_sims = 100)
  expect_identical(spec$scheduler_vm_types, "Standard_E16s_v6")  # tier 3 from workers
  # Small worker count, huge n_sims: sim tier wins (not lowered by workers).
  spec2 <- mosaic_dask_presets(60, n_sims = 200000)
  expect_identical(spec2$scheduler_vm_types, "Standard_E64s_v6") # tier 5 from sims
})

test_that("mosaic_dask_presets() validates n_workers bounds", {
  expect_error(mosaic_dask_presets(), "n_workers")
  expect_error(mosaic_dask_presets(0), "\\[1, 500\\]")
  expect_error(mosaic_dask_presets(501), "\\[1, 500\\]")
  expect_error(mosaic_dask_presets("x"), "n_workers")
  expect_error(mosaic_dask_presets(NA), "n_workers")
})

test_that("mosaic_dask_presets() validates n_sims", {
  expect_error(mosaic_dask_presets(50, n_sims = 0),   "n_sims")
  expect_error(mosaic_dask_presets(50, n_sims = -5),  "n_sims")
  expect_error(mosaic_dask_presets(50, n_sims = NA),  "n_sims")
  expect_error(mosaic_dask_presets(50, n_sims = c(1, 2)), "n_sims")
  # NULL is the default and must be accepted.
  expect_silent(mosaic_dask_presets(50, n_sims = NULL))
  # Non-integer is rounded, not rejected.
  expect_identical(mosaic_dask_presets(1, n_sims = 10000.4)$scheduler_vm_types,
                   "Standard_E4s_v6")
})

test_that("mosaic_dask_presets() keeps D4s_v6 as the last worker vm_types fallback", {
  # Coiled honours list order as decreasing priority, so D4s_v6 being LAST
  # means it is only used when no D2 SKU is available. It is retained as an
  # availability + RAM hedge.
  spec <- mosaic_dask_presets(100)
  expect_identical(spec$vm_types[length(spec$vm_types)], "Standard_D4s_v6")
  expect_identical(spec$vm_types[1], "Standard_D2s_v6")
})

test_that("mosaic_io_presets() returns the documented shapes", {
  expect_identical(mosaic_io_presets("debug")$format, "csv")
  expect_identical(mosaic_io_presets("default")$compression, "zstd")
  expect_identical(mosaic_io_presets("default")$compression_level, 3L)
  expect_identical(mosaic_io_presets("archive")$compression_level, 9L)
  expect_error(mosaic_io_presets("nope"))
})
