test_that("write_trajectory_csv writes one wide CSV per location", {
     tr <- mock_trajectories(locs = c("MOZ", "MWI"), n_t = 10L,
                             channels = c("incidence", "disease_deaths"))
     dir_out <- withr::local_tempdir()

     written <- write_trajectory_csv(tr, dir_out, verbose = FALSE)

     expect_length(written, 2L)
     expect_setequal(basename(written),
                     c("trajectories_MOZ.csv", "trajectories_MWI.csv"))

     moz <- utils::read.csv(file.path(dir_out, "trajectories_MOZ.csv"))
     # WIDE: one row per date, one column per channel -- not one row per
     # (date, channel). A long table would be 10 * 2 = 20 rows here.
     expect_equal(nrow(moz), 10L)
     expect_named(moz, c("location", "date", "incidence", "disease_deaths"))
     expect_true(all(moz$location == "MOZ"))
})

test_that("the date axis is derived from date_start and matches tick order", {
     tr <- mock_trajectories(locs = "MOZ", n_t = 5L, channels = "incidence",
                             date_start = "2020-03-01")
     dir_out <- withr::local_tempdir()
     write_trajectory_csv(tr, dir_out, verbose = FALSE)
     moz <- utils::read.csv(file.path(dir_out, "trajectories_MOZ.csv"))

     expect_equal(moz$date,
                  as.character(seq(as.Date("2020-03-01"), by = "day", length.out = 5L)))
     # Column t of the source matrix must land on row t, not reversed or shifted.
     expect_equal(moz$incidence, tr$summary$incidence$median[1, ])
})

test_that("rows are not interleaved across locations", {
     # Regression guard: building the table with rep() in the wrong order
     # silently transposes location against date, which still produces a
     # plausible-looking file of the right size.
     tr <- mock_trajectories(locs = c("AAA", "BBB"), n_t = 4L, channels = "incidence")
     tr$summary$incidence$median <- matrix(c(1:4, 101:104), nrow = 2, byrow = TRUE)
     dir_out <- withr::local_tempdir()
     write_trajectory_csv(tr, dir_out, verbose = FALSE)

     expect_equal(utils::read.csv(file.path(dir_out, "trajectories_AAA.csv"))$incidence,
                  c(1, 2, 3, 4))
     expect_equal(utils::read.csv(file.path(dir_out, "trajectories_BBB.csv"))$incidence,
                  c(101, 102, 103, 104))
})

test_that("values are rounded to the requested significant figures", {
     tr <- mock_trajectories(locs = "MOZ", n_t = 1L, channels = "incidence")
     tr$summary$incidence$median <- matrix(1234.56789, nrow = 1)
     dir_out <- withr::local_tempdir()

     write_trajectory_csv(tr, dir_out, digits = 4L, verbose = FALSE)
     expect_equal(utils::read.csv(file.path(dir_out, "trajectories_MOZ.csv"))$incidence,
                  1235)

     write_trajectory_csv(tr, dir_out, digits = 8L, verbose = FALSE)
     expect_equal(utils::read.csv(file.path(dir_out, "trajectories_MOZ.csv"))$incidence,
                  1234.5679)
})

test_that("channels can be subset, and an unknown channel is an error", {
     tr <- mock_trajectories(locs = "MOZ", n_t = 3L,
                             channels = c("incidence", "disease_deaths", "Isym"))
     dir_out <- withr::local_tempdir()

     write_trajectory_csv(tr, dir_out, channels = c("incidence", "Isym"),
                          verbose = FALSE)
     expect_named(utils::read.csv(file.path(dir_out, "trajectories_MOZ.csv")),
                  c("location", "date", "incidence", "Isym"))

     expect_error(write_trajectory_csv(tr, dir_out, channels = "not_a_channel",
                                       verbose = FALSE),
                  "not present in this artifact")
})

test_that("a listed but unreduced channel becomes an NA column, not a dropped one", {
     # The file's shape must not vary silently between models.
     tr <- mock_trajectories(locs = "MOZ", n_t = 3L,
                             channels = c("incidence", "CFR"))
     tr$summary$CFR <- list(median = NULL)
     dir_out <- withr::local_tempdir()
     write_trajectory_csv(tr, dir_out, verbose = FALSE)

     moz <- utils::read.csv(file.path(dir_out, "trajectories_MOZ.csv"))
     expect_true("CFR" %in% names(moz))
     expect_true(all(is.na(moz$CFR)))
})

test_that("a path is accepted in place of an object", {
     tr <- mock_trajectories(locs = "MOZ", n_t = 3L, channels = "incidence")
     rds <- withr::local_tempfile(fileext = ".rds")
     saveRDS(tr, rds)
     dir_out <- withr::local_tempdir()

     expect_length(write_trajectory_csv(rds, dir_out, verbose = FALSE), 1L)
     expect_error(write_trajectory_csv(file.path(dir_out, "nope.rds"), dir_out),
                  "does not exist")
})

test_that("a non-trajectory object is rejected rather than half-written", {
     expect_error(write_trajectory_csv(list(a = 1), withr::local_tempdir()),
                  "not a mosaic_trajectories object")
})
