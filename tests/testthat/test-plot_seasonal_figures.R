# Documentation figures built from est_seasonal_dynamics() outputs:
# plot_seasonal_transmission(), plot_seasonal_transmission_example() and
# plot_seasonal_clustering(). Regression tests for (a) the hardcoded legend and
# title years ("1994-2024", "2023-2024", "2014-2024"), which no longer described
# the data drawn, and (b) plot_seasonal_clustering() requiring a weekly
# DOCS_TABLES/pred_seasonal_dynamics.csv that nothing writes any more.

# Fixture: est_seasonal_dynamics()-shaped outputs for six countries in three
# well-separated seasonal shapes. Precipitation covers 2011-03-07..2013-12-29;
# cases are observed in 2012 only, and BWA has no cases (its case fit is
# inferred from AGO, as the estimator does for countries without data).
.seasonal_fixture <- function(dir) {
     isos <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF")
     shapes <- list(function(d) sin(2 * pi * d / 365),
                    function(d) cos(2 * pi * d / 365),
                    function(d) -sin(2 * pi * d / 365))
     days <- 1:365
     set.seed(42)
     daily <- do.call(rbind, lapply(seq_along(isos), function(i) {
          f <- shapes[[(i + 1L) %/% 2L]]
          data.frame(day = days, iso_code = isos[i],
                     Country = MOSAIC::convert_iso_to_country(isos[i]),
                     fitted_values_fourier_precip = 2 * f(days) + stats::rnorm(365, sd = 0.02),
                     fitted_values_fourier_cases = f(days + 30),
                     inferred_from_neighbor = NA_character_,
                     stringsAsFactors = FALSE)
     }))
     daily$fitted_values_fourier_cases[daily$iso_code == "BWA"] <-
          daily$fitted_values_fourier_cases[daily$iso_code == "AGO"]
     daily$inferred_from_neighbor[daily$iso_code == "BWA"] <- "Angola"
     utils::write.csv(daily, file.path(dir, "pred_seasonal_dynamics_day.csv"), row.names = FALSE)

     dates <- seq(as.Date("2011-03-07"), as.Date("2013-12-29"), by = "day")
     precip <- do.call(rbind, lapply(isos, function(iso) {
          in_2012 <- format(dates, "%Y") == "2012" & iso != "BWA"
          data.frame(date = dates, iso_code = iso,
                     weekly_precipitation_sum = 10,
                     cases = ifelse(in_2012, 5, NA),
                     precip_scaled = stats::rnorm(length(dates)),
                     cases_scaled = ifelse(in_2012, stats::rnorm(length(dates)), NA),
                     stringsAsFactors = FALSE)
     }))
     utils::write.csv(precip, file.path(dir, "data_seasonal_precipitation.csv"), row.names = FALSE)
     invisible(list(daily = daily, precip = precip, isos = isos))
}

# ggplot2 warns "Removed N rows containing missing values" for the weeks without
# a scaled value (expected: cases are NA outside the observed weeks). Muffle that
# warning only, so any other warning still surfaces.
.quiet_removed_rows <- function(expr) {
     withCallingHandlers(expr, warning = function(w) {
          if (grepl("^Removed [0-9]+ rows", conditionMessage(w))) invokeRestart("muffleWarning")
     })
}

# Square polygons standing in for country shapefiles (written with sf, in Imports).
.write_square_shapefile <- function(path, isos) {
     polys <- lapply(seq_along(isos), function(i) {
          x0 <- 10 + 3 * i
          sf::st_polygon(list(rbind(c(x0, 0), c(x0 + 2, 0), c(x0 + 2, 2), c(x0, 2), c(x0, 0))))
     })
     shp <- sf::st_sf(iso_a3 = isos, geometry = sf::st_sfc(polys, crs = 4326))
     sf::st_write(shp, path, quiet = TRUE)
     invisible(path)
}

test_that(".seasonal_year_span() and .seasonal_label() describe the dates given", {
     span <- MOSAIC:::.seasonal_year_span
     lab <- MOSAIC:::.seasonal_label
     expect_identical(span(as.Date(c("2010-09-06", "2025-08-31"))), "2010-2025")
     expect_identical(span(as.Date(c("2023-03-27", "2023-07-30", NA))), "2023")
     expect_identical(span(c("2014-12-29", "2016-01-04")), "2014-2016")
     expect_identical(span(as.Date(NA)), NA_character_)
     expect_identical(span(NULL), NA_character_)
     expect_identical(span(as.Date(character(0))), NA_character_)
     expect_identical(lab("Precipitation", as.Date(c("2011-01-03", "2013-12-29"))),
                      "Precipitation (2011-2013)")
     expect_identical(lab("Cholera Cases", as.Date(character(0))), "Cholera Cases")
})

test_that(".seasonal_point_labels() spans only the rows that are drawn", {
     d <- data.frame(date = c("2011-01-03", "2012-06-04", "2013-12-30"),
                     precip_scaled = c(0.1, NA, 0.3),
                     cases_scaled = c(NA, 1.2, NA))
     expect_identical(MOSAIC:::.seasonal_point_labels(d),
                      c(precip = "Precipitation (2011-2013)", cases = "Cholera Cases (2012)"))
     d$cases_scaled <- NA
     expect_identical(unname(MOSAIC:::.seasonal_point_labels(d)["cases"]), "Cholera Cases")
})

test_that("plot_seasonal_transmission() labels its points with the years drawn", {
     skip_if_not(isTRUE(capabilities("png")), "no png device")
     local_null_device()
     mi <- withr::local_tempdir()
     fig <- withr::local_tempdir()
     .seasonal_fixture(mi)
     expect_message(.quiet_removed_rows(plot_seasonal_transmission(list(MODEL_INPUT = mi, DOCS_FIGURES = fig))),
                    "seasonal_transmission_all.png")
     expect_true(file.exists(file.path(fig, "seasonal_transmission_all.png")))
     p <- ggplot2::last_plot()
     legend_keys <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")$get_labels()
     expect_identical(legend_keys, c("Precipitation (2011-2013)", "Cholera Cases (2012)",
                                     "Fourier Series (Precip)", "Fourier Series (Cases)"))
     expect_false(any(grepl("1994|2023-2024", legend_keys)))
})

test_that("plot_seasonal_transmission_example() labels the country's own years", {
     skip_if_not(isTRUE(capabilities("png")), "no png device")
     local_null_device()
     mi <- withr::local_tempdir()
     shp <- withr::local_tempdir()
     fig <- withr::local_tempdir()
     .seasonal_fixture(mi)
     .write_square_shapefile(file.path(shp, "AGO_ADM0.shp"), "AGO")
     .write_square_shapefile(file.path(shp, "BWA_ADM0.shp"), "BWA")
     P <- list(MODEL_INPUT = mi, DATA_SHAPEFILES = shp, DOCS_FIGURES = fig)

     expect_message(.quiet_removed_rows(plot_seasonal_transmission_example(P, country_iso_code = "AGO", n_points = 4)),
                    "seasonal_transmission_example_AGO.png")
     labels_ago <- plot_text_labels(ggplot2::last_plot())
     expect_true(all(c("Precipitation (2011-2013)", "Cholera Cases (2012)") %in% labels_ago))
     expect_false(any(grepl("1994|2023-2024", labels_ago)))

     # A country with no case points of its own gets a label without years
     expect_message(.quiet_removed_rows(plot_seasonal_transmission_example(P, country_iso_code = "BWA", n_points = 4)),
                    "seasonal_transmission_example_BWA.png")
     labels_bwa <- plot_text_labels(ggplot2::last_plot())
     expect_true(all(c("Precipitation (2011-2013)", "Cholera Cases") %in% labels_bwa))
     expect_false(any(grepl("^Cholera Cases \\(", labels_bwa)))
})

test_that(".seasonal_clustering_fits() reads the daily est_seasonal_dynamics() output", {
     mi <- withr::local_tempdir()
     fx <- .seasonal_fixture(mi)
     res <- MOSAIC:::.seasonal_clustering_fits(list(MODEL_INPUT = mi))
     expect_identical(res$source, "daily")
     expect_identical(res$path, file.path(mi, "pred_seasonal_dynamics_day.csv"))

     # weekly means of the 7-day blocks of days 1-364 (day 365 is not drawn)
     expect_equal(nrow(res$weekly), 6L * 52L)
     expect_setequal(unique(res$weekly$week), 1:52)
     ago <- fx$daily[fx$daily$iso_code == "AGO", ]
     wk3 <- res$weekly[res$weekly$iso_code == "AGO" & res$weekly$week == 3, ]
     expect_equal(wk3$fitted_values_fourier_precip,
                  mean(ago$fitted_values_fourier_precip[ago$day %in% 15:21]))
     expect_identical(unique(res$weekly$inferred_from_neighbor[res$weekly$iso_code == "BWA"]), "Angola")

     # the fit window drives the title years
     expect_identical(MOSAIC:::.seasonal_year_span(res$window$precip), "2011-2013")
     expect_identical(MOSAIC:::.seasonal_year_span(res$window$cases), "2012")
})

test_that("weekly means reproduce the estimator's clustering of the daily fits", {
     mi <- withr::local_tempdir()
     fx <- .seasonal_fixture(mi)
     res <- MOSAIC:::.seasonal_clustering_fits(list(MODEL_INPUT = mi))
     # est_seasonal_dynamics() (R/est_seasonal_dynamics.R, clustering block):
     # cutree(hclust(dist(<daily wide matrix>), method = clustering_method), k)
     daily_wide <- stats::reshape(fx$daily[, c("iso_code", "day", "fitted_values_fourier_precip")],
                                  idvar = "iso_code", timevar = "day", direction = "wide")
     est <- stats::setNames(stats::cutree(stats::hclust(stats::dist(daily_wide[, -1]), method = "ward.D2"),
                                          k = 3), daily_wide$iso_code)
     weekly_wide <- tidyr::spread(res$weekly[, c("iso_code", "week", "fitted_values_fourier_precip")],
                                  key = "week", value = "fitted_values_fourier_precip")
     expect_equal(ncol(weekly_wide) - 1L, 52L)
     plt <- stats::setNames(MOSAIC:::.seasonal_cluster(weekly_wide[, -1], "ward.D2", k = 3),
                            weekly_wide$iso_code)
     tab <- table(est[names(plt)], plt)
     expect_true(all(rowSums(tab > 0) == 1L) && all(colSums(tab > 0) == 1L))
     expect_identical(as.vector(table(plt)), c(2L, 2L, 2L))
     expect_identical(unname(plt["AGO"]), unname(plt["BDI"]))
})

test_that("the legacy weekly table is used only when the daily fits are absent", {
     mi <- withr::local_tempdir()
     tabs <- withr::local_tempdir()
     legacy <- data.frame(week = rep(1:52, 2), iso_code = rep(c("AGO", "BDI"), each = 52),
                          fitted_values_fourier_precip = c(sin(1:52), cos(1:52)),
                          fitted_values_fourier_cases = c(cos(1:52), sin(1:52)),
                          Country = rep(c("Angola", "Burundi"), each = 52),
                          inferred_from_neighbor = NA)
     utils::write.csv(legacy, file.path(tabs, "pred_seasonal_dynamics.csv"), row.names = FALSE)

     old <- MOSAIC:::.seasonal_clustering_fits(list(MODEL_INPUT = mi, DOCS_TABLES = tabs))
     expect_identical(old$source, "weekly_legacy")
     expect_equal(nrow(old$weekly), 104L)
     expect_null(old$window)       # the legacy table records no window: no years in the title
     expect_identical(MOSAIC:::.seasonal_label("Fourier series fitted to weekly precipitation",
                                               old$window$precip),
                      "Fourier series fitted to weekly precipitation")

     # Once est_seasonal_dynamics() output exists it wins over the stale weekly table
     .seasonal_fixture(mi)
     new <- MOSAIC:::.seasonal_clustering_fits(list(MODEL_INPUT = mi, DOCS_TABLES = tabs))
     expect_identical(new$source, "daily")

     # Neither file: an error that names both and the producer
     empty <- withr::local_tempdir()
     expect_error(MOSAIC:::.seasonal_clustering_fits(list(MODEL_INPUT = empty, DOCS_TABLES = empty)),
                  "pred_seasonal_dynamics_day.csv.*pred_seasonal_dynamics.csv.*est_seasonal_dynamics")
     expect_error(MOSAIC:::.seasonal_clustering_fits(list()), "PATHS\\$MODEL_INPUT not set")
})

test_that("plot_seasonal_clustering() renders from the daily fits with the fit-window title", {
     skip_if_not(isTRUE(capabilities("png")), "no png device")
     local_null_device()
     mi <- withr::local_tempdir()
     shp <- withr::local_tempdir()
     fig <- withr::local_tempdir()
     fx <- .seasonal_fixture(mi)
     .write_square_shapefile(file.path(shp, "AFRICA_ADM0.shp"), fx$isos)
     P <- list(MODEL_INPUT = mi, DATA_SHAPEFILES = shp, DOCS_FIGURES = fig)

     expect_message(
          res <- plot_seasonal_clustering(P, use_cases = FALSE, clustering_method = "ward.D2", k = 3),
          "clustering the daily seasonal fits")
     expect_true(file.exists(file.path(fig, "seasonal_precip_ward.D2_cluster.png")))
     expect_identical(res$source, file.path(mi, "pred_seasonal_dynamics_day.csv"))

     # The map's clusters are the ones est_seasonal_dynamics() computes from the
     # daily precipitation fits (R/est_seasonal_dynamics.R, clustering block)
     wide <- stats::reshape(fx$daily[, c("iso_code", "day", "fitted_values_fourier_precip")],
                            idvar = "iso_code", timevar = "day", direction = "wide")
     est <- stats::setNames(stats::cutree(stats::hclust(stats::dist(wide[, -1]), method = "ward.D2"), k = 3),
                            wide$iso_code)
     expect_setequal(names(res$clusters), fx$isos)
     tab <- table(est[names(res$clusters)], res$clusters)
     expect_true(all(rowSums(tab > 0) == 1L) && all(colSums(tab > 0) == 1L))

     labs <- plot_text_labels(res$plot)
     expect_true(any(grepl("precipitation\\s+\\(2011-2013\\)", labs)))
     expect_false(any(grepl("2014-2024", labs)))

     expect_message(
          plot_seasonal_clustering(P, use_cases = TRUE, set_inferred_to_na = TRUE,
                                   clustering_method = "ward.D2", k = 3),
          "clustering the daily seasonal fits")
     expect_true(file.exists(file.path(fig, "seasonal_cases_ward.D2_cluster_inferred.png")))
     labs <- plot_text_labels(ggplot2::last_plot())

     # Legacy input: only the old weekly table exists, so it is clustered (by week)
     # and the title carries no years
     tabs <- withr::local_tempdir()
     legacy <- MOSAIC:::.seasonal_clustering_fits(P)$weekly
     utils::write.csv(legacy, file.path(tabs, "pred_seasonal_dynamics.csv"), row.names = FALSE)
     P_old <- list(DOCS_TABLES = tabs, DATA_SHAPEFILES = shp, DOCS_FIGURES = fig)
     expect_message(res_old <- plot_seasonal_clustering(P_old, clustering_method = "ward.D2", k = 3),
                    "clustering the weekly_legacy seasonal fits")
     expect_setequal(names(res_old$clusters), fx$isos)
     labs_old <- plot_text_labels(res_old$plot)
     expect_true(any(grepl("weekly\\s+precipitation$", labs_old)))
     expect_true(any(grepl("cholera cases\\s+\\(2012\\)", labs)))
})

test_that("dbscan keeps its weekly-scale eps: clusters are found on the weekly means", {
     # dbscan's eps = 1 is a distance on the weekly scale. Daily vectors are about
     # sqrt(7) times further apart, so clustering the 365-day matrix would leave
     # every country as noise (this happens on the shipped fits). Here countries
     # in a group are 5 days apart in phase: ~0.88 apart as weekly means, ~2.3 as
     # daily vectors.
     skip_if_not(isTRUE(capabilities("png")), "no png device")
     local_null_device()
     mi <- withr::local_tempdir()
     shp <- withr::local_tempdir()
     fig <- withr::local_tempdir()
     isos <- MOSAIC::iso_codes_mosaic[1:12]
     days <- 1:365
     daily <- do.call(rbind, lapply(seq_along(isos), function(i) {
          group <- (i - 1L) %/% 4L
          shift <- 5 * ((i - 1L) %% 4L)
          f <- 2 * sin(2 * pi * (days + shift) / 365 + group * 2 * pi / 3)
          data.frame(day = days, iso_code = isos[i], Country = MOSAIC::convert_iso_to_country(isos[i]),
                     fitted_values_fourier_precip = f, fitted_values_fourier_cases = f,
                     inferred_from_neighbor = NA_character_, stringsAsFactors = FALSE)
     }))
     utils::write.csv(daily, file.path(mi, "pred_seasonal_dynamics_day.csv"), row.names = FALSE)
     .write_square_shapefile(file.path(shp, "AFRICA_ADM0.shp"), isos)

     res <- suppressMessages(plot_seasonal_clustering(
          list(MODEL_INPUT = mi, DATA_SHAPEFILES = shp, DOCS_FIGURES = fig),
          clustering_method = "dbscan", k = 3))
     expect_true(file.exists(file.path(fig, "seasonal_precip_dbscan_cluster.png")))
     cl <- as.character(res$clusters[isos])
     expect_false(any(cl == "0"))                         # no country left as noise
     expect_identical(as.vector(table(cl)), c(4L, 4L, 4L))
     expect_identical(length(unique(cl[1:4])), 1L)
     # the same rule on the daily matrix would mark every country as noise
     daily_wide <- stats::reshape(daily[, c("iso_code", "day", "fitted_values_fourier_precip")],
                                  idvar = "iso_code", timevar = "day", direction = "wide")
     expect_true(all(as.character(MOSAIC:::.seasonal_cluster(daily_wide[, -1], "dbscan", 3)) == "0"))
})
