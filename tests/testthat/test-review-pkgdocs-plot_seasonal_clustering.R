# Regression test for x-docs-skills-04: the default clustering_method was
# "hierarchical", which the dispatch rejected, so every default call errored.

test_that("plot_seasonal_clustering() defaults to the documented ward.D2 method", {
     choices <- eval(formals(plot_seasonal_clustering)$clustering_method)
     expect_identical(choices[1], "ward.D2")
     expect_setequal(choices, c("ward.D2", "kmeans", "dbscan", "knn"))
})

test_that("an unknown clustering_method is rejected before any file is read", {
     paths <- list(DOCS_TABLES = tempfile("absent"), DATA_SHAPEFILES = tempfile("absent"))
     expect_error(plot_seasonal_clustering(paths, clustering_method = "hierarchical"),
                  "should be one of")
})

test_that("every advertised clustering_method returns one label per country", {
     set.seed(1)
     # Three well-separated seasonal shapes, four countries each, 52 weeks.
     wk <- seq_len(52)
     shapes <- rbind(sin(2 * pi * wk / 52), cos(2 * pi * wk / 52), rep(0, 52))
     x <- shapes[rep(1:3, each = 4), ] * 5 + matrix(rnorm(12 * 52, sd = 0.05), 12)
     for (m in eval(formals(plot_seasonal_clustering)$clustering_method)) {
          cl <- MOSAIC:::.seasonal_cluster(as.data.frame(x), m, k = 3)
          expect_length(cl, 12L)
          expect_false(anyNA(cl), info = m)
     }
     # The distance-based methods recover the three groups.
     for (m in c("ward.D2", "kmeans")) {
          cl <- MOSAIC:::.seasonal_cluster(x, m, k = 3)
          expect_equal(length(unique(cl)), 3L, info = m)
          expect_equal(as.vector(table(cl)), c(4L, 4L, 4L), info = m)
     }
     # knn used to call FNN::get.knnx() without a query set and always errored.
     expect_no_error(MOSAIC:::.seasonal_cluster(x, "knn", k = 3))
})
