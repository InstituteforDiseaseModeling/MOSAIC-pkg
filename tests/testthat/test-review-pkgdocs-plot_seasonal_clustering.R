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
