library(testthat)

test_that("calculate_metrics_weight returns correct weights", {
  expect_equal(
    unname(calculate_metrics_weight(node_strength = TRUE)),
    c(0, 1, 0, 0)
  )

  expect_equal(
    unname(calculate_metrics_weight(sum_of_nearest_neighbors = TRUE)),
    c(0, 0, 1, 0)
  )

  expect_equal(
    unname(calculate_metrics_weight(eigenvector_centrality = TRUE)),
    c(0, 0, 0, 1)
  )

  expect_equal(
    unname(calculate_metrics_weight(node_strength = TRUE, sum_of_nearest_neighbors = TRUE,
                                    eigenvector_centrality = TRUE)),
    c(0, 3, 3, 3)
  )

  expect_equal(
    unname(calculate_metrics_weight(
      betweenness_metric = TRUE, sum_of_nearest_neighbors = TRUE,
      eigenvector_centrality = TRUE
    )),
    c(2, 0, 4, 4)
  )

  expect_equal(
    unname(calculate_metrics_weight(
      betweenness_metric = TRUE, node_strength = TRUE, sum_of_nearest_neighbors = TRUE,
      eigenvector_centrality = TRUE
    )),
    c(2, 6, 6, 6)
  )
})

test_that("get helper files", {
  expect_true(file.exists(.get_helper_filepath("parameters")))
  expect_true(file.exists(.get_helper_filepath("zero_raster")))
  expect_true(file.exists(.get_helper_filepath("map_grey_background")))
})
