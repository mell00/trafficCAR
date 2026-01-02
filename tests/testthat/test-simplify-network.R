test_that("build_network(simplify=TRUE) keeps edges/graph consistent", {
  roads_dup <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)) # duplicate
    ),
    crs = 4326
  )

  net <- build_network(roads_dup, simplify = TRUE)

  expect_equal(nrow(net$edges), 1)
  expect_equal(igraph::ecount(net$graph), 1)
  expect_equal(Matrix::nnzero(net$A), 2)
})
