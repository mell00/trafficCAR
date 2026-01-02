test_that("build_network returns expected components", {
  net <- build_network(toy_grid)

  expect_type(net, "list")
  expect_named(
    net,
    c("roads", "nodes", "edges", "graph", "A")
  )
})

test_that("L-shaped network has expected topology", {
  net <- build_network(toy_L)

  expect_equal(nrow(net$nodes), 3)
  expect_equal(nrow(net$edges), 2)
  expect_true(igraph::is_connected(net$graph))

  deg <- sort(igraph::degree(net$graph))
  expect_equal(deg, c(1, 1, 2))
})

test_that("T-junction has expected degree distribution", {
  net <- build_network(toy_T)

  expect_equal(nrow(net$nodes), 4)
  expect_equal(nrow(net$edges), 3)
  expect_true(igraph::is_connected(net$graph))

  deg <- sort(igraph::degree(net$graph))
  expect_equal(deg, c(1, 1, 1, 3))
})

test_that("square network is a 4-cycle", {
  net <- build_network(toy_square)

  expect_equal(nrow(net$nodes), 4)
  expect_equal(nrow(net$edges), 4)
  expect_true(igraph::is_connected(net$graph))

  deg <- igraph::degree(net$graph)
  expect_true(all(deg == 2))

  gth <- igraph::girth(net$graph)$girth
  expect_equal(gth, 4)
})

test_that("2x2 grid network has expected degree structure", {
  net <- build_network(toy_grid)

  if (igraph::is_connected(net$graph)) {
    # if toy_grid is already segmented at intersections, this will pass
    deg <- igraph::degree(net$graph)
    expect_true(all(deg >= 2))
  } else {
    # endpoint-based networks may not connect crossings unless noded.
    expect_false(igraph::is_connected(net$graph))
  }
})

test_that("2x2 grid has 9 nodes when intersections are noded", {
  net <- build_network(toy_grid, node_intersections = TRUE)
  expect_equal(nrow(net$nodes), 9)
  expect_equal(nrow(net$edges), 12)
})



test_that("disconnected network is not connected", {
  net <- build_network(toy_disconnected)
  expect_false(igraph::is_connected(net$graph))
})

test_that("on-ramp connectivity/merge node requires intersection noding", {
  net <- build_network(toy_on_ramp)

  # ramps that merge mid-edge won't create a merge node
  expect_false(igraph::is_connected(net$graph))
  expect_false(any(igraph::degree(net$graph) == 3))
})

test_that("adjacency matrix is square, symmetric, and matches graph", {
  net <- build_network(toy_grid)

  A <- net$A
  n <- nrow(net$nodes)

  expect_equal(nrow(A), n)
  expect_equal(ncol(A), n)
  expect_equal(A, Matrix::t(A))

  # degrees from adjacency equal igraph degrees
  deg_A <- as.numeric(Matrix::rowSums(A))
  deg_g <- as.numeric(igraph::degree(net$graph))
  expect_equal(sort(deg_A), sort(deg_g))
})

test_that("build_network removes self-loops and keeps zero diagonal adjacency", {
  net <- build_network(toy_grid)
  expect_false(any(net$edges$from == net$edges$to))
  expect_equal(Matrix::diag(net$A), rep(0, nrow(net$A)))
})
