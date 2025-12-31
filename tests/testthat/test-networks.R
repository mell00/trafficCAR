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

test_that("2x2 grid network has expected node/edge counts", {
  net <- build_network(toy_grid)

  expect_equal(nrow(net$nodes), 9)
  expect_equal(nrow(net$edges), 12)
  expect_true(igraph::is_connected(net$graph))

  deg <- igraph::degree(net$graph)
  expect_equal(sum(deg == 4), 1)  # center
  expect_equal(sum(deg == 3), 4)  # edge centers
  expect_equal(sum(deg == 2), 4)  # corners
})

test_that("disconnected network is not connected", {
  net <- build_network(toy_disconnected)

  expect_equal(nrow(net$nodes), 4)
  expect_equal(nrow(net$edges), 2)
  expect_false(igraph::is_connected(net$graph))
})

test_that("on-ramp is connected and has a merge node", {
  net <- build_network(toy_on_ramp)

  expect_true(igraph::is_connected(net$graph))
  expect_true(any(igraph::degree(net$graph) == 3))
  expect_true(any(igraph::degree(net$graph) == 1)) # ramp endpoint
})
