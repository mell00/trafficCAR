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
