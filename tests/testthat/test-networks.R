test_that("build_network returns expected components", {
  net <- build_network(toy_grid)

  expect_type(net, "list")
  expect_named(
    net,
    c("roads", "nodes", "edges", "graph", "A")
  )
})
