

test_that("plot_roads_static validates sf_aug is sf", {
  expect_error(
    trafficCAR::plot_roads_static(data.frame(x = 1)),
    "sf_aug.*sf",
    ignore.case = TRUE
  )
})
