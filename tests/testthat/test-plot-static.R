

test_that("plot_roads_static validates sf_aug is sf", {
  expect_error(
    trafficCAR::plot_roads_static(data.frame(x = 1)),
    "sf_aug.*sf",
    ignore.case = TRUE
  )
})



test_that("plot_roads_static validates value choices via match.arg", {
  sf_aug <- sf::st_sf(
    predicted_speed = 10,
    predicted_volume = 100,
    relative_congestion = 0.2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  expect_error(
    trafficCAR::plot_roads_static(sf_aug, value = "nope"),
    "should be one of|one of",
    ignore.case = TRUE
  )
})

