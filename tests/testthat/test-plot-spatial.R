
test_that("plot_predicted returns a ggplot with an sf layer and viridis scale", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  roads <- sf::st_sf(
    segment_id = 1:3,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 1, 2, 1), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  mu <- matrix(
    c(10, 11, 12,
      20, 21, 22,
      30, 31, 32,
      40, 41, 42),
    nrow = 4, byrow = TRUE
  )

  fit <- structure(
    list(draws = list(mu = mu), outcome_label = "Predicted speed"),
    class = "traffic_fit"
  )

  roads_copy <- roads
  p <- plot_predicted(fit, roads)

  expect_s3_class(p, "ggplot")
  expect_false("predicted" %in% names(roads_copy))

  expect_true(length(p$layers) >= 1)
  expect_true(inherits(p$layers[[1]]$geom, "GeomSf"))

  expect_true(length(p$scales$scales) >= 1)
  expect_match(p$scales$scales[[1]]$name, "Predicted speed")
})
