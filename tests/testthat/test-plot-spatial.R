
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



test_that("plot_relative_congestion returns ggplot with sf layer and gradient2 scale", {
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

  x <- matrix(
    c(-1, 0, 1,
      -2, 0, 2,
      -1, 0, 1,
      -3, 0, 3),
    nrow = 4, byrow = TRUE
  )

  fit <- structure(
    list(draws = list(x = x), outcome_label = "ignored"),
    class = "traffic_fit"
  )

  roads_copy <- roads
  p <- plot_relative_congestion(fit, roads)

  expect_s3_class(p, "ggplot")
  expect_false("relative_congestion" %in% names(roads_copy))

  expect_true(length(p$layers) >= 1)
  expect_true(inherits(p$layers[[1]]$geom, "GeomSf"))

  expect_true(length(p$scales$scales) >= 1)
  expect_match(p$scales$scales[[1]]$name, "Relative congestion")
})


test_that("plot_predicted rejects non-sf roads and missing geometry", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  fit <- structure(
    list(draws = list(mu = matrix(rnorm(6), nrow = 2)), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_predicted(fit, data.frame(segment_id = 1:3)),
    "sf|geometry",
    ignore.case = TRUE
  )

  roads <- sf::st_sf(
    segment_id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 0)),
      sf::st_point(c(2, 0))
    ),
    geometry = sf::st_sfc(sf::st_point(c(0, 0))),
    crs = 4326
  )
  roads_nogeo <- sf::st_drop_geometry(roads)

  expect_error(
    plot_predicted(fit, roads_nogeo),
    "geometry|sf",
    ignore.case = TRUE
  )
})


test_that("plot_relative_congestion rejects non-sf roads and missing geometry", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  fit <- structure(
    list(draws = list(x = matrix(rnorm(6), nrow = 2)), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_relative_congestion(fit, data.frame(segment_id = 1:3)),
    "sf|geometry",
    ignore.case = TRUE
  )

  roads <- sf::st_sf(
    segment_id = 1:3,

    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 0)),
      sf::st_point(c(2, 0))
    ),

    geometry = sf::st_sfc(sf::st_point(c(0, 0))),
    crs = 4326
  )
  roads_nogeo <- sf::st_drop_geometry(roads)

  expect_error(
    plot_relative_congestion(fit, roads_nogeo),
    "geometry|sf",
    ignore.case = TRUE
  )
})


test_that("plot_predicted rejects invalid fit structure / missing draws", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  roads <- sf::st_sf(
    segment_id = 1:2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  expect_error(
    plot_predicted(list(draws = list(mu = matrix(rnorm(4), nrow = 2))), roads),
    "traffic_fit|fit",
    ignore.case = TRUE
  )

  fit_bad <- structure(
    list(draws = list(), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_predicted(fit_bad, roads),
    "mu|draws",
    ignore.case = TRUE
  )
})



test_that("plot_relative_congestion rejects invalid fit structure / missing draws", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  roads <- sf::st_sf(
    segment_id = 1:2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  expect_error(
    plot_relative_congestion(list(draws = list(x = matrix(rnorm(4), nrow = 2))), roads),
    "traffic_fit|fit",
    ignore.case = TRUE
  )

  fit_bad <- structure(
    list(draws = list(), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_relative_congestion(fit_bad, roads),
    "x|draws",
    ignore.case = TRUE
  )
})



test_that("plot_predicted rejects draw matrices with wrong dimensions or non-numeric", {
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

  fit_wrong_n <- structure(
    list(draws = list(mu = matrix(rnorm(8), nrow = 2, ncol = 4)), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_predicted(fit_wrong_n, roads),
    "length|dimension|nrow|segments",
    ignore.case = TRUE
  )

  fit_nonnum <- structure(
    list(draws = list(mu = matrix(letters[1:6], nrow = 2)), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_predicted(fit_nonnum, roads),
    "numeric",
    ignore.case = TRUE
  )
})

test_that("plot_relative_congestion rejects draw matrices with wrong dimensions or non-numeric", {
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

  fit_wrong_n <- structure(
    list(draws = list(x = matrix(rnorm(8), nrow = 2, ncol = 4)), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_relative_congestion(fit_wrong_n, roads),
    "length|dimension|nrow|segments",
    ignore.case = TRUE
  )

  fit_nonnum <- structure(
    list(draws = list(x = matrix(letters[1:6], nrow = 2)), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_relative_congestion(fit_nonnum, roads),
    "numeric",
    ignore.case = TRUE
  )
})


test_that("non-finite values in draws are rejected (NA/Inf/NaN)", {
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

  mu_bad <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 2, byrow = TRUE)
  fit_mu_bad <- structure(
    list(draws = list(mu = mu_bad), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_predicted(fit_mu_bad, roads),
    "finite|NA|Inf|NaN",
    ignore.case = TRUE
  )

  x_bad <- matrix(c(1, 2, Inf, 4, 5, 6), nrow = 2, byrow = TRUE)
  fit_x_bad <- structure(
    list(draws = list(x = x_bad), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_error(
    plot_relative_congestion(fit_x_bad, roads),
    "finite|NA|Inf|NaN",
    ignore.case = TRUE
  )
})


test_that("constant x implies sd=0 is handled (no NaN/Inf in mapped column)", {
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

  x_const <- matrix(rep(5, 12), nrow = 4, ncol = 3)
  fit <- structure(
    list(draws = list(x = x_const), outcome_label = "y"),
    class = "traffic_fit"
  )

  expect_warning(
    p <- plot_relative_congestion(fit, roads),
    "sd|zero|constant",
    ignore.case = TRUE
  )

  built <- ggplot2::ggplot_build(p)
  expect_true("colour" %in% names(built$data[[1]]))

  # ggplot object stores the sf data with the computed column
  expect_true("relative_congestion" %in% names(p$data))
  expect_true(is.numeric(p$data$relative_congestion))
  expect_true(all(is.finite(p$data$relative_congestion)))
  expect_true(all(p$data$relative_congestion == 0))

})


test_that("plot_predicted works when outcome_label is NULL/empty", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  roads <- sf::st_sf(
    segment_id = 1:2,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  mu <- matrix(c(1, 2, 3, 4), nrow = 2)
  fit <- structure(
    list(draws = list(mu = mu), outcome_label = NULL),
    class = "traffic_fit"
  )

  expect_silent(p <- plot_predicted(fit, roads))
  expect_s3_class(p, "ggplot")
})

