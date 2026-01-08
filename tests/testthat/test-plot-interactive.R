
.make_fake_sf <- function(n = 5) {
  sf::st_sf(
    seg_id = seq_len(n),
    predicted_mean = runif(n, 20, 60),
    relative_congestion = rnorm(n),
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(i) {
        sf::st_linestring(matrix(c(i, 0, i + 1, 0), ncol = 2, byrow = TRUE))
      }),
      crs = 4326
    )
  )
}



test_that(".value_registry is well-formed", {
  expect_true(is.list(.value_registry))
  expect_true(all(c("column", "label") %in% names(.value_registry[[1]])))

  for (v in names(.value_registry)) {
    expect_true(is.character(.value_registry[[v]]$column))
    expect_true(is.character(.value_registry[[v]]$label))
    expect_length(.value_registry[[v]]$label, 1)
  }
})


test_that("map_roads_interactive validates inputs", {
  sf_ok <- .make_fake_sf()

  expect_error(map_roads_interactive(list()), "sf object")

  expect_error(map_roads_interactive(sf_ok, value = "bad_value"), "arg")

  expect_error(map_roads_interactive(sf_ok, engine = "plotly"), "leaflet")
})


test_that("map_roads_interactive_layers validates inputs", {
  sf_ok <- .make_fake_sf()

  expect_error(map_roads_interactive_layers(list()), "sf")

  expect_error(map_roads_interactive_layers(sf_ok, values = "bad_value"),
    "No valid traffic measures")
})

