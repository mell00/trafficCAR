
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
