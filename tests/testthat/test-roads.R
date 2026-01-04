
test_that("roads_to_segments explodes MULTILINESTRING and creates seg_id/length_m", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  ml <- sf::st_sfc(
    sf::st_multilinestring(list(
      matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE),
      matrix(c(1,0, 1,1), ncol = 2, byrow = TRUE)
    )),
    crs = 3857
  )
  roads <- sf::st_sf(id = 1, geometry = ml)

  segs <- roads_to_segments(roads)

  expect_s3_class(segs, "sf")
  expect_true(all(names(segs)[1:2] == c("seg_id", "length_m")))
  expect_equal(nrow(segs), 2)
  expect_equal(segs$seg_id, 1:2)
  expect_true(all(segs$length_m > 0))
  expect_true(all(as.character(sf::st_geometry_type(segs)) == "LINESTRING"))
})
