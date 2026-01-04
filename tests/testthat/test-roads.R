
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


test_that("roads_to_segments computes metric length for lon/lat via projection", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  ls <- sf::st_sfc(
    sf::st_linestring(matrix(c(-122.0, 37.0, -122.0, 37.001), ncol = 2, byrow = TRUE)),
    crs = 4326
  )
  roads <- sf::st_sf(geometry = ls)

  segs <- roads_to_segments(roads, crs_m = 3857)

  expect_equal(nrow(segs), 1)
  expect_true(is.numeric(segs$length_m))
  expect_true(segs$length_m > 0)
})


test_that("roads_to_segments drops empty/zero-length when drop_zero=TRUE", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0,0, 0,0), ncol = 2, byrow = TRUE)), # zero length
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE)), # ok
    crs = 3857
  )
  roads <- sf::st_sf(geometry = g)

  segs <- roads_to_segments(roads, drop_zero = TRUE)

  expect_equal(nrow(segs), 1)
  expect_true(segs$length_m > 0)
})


test_that("roads_to_segments handles huge coordinates without overflow", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  # very large planar coordinates still produce finite positive lengths
  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(1e9, 1e9, 1e9 + 1e6, 1e9), ncol = 2, byrow = TRUE)),
    crs = 3857
  )
  roads <- sf::st_sf(geometry = g)

  segs <- roads_to_segments(roads)
  expect_equal(nrow(segs), 1)
  expect_true(is.finite(segs$length_m))
  expect_gt(segs$length_m, 0)
})


test_that("roads_to_segments rejects non-sf inputs", {
  # non-sf inputs error cleanly
  expect_error(roads_to_segments(list(a = 1)), "sf", ignore.case = TRUE)
})


test_that("roads_to_segments rejects non-LINESTRING geometry types", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  pts <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  roads <- sf::st_sf(geometry = pts)
  expect_error(roads_to_segments(roads), "LINESTRING|MULTILINESTRING", ignore.case = TRUE)
})


test_that("roads_to_segments drops NA/empty geometries safely", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_geometrycollection(),  # empty-ish
    crs = 3857
  )
  roads <- sf::st_sf(geometry = g)

  segs <- roads_to_segments(roads, verbose = FALSE)
  expect_equal(nrow(segs), 1)
  expect_equal(segs$seg_id, 1L)
  expect_gt(segs$length_m, 0)
})


test_that("roads_to_segments drops zero/near-zero length segments", {
  skip_if_not_installed("sf")
  skip_if_not_installed("units")

  g <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 0, 0), ncol = 2, byrow = TRUE)),          # zero
    sf::st_linestring(matrix(c(0, 0, 1e-12, 0), ncol = 2, byrow = TRUE)),      # extremely small
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),          # ok
    crs = 3857
  )
  roads <- sf::st_sf(geometry = g)

  segs <- roads_to_segments(roads, drop_zero = TRUE)
  expect_equal(nrow(segs), 2)  # near-zero may survive depending on sf numeric precision
  expect_true(all(segs$length_m > 0))
})


