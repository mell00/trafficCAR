test_that("fetch_osm_roads errors if osmdata is not installed", {
  if (requireNamespace("osmdata", quietly = TRUE)) {
    skip("osmdata is installed; cannot test missing-dependency error path.")
  }

  expect_error(
    fetch_osm_roads("Berkeley, CA"),
    "osmdata",
    ignore.case = TRUE
  )
})


test_that("fetch_osm_roads validates `place` when character", {
  skip_if_not_installed("osmdata")
  skip_if_not_installed("sf")

  expect_error(
    fetch_osm_roads(c("A", "B")),
    "`place` must be a single place name",
    fixed = TRUE
  )

  # unresolved place -> NULL bbox
  local_mocked_bindings(
    osm_getbb = function(place) NULL,
    .env = asNamespace("trafficCAR")
  )

  expect_error(
    fetch_osm_roads("this is not a real place probably"),
    "Unable to resolve `place` to a bounding box",
    ignore.case = FALSE
  )
})


test_that("fetch_osm_roads accepts character place and passes bbox through opq()", {
  skip_if_not_installed("osmdata")
  skip_if_not_installed("sf")

  bbox <- matrix(
    c(-122.30, 37.80, -122.20, 37.90),
    nrow = 2,
    dimnames = list(c("x", "y"), c("min", "max"))
  )

  got_bbox <- NULL
  got_features <- list()

  roads_sf <- sf::st_sf(
    osm_id = 1,
    geometry = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(1, 1)))),
    crs = 4326
  )

  local_mocked_bindings(
    osm_getbb = function(place) bbox,
    osm_opq = function(bbox_arg) {
      got_bbox <<- bbox_arg
      structure(list(bbox = bbox_arg), class = "opq")
    },
    osm_add_feature = function(q, key, value = NULL) {
      got_features[[length(got_features) + 1L]] <<- list(key = key, value = value)
      q
    },
    osm_sf = function(q, quiet = TRUE, ...) {
      list(osm_lines = roads_sf)
    },
    .env = asNamespace("trafficCAR")
  )

  res <- fetch_osm_roads(
    "Berkeley, CA",
    key = "highway",
    value = c("primary", "secondary"),
    layer = "osm_lines",
    quiet = TRUE
  )

  expect_s3_class(res, "sf")
  expect_identical(res, roads_sf)

  expect_true(is.matrix(got_bbox))
  expect_identical(got_bbox, bbox)

  expect_true(length(got_features) >= 1L)
  expect_identical(got_features[[1L]]$key, "highway")
  expect_identical(got_features[[1L]]$value, c("primary", "secondary"))
})


test_that("fetch_osm_roads accepts bbox object input and does not call getbb()", {
  skip_if_not_installed("osmdata")
  skip_if_not_installed("sf")

  bbox <- matrix(
    c(-0.2, 51.4, 0.2, 51.6),
    nrow = 2,
    dimnames = list(c("x", "y"), c("min", "max"))
  )

  getbb_called <- 0L
  got_bbox <- NULL

  roads_sf <- sf::st_sf(
    osm_id = 1,
    geometry = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(1, 0)))),
    crs = 4326
  )

  local_mocked_bindings(
    osm_getbb = function(...) {
      getbb_called <<- getbb_called + 1L
      bbox
    },
    osm_opq = function(bbox_arg) {
      got_bbox <<- bbox_arg
      structure(list(bbox = bbox_arg), class = "opq")
    },
    osm_add_feature = function(q, key, value = NULL) q,
    osm_sf = function(q, quiet = TRUE, ...) list(osm_lines = roads_sf),
    .env = asNamespace("trafficCAR")
  )

  res <- fetch_osm_roads(bbox, layer = "osm_lines")

  expect_s3_class(res, "sf")
  expect_identical(res, roads_sf)
  expect_identical(got_bbox, bbox)
  expect_identical(getbb_called, 0L)
})
