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

