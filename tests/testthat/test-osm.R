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



