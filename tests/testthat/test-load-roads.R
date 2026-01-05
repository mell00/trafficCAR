
test_that("load_roads returns sf object unchanged", {
  data(roads_small, package = "trafficCAR")
  out <- trafficCAR:::load_roads(roads_small)
  expect_s3_class(out, "sf")
})

