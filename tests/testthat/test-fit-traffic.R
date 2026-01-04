test_that("prep_speed log transform + metadata works", {
  x <- c(0, 5, 10)
  out <- prep_speed(x, transform = "log", eps = 1e-6)

  expect_true(is.list(out))
  expect_true(is.numeric(out$y))
  expect_equal(length(out$y), length(x))
  expect_equal(out$meta$outcome, "speed")
  expect_equal(out$meta$transform, "log")

  mu <- out$y
  bt <- out$meta$inv(mu)
  expect_true(all(bt >= 0))
  expect_equal(length(bt), length(x))
})



test_that("prep_speed handles adversarial inputs", {
  expect_error(prep_speed("a"), "numeric")
  expect_error(prep_speed(c(1, NA)), "finite")
  expect_error(prep_speed(c(1, Inf)), "finite")
  expect_error(prep_speed(c(-1, 2)), "nonnegative")
  expect_error(prep_speed(c(1, 2), eps = 0), "eps")
  expect_error(prep_speed(c(1, 2), eps = -1), "eps")
})



test_that("prep_travel_time per-distance + log works", {
  tt <- c(10, 20, 30)
  d <- c(5, 10, 15)

  out <- prep_travel_time(
    tt, distance = d, per_distance = TRUE,
    transform = "log", eps = 1e-6
  )

  expect_equal(out$meta$outcome, "travel_time")
  expect_true(out$meta$per_distance)
  expect_equal(out$meta$base, "travel_time_per_distance")

  bt <- out$meta$inv(out$y)
  expect_true(all(bt >= 0))
})



test_that("prep_travel_time adversarial inputs", {
  expect_error(prep_travel_time("a"), "numeric")
  expect_error(prep_travel_time(c(1, NA)), "finite")
  expect_error(prep_travel_time(c(-1, 2)), "nonnegative")
  expect_error(prep_travel_time(c(1, 2), per_distance = TRUE), "requires")
  expect_error(
    prep_travel_time(c(1, 2), distance = c(0, 1), per_distance = TRUE),
    "positive"
  )
  expect_error(prep_travel_time(c(1, 2), eps = 0), "eps")
})
