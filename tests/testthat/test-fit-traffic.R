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
