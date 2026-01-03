test_that("update_beta_gaussian returns correct length and is reproducible", {
  set.seed(1)
  n <- 20
  p <- 3
  X <- matrix(rnorm(n * p), n, p)
  y <- rnorm(n)
  x <- rnorm(n)
  sigma2 <- 0.5
  b0 <- rep(0, p)
  B0 <- diag(10, p)

  set.seed(123)
  b1 <- update_beta_gaussian(y, X, x, sigma2, b0, B0)
  set.seed(123)
  b2 <- update_beta_gaussian(y, X, x, sigma2, b0, B0)

  expect_length(b1, p)
  expect_equal(b1, b2)
})
