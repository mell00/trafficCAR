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


test_that("update_beta_gaussian recovers signal in a simple toy setup", {
  set.seed(2)
  n <- 200
  p <- 2
  X <- cbind(1, rnorm(n))
  beta_true <- c(0.7, -1.2)
  x <- rep(0, n) # isolate regression layer from spatial effects
  sigma2 <- 0.25^2
  y <- as.numeric(X %*% beta_true + x + rnorm(n, sd = sqrt(sigma2)))

  # weak prior
  b0 <- rep(0, p)
  B0 <- diag(1e6, p)

  # draw many betas (conditional on x,sigma2) and average
  B <- 2000
  draws <- replicate(B, update_beta_gaussian(y, X, x, sigma2, b0, B0))
  beta_hat <- rowMeans(draws)

  expect_equal(beta_hat[1], beta_true[1], tolerance = 0.1)
  expect_equal(beta_hat[2], beta_true[2], tolerance = 0.1)
})


test_that("update_sigma2_ig is reproducible and positive", {
  set.seed(3)
  n <- 50
  X <- cbind(1, rnorm(n))
  beta <- c(0.2, 0.5)
  x <- rnorm(n, sd = 0.3)
  y <- as.numeric(X %*% beta + x + rnorm(n, sd = 0.4))

  set.seed(99)
  s1 <- update_sigma2_ig(y, X, beta, x, a0 = 2, b0 = 1)
  set.seed(99)
  s2 <- update_sigma2_ig(y, X, beta, x, a0 = 2, b0 = 1)

  expect_true(is.finite(s1) && s1 > 0)
  expect_equal(s1, s2)
})
