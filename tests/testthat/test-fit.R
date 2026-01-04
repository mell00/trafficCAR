testthat::test_that("fit_car basic smoke test: runs and returns correct structure", {
  skip_if_not(exists("fit_car", mode = "function"))

  set.seed(1)

  n <- 6
  A <- matrix(0, n, n)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  A[4, 5] <- 1; A[5, 4] <- 1
  # node 6 isolate

  X <- cbind(1, seq_len(n))
  beta_true <- c(0.5, -0.2)
  x_true <- c(0.2, -0.1, -0.1, 0.3, 0.0, 0.0)
  sigma2_true <- 0.25

  y <- as.double(X %*% beta_true + x_true + rnorm(n, sd = sqrt(sigma2_true)))

  fit <- fit_car(
    y = y, A = A, X = X,
    type = "proper", rho = 0.7, tau = 2,
    n_iter = 50, burn_in = 10, thin = 2,
    a0 = 2, b0_sigma = 1
  )

  testthat::expect_s3_class(fit, "trafficCAR_fit")
  testthat::expect_true(is.list(fit$draws))
  testthat::expect_true(all(c("x", "beta", "sigma2") %in% names(fit$draws)))

  n_keep <- length(seq.int(11, 50, by = 2))
  testthat::expect_equal(dim(fit$draws$x), c(n_keep, n))
  testthat::expect_equal(dim(fit$draws$beta), c(n_keep, ncol(X)))
  testthat::expect_equal(length(fit$draws$sigma2), n_keep)

  testthat::expect_true(all(is.finite(fit$draws$x)))
  testthat::expect_true(all(is.finite(fit$draws$beta)))
  testthat::expect_true(all(is.finite(fit$draws$sigma2)))
  testthat::expect_true(all(fit$draws$sigma2 > 0))
})
