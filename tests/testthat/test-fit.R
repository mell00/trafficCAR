testthat::test_that("fit_car basic smoke test runs and returns correct structure", {
  skip_if_not(exists("fit_car", mode = "function"))

  set.seed(1)

  n <- 6
  A <- matrix(0, n, n)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  A[4, 5] <- 1; A[5, 4] <- 1
  # node 6 isolate (ICAR allowed, but should warn)

  X <- cbind(1, seq_len(n))
  beta_true <- c(0.5, -0.2)
  x_true <- c(0.2, -0.1, -0.1, 0.3, 0.0, 0.0)
  sigma2_true <- 0.25
  y <- as.double(X %*% beta_true + x_true + rnorm(n, sd = sqrt(sigma2_true)))

  warned <- FALSE
  fit <- withCallingHandlers(
    fit_car(
      y = y, A = A, X = X,
      type = "icar", tau = 2,
      n_iter = 50, burn_in = 10, thin = 2,
      a0 = 2, b0_sigma = 1,
      center_icar = TRUE
    ),
    warning = function(w) {
      warned <<- TRUE
      testthat::expect_match(
        conditionMessage(w),
        "isolat|degree 0|singular",
        ignore.case = TRUE
      )
      invokeRestart("muffleWarning")
    }
  )
  testthat::expect_true(warned)

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



testthat::test_that("fit_car reproducibility under set.seed", {
  skip_if_not(exists("fit_car", mode = "function"))

  n <- 5
  A <- matrix(0, n, n)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  A[3, 4] <- 1; A[4, 3] <- 1
  A[4, 5] <- 1; A[5, 4] <- 1

  X <- cbind(1, seq_len(n))
  y <- as.double(rnorm(n))

  set.seed(123)
  fit1 <- fit_car(y, A, X = X, type = "proper", rho = 0.5, tau = 1, n_iter = 30, burn_in = 10, thin = 1)

  set.seed(123)
  fit2 <- fit_car(y, A, X = X, type = "proper", rho = 0.5, tau = 1, n_iter = 30, burn_in = 10, thin = 1)

  testthat::expect_equal(fit1$draws$sigma2, fit2$draws$sigma2)
  testthat::expect_equal(fit1$draws$beta, fit2$draws$beta)
  testthat::expect_equal(fit1$draws$x, fit2$draws$x)
})


testthat::test_that("fit_car ICAR centering: sum-to-zero per connected component (including isolate)", {
  skip_if_not(exists("fit_car", mode = "function"))

  set.seed(2)

  n <- 6
  A <- matrix(0, n, n)
  # component 1: 1-2-3
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  # component 2: 4-5
  A[4, 5] <- 1; A[5, 4] <- 1
  # isolate: 6

  y <- as.double(rnorm(n))

  warned <- FALSE
  fit <- withCallingHandlers(
    fit_car(
      y = y, A = A, X = NULL,
      type = "icar", tau = 1,
      n_iter = 40, burn_in = 10, thin = 1,
      center_icar = TRUE
    ),
    warning = function(w) {
      warned <<- TRUE
      testthat::expect_match(
        conditionMessage(w),
        "isolat|degree 0|singular",
        ignore.case = TRUE
      )
      invokeRestart("muffleWarning")
    }
  )
  testthat::expect_true(warned)

  x_draws <- fit$draws$x

  comp1 <- 1:3
  comp2 <- 4:5
  iso <- 6

  m1 <- rowMeans(x_draws[, comp1, drop = FALSE])
  m2 <- rowMeans(x_draws[, comp2, drop = FALSE])
  m3 <- x_draws[, iso]

  testthat::expect_true(max(abs(m1)) < 1e-10)
  testthat::expect_true(max(abs(m2)) < 1e-10)
  testthat::expect_true(max(abs(m3)) < 1e-10)
})
