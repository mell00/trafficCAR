

test_that("sample_proper_car returns expected shapes and finite values", {
  set.seed(1)

  # simple 4-node path graph adjacency
  A <- Matrix::Matrix(0, 4, 4, sparse = TRUE)
  A[1, 2] <- 1; A[2, 1] <- 1
  A[2, 3] <- 1; A[3, 2] <- 1
  A[3, 4] <- 1; A[4, 3] <- 1

  y <- rep(0, 4)

  fit <- sample_proper_car(
    y = y, A = A, rho = 0.9,
    n_iter = 50, burn = 10, thin = 2,
    a_tau = 1, b_tau = 1, a_kappa = 1, b_kappa = 1
  )

  expect_true(is.list(fit))
  expect_true(is.matrix(fit$x))
  expect_equal(ncol(fit$x), 4)
  expect_equal(nrow(fit$x), length(seq.int(11, 50, by = 2)))
  expect_equal(length(fit$tau), nrow(fit$x))
  expect_equal(length(fit$kappa), nrow(fit$x))

  expect_true(all(is.finite(fit$x)))
  expect_true(all(is.finite(fit$tau)))
  expect_true(all(is.finite(fit$kappa)))
  expect_true(all(fit$tau > 0))
  expect_true(all(fit$kappa > 0))
})
