test_that("proper CAR behaves near the ICAR boundary", {
  n <- 50
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- A[i + 1, i] <- 1
  }

  Q1 <- car_precision(
    A,
    type = "proper",
    rho = 0.999,
    tau = 1,
    symmetrize = TRUE
  )

  Q2 <- car_precision(
    A,
    type = "proper",
    rho = 0.9,
    tau = 1,
    symmetrize = TRUE
  )

  ## near-singular but finite
  expect_true(all(is.finite(Matrix::diag(Q1))))
  expect_gt(kappa(as.matrix(Q1)), kappa(as.matrix(Q2)))
})
