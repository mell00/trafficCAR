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

test_that("high-degree hubs do not break construction", {
  n <- 30
  A <- matrix(0, n, n)
  A[1, 2:n] <- 1
  A[2:n, 1] <- 1

  Q <- suppressWarnings(
    intrinsic_car_precision(
      A,
      scale = TRUE,
      symmetrize = TRUE
    )
  )

  expect_s4_class(Q, "dsCMatrix")
  expect_true(Matrix::isSymmetric(Q))
})


test_that("non-symmetric adjacency is symmetrized correctly", {
  A <- matrix(0, 5, 5)
  A[1, 2] <- 1
  A[2, 3] <- 1

  Q <- suppressWarnings(
    car_precision(A, symmetrize = TRUE)
  )

  expect_true(Matrix::isSymmetric(Q))
})


test_that("large sparse graph preserves sparsity", {
  set.seed(1)
  n <- 200

  A <- Matrix::rsparsematrix(n, n, density = 0.01)
  A <- abs(A)
  A[A > 0] <- 1
  diag(A) <- 0

  Q <- suppressWarnings(
    car_precision(
      A,
      type = "icar",
      symmetrize = TRUE
    )
  )

  expect_lt(length(Q@x), n * 20)
})


test_that("permutation of nodes leaves spectrum invariant", {
  n <- 40
  A <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    A[i, i + 1] <- A[i + 1, i] <- 1
  }

  perm <- sample(n)
  A_perm <- A[perm, perm]

  Q1 <- suppressWarnings(
    intrinsic_car_precision(A, scale = TRUE, symmetrize = TRUE)
  )
  Q2 <- suppressWarnings(
    intrinsic_car_precision(A_perm, scale = TRUE, symmetrize = TRUE)
  )

  ev1 <- sort(Re(eigen(as.matrix(Q1), symmetric = TRUE)$values))
  ev2 <- sort(Re(eigen(as.matrix(Q2), symmetric = TRUE)$values))

  expect_equal(ev1, ev2, tolerance = 1e-8)
})


test_that("heterogeneous connected components scale correctly", {
  ## chain + star
  A1 <- matrix(0, 10, 10)
  for (i in 1:9) {
    A1[i, i + 1] <- A1[i + 1, i] <- 1
  }

  A2 <- matrix(0, 6, 6)
  A2[1, 2:6] <- 1
  A2[2:6, 1] <- 1

  A <- Matrix::bdiag(A1, A2)

  Q <- suppressWarnings(intrinsic_car_precision(A, scale = TRUE, symmetrize = TRUE))

  expect_true(all(is.finite(Matrix::diag(Q))))
})

test_that("proper CAR rejects invalid rho", {
  A <- matrix(0, 5, 5)
  for (i in 1:4) A[i, i+1] <- A[i+1, i] <- 1

  expect_error(
    car_precision(A, type = "proper", rho = 1, symmetrize = TRUE),
    "admissible"
  )
})

