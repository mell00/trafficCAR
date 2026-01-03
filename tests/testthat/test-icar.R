test_that("ICAR enforces sum-to-zero per connected component", {

  A <- Matrix::Matrix(0, 6, 6, sparse = TRUE)

  # component 1: 1-2-3
  A[1,2] <- A[2,1] <- 1
  A[2,3] <- A[3,2] <- 1

  # component 2: 4-5
  A[4,5] <- A[5,4] <- 1

  # node 6 isolate

  set.seed(1)
  x <- sample_icar(A, tau = 2)

  expect_equal(sum(x[1:3]), 0, tolerance = 1e-6)
  expect_equal(sum(x[4:5]), 0, tolerance = 1e-6)
  expect_true(is.finite(x[6]))
})


test_that("ICAR isolate = drop sets isolated nodes to NA", {

  A <- Matrix::Matrix(0, 4, 4, sparse = TRUE)
  A[1,2] <- A[2,1] <- 1
  A[2,3] <- A[3,2] <- 1
  # node 4 isolate

  x <- sample_icar(A, isolate = "drop")

  expect_true(is.na(x[4]))
  expect_equal(sum(x[1:3]), 0, tolerance = 1e-6)
})


test_that("ICAR sampling is reproducible", {

  A <- Matrix::bandSparse(
    10,
    k = c(-1, 1),
    diag = list(rep(1, 9), rep(1, 9))
  )

  set.seed(123)
  x1 <- sample_icar(A)

  set.seed(123)
  x2 <- sample_icar(A)

  expect_equal(x1, x2)
})


test_that("ICAR handles all-isolates graph", {
  A <- Matrix::Matrix(0, 5, 5, sparse = TRUE)

  set.seed(1)
  x_ind <- sample_icar(A, isolate = "independent", tau = 2)
  expect_true(all(is.finite(x_ind)))
  expect_equal(length(x_ind), 5)

  x_drop <- sample_icar(A, isolate = "drop", tau = 2)
  expect_true(all(is.na(x_drop)))
  expect_equal(length(x_drop), 5)
})
