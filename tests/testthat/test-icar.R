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
