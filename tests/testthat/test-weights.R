test_that("as_sparse_adjacency rejects NULL and non-matrices", {
  expect_error(as_sparse_adjacency(NULL))
  expect_error(as_sparse_adjacency(1:5))
  expect_error(as_sparse_adjacency(list(a = 1)))
})


test_that("as_sparse_adjacency rejects non-square matrices", {
  A <- matrix(1, nrow = 3, ncol = 2)
  expect_error(as_sparse_adjacency(A))
})


test_that("as_sparse_adjacency rejects NA values", {
  A <- matrix(c(0, 1, NA, 0), 2, 2)
  expect_error(as_sparse_adjacency(A))
})


test_that("as_sparse_adjacency coerces to numeric sparse matrix", {
  A <- matrix(c(TRUE, FALSE, FALSE, TRUE), 2, 2)
  B <- as_sparse_adjacency(A, symmetrize = TRUE)

  expect_s4_class(B, "dgCMatrix")
  expect_type(B@x, "double")
})


test_that("diagonal is forced to zero", {
  A <- matrix(c(1, 1, 1, 1), 2, 2)
  B <- as_sparse_adjacency(A, symmetrize = TRUE)
  expect_equal(Matrix::diag(B), c(0, 0))
})



