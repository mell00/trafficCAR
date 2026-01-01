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


test_that("as_sparse_adjacency rejects non-finite values", {
  A <- matrix(c(0, Inf, Inf, 0), 2, 2)
  expect_error(as_sparse_adjacency(A), "finite|Inf|infinite", ignore.case = TRUE)

  B <- matrix(c(0, NaN, NaN, 0), 2, 2)
  expect_error(as_sparse_adjacency(B), "NA|NaN", ignore.case = TRUE)
})


test_that("as_sparse_adjacency drops explicitly stored zeros", {
  A <- Matrix::sparseMatrix(
    i = c(1, 2), j = c(2, 1), x = c(0, 0),
    dims = c(2, 2)
  )
  B <- as_sparse_adjacency(A, symmetrize = FALSE, check = FALSE)
  expect_equal(length(B@x), 0L)
})





