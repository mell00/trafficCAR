test_that("as_sparse_adjacency rejects NULL and non-matrices", {
  expect_error(as_sparse_adjacency(NULL))
  expect_error(as_sparse_adjacency(1:5))
  expect_error(as_sparse_adjacency(list(a = 1)))
})

test_that("as_sparse_adjacency rejects non-square matrices", {
  A <- matrix(1, nrow = 3, ncol = 2)
  expect_error(as_sparse_adjacency(A))
})
