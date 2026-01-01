
#' Coerce/validate adjacency matrix as sparse with zero diagonal
#'
#' @param A Square matrix-like object.
#' @param symmetrize If TRUE, replace A by (A + t(A))/2.
#' @param check If TRUE, validate shape/type and warn on common issues.
#'
#' @return A sparse `dgCMatrix` with diagonal set to 0.
#' @keywords internal
as_sparse_adjacency <- function(A, symmetrize = FALSE, check = TRUE) {
  if (is.null(A)) stop("`A` is NULL.")
  if (!(is.matrix(A) || inherits(A, "Matrix"))) {
    stop("`A` must be a base matrix or a Matrix sparse/dense object.")
  }

  nr <- nrow(A)
  nc <- ncol(A)
  if (is.null(nr) || is.null(nc) || nr != nc) {
    stop("`A` must be a square (n x n) matrix.")
  }

  # Coerce to sparse numeric matrix (does not require a library() call)
  A <- Matrix::Matrix(A, sparse = TRUE)
  storage.mode(A@x) <- "double"

  if (check && anyNA(A@x)) stop("`A` contains NA values; please remove/replace them.")
  if (symmetrize) {
    A <- (A + Matrix::t(A)) / 2
  } else if (check) {
    # symmetry check on a small random sample of entries could miss issues;
    # instead do an exact check for small matrices, otherwise skip
    if (nr <= 2000L) {
      if (!isTRUE(all.equal(A, Matrix::t(A), tolerance = 0))) {
        stop("`A` must be symmetric (or set `symmetrize = TRUE`).")
      }
    }
  }

  # ensure diagonal is exactly zero
  Matrix::diag(A) <- 0

  # drop explicit stored zeros (helps keep matrices sparse)
  A <- Matrix::drop0(A, tol = 0)

  if (check) {
    if (any(A@x < 0)) {
      warning("`A` contains negative weights; ensure this is intended for your CAR specification")
    }
  }

  methods::as(A, "generalMatrix")
}



#' Degree matrix from adjacency
#'
#' @param A Sparse symmetric adjacency matrix.
#' @return Diagonal sparse matrix of row sums.
#' @keywords internal
degree_matrix <- function(A) {
  d <- Matrix::rowSums(A)
  Matrix::Diagonal(x = as.numeric(d))
}



#' Row-standardize adjacency matrix
#'
#' @param A Sparse adjacency matrix.
#' @param zero_policy What to do with zero-degree nodes.
#' @return Sparse row-standardized weight matrix.
#' @keywords internal
row_standardize_weights <- function(A, zero_policy = c("keep", "error")) {
  zero_policy <- match.arg(zero_policy)
  d <- Matrix::rowSums(A)

  if (any(d == 0)) {
    if (zero_policy == "error") {
      stop("Adjacency contains isolated nodes (degree 0).")
    }
  }

  Dinv <- Matrix::Diagonal(x = ifelse(d > 0, 1 / d, 0))
  Dinv %*% A
}


