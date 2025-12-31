#' CAR precision matrix from an adjacency matrix
#'
#' Constructs the precision matrix for an intrinsic CAR (ICAR) or proper CAR model:
#' \deqn{Q = \tau (D - \rho A), \quad D = \mathrm{diag}(A \mathbf{1}).}
#'
#' For ICAR, set `type = "icar"` (internally uses \eqn{\rho=1}).
#' For proper CAR, set `type = "proper"` and choose `rho` so that \eqn{D - \rho A}
#' is positive definite (no automatic spectral checks are performed).
#'
#' @param A Square adjacency/weight matrix (base matrix or a `Matrix` sparse type).
#'   Diagonal entries are ignored (set to 0).
#' @param type Either `"icar"` or `"proper"`.
#' @param rho Spatial dependence parameter for proper CAR. Ignored for ICAR.
#' @param tau Positive scalar precision multiplier.
#' @param symmetrize If `TRUE`, replaces `A` by `(A + t(A))/2` before construction.
#' @param check If `TRUE`, performs basic validation and warnings.
#'
#' @return A symmetric sparse precision matrix `Q` (class `"dsCMatrix"`).
#'
#' @examples
#' A <- matrix(0, 4, 4)
#' A[1,2] <- A[2,1] <- 1
#' A[2,3] <- A[3,2] <- 1
#' A[3,4] <- A[4,3] <- 1
#' Q_icar <- car_precision(A, type = "icar", tau = 1)
#' Q_prop <- car_precision(A, type = "proper", rho = 0.9, tau = 2)
#'
#' @export
car_precision <- function(A,
                          type = c("icar", "proper"),
                          rho = 0.99,
                          tau = 1,
                          symmetrize = FALSE,
                          check = TRUE) {
  type <- match.arg(type)

  A <- as_sparse_adjacency(A, symmetrize = symmetrize, check = check)

  if (!is.numeric(tau) || length(tau) != 1L || !is.finite(tau) || tau <= 0) {
    stop("`tau` must be a single positive finite number")
  }

  rho_use <- switch(
    type,
    icar = 1,
    proper = {
      if (!is.numeric(rho) || length(rho) != 1L || !is.finite(rho)) {
        stop("`rho` must be a single finite number when `type = \"proper\"`.")
      }
      rho
    }
  )

  d <- Matrix::rowSums(A)
  if (check && any(d == 0)) {
    warning("adjacency has isolated node(s) with degree 0; Q will be singular for those components")
  }

  Q <- Matrix::Diagonal(x = d) - (rho_use * A)
  if (!identical(tau, 1)) {
    Q <- tau * Q
  }

  Q <- Matrix::forceSymmetric(Q, uplo = "U")
  Matrix::as(Q, "dsCMatrix")
}





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

  Matrix::as(A, "dgCMatrix")
}
