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
