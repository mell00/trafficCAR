#' Draw from a multivariate normal with sparse precision
#'
#' Sample x ~ N(Q^{-1} b, Q^{-1}) where Q is symmetric positive definite.
#'
#' @param Q Sparse symmetric positive definite precision matrix (dgCMatrix).
#' @param b Optional numeric vector. If NULL, mean is 0.
#'
#' @return Numeric vector x.
#' @keywords internal

rmvnorm_prec <- function(Q, b = NULL) {
  if (!inherits(Q, "Matrix")) stop("`Q` must be a Matrix object.")
  if (nrow(Q) != ncol(Q)) stop("`Q` must be square.")
  n <- nrow(Q)

  if (!is.null(b)) {
    if (!is.numeric(b) || length(b) != n) stop("`b` must be numeric length nrow(Q).")
  }

  # sparse Cholesky of SPD precision
  ch <- tryCatch(
    Matrix::Cholesky(Q, LDL = FALSE, perm = TRUE),
    error = function(e) stop("Cholesky failed (Q must be SPD): ", conditionMessage(e))
  )

  # mean: mu = Q^{-1} b
  mu <- if (is.null(b)) rep.int(0, n) else as.numeric(Matrix::solve(ch, b))

  # sample z ~ N(0, Q^{-1}).
  # if Q = P' L L' P, then Q^{-1} = P' (L')^{-1} L^{-1} P
  # draw u ~ N(0, I), set w = (L')^{-1} u, then z = P' w
  ex <- Matrix::expand(ch)
  L <- ex$L
  P <- ex$P

  u <- stats::rnorm(n)
  w <- Matrix::solve(Matrix::t(L), u)
  z <- as.numeric(Matrix::t(P) %*% w)

  mu + z
}


