#' Gibbs update for beta in y = X beta + x + eps
#'
#' Model:
#'   y | beta, x, sigma2 ~ N(X beta + x, sigma2 I)
#'   beta ~ N(b0, B0)
#'
#' @param y numeric length n
#' @param X numeric n x p
#' @param x numeric length n (latent effect)
#' @param sigma2 positive scalar
#' @param b0 numeric length p prior mean
#' @param B0 positive definite p x p prior covariance
#'
#' @return numeric length p beta draw
#' @keywords internal
update_beta_gaussian <- function(y, X, x, sigma2, b0, B0) {
  if (!is.numeric(y)) stop("`y` must be numeric.")
  if (!is.matrix(X) || !is.numeric(X)) stop("`X` must be a numeric matrix.")
  n <- length(y)
  if (nrow(X) != n) stop("`X` must have nrow(X) == length(y).")
  p <- ncol(X)

  if (!is.numeric(x) || length(x) != n) stop("`x` must be numeric length length(y).")
  if (!is.numeric(sigma2) || length(sigma2) != 1 || !is.finite(sigma2) || sigma2 <= 0)
    stop("`sigma2` must be a positive scalar.")

  if (!is.numeric(b0) || length(b0) != p) stop("`b0` must be numeric length ncol(X).")
  if (!is.matrix(B0) || !is.numeric(B0) || any(dim(B0) != c(p, p)))
    stop("`B0` must be a numeric p x p matrix.")

  # Prior precision
  B0_inv <- tryCatch(solve(B0), error = function(e) stop("`B0` must be invertible (PD)."))

  XtX <- crossprod(X)               # p x p
  Vinv <- XtX / sigma2 + B0_inv     # p x p

  V <- tryCatch(solve(Vinv), error = function(e) stop("Posterior covariance not invertible."))
  rhs <- crossprod(X, (y - x)) / sigma2 + B0_inv %*% b0
  m <- V %*% rhs

  L <- tryCatch(chol(V), error = function(e) stop("Posterior covariance not PD."))
  as.numeric(m + t(L) %*% rnorm(p))
}
