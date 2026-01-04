#' Summarize draws into mean and equal-tail interval
#' @param draws numeric matrix S x n (S draws, n locations) OR numeric vector length S for scalar
#' @param probs length-2 numeric in (0,1), e.g. c(0.025, 0.975)
#' @return list(mean=..., lo=..., hi=...)
#' @keywords internal
.summarize_draws <- function(draws, probs = c(0.025, 0.975)) {
  if (is.vector(draws)) {
    m <- mean(draws)
    qs <- stats::quantile(draws, probs = probs, names = FALSE)
    return(list(mean = m, lo = qs[1], hi = qs[2]))
  }
  if (!is.matrix(draws)) stop("`draws` must be matrix or vector.")
  m <- colMeans(draws)
  qs <- apply(draws, 2, stats::quantile, probs = probs, names = FALSE)
  list(mean = m, lo = qs[1, ], hi = qs[2, ])
}






#' Extract draws from a base fit object (adapter)
#' EDIT FIELDS LATER
#'
#' Expected:
#' - x_draws: matrix S x n
#' - beta_draws: matrix S x p (optional; if absent we treat X beta = 0)
#' @keywords internal
.extract_gaussian_draws <- function(base_fit) {
  # common patterns:
  # base_fit$draws$x or base_fit$x
  x_draws <- NULL
  beta_draws <- NULL
  X <- NULL

  if (!is.null(base_fit$draws) && is.list(base_fit$draws)) {
    if (!is.null(base_fit$draws$x)) x_draws <- base_fit$draws$x
    if (!is.null(base_fit$draws$beta)) beta_draws <- base_fit$draws$beta
  }
  if (is.null(x_draws) && !is.null(base_fit$x)) x_draws <- base_fit$x
  if (is.null(beta_draws) && !is.null(base_fit$beta)) beta_draws <- base_fit$beta

  # design matrix might be stored too (optional)
  if (!is.null(base_fit$X)) X <- base_fit$X

  if (is.null(x_draws)) stop("Could not find x draws in `fit`. Expected `fit$draws$x` or `fit$x`.")
  if (!is.matrix(x_draws)) stop("x draws must be a matrix (S x n).")

  if (!is.null(beta_draws) && !is.matrix(beta_draws)) stop("beta draws must be a matrix (S x p).")

  list(x = x_draws, beta = beta_draws, X = X)
}
