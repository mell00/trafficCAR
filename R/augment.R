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
