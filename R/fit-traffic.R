#' Prepare speed outcome for Gaussian modeling
#'
#' Produces a transformed response and metadata needed to back-transform.
#' Default transform is log(speed).
#'
#' @param speed numeric vector (e.g., mph, km/h).
#' @param transform character; currently supports "log" or "identity".
#' @param eps small positive constant added before log to avoid log(0).
#' @return list with y (transformed), meta (transform info), and original scale label.
#' @keywords internal
prep_speed <- function(speed, transform = c("log", "identity"), eps = 1e-6) {
  transform <- match.arg(transform)
  if (!is.numeric(speed) || any(!is.finite(speed))) stop("`speed` must be finite numeric.")
  if (any(speed < 0)) stop("`speed` must be nonnegative.")
  if (!is.numeric(eps) || length(eps) != 1 || eps <= 0) stop("`eps` must be positive scalar.")

  if (transform == "log") {
    y <- log(speed + eps)
    meta <- list(
      outcome = "speed",
      transform = "log",
      eps = eps,
      # back-transform for means on link scale:
      inv = function(mu) pmax(exp(mu) - eps, 0),
      # back-transform for interval endpoints on link scale:
      inv_interval = function(lo, hi) c(pmax(exp(lo) - eps, 0), pmax(exp(hi) - eps, 0))
    )
  } else {
    y <- speed
    meta <- list(
      outcome = "speed",
      transform = "identity",
      eps = NA_real_,
      inv = function(mu) mu,
      inv_interval = function(lo, hi) c(lo, hi)
    )
  }

  list(y = y, meta = meta)
}
