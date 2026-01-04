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





#' Prepare travel time outcome for Gaussian modeling
#'
#' Common choice is log(travel_time). If `distance` is provided, you can
#' optionally model time-per-distance.
#'
#' @param travel_time numeric vector (e.g., seconds).
#' @param distance optional numeric vector of same length (e.g., meters).
#' @param per_distance logical; if TRUE and distance provided, model travel_time / distance.
#' @param transform character; currently supports "log" or "identity".
#' @param eps small positive constant added before log to avoid log(0).
#' @return list with y (transformed), meta (transform info).
#' @keywords internal
prep_travel_time <- function(travel_time,
                             distance = NULL,
                             per_distance = FALSE,
                             transform = c("log", "identity"),
                             eps = 1e-6) {
  transform <- match.arg(transform)
  if (!is.numeric(travel_time) || any(!is.finite(travel_time))) stop("`travel_time` must be finite numeric.")
  if (any(travel_time < 0)) stop("`travel_time` must be nonnegative.")
  if (!is.numeric(eps) || length(eps) != 1 || eps <= 0) stop("`eps` must be positive scalar.")

  base <- travel_time
  scale_label <- "travel_time"
  if (!is.null(distance)) {
    if (!is.numeric(distance) || any(!is.finite(distance))) stop("`distance` must be finite numeric when provided.")
    if (any(distance <= 0)) stop("`distance` must be positive when provided.")
    if (isTRUE(per_distance)) {
      base <- travel_time / distance
      scale_label <- "travel_time_per_distance"
    }
  } else if (isTRUE(per_distance)) {
    stop("`per_distance = TRUE` requires `distance`.")
  }

  if (transform == "log") {
    y <- log(base + eps)
    meta <- list(
      outcome = "travel_time",
      base = scale_label,
      transform = "log",
      eps = eps,
      per_distance = isTRUE(per_distance),
      inv = function(mu) pmax(exp(mu) - eps, 0),
      inv_interval = function(lo, hi) c(pmax(exp(lo) - eps, 0), pmax(exp(hi) - eps, 0))
    )
  } else {
    y <- base
    meta <- list(
      outcome = "travel_time",
      base = scale_label,
      transform = "identity",
      eps = NA_real_,
      per_distance = isTRUE(per_distance),
      inv = function(mu) mu,
      inv_interval = function(lo, hi) c(lo, hi)
    )
  }

  list(y = y, meta = meta)
}
