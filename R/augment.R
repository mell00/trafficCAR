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





#' Augment an sf roads object with posterior summaries
#'
#' Adds posterior mean and interval columns for latent effect x and fitted mean mu.
#' If the model used a log transform, also adds back-transformed summaries.
#'
#' @param fit a `traffic_fit` object from `fit_traffic()`.
#' @param roads an sf object (or data.frame) with a segment id column matching `fit$segment_id_col`.
#' @param probs length-2 numeric for equal-tail intervals.
#' @param keep_geometry logical; if FALSE returns a data.frame even if sf.
#' @return `roads` with added columns.
#' @export
augment_roads <- function(fit, roads, probs = c(0.025, 0.975), keep_geometry = TRUE) {
  if (!inherits(fit, "traffic_fit")) stop("`fit` must be a `traffic_fit`.")
  if (!(is.data.frame(roads) || inherits(roads, "sf"))) stop("`roads` must be a data.frame or sf object.")

  id_col <- fit$segment_id_col
  if (!id_col %in% names(roads)) stop("roads is missing join column: ", id_col)

  base_fit <- fit$fit
  draws <- .extract_gaussian_draws(base_fit)
  x_draws <- draws$x
  S <- nrow(x_draws)
  n <- ncol(x_draws)

  if (length(fit$segment_id) != n) {
    stop("Segment id length does not match ncol(x draws).")
  }

  # build X used for fitted mean; prefer extractor X, else assume intercept-only (0 already handled)
  X <- draws$X
  if (is.null(X)) {
    # if `fit_car()` already stored X elsewhere, update extractor.
    # assume intercept-only if beta missing; otherwise error.
    if (!is.null(draws$beta)) stop("Have beta draws but no X found in fit; store X or pass through in `fit_car()`.")
    mu_draws <- x_draws
  } else {
    if (!is.matrix(X) || nrow(X) != n) stop("Extracted X is invalid or wrong dimension.")
    beta_draws <- draws$beta
    if (is.null(beta_draws)) {
      xb <- matrix(0, nrow = S, ncol = n)
    } else {
      if (ncol(beta_draws) != ncol(X)) stop("beta draws p does not match ncol(X).")
      xb <- beta_draws %*% t(X) # S x n
    }
    mu_draws <- xb + x_draws
  }

  xs <- .summarize_draws(x_draws, probs = probs)
  mus <- .summarize_draws(mu_draws, probs = probs)

  # back-transform (if any)
  inv <- fit$transform_meta$inv
  inv_int <- fit$transform_meta$inv_interval

  mu_mean_bt <- inv(mus$mean)
  mu_lohi_bt <- vapply(seq_len(n), function(i) inv_int(mus$lo[i], mus$hi[i]), numeric(2))
  mu_lo_bt <- mu_lohi_bt[1, ]
  mu_hi_bt <- mu_lohi_bt[2, ]

  aug <- data.frame(
    segment_id = fit$segment_id,
    x_mean = xs$mean,
    x_lo = xs$lo,
    x_hi = xs$hi,
    mu_mean = mus$mean,
    mu_lo = mus$lo,
    mu_hi = mus$hi,
    fitted_mean = mu_mean_bt,
    fitted_lo = mu_lo_bt,
    fitted_hi = mu_hi_bt,
    stringsAsFactors = FALSE
  )
  names(aug)[1] <- id_col

  # join back
  out <- merge(roads, aug, by = id_col, all.x = TRUE, sort = FALSE)

  if (!keep_geometry && inherits(out, "sf")) {
    out <- sf::st_drop_geometry(out)
  }
  out
}
