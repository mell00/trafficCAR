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
  if (is.null(base_fit$draws) || !is.list(base_fit$draws)) {
    stop("Expected `fit$draws` to be a list with x/beta/sigma2 draws.")
  }

  x_draws <- base_fit$draws$x
  beta_draws <- base_fit$draws$beta
  sigma2_draws <- base_fit$draws$sigma2

  if (is.null(x_draws) || !is.matrix(x_draws)) {
    stop("Expected `fit$draws$x` to be a matrix (S x n).")
  }

  if (!is.null(beta_draws) && !is.matrix(beta_draws)) {
    stop("Expected `fit$draws$beta` to be a matrix (S x p) or NULL.")
  }

  if (is.null(sigma2_draws) || !(is.numeric(sigma2_draws) && is.vector(sigma2_draws))) {
    stop("Expected `fit$draws$sigma2` to be a numeric vector (length S).")
  }

  if (nrow(x_draws) != length(sigma2_draws)) {
    stop("Inconsistent draws: nrow(x) must equal length(sigma2).")
  }
  if (!is.null(beta_draws) && nrow(beta_draws) != nrow(x_draws)) {
    stop("Inconsistent draws: nrow(beta) must equal nrow(x).")
  }

  list(x = x_draws, beta = beta_draws, sigma2 = sigma2_draws)
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

  X <- fit$X
  if (!is.matrix(X) || nrow(X) != n) stop("`fit$X` is missing or wrong dimension.")

  beta_draws <- draws$beta
  if (is.null(beta_draws)) {
    xb <- matrix(0, nrow = S, ncol = n)
  } else {
    if (ncol(beta_draws) != ncol(X)) stop("beta draws p does not match ncol(X).")
    xb <- beta_draws %*% t(X) # S x n
  }
  mu_draws <- xb + x_draws

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




#' Quick map helper for augmented roads
#'
#' @param roads_aug sf object returned by augment_roads()
#' @param fill character; column name to color by (default "fitted_mean")
#' @export
plot_traffic_map <- function(roads_aug, fill = "fitted_mean") {
  if (!inherits(roads_aug, "sf")) stop("`roads_aug` must be an sf object.")
  if (!fill %in% names(roads_aug)) stop("Column not found: ", fill)

  # base plotting to avoid hard ggplot2 dependency
  vals <- roads_aug[[fill]]
  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op), add = TRUE)

  # simple continuous palette
  pal <- grDevices::colorRampPalette(c("navy", "skyblue", "yellow", "orange", "red"))
  k <- 200
  cuts <- stats::quantile(vals, probs = seq(0, 1, length.out = k + 1), na.rm = TRUE, names = FALSE)
  idx <- findInterval(vals, vec = cuts, all.inside = TRUE)
  cols <- pal(k)[idx]
  plot(sf::st_geometry(roads_aug), col = cols, lwd = 2, axes = FALSE)
  invisible(roads_aug)
}
