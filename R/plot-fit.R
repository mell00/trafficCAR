#' Plot observed vs predicted traffic values
#'
#' @param fit traffic_fit
#' @param data data.frame
#' @return ggplot
#' @export
plot_observed_fitted <- function(fit, data) {
  if (!inherits(fit, "traffic_fit")) stop("`fit` must be a `traffic_fit`.")
  if (!is.data.frame(data)) stop("`data` must be a data.frame.")
  if (is.null(fit$draws) || !is.list(fit$draws)) stop("`fit$draws` must be a list.")
  if (is.null(fit$draws$mu)) stop("`fit$draws$mu` is required.")
  mu <- fit$draws$mu

  # allow matrix or vector; coerce vector to 1-row matrix (single draw)
  if (is.null(dim(mu))) mu <- matrix(mu, nrow = 1)
  if (!is.matrix(mu)) stop("`fit$draws$mu` must be a numeric matrix (draws x observations) or a numeric vector.")
  if (!is.numeric(mu)) stop("`fit$draws$mu` must be numeric.")

  if (is.null(fit$outcome_col) || !is.character(fit$outcome_col) || length(fit$outcome_col) != 1L ||
      is.na(fit$outcome_col) || !nzchar(fit$outcome_col)) {
    stop("`fit$outcome_col` must be a non-empty character scalar.")
  }
  if (!fit$outcome_col %in% names(data)) {
    stop("Required column `", fit$outcome_col, "` not found in `data`.")
  }

  if (is.null(fit$outcome_label) || !is.character(fit$outcome_label) || length(fit$outcome_label) != 1L ||
      is.na(fit$outcome_label) || !nzchar(fit$outcome_label)) {
    stop("`fit$outcome_label` must be a non-empty character scalar.")
  }

  pred <- colMeans(mu)
  obs <- data[[fit$outcome_col]]

  if (length(pred) != length(obs)) {
    stop("Length mismatch: `fit$draws$mu` implies ", length(pred),
         " predictions but `data[[fit$outcome_col]]` has length ", length(obs), ".")
  }

  df <- data.frame(
    observed = obs,
    predicted = pred
  )

  ggplot2::ggplot(df, ggplot2::aes(observed, predicted)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(linetype = 2) +
    ggplot2::labs(
      x = paste("Observed", fit$outcome_label),
      y = paste("Predicted", fit$outcome_label)
    )
}

