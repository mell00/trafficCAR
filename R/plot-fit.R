#' Plot observed vs predicted traffic values
#'
#' @param fit traffic_fit
#' @param data data.frame
#' @return ggplot
#' @export
plot_observed_fitted <- function(fit, data) {
  pred <- colMeans(fit$draws$mu)

  df <- data.frame(
    observed = data[[fit$outcome_col]],
    predicted = pred
  )

  ggplot2::ggplot(df,
                  ggplot2::aes(observed, predicted)
  ) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(linetype = 2) +
    ggplot2::labs(
      x = paste("Observed", fit$outcome_label),
      y = paste("Predicted", fit$outcome_label)
    )
}
