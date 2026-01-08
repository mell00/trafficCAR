#' Plot predicted traffic outcome on road network
#'
#' @param fit traffic_fit
#' @param roads sf with segment_id
#' @return ggplot
#' @export
plot_predicted <- function(fit, roads) {
  pred <- colMeans(fit$draws$mu)

  roads$predicted <- pred

  ggplot2::ggplot(roads) +
    ggplot2::geom_sf(
      ggplot2::aes(color = predicted),
      linewidth = 1
    ) +
    ggplot2::scale_color_viridis_c(
      name = fit$outcome_label
    )
}



#' Plot relative congestion on road network
#'
#' Shows systematic deviations after accounting for covariates.
#'
#' @param fit traffic_fit
#' @param roads sf
#' @return ggplot
#' @export
plot_relative_congestion <- function(fit, roads) {
  x_mean <- colMeans(fit$draws$x)
  rel <- x_mean / sd(x_mean)

  roads$relative_congestion <- rel

  ggplot2::ggplot(roads) +
    ggplot2::geom_sf(
      ggplot2::aes(color = relative_congestion),
      linewidth = 1
    ) +
    ggplot2::scale_color_gradient2(
      low = "blue", mid = "white", high = "red",
      name = "Relative congestion\n(vs city average)"
    )
}
