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
