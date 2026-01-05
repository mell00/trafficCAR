#' Static map of road-segment values
#'
#' @param sf_aug sf object with road geometries
#' @param value_col numeric column to plot
#' @importFrom rlang .data
#' @return ggplot object
#' @export
plot_roads_static <- function(sf_aug, value_col) {
  if (!inherits(sf_aug, "sf")) {
    stop("`sf_aug` must be an sf object.")
  }
  if (!value_col %in% names(sf_aug)) {
    stop("`value_col` not found in `sf_aug`.")
  }
  if (!is.numeric(sf_aug[[value_col]])) {
    stop("`value_col` must be numeric.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for static plotting.")
  }

  ggplot2::ggplot(sf_aug) +
    ggplot2::geom_sf(
      ggplot2::aes(color = .data[[value_col]]),
      linewidth = 0.7
    ) +
    ggplot2::scale_color_viridis_c(option = "viridis", na.value = "grey80") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      color = value_col,
      title = paste("Road-segment map of", value_col)
    )
}
