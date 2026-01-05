#' Interactive map of road-segment values
#'
#' Creates a simple interactive map for road segments with a numeric value
#' (e.g. posterior mean speed, spatial effect).
#'
#' @param sf_aug An `sf` object returned by `augment_roads()`.
#' @param value_col Character scalar giving the name of a numeric column to map.
#' @param engine Currently only `"leaflet"` is supported.
#'
#' @return An interactive map widget.
#' @export
#'
#' @examples
#' \dontrun{
#' map_roads_interactive(roads_aug, "mu_mean")
#' }
map_roads_interactive <- function(sf_aug, value_col, engine = "leaflet") {
  if (!inherits(sf_aug, "sf")) {
    stop("`sf_aug` must be an sf object.")
  }

  if (!is.character(value_col) || length(value_col) != 1) {
    stop("`value_col` must be a single character string.")
  }

  if (!value_col %in% names(sf_aug)) {
    stop("`value_col` not found in `sf_aug`.")
  }

  vals <- sf_aug[[value_col]]
  if (!is.numeric(vals)) {
    stop("`value_col` must refer to a numeric column.")
  }

  if (engine != "leaflet") {
    stop("Only engine = 'leaflet' is currently supported.")
  }

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive maps.")
  }

  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Package 'viridisLite' is required for color scales.")
  }

  pal <- leaflet::colorNumeric(
    palette = viridisLite::viridis(256),
    domain = vals,
    na.color = "#CCCCCC"
  )

  # Tooltip text
  tooltip <- paste0(
    "<strong>Segment:</strong> ", sf_aug$seg_id,
    "<br><strong>", value_col, ":</strong> ",
    signif(vals, 4)
  )

  leaflet::leaflet(sf_aug) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addPolylines(
      color = ~pal(vals),
      weight = 4,
      opacity = 0.9,
      label = lapply(tooltip, htmltools::HTML)
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = vals,
      title = value_col,
      opacity = 1
    )
}
