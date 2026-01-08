

.value_registry <- list(
  predicted_speed = list(
    column = "mu_mean",
    label  = "Predicted speed (km/h)"
  ),
  predicted_volume = list(
    column = "mu_mean",
    label  = "Predicted traffic volume"
  ),
  relative_congestion = list(
    column = "relative_congestion",
    label  = "Relative congestion (vs city average)"
  )
)





#' Interactive map of road-segment traffic measures
#'
#' Displays standard traffic quantities such as predicted speed,
#' predicted volume, or relative congestion on an interactive map.
#'
#' @param sf_aug An `sf` object returned by `augment_roads()`.
#' @param value Character scalar. One of:
#'   `"predicted_speed"`, `"predicted_volume"`, `"relative_congestion"`.
#' @param engine Currently only `"leaflet"` is supported.
#'
#' @return A leaflet widget.
#' @export
map_roads_interactive <- function(sf_aug,
                                  value = c("predicted_speed",
                                            "predicted_volume",
                                            "relative_congestion"),
                                  engine = "leaflet") {
  if (!inherits(sf_aug, "sf")) {
    stop("`sf_aug` must be an sf object.")
  }

  value <- match.arg(value)

  spec <- .value_registry[[value]]
  col  <- spec$column
  lab  <- spec$label

  if (!col %in% names(sf_aug)) {
    stop("Required column `", col, "` not found in `sf_aug`.")
  }

  vals <- sf_aug[[col]]
  if (!is.numeric(vals)) {
    stop("Mapped column must be numeric.")
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

  tooltip <- paste0(
    "<strong>Segment:</strong> ", sf_aug$seg_id,
    "<br><strong>", lab, ":</strong> ",
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
      title = lab,
      opacity = 1
    )
}




#' Interactive map with multiple road layers
#'
#' @param sf_aug sf object with road geometries
#' @param layers named character vector:
#'   names = layer labels, values = column names
#'
#' @return leaflet widget
#' @export
map_roads_interactive_layers <- function(sf_aug, layers) {
  if (!inherits(sf_aug, "sf")) stop("`sf_aug` must be sf.")
  if (!is.character(layers) || is.null(names(layers))) {
    stop("`layers` must be a named character vector.")
  }

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required.")
  }
  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Package 'viridisLite' is required.")
  }

  m <- leaflet::leaflet(sf_aug) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  for (nm in names(layers)) {
    col <- layers[[nm]]
    if (!col %in% names(sf_aug)) next
    if (!is.numeric(sf_aug[[col]])) next

    pal <- leaflet::colorNumeric(
      viridisLite::viridis(256),
      domain = sf_aug[[col]],
      na.color = "#CCCCCC"
    )

    m <- m |>
      leaflet::addPolylines(
        color = pal(sf_aug[[col]]),
        weight = 4,
        opacity = 0.9,
        group = nm
      )
  }

  leaflet::addLayersControl(
    m,
    overlayGroups = names(layers),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
}


