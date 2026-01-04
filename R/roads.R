#' Convert road geometries to modeling segments
#'
#' Takes an `sf` object of LINESTRING/MULTILINESTRING road geometries and returns
#' a segment-level `sf` with stable segment IDs and metric lengths.
#'
#' v1 behavior:
#' * Drops Z/M dimensions
#' * Casts MULTILINESTRING -> LINESTRING (one row per linestring)
#' * Computes `length_m` in meters (projects if lon/lat)
#' * Drops empty and (optionally) zero-length segments
#'
#' @param roads An `sf` object with LINESTRING or MULTILINESTRING geometries.
#' @param crs_m Metric CRS used for length calculation when `roads` is lon/lat.
#'   Default 3857. For best accuracy, pass an appropriate local UTM EPSG code.
#' @param keep_attrs Optional character vector of non-geometry columns to keep.
#'   If `NULL`, keeps all attributes.
#' @param drop_zero Logical; drop segments with non-positive length. Default TRUE.
#' @param verbose Logical; emit simple messages about dropped rows. Default FALSE.
#'
#' @return An `sf` with columns:
#'   * `seg_id` integer 1..n
#'   * `length_m` numeric meters
#'   * geometry LINESTRING
#'   plus kept attributes.
#'
#' @export
roads_to_segments <- function(roads,
                              crs_m = 3857,
                              keep_attrs = NULL,
                              drop_zero = TRUE,
                              verbose = FALSE) {
  if (!inherits(roads, "sf")) stop("`roads` must be an sf object.")
  if (!is.numeric(crs_m) || length(crs_m) != 1L) stop("`crs_m` must be a single EPSG code (numeric).")

  # Optionally subset attributes early (but keep geometry)
  if (!is.null(keep_attrs)) {
    keep_attrs <- unique(as.character(keep_attrs))
    missing_cols <- setdiff(keep_attrs, names(roads))
    if (length(missing_cols) > 0) {
      stop("`keep_attrs` contains missing columns: ", paste(missing_cols, collapse = ", "))
    }
    roads <- roads[, unique(c(keep_attrs, attr(roads, "sf_column"))), drop = FALSE]
  }

  # Drop Z/M dimensions to avoid length surprises
  roads <- sf::st_zm(roads, drop = TRUE, what = "ZM")

  # Drop empty geometries
  empty <- sf::st_is_empty(roads)
  if (any(empty)) {
    if (verbose) message("Dropping ", sum(empty), " empty geometries.")
    roads <- roads[!empty, , drop = FALSE]
  }
  if (nrow(roads) == 0L) {
    # return empty sf with expected columns
    out <- roads
    out$seg_id <- integer(0)
    out$length_m <- numeric(0)
    return(out[, c("seg_id", "length_m", setdiff(names(out), c("seg_id","length_m"))), drop = FALSE])
  }

  # Cast to LINESTRING (explodes MULTILINESTRING into multiple rows)
  geom_type <- unique(as.character(sf::st_geometry_type(roads)))
  ok <- geom_type %in% c("LINESTRING", "MULTILINESTRING")
  if (!all(ok)) {
    stop("`roads` must have LINESTRING/MULTILINESTRING geometry. Found: ",
         paste(geom_type[!geom_type %in% c("LINESTRING","MULTILINESTRING")], collapse = ", "))
  }
  segs <- sf::st_cast(roads, "LINESTRING", warn = FALSE)

  # Compute length in meters
  g <- sf::st_geometry(segs)
  if (isTRUE(sf::st_is_longlat(segs))) {
    # project for metric length
    segs_m <- sf::st_transform(segs, crs_m)
    len <- sf::st_length(segs_m)
  } else {
    len <- sf::st_length(segs)
  }
  # Convert units to plain numeric meters
  len_m <- as.numeric(units::set_units(len, "m"))

  segs$length_m <- len_m

  # Drop zero/negative lengths (and any NA)
  bad <- is.na(segs$length_m) | (!is.finite(segs$length_m))
  if (drop_zero) bad <- bad | (segs$length_m <= 0)

  if (any(bad)) {
    if (verbose) message("Dropping ", sum(bad), " segments with bad length.")
    segs <- segs[!bad, , drop = FALSE]
  }
  if (nrow(segs) == 0L) {
    out <- segs
    out$seg_id <- integer(0)
    return(out[, c("seg_id", "length_m", setdiff(names(out), c("seg_id","length_m"))), drop = FALSE])
  }

  # Stable IDs
  segs$seg_id <- seq_len(nrow(segs))

  # Put seg_id first
  # (keep geometry + attributes + length)
  keep_order <- c("seg_id", "length_m", setdiff(names(segs), c("seg_id", "length_m")))
  segs <- segs[, keep_order, drop = FALSE]

  segs
}
