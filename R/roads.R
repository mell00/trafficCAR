#' Convert road geometries to modeling segments
#'
#' Takes an `sf` object of LINESTRING/MULTILINESTRING road geometries and returns
#' a segment-level `sf` with stable segment IDs and metric lengths.
#'
#' v1 behavior:
#' * Drops Z/M dimensions
#' * Casts MULTILINESTRING -> LINESTRING (one row per linestring)
#' * Optionally splits at intersections (noding) when `split_at_intersections=TRUE`
#' * Computes `length_m` in meters (projects if lon/lat)
#' * Drops empty and (optionally) zero-length segments
#'
#' @param roads An `sf` object with LINESTRING or MULTILINESTRING geometries.
#' @param crs_m Metric CRS used for length calculation (and intersection splitting)
#'   when `roads` is lon/lat. Default 3857. For best accuracy, pass a local UTM EPSG.
#' @param keep_attrs Optional character vector of non-geometry columns to keep.
#'   If `NULL`, keeps all attributes.
#' @param drop_zero Logical; drop segments with non-positive length. Default TRUE.
#' @param split_at_intersections Logical; if TRUE, split lines at all intersections.
#'   Implemented via GEOS noding (`sf::st_union` + `sf::st_cast`)
#'   Default FALSE.
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
                              split_at_intersections = FALSE,
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
    out <- roads
    out$seg_id <- integer(0)
    out$length_m <- numeric(0)
    return(out[, c("seg_id", "length_m", setdiff(names(out), c("seg_id","length_m"))), drop = FALSE])
  }

  # Cast to LINESTRING, safely handling mixed LINESTRING + MULTILINESTRING inputs
  gtype <- as.character(sf::st_geometry_type(roads, by_geometry = TRUE))

  bad_types <- setdiff(unique(gtype), c("LINESTRING", "MULTILINESTRING"))
  if (length(bad_types) > 0) {
    stop("`roads` must have LINESTRING/MULTILINESTRING geometry. Found: ",
         paste(bad_types, collapse = ", "))
  }

  is_ls  <- gtype == "LINESTRING"
  is_mls <- gtype == "MULTILINESTRING"

  segs_ls <- if (any(is_ls)) roads[is_ls, , drop = FALSE] else roads[0, , drop = FALSE]

  # cast ONLY the MULTILINESTRING rows; casting a mixed sfc can drop parts
  segs_mls <- if (any(is_mls)) {
    sf::st_cast(roads[is_mls, , drop = FALSE], "LINESTRING", warn = FALSE)
  } else {
    roads[0, , drop = FALSE]
  }

  segs <- rbind(segs_ls, segs_mls)

  # Optional: split lines at intersections (noding) using GEOS via st_union
  if (isTRUE(split_at_intersections)) {
    crs_out <- sf::st_crs(segs)

    # Work in planar/metric CRS for robust topology
    if (isTRUE(sf::st_is_longlat(segs))) {
      segs_work <- sf::st_transform(segs, crs_m)
    } else {
      segs_work <- segs
    }

    # Union nodes the linework at all intersections; attributes will be dropped here,
    # so restore attributes by intersecting back with original features below.
    u <- sf::st_union(sf::st_geometry(segs_work))
    pieces <- sf::st_cast(u, "LINESTRING", warn = FALSE)

    # Wrap as sf for attribute restoration
    pieces_sf <- sf::st_sf(piece_id = seq_along(pieces), geometry = pieces)

    # Restore attributes, assign each piece to the original feature it intersects
    idx <- sf::st_intersects(pieces_sf, segs_work)

    if (length(idx) == 0L) {
      segs_work2 <- pieces_sf
    } else {
      # deterministic choice: first intersecting original segment
      pick <- vapply(idx, function(ii) if (length(ii) == 0L) NA_integer_ else ii[1L], integer(1))

      keep <- !is.na(pick)
      pieces_sf <- pieces_sf[keep, , drop = FALSE]
      pick <- pick[keep]

      # bind attributes from segs_work onto pieces
      attr_cols <- setdiff(names(segs_work), attr(segs_work, "sf_column"))
      segs_work2 <- cbind(segs_work[pick, attr_cols, drop = FALSE], pieces_sf)
      sf::st_geometry(segs_work2) <- sf::st_geometry(pieces_sf)
    }

    # Drop empty pieces that can appear after union/cast
    empty2 <- sf::st_is_empty(segs_work2)
    if (any(empty2)) {
      if (verbose) message("Dropping ", sum(empty2), " empty pieces after intersection splitting.")
      segs_work2 <- segs_work2[!empty2, , drop = FALSE]
    }

    # Transform back to original CRS for output geometry
    if (isTRUE(sf::st_is_longlat(segs))) {
      segs <- sf::st_transform(segs_work2, crs_out)
    } else {
      segs <- segs_work2
      sf::st_crs(segs) <- crs_out
    }
  }

  # Compute length in meters
  if (isTRUE(sf::st_is_longlat(segs))) {
    segs_m <- sf::st_transform(segs, crs_m)
    len <- sf::st_length(segs_m)
  } else {
    len <- sf::st_length(segs)
  }
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
  keep_order <- c("seg_id", "length_m", setdiff(names(segs), c("seg_id", "length_m")))
  segs <- segs[, keep_order, drop = FALSE]

  segs
}
