prepare_roads_data <- function(
    geojson_path,
    name,
    crs = 4326,
    simplify_tol = NULL,
    keep_cols = NULL,
    out_dir = "data"
) {
  stopifnot(file.exists(geojson_path))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  sf::sf_use_s2(FALSE)

  roads <- sf::st_read(geojson_path, quiet = TRUE)
  roads <- sf::st_zm(roads, drop = TRUE)
  roads <- sf::st_make_valid(roads)

  if (!is.null(crs)) {
    roads <- sf::st_transform(roads, crs)
  }

  roads <- sf::st_cast(roads, "LINESTRING")

  if (!is.null(keep_cols)) {
    keep_cols <- intersect(keep_cols, names(roads))
    roads <- roads[, keep_cols, drop = FALSE]
  }

  if (!is.null(simplify_tol)) {
    roads <- sf::st_simplify(roads, dTolerance = simplify_tol)
    roads <- sf::st_make_valid(roads)
  }

  out_path <- file.path(out_dir, paste0(name, ".rda"))
  assign(name, roads, envir = parent.frame())
  save(list = name, file = out_path, compress = "xz")

  invisible(out_path)
}

prepare_roads_data(
  geojson_path = "data-raw/roads.geojson",
  name = "roads",
  simplify_tol = 5
)
