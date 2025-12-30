
# L-shaped network w/ two line segments, three nodes (endpoints + intersection)

toy_L <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

