
# L-shaped network w/ two line segments, three nodes (endpoints + intersection)

toy_L <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# T-junction network (3 segments, 4 nodes: one degree-3 intersection)

toy_T <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, -1, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)
