## Quick interactive plot run in console to check shape

quick_plot <- function(name_of_toy_road){
  plot(sf::st_geometry(name_of_toy_road), col = "black", lwd = 2)
  points(sf::st_coordinates(sf::st_cast(toy_roads, "POINT")), pch = 19, col = "red")
}

## ex: quick_plot(toy_L)

## ----------------------------------------------------------------------------------

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

# square loop network (4 segments, 4 nodes, one cycle)

toy_square <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 0, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 1, 0, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 0, 0), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# 2x2 grid network (6 segments, 9 nodes, planar grid)

toy_grid <- sf::st_sf(
  geometry = sf::st_sfc(
    # horizontal
    sf::st_linestring(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 2, 1), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 2, 2, 2), ncol = 2, byrow = TRUE)),
    # vertical
    sf::st_linestring(matrix(c(0, 0, 0, 2), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(1, 0, 1, 2), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(2, 0, 2, 2), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)

# disconnected network (2 components, 2 segments, 4 nodes)

toy_disconnected <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(3, 0, 4, 0), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)
