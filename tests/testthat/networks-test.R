## Quick interactive plot run in console to check shape

quick_plot <- function(name_of_toy_road){
  plot(sf::st_geometry(name_of_toy_road), col = "black", lwd = 2)
  points(sf::st_coordinates(sf::st_cast(toy_roads, "POINT")), pch = 19, col = "red")
}

## ex: quick_plot(toy_L)


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


