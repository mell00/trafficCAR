#' Build a road network graph from sf LINESTRING data
#'
#' @param roads_sf An sf object with LINESTRING geometry
#' @param crs_out Integer EPSG code for projected CRS
#'
#' @return A list with components:
#' \itemize{
#'   \item roads: cleaned sf object
#'   \item nodes: sf POINT object with node_id
#'   \item edges: sf LINESTRING object with from, to, length
#'   \item graph: igraph object
#'   \item A: sparse adjacency matrix
#' }
#'
#' @export
build_network <- function(roads_sf, crs_out = 3857) {
  stopifnot(inherits(roads_sf, "sf"))

  # clean + project
  roads <- roads_sf[!sf::st_is_empty(roads_sf), ]
  roads <- sf::st_transform(roads, crs_out)
  roads <- sf::st_cast(roads, "LINESTRING")

  # node intersections (sf-stable)
  geom <- sf::st_union(sf::st_geometry(roads))
  geom <- sf::st_node(geom)
  geom <- sf::st_cast(geom, "LINESTRING")
  roads <- sf::st_sf(geometry = geom, crs = sf::st_crs(roads))

  # nodes (unique by geometry)
  node_geom <- sf::st_cast(sf::st_union(roads), "POINT")
  coords <- sf::st_coordinates(node_geom)
  node_geom <- node_geom[!duplicated(coords), ]

  nodes <- sf::st_sf(geometry = node_geom, crs = sf::st_crs(roads))
  nodes$node_id <- seq_len(nrow(nodes))

  # edges
  edges <- roads

  endpts <- sf::st_cast(
    sf::st_line_sample(edges, sample = c(0, 1)),
    "POINT"
  )

  edges$from <- sf::st_nearest_feature(endpts[c(TRUE, FALSE)], nodes)
  edges$to   <- sf::st_nearest_feature(endpts[c(FALSE, TRUE)], nodes)
  edges$length <- sf::st_length(edges)

  # graph (integer node IDs)
  edge_mat <- as.matrix(
    sf::st_drop_geometry(edges[, c("from", "to")])
  )

  g <- igraph::graph_from_edgelist(edge_mat, directed = FALSE)

  # adjacency
  A <- igraph::as_adjacency_matrix(g, sparse = TRUE)

  list(
    roads = roads,
    nodes = nodes,
    edges = edges,
    graph = g,
    A = A
  )
}
