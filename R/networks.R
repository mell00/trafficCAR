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

  # create node intersections
  roads <- lwgeom::st_node(roads)
  roads <- sf::st_cast(roads, "LINESTRING")

  # create nodes
  nodes <- sf::st_as_sf(sf::st_cast(sf::st_union(roads), "POINT"))
  nodes$node_id <- seq_len(nrow(nodes))

  # create edges
  edges <- roads
  edges$from <- sf::st_nearest_feature(
    sf::st_startpoint(edges), nodes
  )
  edges$to <- sf::st_nearest_feature(
    sf::st_endpoint(edges), nodes
  )
  edges$length <- sf::st_length(edges)

  # create graph
  g <- igraph::graph_from_data_frame(
    edges[, c("from", "to")],
    directed = FALSE,
    vertices = nodes
  )

  # create adjacency matrix
  A <- igraph::as_adjacency_matrix(g, sparse = TRUE)

  list(
    roads = roads,
    nodes = nodes,
    edges = edges,
    graph = g,
    A = A
  )
}
