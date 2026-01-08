
.make_fake_sf <- function(n = 5) {
  sf::st_sf(
    seg_id = seq_len(n),
    predicted_mean = runif(n, 20, 60),
    relative_congestion = rnorm(n),
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(i) {
        sf::st_linestring(matrix(c(i, 0, i + 1, 0), ncol = 2, byrow = TRUE))
      }),
      crs = 4326
    )
  )
}
