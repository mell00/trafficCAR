
.make_fake_traffic_fit <- function(n = 4, p = 1, S = 20) {
  x <- matrix(rnorm(S * n), S, n)
  beta <- matrix(rnorm(S * p), S, p)
  sigma2 <- rgamma(S, 2, 2)

  base_fit <- structure(
    list(
      draws = list(x = x, beta = beta, sigma2 = sigma2),
      keep = seq_len(S),
      type = "proper",
      rho = 0.9,
      tau = 1
    ),
    class = "trafficCAR_fit"
  )

  structure(
    list(
      fit = base_fit,
      X = matrix(1, n, p),
      segment_id = seq_len(n),
      segment_id_col = "segment_id",
      transform_meta = list(
        inv = function(mu) mu,
        inv_interval = function(lo, hi) c(lo, hi)
      )
    ),
    class = "traffic_fit"
  )
}
