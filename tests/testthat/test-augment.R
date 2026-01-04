
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


test_that("extract_gaussian_draws accepts fit_car structure", {
  tf <- .make_fake_traffic_fit(n = 3, p = 2, S = 10)
  d <- .extract_gaussian_draws(tf$fit)

  expect_true(is.matrix(d$x))
  expect_true(is.matrix(d$beta))
  expect_true(is.numeric(d$sigma2))
  expect_equal(nrow(d$x), length(d$sigma2))
})


test_that("extract_gaussian_draws rejects malformed inputs", {
  expect_error(.extract_gaussian_draws(list(draws = NULL)), "draws")

  expect_error(
    .extract_gaussian_draws(list(draws = list(x = 1, beta = NULL, sigma2 = 1:3))),
    "matrix"
  )

  expect_error(
    .extract_gaussian_draws(list(draws = list(
      x = matrix(0, 2, 3), beta = NULL, sigma2 = 1:3
    ))),
    "nrow"
  )
})


test_that("augment_roads joins and adds posterior columns", {
  tf <- .make_fake_traffic_fit(n = 5, p = 1, S = 30)
  roads <- data.frame(segment_id = 1:5)

  out <- augment_roads(tf, roads)

  expect_true(all(c(
    "x_mean","x_lo","x_hi",
    "mu_mean","mu_lo","mu_hi",
    "fitted_mean","fitted_lo","fitted_hi"
  ) %in% names(out)))

  expect_equal(nrow(out), nrow(roads))
  expect_true(all(is.finite(out$fitted_mean)))
})


test_that("augment_roads rejects adversarial inputs", {
  tf <- .make_fake_traffic_fit(n = 3, p = 1, S = 10)

  expect_error(
    augment_roads(tf, data.frame(other = 1:3)),
    "join column"
  )

  tf_bad <- tf
  tf_bad$X <- NULL
  expect_error(augment_roads(tf_bad, data.frame(segment_id = 1:3)), "`fit\\$X`")
})

