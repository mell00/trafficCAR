

test_that("plot_mcmc_diagnostics returns a data.frame with parameter + ess", {
  # minimal valid traffic_fit
  fit <- structure(
    list(draws = list(mu = rnorm(50), tau = rnorm(50))),
    class = "traffic_fit"
  )

  out <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_s3_class(out, "data.frame")
  expect_true(all(c("parameter", "ess") %in% names(out)))
  expect_type(out$parameter, "character")
  expect_type(out$ess, "double")
  expect_equal(nrow(out), length(fit$draws))
  expect_setequal(out$parameter, names(fit$draws))
  expect_true(all(is.finite(out$ess)))
  expect_true(all(out$ess >= 0))
})


test_that("plot_mcmc_diagnostics is deterministic for fixed inputs", {
  set.seed(123)
  fit <- structure(list(draws = list(a = rnorm(100), b = rnorm(100))), class = "traffic_fit")

  out1 <- suppressWarnings(plot_mcmc_diagnostics(fit))
  out2 <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_identical(out1, out2)
})


test_that("plot_mcmc_diagnostics handles empty draws", {
  fit <- structure(list(draws = list()), class = "traffic_fit")

  out <- plot_mcmc_diagnostics(fit)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
  expect_true("ess" %in% names(out))
  expect_true(all(out$ess >= 0))
  expect_true(all(is.finite(out$ess)))

  # parameter column is allowed to be absent for empty draws
  if ("parameter" %in% names(out)) {
    expect_type(out$parameter, "character")
    expect_equal(length(out$parameter), 0)
  }
})


test_that("plot_mcmc_diagnostics works with a single parameter", {
  fit <- structure(list(draws = list(mu = rnorm(80))), class = "traffic_fit")

  out <- plot_mcmc_diagnostics(fit)

  expect_equal(nrow(out), 1)
  expect_identical(out$parameter, "mu")
  expect_true(is.finite(out$ess))
  expect_true(out$ess >= 0)
})


test_that("plot_mcmc_diagnostics rejects missing draws element", {
  fit <- structure(list(), class = "traffic_fit")
  expect_error(plot_mcmc_diagnostics(fit), "draws|\\$draws|subscript|NULL", ignore.case = TRUE)
})
