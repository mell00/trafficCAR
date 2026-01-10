

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
