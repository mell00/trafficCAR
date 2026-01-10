

test_that("plot_observed_fitted returns a ggplot with correct labels and data", {
  skip_if_not_installed("ggplot2")

  fit <- list(
    draws = list(mu = matrix(c(1, 2,
                               3, 4,
                               5, 6), nrow = 3, byrow = TRUE)),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  class(fit) <- "traffic_fit"

  data <- data.frame(speed = c(10, 20))

  p <- plot_observed_fitted(fit, data)

  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$x, "Observed Speed")
  expect_identical(p$labels$y, "Predicted Speed")

  expect_equal(p$data$observed, c(10, 20))
  expect_equal(p$data$predicted, colMeans(fit$draws$mu))
})



test_that("plot_observed_fitted validates fit class and data type", {
  skip_if_not_installed("ggplot2")

  fit <- list(
    draws = list(mu = matrix(1, nrow = 2, ncol = 2)),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  data <- data.frame(speed = c(1, 2))

  expect_error(
    plot_observed_fitted(fit, data),
    "`fit` must be a `traffic_fit`",
    fixed = TRUE
  )

  class(fit) <- "traffic_fit"
  expect_error(
    plot_observed_fitted(fit, as.list(data)),
    "`data` must be a data.frame",
    fixed = TRUE
  )
})


test_that("plot_observed_fitted validates draws and mu structure", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(speed = c(1, 2))

  fit0 <- list(outcome_col = "speed", outcome_label = "Speed")
  class(fit0) <- "traffic_fit"
  expect_error(plot_observed_fitted(fit0, data), "`fit$draws` must be a list", fixed = TRUE)

  fit1 <- list(draws = list(), outcome_col = "speed", outcome_label = "Speed")
  class(fit1) <- "traffic_fit"
  expect_error(plot_observed_fitted(fit1, data), "`fit$draws$mu` is required", fixed = TRUE)

  fit2 <- list(draws = list(mu = "nope"), outcome_col = "speed", outcome_label = "Speed")
  class(fit2) <- "traffic_fit"
  expect_error(plot_observed_fitted(fit2, data), "must be numeric", ignore.case = TRUE)
})


test_that("plot_observed_fitted validates outcome_col and data column presence", {
  skip_if_not_installed("ggplot2")

  base_fit <- list(
    draws = list(mu = matrix(1, nrow = 2, ncol = 2)),
    outcome_label = "Speed"
  )
  class(base_fit) <- "traffic_fit"

  data <- data.frame(speed = c(1, 2))

  base_fit$outcome_col <- NULL
  expect_error(
    plot_observed_fitted(base_fit, data),
    "`fit$outcome_col` must be a non-empty character scalar",
    fixed = TRUE
  )

  base_fit$outcome_col <- "missing"
  expect_error(
    plot_observed_fitted(base_fit, data),
    "Required column `missing` not found in `data`.",
    fixed = TRUE
  )
})


test_that("plot_observed_fitted validates outcome_label", {
  skip_if_not_installed("ggplot2")

  fit <- list(
    draws = list(mu = matrix(1, nrow = 2, ncol = 2)),
    outcome_col = "speed"
  )
  class(fit) <- "traffic_fit"

  data <- data.frame(speed = c(1, 2))

  fit$outcome_label <- NULL
  expect_error(
    plot_observed_fitted(fit, data),
    "`fit$outcome_label` must be a non-empty character scalar",
    fixed = TRUE
  )
})
