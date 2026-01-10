

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

