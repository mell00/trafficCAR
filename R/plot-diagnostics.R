#' MCMC diagnostic plots
#'
#' @param fit traffic_fit
#' @importFrom posterior ess_basic
#' @export
plot_mcmc_diagnostics <- function(fit) {
  if (is.null(fit$draws)) stop("`fit$draws` is missing.")
  if (!is.list(fit$draws)) stop("`fit$draws` must be a list.")
  if (is.null(fit$draws)) fit$draws <- list()
  if (!is.list(fit$draws)) stop("`fit$draws` must be a list.")

  bad <- vapply(
    fit$draws,
    function(x) !is.numeric(x) || any(!is.finite(x)),
    logical(1)
  )
  if (any(bad)) stop("All draws must be numeric and finite.")

  ess <- vapply(fit$draws, posterior::ess_basic, numeric(1))
  data.frame(parameter = names(ess), ess = ess)
}
