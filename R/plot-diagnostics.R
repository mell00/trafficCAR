#' MCMC diagnostic plots
#'
#' @param fit traffic_fit
#' @importFrom posterior ess_basic
#' @export
plot_mcmc_diagnostics <- function(fit) {
  if (is.null(fit$draws)) stop("`fit$draws` is missing.")
  if (!is.list(fit$draws)) stop("`fit$draws` must be a list.")
  ess <- vapply(fit$draws, posterior::ess_basic, numeric(1))
  data.frame(parameter = names(ess), ess = ess)
}
