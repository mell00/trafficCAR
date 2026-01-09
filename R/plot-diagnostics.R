#' MCMC diagnostic plots
#'
#' @param fit traffic_fit
#' @importFrom posterior ess_basic
#' @export
plot_mcmc_diagnostics <- function(fit) {
  ess <- vapply(fit$draws, posterior::ess_basic, numeric(1))
  data.frame(parameter = names(ess), ess = ess)
}
