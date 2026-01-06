#' Moran's I for trafficCAR residuals
#'
#' Computes Moran's I statistic for model residuals using the model adjacency.
#'
#' @param fit A `traffic_fit` object.
#' @param type Residual type: "raw" or "unstructured".
#' @param nsim Number of permutations for permutation test.
#' @param method "analytic" or "permutation".
#'
#' @return An object of class `traffic_moran`.
#' @export
moran_residuals <- function(fit,
                            type = c("raw", "unstructured"),
                            nsim = 199,
                            method = c("analytic", "permutation")) {
  type <- match.arg(type)
  method <- match.arg(method)

  if (is.null(fit$A)) stop("`fit` must contain adjacency matrix `A`.")

  r <- residuals(fit, type)
  r <- as.numeric(r)

  A <- fit$A
  if (!inherits(A, "Matrix")) {
    stop("`A` must be a sparse Matrix.")
  }

  n <- length(r)
  if (nrow(A) != n || ncol(A) != n) {
    stop("Dimensions of `A` must match residual length.")
  }

  # Binary weights
  W <- A
  W@x <- rep(1, length(W@x))

  r_cent <- r - mean(r)
  denom <- sum(r_cent^2)

  if (denom == 0) stop("Residual variance is zero.")

  num <- as.numeric(crossprod(r_cent, W %*% r_cent))
  S0 <- sum(W)

  I_obs <- (n / S0) * (num / denom)

  if (method == "analytic") {
    E_I <- -1 / (n - 1)

    out <- list(
      I = I_obs,
      expected = E_I,
      type = type,
      method = "analytic",
      n = n
    )
    class(out) <- "traffic_moran"
    return(out)
  }

  # permutation test
  I_perm <- numeric(nsim)
  for (b in seq_len(nsim)) {
    rp <- sample(r_cent, replace = FALSE)
    I_perm[b] <- (n / S0) *
      as.numeric(crossprod(rp, W %*% rp)) / sum(rp^2)
  }

  p_val <- (1 + sum(abs(I_perm) >= abs(I_obs))) / (nsim + 1)

  out <- list(
    I = I_obs,
    p_value = p_val,
    permuted = I_perm,
    type = type,
    method = "permutation",
    n = n,
    nsim = nsim
  )
  class(out) <- "traffic_moran"
  out
}
