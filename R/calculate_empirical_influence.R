#' Calculate Empirical Influence Values
#'
#' This function estimates the empirical influence of each case on
#' the global strength of the network using bootstrap inclusion frequencies.
#' It follows the approach of the `empinf.reg` function in the boot package,
#' adapted for network analysis.
#'
#' @param boot_result An object of class `"bootnetWithData"` returned by
#'   [bootnet_with_data()], containing bootstrap samples, bootstrap indices,
#'   and the original sample network.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of length equal to the number of cases in the
#'   original data. Each element represents the centered empirical influence
#'   of the corresponding case on the global strength (sum of absolute edge
#'   weights). Positive values indicate cases that increase global strength
#'   when included more often, while negative values indicate cases that
#'   decrease it.
#'
#' @details
#' The empirical influence is computed by regressing the bootstrap global
#' strength values on the standardized inclusion frequencies (inclusion count
#' divided by sample size). To avoid collinearity, the first case is omitted
#' as the baseline, and the resulting coefficients are centered to have mean
#' zero.
#'
#' @export
calculate_empirical_influence <- function(boot_result,...) {

  nCases <- boot_result$sampleSize
  nBoots <- boot_result$nBoots
  full_graph <- boot_result$sample$graph
  global_full <- sum(abs(full_graph[upper.tri(full_graph)]))

  inclusion_matrix <- matrix(0, nrow = nBoots, ncol = nCases)
  for (b in 1:nBoots) {
    inclusion_matrix[b, ] <- tabulate(boot_result$bootIndices[[b]],
                                       nbins = nCases)
  }
  global_boots <- sapply(boot_result$boots, function(x) {
    g <- x$graph
    sum(abs(g[upper.tri(g)]))
  })
  fins <- which(is.finite(global_boots))
  global_boots <- global_boots[fins]
  inclusion_matrix <- inclusion_matrix[fins, , drop = FALSE]
  R <- length(global_boots)
  n <- nCases
  X <- inclusion_matrix / n
  inc <- 2:n
  X <- X[, inc, drop = FALSE]
  beta <- coefficients(glm(global_boots ~ X))[-1L]
  l <- rep(0, n)
  l[inc] <- beta
  l <- l - mean(l)
  return(l)
}
