#' Calculate Generalized Cook's Distance for Centrality Measures
#'
#' Computes generalized Cook's distance (gCD) for centrality measures
#' (Strength, Closeness, Betweenness) based on bootstrap covariance matrices.
#' For each case, the centrality vector from the full sample is compared to
#' the centrality vector obtained after removing that case.
#'
#' @param boot_result An object of class `"bootnetWithData"` returned by
#'   [bootnet_with_data()].
#' @param case_ids Numeric vector of case IDs to evaluate.
#' @param data Original data frame used in the bootstrap analysis.
#' @param metric Character vector of centrality measures to compute.
#'   Can be one or more of `"Strength"`, `"Closeness"`, `"Betweenness"`.
#' @param default Network estimation method passed to `estimateNetwork`.
#' @param ... Additional arguments passed to [bootnet::estimateNetwork()].
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item `case_id`: case identifier
#'     \item `metric`: centrality measure
#'     \item `gCD`: generalized Cook's distance
#'   }
#'
#' @details
#' The covariance matrix of the centrality measures is estimated from the
#' bootstrap samples contained in `boot_result$boots`. If this covariance
#' matrix is singular, the Moore-Penrose pseudo-inverse is used instead of
#' the usual inverse, and a warning is issued.
#'
#' @export
calculate_centrality_gCD <- function(boot_result,
                                     case_ids,
                                     data,
                                     metric = c("Strength", "Closeness", "Betweenness"),
                                     default = "EBICglasso",
                                     ...) {

  metric <- match.arg(metric, several.ok = TRUE)

  networks_without <- list()
  for (k in seq_along(case_ids)) {
    id <- case_ids[k]
    data_without <- data[-id, , drop = FALSE]
    networks_without[[k]] <- bootnet::estimateNetwork(data_without,
                                                      default = default,
                                                      verbose = FALSE,
                                                      ...)
  }

  full_graph <- boot_result$sample$graph

  inverse_with_fallback <- function(V, metric_name) {
    inv <- tryCatch(solve(V), error = function(e) NULL)
    if (is.null(inv)) {
      warning(sprintf("Covariance matrix for %s is singular; Moore-Penrose pseudo-inverse was used.", metric_name))
      inv <- MASS::ginv(V)
    }
    return(inv)
  }

  all_results <- list()

  for (m in metric) {
    centrality_list <- lapply(boot_result$boots, function(net) {
      g <- net$graph
      if (m == "Strength") {
        rowSums(abs(g))
      } else {
        qgraph::centrality(g)[[m]]
      }
    })
    cent_mat <- do.call(rbind, centrality_list)

    V_cent <- stats::cov(cent_mat)

    cent_full <- if (m == "Strength") {
      rowSums(abs(full_graph))
    } else {
      qgraph::centrality(full_graph)[[m]]
    }

    V_inv <- inverse_with_fallback(V_cent, m)

    for (k in seq_along(case_ids)) {
      id <- case_ids[k]
      net_without <- networks_without[[k]]

      cent_without <- if (m == "Strength") {
        rowSums(abs(net_without$graph))
      } else {
        qgraph::centrality(net_without$graph)[[m]]
      }

      diff_vec <- cent_full - cent_without
      gCD <- as.numeric(t(diff_vec) %*% V_inv %*% diff_vec)

      all_results[[length(all_results) + 1]] <- data.frame(
        case_id = id,
        metric = m,
        gCD = gCD,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, all_results)
  rownames(out) <- NULL
  return(out)
}
