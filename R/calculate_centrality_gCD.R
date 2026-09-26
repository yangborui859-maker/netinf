#' Calculate Generalized Cook's Distance for Centrality Measures
#'
#' Computes generalized Cook's distance (gCD) for centrality measures
#' (Strength, Closeness, Betweenness) based on bootstrap covariance matrices.
#' For each case, the centrality vector from the full sample is compared to
#' the centrality vector obtained after removing that case.
#'
#' @param data Original data frame used in the bootstrap analysis. The first
#'   argument, so that it can be passed positionally, matching the convention of
#'   [bootnet::estimateNetwork()].
#' @param boot_result An object of class `"bootnetWithIndices"` returned by
#'   [bootnet_with_indices()]. Provides the full sample network (`sample$graph`)
#'   and the bootstrap networks (`boots`) used to estimate the covariance
#'   matrix of the centrality measures.
#' @param case_ids Numeric vector of case IDs (row indices) to evaluate. If
#'   `NULL` (default), all cases in `data` are evaluated.
#' @param metric Character vector of centrality measures to compute. Can be one
#'   or more of `"Strength"`, `"Closeness"`, `"Betweenness"`.
#' @param nCores Integer. Number of CPU cores for parallel estimation of the
#'   leave-one-out networks. Default is `1` (sequential). Values greater than 1
#'   use a SOCK cluster via [parallel::makeCluster()] and
#'   [pbapply::pblapply()].
#' @param ... Additional arguments passed to [bootnet::estimateNetwork()].
#'   The `default` argument is required, e.g. `default = "EBICglasso"`.
#' @return A data frame with one row per case-metric combination and columns:
#'   \itemize{
#'     \item `case_id`: the case identifier
#'     \item `metric`: the centrality measure
#'     \item `gCD`: the generalized Cook's distance
#'   }
#'   The list of raw difference vectors (full-sample centrality minus
#'   leave-one-out centrality) is attached as the attribute `"diff_vectors"`.
#'
#' @details
#' For each centrality measure, the covariance matrix of node-level centrality
#' values is estimated from the bootstrap samples stored in
#' `boot_result$boots`. The gCD for case \eqn{i} is computed as
#' \deqn{gCD_i = (\theta - \theta_{(-i)})' V^{-1} (\theta - \theta_{(-i)})}
#' where \eqn{\theta} is the vector of centrality values in the full sample,
#' \eqn{\theta_{(-i)}} is the vector after removing case \eqn{i}, and \eqn{V}
#' is the bootstrap covariance matrix. If \eqn{V} is singular, the
#' Moore-Penrose pseudo-inverse from [MASS::ginv()] will be used.
#'
#' @seealso [bootnet_with_indices()], [bootnet::estimateNetwork()]
#'
#' @export
calculate_centrality_gCD <- function(data,
                                     boot_result,
                                     case_ids = NULL,
                                     metric = c("Strength", "Closeness", "Betweenness"),
                                     nCores = 1,
                                     ...) {

  if(is.null(case_ids)){
    case_ids <- seq_len(nrow(data))}else{
  
  if (!is.numeric(case_ids)) {
    stop("'case_ids' must be a numeric vector.")
  }
    
  if (any(case_ids != round(case_ids))) {
    stop("'case_ids' must be integers.")
  }
  
  if (any(case_ids < 1 | case_ids > nrow(data))) {
    stop("'case_ids' must be between 1 and ", nrow(data), ".")
  }}
  
  metric <- match.arg(metric, several.ok = TRUE)
  
  if (nCores > 1) {
    cl <- parallel::makeCluster(nCores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl,c('data'), envir = environment())
    networks_without <- pbapply::pblapply(case_ids, function(id) {
      data_without <- data[-id, , drop = FALSE]
      bootnet::estimateNetwork(data_without,
                               ...)
    }, cl = cl)

  } else {
    networks_without <- lapply(case_ids, function(id) {
      data_without <- data[-id, , drop = FALSE]
      bootnet::estimateNetwork(data_without,
                               ...)
    })
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
  diff_vectors <- list()
  
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
      diff_vectors[[length(diff_vectors) + 1]] <- diff_vec
    }
  }

  out <- do.call(rbind, all_results)
  rownames(out) <- NULL
  attr(out, "diff_vectors") <- diff_vectors
  return(out)
}
