#' Leave-One-Out Analysis for Influential Cases
#'
#' Performs leave-one-out (or leave-multiple-out) analysis on candidate
#' influential cases identified by empirical influence values.
#'
#' @param influence_result Numeric vector of empirical influence values.
#' @param data Original data frame.
#' @param remove_cases Optional numeric vector of case IDs to remove manually.
#' @param threshold Optional threshold for selecting cases based on absolute
#'   influence values.
#' @param top_n Number of top influential cases to select when `remove_cases`
#'   and `threshold` are not specified.
#' @param direction Character, direction of selection: `"both"`, `"positive"`,
#'   or `"negative"`.
#' @param verbose Logical, whether to print progress messages.
#' @param digits Optional integer. If supplied, controls the number of
#'   decimal places used when formatting edge changes. Default `NULL`
#'   uses full precision.
#' @param ... Additional arguments passed to [bootnet::estimateNetwork()].
#'   The `default` argument is required, e.g. `default = "EBICglasso"`.
#' @details
#' The function validates all inputs before running the analysis:
#' \itemize{
#'   \item `influence_result` must be a numeric vector whose length equals
#'         `nrow(data)`.
#'   \item `remove_cases` must be a vector of unique integers in `1:nrow(data)`.
#'   \item `threshold` must be a single non-negative numeric value.
#'   \item `top_n` must be a single positive integer; if it exceeds the number
#'         of cases, a warning is issued and all cases are used.
#' }
#'
#' When multiple selection arguments are supplied, the priority is
#' `remove_cases` > `threshold` > `top_n`. A warning is issued when a
#' lower-priority argument is ignored.
#' @return If `remove_cases` has length > 1, an object of class
#'   `"looMultiAnalysis"`. Otherwise, an object of class `"looAnalysis"`.
#' @examples
#' data("test_data", package = "netinf")
#'
#' boot_res <- bootnet_with_indices(
#'   test_data,
#'   nBoots  = 100,        # for demonstration; use 5000+ in practice
#'   default = "EBICglasso"
#' )
#' empic <- calculate_empirical_influence(boot_res)
#'
#' # Single-case removal of the top 2 candidates
#' loo_res <- leave_one_out_analysis(
#'   empic, test_data,
#'   top_n   = 2,
#'   default = "EBICglasso",
#'   verbose = FALSE
#' )
#'
#' loo_res
#'
#' # Multi-case removal (all three at once)
#' loo_multi <- leave_one_out_analysis(
#'   empic, test_data,
#'   remove_cases = c(1, 2, 3),
#'   default      = "EBICglasso",
#'   verbose      = FALSE
#' )
#'
#'loo_multi
#'
#' @export
leave_one_out_analysis <- function(influence_result,
                                    data,
                                    remove_cases = NULL,
                                    threshold = NULL,
                                    top_n = NULL,
                                    direction = c("both", "positive", "negative"),
                                    digits = NULL,
                                    verbose = TRUE,
                                    ...) {
  n <- nrow(data)

  if (!is.numeric(influence_result)) {
    stop("'influence_result' must be a numeric vector.")
  }
  if (length(influence_result) != n) {
    stop("Length of 'influence_result' (", length(influence_result),
         ") must equal the number of rows in 'data' (", n, ").")
  }

  if (is.null(remove_cases) && is.null(threshold) && is.null(top_n)) {
    top_n = n
  }

  if (!is.null(remove_cases)) {
    if (!is.numeric(remove_cases)) {
      stop("'remove_cases' must be a numeric vector.")
    }
    if (any(remove_cases != round(remove_cases))) {
      stop("'remove_cases' must be integers.")
    }
    if (any(remove_cases < 1 | remove_cases > n)) {
      stop("'remove_cases' must be between 1 and ", n, ".")
    }
    if (anyDuplicated(remove_cases)) {
      stop("'remove_cases' can not contain duplicated values.")
    }
  }

  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1) {
      stop("'threshold' must be a single numeric value.")
    }
    if (threshold < 0) {
      stop("'threshold' can not be negative, please use direction = 'negative'.")
    }
  }

if (!is.null(top_n)) {
  if (!is.numeric(top_n) || length(top_n) != 1 ||
      top_n < 1 || top_n != round(top_n)) {
    stop("'top_n' must be a single positive integer.")
  }
  if (top_n > n) {
    warning("'top_n' (", top_n, ") exceeds the number of cases (", n,
            "); using all cases instead.")
    top_n <- n
  }
}

  if (!is.null(remove_cases) && (!is.null(threshold) || !is.null(top_n))) {
      warning("remove_cases is specified; ignoring threshold and top_n.")
  } else if (!is.null(threshold) && !is.null(top_n)) {
      warning("threshold is specified; ignoring top_n.")
  }
  direction <- match.arg(direction)
  influence_values <- influence_result

  if (!is.null(remove_cases) && length(remove_cases) > 1) {

    if (verbose) message("Performing leave-multiple-out analysis...")

    full_network <- bootnet::estimateNetwork(data,
                                              verbose = FALSE,
                                              ...)

    full_graph <- full_network$graph
    global_full <- sum(abs(full_graph[upper.tri(full_graph)]))

    data_without <- data[-remove_cases, , drop = FALSE]

    network_without <- bootnet::estimateNetwork(data_without,
                                                 verbose = FALSE,
                                                 ...)

    graph_without <- network_without$graph
    global_without <- sum(abs(graph_without[upper.tri(graph_without)]))

    structure_changes <- diagnose_structure_change(full_graph,
                                                     graph_without,
                                                     colnames(data),
                                                     digits = digits)

    result <- list(
      case_ids = remove_cases,
      n_removed = length(remove_cases),
      global_full = global_full,
      global_without = global_without,
      global_weight_strength_change = global_full - global_without,
      global_weight_strength_change_pct =
        (global_full - global_without) / global_full * 100,
      structure_changes = structure_changes,
      network_full = full_network,
      network_without = network_without
    )

    class(result) <- c("looMultiAnalysis", "list")
    return(result)
  }

  if (!is.null(remove_cases)) {
    candidate_ids <- remove_cases
    selection_method <- paste0("manual_", length(remove_cases))
  } else if (!is.null(threshold)) {
    if (direction == "both") {
      candidate_ids <- which(abs(influence_values) > threshold)
    } else if (direction == "positive") {
      candidate_ids <- which(influence_values > threshold)
    } else if (direction == "negative") {
      candidate_ids <- which(influence_values < -threshold)
    }
    selection_method <- paste0("threshold_", threshold)
  } else {
    if (direction == "both") {
      candidate_ids <- order(abs(influence_values), decreasing = TRUE)[1:min(top_n, n)]
    } else if (direction == "positive") {
      candidate_ids <- order(influence_values, decreasing = TRUE)[1:min(top_n, n)]
      candidate_ids <- candidate_ids[influence_values[candidate_ids] > 0]
    } else if (direction == "negative") {
      candidate_ids <- order(influence_values)[1:min(top_n, n)]
      candidate_ids <- candidate_ids[influence_values[candidate_ids] < 0]
    }
    selection_method <- paste0("top_", top_n)
  }

  if (length(candidate_ids) == 0) {
    stop("No candidate cases selected. Try adjusting 'remove_cases', 'top_n' or 'threshold'.")
  }

  if (verbose) {
    message(sprintf("Selected %d candidate case(s) for leave-one-out analysis",
                    length(candidate_ids)))
    message("Case IDs: ", paste(candidate_ids, collapse = ", "))
  }

  if (verbose) message("Estimating full network...")
  full_network <- bootnet::estimateNetwork(data,
                                            verbose = FALSE,
                                            ...)

  full_graph <- full_network$graph
  global_full <- sum(abs(full_graph[upper.tri(full_graph)]))

  results <- list()

  if (verbose) message("Performing leave-one-out analysis...")
  if (verbose) pb <- utils::txtProgressBar(0, length(candidate_ids), style = 3)

  for (i in seq_along(candidate_ids)) {
    case_id <- candidate_ids[i]

    data_without <- data[-case_id, , drop = FALSE]

    network_without <- bootnet::estimateNetwork(data_without,
                                                 verbose = FALSE,
                                                 ...)

    graph_without <- network_without$graph
    global_without <- sum(abs(graph_without[upper.tri(graph_without)]))

    structure_changes <- diagnose_structure_change(full_graph,
                                                     graph_without,
                                                     colnames(data),
                                                     digits = digits)

    results[[i]] <- list(
      case_id = case_id,
      empirical_influence = influence_values[case_id],
      global_full = global_full,
      global_without = global_without,
      global_weight_strength_change = global_full - global_without,
      global_weight_strength_change_pct =
        (global_full - global_without) / global_full * 100,
      structure_changes = structure_changes,
      network_full = full_network,
      network_without = network_without
    )

    if (verbose) utils::setTxtProgressBar(pb, i)
  }

  if (verbose) close(pb)

  summary_table <- data.frame(
    case_id = sapply(results, function(x) x$case_id),
    empirical_influence = sapply(results, function(x) x$empirical_influence),
    global_weight_strength_change =
      sapply(results, function(x) x$global_weight_strength_change),
    global_weight_strength_change_pct =
      sapply(results, function(x) x$global_weight_strength_change_pct),
    n_edges_disappeared =
      sapply(results, function(x) x$structure_changes$n_disappeared),
    n_edges_appeared =
      sapply(results, function(x) x$structure_changes$n_appeared),
    n_edges_reversed =
      sapply(results, function(x) x$structure_changes$n_reversed)
  )

  result <- list(
    results = results,
    summary = summary_table,
    case_ids = candidate_ids,
    n_cases = length(candidate_ids),
    selection_method = selection_method,
    direction = direction
  )

  class(result) <- c("looAnalysis", "list")
  return(result)
}
