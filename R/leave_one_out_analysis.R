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
#' @param default Network estimation method. Default is `"EBICglasso"`.
#' @param verbose Logical, whether to print progress messages.
#' @param ... Additional arguments passed to [bootnet::estimateNetwork()].
#'
#' @return If `remove_cases` has length > 1, an object of class
#'   `"looMultiAnalysis"`. Otherwise, an object of class `"looAnalysis"`.
#' @export
leave_one_out_analysis <- function(influence_result,
                                    data,
                                    remove_cases = NULL,
                                    threshold = NULL,
                                    top_n = 1,
                                    direction = c("both", "positive", "negative"),
                                    default = "EBICglasso",
                                    verbose = TRUE,
                                    ...) {
  if (!is.null(remove_cases) && (!is.null(threshold) || !missing(top_n))) {
      warning("remove_cases is specified; ignoring threshold and top_n.")
  } else if (!is.null(threshold) && !missing(top_n)) {
      warning("threshold is specified; ignoring top_n.")
  }
  direction <- match.arg(direction)
  influence_values <- influence_result
  n <- length(influence_values)

  if (!is.null(remove_cases) && length(remove_cases) > 1) {

    if (verbose) message("Performing leave-multiple-out analysis...")

    full_network <- bootnet::estimateNetwork(data,
                                              default = default,
                                              verbose = FALSE,
                                              ...)

    full_graph <- full_network$graph
    global_full <- sum(abs(full_graph[upper.tri(full_graph)]))

    data_without <- data[-remove_cases, , drop = FALSE]

    network_without <- bootnet::estimateNetwork(data_without,
                                                 default = default,
                                                 verbose = FALSE,
                                                 ...)

    graph_without <- network_without$graph
    global_without <- sum(abs(graph_without[upper.tri(graph_without)]))

    structure_changes <- diagnose_structure_change(full_graph,
                                                     graph_without,
                                                     colnames(data))

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
                                            default = default,
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
                                                 default = default,
                                                 verbose = FALSE,
                                                 ...)

    graph_without <- network_without$graph
    global_without <- sum(abs(graph_without[upper.tri(graph_without)]))

    structure_changes <- diagnose_structure_change(full_graph,
                                                     graph_without,
                                                     colnames(data))

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
