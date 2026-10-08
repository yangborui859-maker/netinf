#' Case Influence Diagnostic for Network Models
#'
#' All-in-one function that performs empirical influence screening,
#' leave-one-out validation, and centrality gCD computation.
#'
#' @param data A data frame, matrix, or `bootnetWithIndices` object.
#'   If a data frame/matrix is provided, bootstrapping is performed
#'   internally via [bootnet_with_indices()]. If a `bootnetWithIndices`
#'   object is provided, it is used directly and `nBoots` is ignored.
#' @param nBoots Number of bootstrap samples. Default is 1000.
#' @param remove_cases Optional numeric vector of case IDs to remove manually for [leave_one_out_analysis()].
#' @param threshold Optional threshold for selecting cases based on absolute
#'   influence values for [leave_one_out_analysis()].
#' @param top_n Number of top influential cases to select for [leave_one_out_analysis()] when `remove_cases`
#'   and `threshold` are not specified.
#' @param direction Character. Direction of case selection when
#'   `threshold` or `top_n` is used: `"both"` (default) selects by
#'   absolute influence magnitude; `"positive"` selects only cases
#'   with positive influence; `"negative"` selects only cases with
#'   negative influence. Ignored when `remove_cases` is supplied.
#' @param default Network estimation method. Default is `"EBICglasso"`.
#' @param verbose Logical, whether to print progress messages.
#' @param centrality_metrics Character vector of centrality measures to compute
#'   in Phase 3. Default is `c("Strength", "Closeness", "Betweenness")`.
#' @param nCores Integer, number of CPU cores for parallel computation
#'   in Phase 3. Default is 1.
#' @param ... Additional arguments passed to [bootnet::estimateNetwork()].
#'
#' @return An object of class `"influenceDiagnostic"` containing:
#'   \itemize{
#'     \item `empirical_influence`: list with influence values and candidate IDs.
#'     \item `loo_validation`: output from [leave_one_out_analysis()].
#'     \item `centrality_gCD`: data frame of centrality gCD results (if computed).
#'     \item `boot_result`: original bootstrap result object.
#'     \item `data`: original data used.
#'   }
#'
#' @examples
#' data("test_data", package = "netinf")
#'
#' diag <- influence_diagnostic(
#'   data               = test_data,
#'   nBoots             = 100,        # for demonstration; use 5000+ in practice
#'   top_n              = 2,
#'   default            = "EBICglasso",
#'   centrality_metrics = c("Strength", "Closeness", "Betweenness"),
#'   verbose            = FALSE
#' )
#'
#' diag
#' @export
influence_diagnostic <- function(data,
                                  nBoots = 1000,
                                  remove_cases = NULL,
                                  threshold = NULL,
                                  top_n = NULL,
                                  direction = c("both", "positive", "negative"),
                                  default = "EBICglasso",
                                  verbose = TRUE,
                                  centrality_metrics = c("Strength", "Closeness", "Betweenness"),
                                  nCores = 1,
                                  ...) {

  if (!identical(default, "EBICglasso")) {
    stop("'default' only support 'EBICglasso' now.")
  }

  direction <- match.arg(direction)

  if (!is.numeric(nCores) || length(nCores) != 1 || nCores < 1 ||
      nCores != round(nCores)) {
    stop("'nCores' must be a single positive integer.")
  }

  if (inherits(data, "bootnetWithIndices")) {
    if (!missing(nBoots)) {
      warning("'nBoots' is ignored when 'data' is a bootnetWithIndices ",
              "object. Using the bootstrap samples already present in 'data'.")
    }
    boot_result <- data
    data_used <- boot_result$sample$data

  } else if (is.data.frame(data) || is.matrix(data)) {
    data_used <- data
    boot_result <- bootnet_with_indices(
      data_used,
      nBoots = nBoots,
      keep_data = FALSE,
      default = default,
      ...
    )

  } else {
    stop("'data' must be a data frame, matrix, or bootnetWithIndices object.")
  }


  if (verbose) message("Phase 1: Empirical influence screening...")

  global_influence <- calculate_empirical_influence(boot_result)

  if (verbose) message("Phase 2: Leave-one-out validation...")

  loo_results <- leave_one_out_analysis(
    influence_result = global_influence,
    data = data_used,
    remove_cases = remove_cases,
    top_n = top_n,
    threshold = threshold,
    direction = direction,
    default = default,
    verbose = verbose,
    ...
  )

  if (verbose) message("Phase 3: Centrality gCD computation...")

  centrality_gCD_results <- calculate_centrality_gCD(
    boot_result = boot_result,
    case_ids = loo_results$case_ids,
    data = data_used,
    metric = centrality_metrics,
    default = default,
    nCores = nCores,
    ...
  )

  centrality_diff_vectors <- attr(centrality_gCD_results, "diff_vectors")

  if (is.null(centrality_diff_vectors)) {
    warning("'diff_vectors' attribute missing from gCD results.")
  }

  result <- list(
    empirical_influence = list(
      influence = global_influence,
      candidate_ids = loo_results$case_ids
    ),
    loo_validation = loo_results,
    centrality_gCD = centrality_gCD_results,
    centrality_diff_vectors = centrality_diff_vectors,
    boot_result = boot_result,
    data = data_used,
    top_n = top_n,
    threshold = threshold,
    direction = direction,
    remove_cases = remove_cases,
    centrality_metrics = centrality_metrics,
    nBoots = length(boot_result$bootIndices)
  )

  class(result) <- "influenceDiagnostic"
  return(result)
}
