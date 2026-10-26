#' Print Method for influenceDiagnostic Objects
#'
#' Prints a summary of the influence diagnostic results, including
#' empirical influence values, leave-one-out validation, and centrality
#' gCD results if available.
#'
#' @param x An object of class `"influenceDiagnostic"`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.influenceDiagnostic <- function(x, ...) {
  cat("Influence Diagnostic Results\n")
  cat("============================\n\n")
  cat("Phase 1: Empirical Influence Screening\n")
  cat(sprintf("  Number of cases: %d\n", length(x$empirical_influence$influence)))
  cat(sprintf("  Selected candidates: %d\n", length(x$loo_validation$case_ids)))
  cat(sprintf("  Selection method: %s\n", x$loo_validation$selection_method))
  cat(sprintf("  Direction: %s\n\n", x$loo_validation$direction))
  cat("All Case Influence Values (sorted by absolute influence):\n")
  inf <- x$empirical_influence$influence
  ord <- order(abs(inf), decreasing = TRUE)
  influence_table <- data.frame(
    case_id = ord,
    influence = inf[ord]
  )
  print(influence_table, row.names = FALSE)
  cat("\n")

  cat("Phase 2: Leave-One-Out Validation\n")
  cat("=================================\n\n")

  print(x$loo_validation)

  if (!is.null(x$centrality_gCD)) {
    cat("\nPhase 3: Centrality gCD\n")
    cat("========================\n\n")
    print(x$centrality_gCD)
  }

  invisible(x)
}

#' Print Method for looAnalysis Objects
#'
#' Prints a summary of leave-one-out analysis results, including
#' global strength changes and structural changes for each case.
#'
#' @param x An object of class `"looAnalysis"`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.looAnalysis <- function(x, ...) {
  cat("Leave-One-Out Analysis Results\n")
  cat("==============================\n\n")

  cat(sprintf("Selection method: %s\n", x$selection_method))
  cat(sprintf("Direction: %s\n", x$direction))
  cat(sprintf("Number of cases analyzed: %d\n\n", x$n_cases))

  cat("Summary:\n")
  summary_print <- x$summary
  names(summary_print)[names(summary_print) == "global_weight_strength_change"] <- "global_weight-strength_change"
  names(summary_print)[names(summary_print) == "global_weight_strength_change_pct"] <- "global_weight-strength_change(%)"
  print(summary_print)

  for (i in seq_along(x$results)) {
    case_id <- x$results[[i]]$case_id
    structure_changes <- x$results[[i]]$structure_changes
    print_structure_changes(structure_changes, case_id)
  }

  invisible(x)
}

#' Print Method for looMultiAnalysis Objects
#'
#' Prints a summary of leave-multiple-out analysis results, including
#' global strength changes and structural changes when multiple cases
#' are removed simultaneously.
#'
#' @param x An object of class `"looMultiAnalysis"`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.looMultiAnalysis <- function(x, ...) {
  cat("Leave-Multiple-Out Analysis Results\n")
  cat("===================================\n\n")

  cat(sprintf("Removed cases: %s\n", paste(x$case_ids, collapse = ", ")))
  cat(sprintf("Number of removed cases: %d\n\n", x$n_removed))

  cat("Summary:\n")
  summary_print <- data.frame(
    case_ids = paste(x$case_ids, collapse = ", "),
    global_weight_strength_change = x$global_weight_strength_change,
    global_weight_strength_change_pct = x$global_weight_strength_change_pct,
    stringsAsFactors = FALSE
  )
  names(summary_print)[names(summary_print) == "global_weight_strength_change"] <- "global_weight-strength_change"
  names(summary_print)[names(summary_print) == "global_weight_strength_change_pct"] <- "global_weight-strength_change(%)"
  print(summary_print)

  print_structure_changes(x$structure_changes, x$case_ids)

  invisible(x)
}


#' Print Method for empiricalInfluence Objects
#'
#' Prints a summary of empirical influence values. By default, it prints
#' summary statistics. If `n` is specified, it prints the top `n` cases
#' with the largest absolute influence values. If `detailed = TRUE`,
#' it prints the full sorted table of all cases.
#'
#' @param x An object of class `"empiricalInfluence"`.
#' @param n If setted, only the top "n" cases with absolute values will be printed.
#' @param detailed If `TRUE`, prints the full table of all cases.
#'   Default is `FALSE`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.empiricalInfluence <- function(x, n = NULL, detailed = FALSE, ...) {
  val <- as.numeric(x)
  ids <- names(x)
  if (is.null(ids)) ids <- as.character(seq_along(val))

  cat("\n=== Empirical Influence ===\n")

  if (detailed) {
    ord <- order(abs(val), decreasing = TRUE)
    df <- data.frame(Case_ID = ids[ord], Influence = val[ord])
    print(df, row.names = FALSE)
    return(invisible(x))
  }

  if (!is.null(n)) {
    n <- min(n, length(val))
    ord <- order(abs(val), decreasing = TRUE)
    df <- data.frame(Case_ID = ids[ord[1:n]], Influence = round(val[ord[1:n]], 3))
    print(df, row.names = FALSE)
    return(invisible(x))
  }

  stats <- c(
    Min = min(val, na.rm = TRUE),
    Median = median(val, na.rm = TRUE),
    Mean = mean(val, na.rm = TRUE),
    Max = max(val, na.rm = TRUE),
    SD = sd(val, na.rm = TRUE)
  )
  cat("Summary Statistics of Influence Values:\n")
  print(round(stats, 3))
  cat("\n* Use `print(x, n = 5)` to view top cases, or `print(x, detailed = TRUE)` to view all.\n")

  invisible(x)
}
