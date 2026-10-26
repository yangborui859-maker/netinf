#' Print Method for influenceDiagnostic Objects
#'
#' Prints a summary of the influence diagnostic results, including
#' empirical influence values, leave-one-out validation, and centrality
#' gCD results if available.
#'
#' @param x An object of class `"influenceDiagnostic"`.
#' @param n If specified, prints the top `n` cases. Default is `NULL`.
#' @param detailed If `TRUE`, prints full details. Default is `FALSE`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.influenceDiagnostic <- function(x, n = NULL, detailed = FALSE, ...) {
  cat("Influence Diagnostic Results\n")
  cat("============================\n\n")
  cat("Phase 1: Empirical Influence Screening\n")
  cat("=================================\n\n")
  print(x$empirical_influence$influence, n = n, detailed = detailed)
  cat("\n")

  cat("Phase 2: Leave-One-Out Validation\n")
  cat("=================================\n\n")

  print(x$loo_validation)

  cat("Phase 3: Centrality gCD\n")
  cat("=================================\n\n")
  print(x$centrality_gCD, n = n, detailed = detailed)

  invisible(x)
}

#' Print Method for looAnalysis Objects
#'
#' Prints a summary of leave-one-out analysis results, including
#' global strength changes and structural changes for each case.
#'
#' @param x An object of class `"looAnalysis"`or `"looMultiAnalysis"`.
#' @param n The number of top n cases with the most structural changes (Edges)  to display for single-case LOO. Default is `NULL`.
#' @param detailed If `TRUE`, prints detailed edge weight changes. Default is `FALSE`.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.looAnalysis <- function(x, n = NULL, detailed = FALSE, ...) {

  if (inherits(x, "looMultiAnalysis")) {
    cat(sprintf("\n=== Leave-Multiple-Out Results (Cases: %s) ===\n",
                paste(x$case_ids, collapse = ", ")))
    cat(sprintf("Global Strength Change: %.3f (%.2f%%)\n",
                x$global_weight_strength_change,
                x$global_weight_strength_change_pct))

    print_structure_changes(x$structure_changes, x$case_ids)

    return(invisible(x))
  }

  cat("Leave-One-Out Analysis Results\n")
  cat("==============================\n")
  cat(sprintf("Selection method: %s\n", x$selection_method))
  cat(sprintf("Number of cases analyzed: %d\n\n", x$n_cases))

  df <- x$summary

  total_edges <- df$n_edges_disappeared + df$n_edges_appeared + df$n_edges_reversed
  cat("Summary of Structural Changes:\n")
  dist_table <- as.data.frame(table(total_edges))
  names(dist_table) <- c("Total_Edges_Changed", "Number_of_Cases")

  dist_table <- dist_table[order(-as.numeric(as.character(dist_table$Total_Edges_Changed))), ]
  print(dist_table, row.names = FALSE)
  cat("\n")

  if (is.null(n) && !detailed) {
    cat("* Use `print(x, n = 5)` to view the top 5 influential cases and their detailed edge changes.\n")
    cat("* Use `print(x, detailed = TRUE)` to view all cases.\n")
    return(invisible(x))
  }

  if (detailed) {
     n_show <- nrow(df)
  } else {
    n_show <- min(n, nrow(df))
  }

  ord <- order(-total_edges, -abs(df$global_weight_strength_change))
  df_sorted <- df[ord, ]
  target_df <- df_sorted[1:n_show, ]

  cat(sprintf("Top %d cases ranked by Total Structural Changes:\n", n_show))

  summary_print <- target_df
  names(summary_print)[names(summary_print) == "global_weight_strength_change"] <- "global_weight-strength_change"
  names(summary_print)[names(summary_print) == "global_weight_strength_change_pct"] <- "global_weight-strength_change(%)"

  print(summary_print, row.names = FALSE)
  cat("\n")

  for (i in seq_len(nrow(target_df))) {
    case_id <- target_df$case_id[i]
    idx <- which(x$summary$case_id == case_id)
    structure_changes <- x$results[[idx]]$structure_changes
    print_structure_changes(structure_changes, case_id)
  }

  if (nrow(df_sorted) > n_show) {
    cat(sprintf("* %d more cases not shown. Use `print(x, n = %d)` or `print(x, detailed = TRUE)` to view all.\n",
                nrow(df_sorted) - n_show, nrow(df_sorted)))
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

#' Print Method for centralityGCD Objects
#'
#' Prints a summary of the generalized Cook's distance (gCD) results.
#' By default, it prints summary statistics. If `n` is specified, it
#' prints the top `n` cases with the largest gCD for each metric. If
#' `detailed = TRUE`, it prints the full table.
#'
#' @param x An object of class `"centralityGCD"`.
#' @param n If specified, prints the top `n` cases per metric.
#' @param detailed If `TRUE`, prints the full sorted table of all cases.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.centralityGCD <- function(x, n = NULL, detailed = FALSE, ...) {
  df <- as.data.frame(x)
  cat("\n=== Centrality generalized Cook's Distance (gCD) ===\n")

  metrics <- unique(df$metric)

  if (detailed) {
    df <- df[order(df$metric, -df$gCD), ]
    print(df, row.names = FALSE)
    return(invisible(x))
  }

  if (!is.null(n)) {
    cat(sprintf("Top %d cases per metric:\n\n", n))
    for (m in metrics) {
      sub_df <- df[df$metric == m, ]
      sub_df <- sub_df[order(-sub_df$gCD), ]
      top_n <- head(sub_df, n)
      cat(sprintf("--- Metric: %s ---\n", m))
      print(data.frame(Case_ID = top_n$case_id, gCD = round(top_n$gCD, 3)), row.names = FALSE)
      cat("\n")
    }
    return(invisible(x))
  }

  cat("Summary Statistics of gCD by Metric:\n\n")
  for (m in metrics) {
    sub_val <- df$gCD[df$metric == m]
    stats <- c(
      Min = min(sub_val, na.rm = TRUE),
      Median = median(sub_val, na.rm = TRUE),
      Mean = mean(sub_val, na.rm = TRUE),
      Max = max(sub_val, na.rm = TRUE)
    )
    cat(sprintf("Metric: %s\n", m))
    print(round(stats, 3))
    cat("\n")
  }
  cat("* Use `print(x, n = 5)` to view top cases per metric, or `print(x, detailed = TRUE)` to view all.\n")

  invisible(x)
}
