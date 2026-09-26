diagnose_structure_change <- function(full_graph, graph_without, labels, digits = NULL) {

  full_edges <- full_graph != 0
  without_edges <- graph_without != 0
  upper_idx <- upper.tri(full_edges)

  disappeared <- which(full_edges & !without_edges & upper_idx, arr.ind = TRUE)
  appeared <- which(!full_edges & without_edges & upper_idx, arr.ind = TRUE)

  common <- which(full_edges & without_edges & upper_idx, arr.ind = TRUE)
  reversed_idx <- which(sign(full_graph[common]) != sign(graph_without[common]))
  reversed <- common[reversed_idx, , drop = FALSE]

  fmt_value <- function(x) {
    if (is.null(digits)) {
      sprintf("%.17g", x)
    } else {
      sprintf(paste0("%.", digits, "f"), x)
    }
  }

  format_edge_list <- function(edge_matrix, type) {
    if (is.null(edge_matrix) || nrow(edge_matrix) == 0) {
      return(character(0))
    }

    edge_strings <- character(nrow(edge_matrix))
    for (i in 1:nrow(edge_matrix)) {
      from <- labels[edge_matrix[i, 1]]
      to <- labels[edge_matrix[i, 2]]
      weight_from <- full_graph[edge_matrix[i, 1], edge_matrix[i, 2]]
      weight_to <- graph_without[edge_matrix[i, 1], edge_matrix[i, 2]]

      if (type == "disappeared") {
        edge_strings[i] <- sprintf("  %s -- %s : %s -> 0",
                                   from, to, fmt_value(weight_from))
      } else if (type == "appeared") {
        edge_strings[i] <- sprintf("  %s -- %s : 0 -> %s",
                                   from, to, fmt_value(weight_to))
      } else if (type == "reversed") {
        edge_strings[i] <- sprintf("  %s -- %s : %s -> %s",
                                   from, to,
                                   fmt_value(weight_from),
                                   fmt_value(weight_to))
      }
    }
    return(edge_strings)
  }

  disappeared_edges <- format_edge_list(disappeared, "disappeared")
  appeared_edges <- format_edge_list(appeared, "appeared")
  reversed_edges <- format_edge_list(reversed, "reversed")

  list(
    disappeared = disappeared_edges,
    appeared = appeared_edges,
    reversed = reversed_edges,
    n_disappeared = length(disappeared_edges),
    n_appeared = length(appeared_edges),
    n_reversed = length(reversed_edges)
  )
}

print_structure_changes <- function(structure_changes, case_ids = NULL) {

  if (!is.null(case_ids)) {
    if (length(case_ids) == 1) {
      cat(sprintf("\n=== Without Case %d: Structure Changes ===\n\n", case_ids))
    } else {
      cat(sprintf("\n=== Without Cases %s: Structure Changes ===\n\n",
                  paste(case_ids, collapse = ", ")))
    }
  } else {
    cat("\n=== Structure Changes ===\n\n")
  }

  cat("Edges disappeared:\n")
  if (structure_changes$n_disappeared > 0) {
    for (edge in structure_changes$disappeared) {
      cat(edge, "\n")
    }
  } else {
    cat("  None\n")
  }

  cat("\nEdges appeared:\n")
  if (structure_changes$n_appeared > 0) {
    for (edge in structure_changes$appeared) {
      cat(edge, "\n")
    }
  } else {
    cat("  None\n")
  }

  cat("\nEdge sign reversals:\n")
  if (structure_changes$n_reversed > 0) {
    for (edge in structure_changes$reversed) {
      cat(edge, "\n")
    }
  } else {
    cat("  None\n")
  }

  cat("\n")
}
