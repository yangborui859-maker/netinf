#' Plot Method for looAnalysis and looMultiAnalysis Objects
#'
#' Visualizes the structural changes in a network before and after removing an
#' influential case. Topological changes (disappeared, appeared, and reversed edges)
#' are highlighted with thick blue solid lines, while unchanged edges are shown in
#' thin black solid lines.
#'
#' @param x An object of class `"looAnalysis"` or `"looMultiAnalysis"`.
#' @param n Integer. Number of top influential cases to plot side-by-side. Default is 1.
#' @param qgraph_args Additional arguments passed to \code{qgraph}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns \code{NULL}.
#' @export

plot.looAnalysis <- function(x, n = 1, qgraph_args = list(), ...) {

  is_multi <- inherits(x, "looMultiAnalysis")
  full_net <- x$full_network$graph

  if (is_multi) {
    case_ids_to_plot <- list(x$case_ids)
    red_nets <- x$reduced_networks
  } else {
    total_edges <- x$summary$n_edges_disappeared +
                   x$summary$n_edges_appeared +
                   x$summary$n_edges_reversed
    ord <- order(-total_edges, -abs(x$summary$global_weight_strength_change))
    n_show <- min(n, length(x$case_ids))
    target_idx <- ord[1:n_show]
    case_ids_to_plot <- as.list(x$case_ids[target_idx])
    red_nets <- x$reduced_networks[target_idx]
  }

  n_cases <- length(case_ids_to_plot)
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  layout(matrix(1:(n_cases * 2), nrow = n_cases, ncol = 2, byrow = TRUE))

  for (i in seq_len(n_cases)) {
    W_red <- red_nets[[i]]$graph
    case_label <- paste(case_ids_to_plot[[i]], collapse = ", ")

    disappeared_idx <- which(full_net != 0 & W_red == 0, arr.ind = TRUE)
    appeared_idx    <- which(full_net == 0 & W_red != 0, arr.ind = TRUE)
    reversed_idx    <- which(full_net * W_red < 0, arr.ind = TRUE)

    max_w_full <- max(abs(full_net))
    max_w_red  <- max(abs(W_red))

    edge_colors_full <- matrix("transparent", nrow(full_net), ncol(full_net))
    edge_widths_full <- matrix(1, nrow(full_net), ncol(full_net))

    edge_colors_red <- matrix("transparent", nrow(W_red), ncol(W_red))
    edge_widths_red <- matrix(1, nrow(W_red), ncol(W_red))

    for (r in 1:nrow(full_net)) {
      for (c in 1:ncol(full_net)) {
        if (full_net[r, c] != 0) {
          rel_w <- abs(full_net[r, c]) / max_w_full
          edge_colors_full[r, c] <- adjustcolor("black", alpha.f = max(0.2, rel_w))
        }
        if (W_red[r, c] != 0) {
          rel_w <- abs(W_red[r, c]) / max_w_red
          edge_colors_red[r, c] <- adjustcolor("black", alpha.f = max(0.2, rel_w))
        }
      }
    }

    if (nrow(disappeared_idx) > 0) {
      for (k in seq_len(nrow(disappeared_idx))) {
        r <- disappeared_idx[k, 1]; c <- disappeared_idx[k, 2]
        edge_colors_full[r, c] <- "blue"; edge_colors_full[c, r] <- "blue"
        edge_widths_full[r, c] <- 4; edge_widths_full[c, r] <- 4
      }
    }

    if (nrow(appeared_idx) > 0) {
      for (k in seq_len(nrow(appeared_idx))) {
        r <- appeared_idx[k, 1]; c <- appeared_idx[k, 2]
        edge_colors_red[r, c] <- "blue"; edge_colors_red[c, r] <- "blue"
        edge_widths_red[r, c] <- 4; edge_widths_red[c, r] <- 4
      }
    }

    if (nrow(reversed_idx) > 0) {
      for (k in seq_len(nrow(reversed_idx))) {
        r <- reversed_idx[k, 1]; c <- reversed_idx[k, 2]
        edge_colors_full[r, c] <- "blue"; edge_colors_full[c, r] <- "blue"
        edge_widths_full[r, c] <- 4; edge_widths_full[c, r] <- 4

        edge_colors_red[r, c] <- "blue"; edge_colors_red[c, r] <- "blue"
        edge_widths_red[r, c] <- 4; edge_widths_red[c, r] <- 4
      }
    }

    base_args <- list(
      layout       = "circle",
      vsize        = 4,
      labels       = FALSE,
      color        = "tomato",
      border.color = "black",
      fade         = FALSE,
      rescale      = FALSE,
      negDashed    = FALSE
    )
    final_args <- utils::modifyList(base_args, qgraph_args)

    args_l <- final_args
    args_l$edge.color <- edge_colors_full
    args_l$edge.width <- edge_widths_full
    do.call(qgraph::qgraph, c(list(input = full_net,
                                   title = if(i==1) "Full Sample Network" else ""), args_l))

    args_r <- final_args
    args_r$edge.color <- edge_colors_red
    args_r$edge.width <- edge_widths_red
    do.call(qgraph::qgraph, c(list(input = W_red,
                                   title = paste0("Without Case ", case_label)), args_r))
  }

  invisible(NULL)
}

#' @rdname plot.looAnalysis
#' @export
loo_plot <- plot.looAnalysis

#' @rdname plot.looAnalysis
#' @export
plot.looMultiAnalysis <- plot.looAnalysis
