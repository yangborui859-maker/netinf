#' Plot Case Influence on Centrality Against gCD
#'
#' Creates a faceted plot with gCD on the x-axis and centrality change
#' on the y-axis, similar to semfindr's est_change_gcd_plot.
#'
#' @param diag An object of class `"influenceDiagnostic"` returned by
#'   [influence_diagnostic()], or a data frame returned by
#'   [calculate_centrality_gCD()] (with a `"diff_vectors"` attribute).
#' @param metrics Character vector of centrality measures to plot.
#'   Can include `"Strength"`, `"Closeness"`, `"Betweenness"`.
#' @param node Optional character, if provided, plot change for this node only.
#' @param largest_gcd Integer, number of cases with largest gCD to label.
#' @param largest_change Integer, number of cases with largest absolute
#'   centrality change to label per metric.
#' @param point_aes Additional aesthetics passed to [ggplot2::geom_point()].
#' @param hline_aes Additional aesthetics passed to [ggplot2::geom_hline()].
#' @param case_label_aes Additional aesthetics passed to
#'   [ggrepel::geom_label_repel()].
#' @param wrap_aes Additional aesthetics passed to [ggplot2::facet_wrap()].
#' @param ... Currently unused.
#'
#' @return A `ggplot` object.
#' @export
gcds_plot <- function(diag,
                            metrics = c("Strength", "Closeness", "Betweenness"),
                            node = NULL,
                            largest_gcd = 1,
                            largest_change = 1,
                            point_aes = list(),
                            hline_aes = list(),
                            case_label_aes = list(),
                            wrap_aes = list(),
                            ...) {

  if (is.data.frame(diag) && !is.null(attr(diag, "diff_vectors"))) {
    gcd_df <- diag
    diff_vectors <- attr(diag, "diff_vectors")
  } else if (inherits(diag, "influenceDiagnostic")) {
    gcd_df <- diag$centrality_gCD
    diff_vectors <- diag$centrality_diff_vectors
  } else {
    stop("diag must be an influenceDiagnostic object or a data frame with diff_vectors attribute.")
  }

  if (is.null(gcd_df) || is.null(diff_vectors)) {
    stop("diag must contain centrality_gCD and centrality_diff_vectors.")
  }

  metrics <- match.arg(metrics, several.ok = TRUE)

  all_dat <- list()

  for (m in metrics) {
    gcd_use <- gcd_df[gcd_df$metric == m, ]
    case_ids <- gcd_use$case_id
    gcd_vals <- gcd_use$gCD

    metric_levels <- unique(gcd_df$metric)
    m_idx <- which(metric_levels == m)
    n_cases <- length(case_ids)
    idx <- (m_idx - 1) * n_cases + seq_len(n_cases)

    diff_list <- diff_vectors[idx]

    if (is.null(node)) {
      change_vals <- sapply(diff_list, function(d) mean(abs(d)))
    } else {
      if (!node %in% names(diff_list[[1]])) stop("node not found in difference vectors.")
      change_vals <- sapply(diff_list, function(d) d[[node]])
    }

    all_dat[[m]] <- data.frame(
      case_id = case_ids,
      metric = m,
      gcd = gcd_vals,
      change = change_vals,
      stringsAsFactors = FALSE
    )
  }

  dat <- do.call(rbind, all_dat)
  dat <- dat[!is.na(dat$gcd) & !is.na(dat$change), ]

  if (nrow(dat) == 0) stop("No valid data for plotting.")

  point_aes <- utils::modifyList(
    list(shape = 21, color = "black", fill = "grey", alpha = 0.75, size = 1.5),
    point_aes
  )

  hline_aes <- utils::modifyList(
    list(linetype = "solid", color = "black", linewidth = 0.5),
    hline_aes
  )
  hline_aes <- utils::modifyList(hline_aes, list(yintercept = 0))

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data[["gcd"]], y = .data[["change"]]))
  p <- p + do.call(ggplot2::geom_point, point_aes)
  p <- p + do.call(ggplot2::geom_hline, hline_aes)

  label_i <- rep(FALSE, nrow(dat))

  if (largest_gcd >= 1) {
    for (m in metrics) {
      sub <- dat$metric == m
      o <- order(dat$gcd[sub], decreasing = TRUE)
      ids <- which(sub)[o[seq_len(min(largest_gcd, sum(sub)))]]
      label_i[ids] <- TRUE
    }
  }

  if (largest_change >= 1) {
    for (m in metrics) {
      sub <- dat$metric == m
      o <- order(abs(dat$change[sub]), decreasing = TRUE)
      ids <- which(sub)[o[seq_len(min(largest_change, sum(sub)))]]
      label_i[ids] <- TRUE
    }
  }

  if (any(label_i)) {
    case_label_aes <- utils::modifyList(list(min.segment.length = 0), case_label_aes)
    case_label_aes <- utils::modifyList(
      case_label_aes,
      list(
        data = dat[label_i, , drop = FALSE],
        mapping = ggplot2::aes(
          x = .data[["gcd"]],
          y = .data[["change"]],
          label = .data[["case_id"]]
        )
      )
    )
    p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)
  }

  wrap_aes <- utils::modifyList(
    list(ncol = 1, scales = "free_y", strip.position = "left"),
    wrap_aes
  )
  wrap_aes <- utils::modifyList(
    wrap_aes,
    list(facets = ggplot2::vars(.data[["metric"]]))
  )
  p <- p + do.call(ggplot2::facet_wrap, wrap_aes)

  p <- p + ggplot2::xlab("Generalized Cook's Distance") +
           ggplot2::ylab("Centrality Change")

  p
}
