#' Plot Empirical Influence Values
#'
#' Creates a plot of empirical influence values for each case.
#' The plot can optionally mark cases with the largest absolute influence.
#'
#' @param object An object of class `"influenceDiagnostic"` or a named numeric vector.
#' @param column If `object` is a matrix or data frame, the column to plot.
#' @param plot_title Title of the plot.
#' @param x_label Label for the y-axis.
#' @param cutoff_x_low Optional lower threshold for highlighting cases.
#' @param cutoff_x_high Optional upper threshold for highlighting cases.
#' @param largest_x Number of cases with largest absolute values to label.
#' @param absolute Logical, whether to plot absolute influence values.
#' @param point_aes Additional arguments passed to [ggplot2::geom_point()].
#' @param vline_aes Additional arguments passed to [ggplot2::geom_segment()].
#' @param hline_aes Additional arguments passed to [ggplot2::geom_hline()].
#' @param cutoff_line_aes Additional arguments passed to [ggplot2::geom_hline()] for cutoff lines.
#' @param case_label_aes Additional arguments passed to [ggrepel::geom_label_repel()].
#' @param ... Additional arguments (currently unused).
#'
#' @return A `ggplot` object.
#' @export
influence_plot <- function(object,
                           column = NULL,
                           plot_title = "Influence Plot",
                           x_label = NULL,
                           cutoff_x_low = NULL,
                           cutoff_x_high = NULL,
                           largest_x = NULL,
                           absolute = FALSE,
                           point_aes = list(),
                           vline_aes = list(),
                           hline_aes = list(),
                           cutoff_line_aes = list(),
                           case_label_aes = list(),
                           ...) {

  if (inherits(object, "influenceDiagnostic")) {
    object <- object$empirical_influence$influence
  }
  if (inherits(object, "empiricalInfluence")) {
    object <- unclass(object)
  }

  if (is.null(dim(object))) {
    if (!is.vector(object)) {
      stop("'object' invalid. Neither a matrix nor a vector.")
    }
  } else {
    if (is.null(column) && (ncol(object) == 1)) {
      tmp <- rownames(object)
      object <- object[, 1, drop = TRUE]
      if (!is.null(tmp)) names(object) <- tmp
    } else {
      if (length(column) != 1) {
        stop("'column' must have length 1.")
      }
      column <- as.character(column)
      if (!(column %in% colnames(object))) {
        stop(sQuote(column), " not found in 'object'.")
      }
      tmp <- rownames(object)
      object <- object[, column, drop = TRUE]
      if (!is.null(tmp)) names(object) <- tmp
    }
  }

  object <- object[!is.na(object)]
  if (length(object) == 0) {
    stop("No cases have valid values.")
  }
  
  if (is.null(names(object))) {
    message("Note: no row names found; using 1:n as case IDs.")
    names(object) <- seq_along(object)
  }

  if (absolute) {
    object <- abs(object)
  }

  if (is.null(x_label)) {
    x_label <- ifelse(absolute, "Absolute(Influence)", "Influence")
  }

  point_aes <- utils::modifyList(list(color = "steelblue", size = 2), point_aes)

  vline_aes <- utils::modifyList(
    list(linewidth = 0.6, color = "grey60", lineend = "butt"),
    vline_aes
  )
  vline_aes <- utils::modifyList(
    vline_aes,
    list(mapping = ggplot2::aes(xend = row_id, yend = 0))
  )

  hline_aes <- utils::modifyList(
    list(linetype = "solid", color = "grey"),
    hline_aes
  )
  hline_aes <- utils::modifyList(hline_aes, list(yintercept = 0))

  case_ids <- names(object)
  row_id <- seq_len(length(object))
  dat <- data.frame(
    row_id = row_id,
    case_id = case_ids,
    x = object,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = row_id, y = x))
  p <- p + do.call(ggplot2::geom_segment, vline_aes)
  p <- p + do.call(ggplot2::geom_point, point_aes)
  p <- p + ggplot2::labs(title = plot_title)
  p <- p + do.call(ggplot2::geom_hline, hline_aes)
  p <- p + ggplot2::xlab("Case ID (or Row Number)") + ggplot2::ylab(x_label)

  c_x_cut_low <- -Inf
  c_x_cut_high <- Inf

  if (is.numeric(cutoff_x_low)) {
    cutoff_line_aes <- utils::modifyList(list(linetype = "dashed"), cutoff_line_aes)
    cutoff_line_aes <- utils::modifyList(cutoff_line_aes, list(yintercept = cutoff_x_low))
    p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes)
    c_x_cut_low <- cutoff_x_low
  }

  if (is.numeric(cutoff_x_high)) {
    cutoff_line_aes <- utils::modifyList(list(linetype = "dashed"), cutoff_line_aes)
    cutoff_line_aes <- utils::modifyList(cutoff_line_aes, list(yintercept = cutoff_x_high))
    p <- p + do.call(ggplot2::geom_hline, cutoff_line_aes)
    c_x_cut_high <- cutoff_x_high
  }

  if (is.numeric(largest_x) && largest_x >= 1) {
    m_x <- min(round(largest_x), nrow(dat))
    o_x <- order(abs(dat$x), decreasing = TRUE)
    m_x_cut <- abs(dat$x)[o_x[m_x]]
  } else {
    m_x_cut <- Inf
  }

  label_x <- (dat$x >= c_x_cut_high) |
             (dat$x <= c_x_cut_low) |
             (abs(dat$x) >= m_x_cut)

  if (any(label_x)) {
    case_label_aes <- utils::modifyList(
      list(position = ggplot2::position_dodge(0.5)),
      case_label_aes
    )
    case_label_aes <- utils::modifyList(
      case_label_aes,
      list(
        data = dat[label_x, , drop = FALSE],
        mapping = ggplot2::aes(x = row_id, y = x, label = case_id)
      )
    )
    p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)
  }

  p
}
