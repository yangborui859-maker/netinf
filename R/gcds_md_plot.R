#' Plot gCD Against Mahalanobis Distance and Mean Centrality Change
#'
#' Creates a bubble plot with Mahalanobis distance on the x-axis,
#' mean absolute centrality change on the y-axis, and bubble size
#' representing gCD.
#'
#' @param diag An object of class `"influenceDiagnostic"`, or a data frame
#'   with a `"diff_vectors"` attribute.
#' @param data Original data frame used in the analysis.
#' @param metric Character, centrality metric to use for bubble size and
#'   y-axis. Must be one of `"Strength"`, `"Closeness"`, `"Betweenness"`.
#' @param circle_size Numeric, maximum point size.
#' @param cutoff_md Logical or numeric. If TRUE, use a chi-square cutoff.
#' @param cutoff_md_qchisq Numeric, quantile for chi-square cutoff.
#' @param cutoff_change Numeric, optional cutoff for mean absolute change.
#' @param cutoff_gcd Numeric, optional cutoff for gCD.
#' @param largest_gcd Integer, number of cases with largest gCD to label.
#' @param largest_md Integer, number of cases with largest Mahalanobis distance to label.
#' @param largest_change Integer, number of cases with largest mean absolute change to label.
#' @param point_aes Additional aesthetics passed to [ggplot2::geom_point()].
#' @param hline_aes Additional aesthetics passed to [ggplot2::geom_hline()].
#' @param cutoff_line_md_aes Additional aesthetics passed to [ggplot2::geom_vline()] for MD cutoff.
#' @param cutoff_line_change_aes Additional aesthetics passed to [ggplot2::geom_hline()] for change cutoff.
#' @param case_label_aes Additional aesthetics passed to [ggrepel::geom_label_repel()].
#' @param ... Currently unused.
#'
#' @return A `ggplot` object.
#' @export
gcds_md_plot <- function(diag,
                        data,
                        metric = c("Strength", "Closeness", "Betweenness"),
                        circle_size = 2,
                        cutoff_md = FALSE,
                        cutoff_md_qchisq = 0.975,
                        cutoff_change = NULL,
                        cutoff_gcd = NULL,
                        largest_gcd = 1,
                        largest_md = 1,
                        largest_change = 1,
                        point_aes = list(),
                        hline_aes = list(),
                        cutoff_line_md_aes = list(),
                        cutoff_line_change_aes = list(),
                        case_label_aes = list(),
                        ...) {

  metric <- match.arg(metric, several.ok = TRUE)
  if (length(metric) > 1){
    plots <- lapply(metric,function(m){
      gcds_md_plot(
        diag = diag,
        data = data,
        metric = m,
        circle_size = circle_size,
        cutoff_md = cutoff_md,
        cutoff_md_qchisq = cutoff_md_qchisq,
        cutoff_change = cutoff_change,
        cutoff_gcd = cutoff_gcd,
        largest_gcd = largest_gcd,
        largest_md = largest_md,
        largest_change = largest_change,
        point_aes = point_aes,
        hline_aes = hline_aes,
        cutoff_line_md_aes = cutoff_line_md_aes,
        cutoff_line_change_aes = cutoff_line_change_aes,
        case_label_aes = case_label_aes
      )})
    names(plots) <- metric
    return(plots)
  }

  if (is.data.frame(diag) && !is.null(attr(diag, "diff_vectors"))) {
    gcd_df <- diag
    diff_vectors <- attr(diag, "diff_vectors")
  } else if (inherits(diag, "influenceDiagnostic")) {
    gcd_df <- diag$centrality_gCD
    diff_vectors <- diag$centrality_diff_vectors
  } else {
    stop("diag must be an influenceDiagnostic object or a data frame with diff_vectors attribute.")
  }

  if (!metric %in% gcd_df$metric) {
    stop(metric, " not found in gCD results.")
  }

  gcd_use <- gcd_df[gcd_df$metric == metric, ]
  case_ids <- gcd_use$case_id
  gcd_vals <- gcd_use$gCD

  metric_levels <- unique(gcd_df$metric)
  m_idx <- which(metric_levels == metric)
  n_cases <- length(case_ids)
  idx <- (m_idx - 1) * n_cases + seq_len(n_cases)

  diff_list <- diff_vectors[idx]

  change_vals <- vapply(diff_list, function(d) mean(abs(d)), numeric(1))
  
  mahalanobis_distance <- function(data) {
  data <- data[sapply(data, is.numeric)]
  data <- na.omit(data)
  mu <- colMeans(data)
  S <- cov(data)
  md <- stats::mahalanobis(data, center = mu, cov = S)
  names(md) <- rownames(data)
  md
}
  
  md_all <- mahalanobis_distance(data)
  md_vals <- md_all[as.character(case_ids)]

  dat <- data.frame(
    case_id = case_ids,
    md = md_vals,
    change = change_vals,
    gcd = gcd_vals,
    stringsAsFactors = FALSE
  )
  dat <- dat[!is.na(dat$md) & !is.na(dat$change) & !is.na(dat$gcd), ]

  if (nrow(dat) == 0) stop("No cases with complete data for plotting.")

  point_aes <- utils::modifyList(
    list(shape = 21, alpha = 0.5, fill = "white"),
    point_aes
  )
  point_aes <- utils::modifyList(
    point_aes,
    list(mapping = ggplot2::aes(size = .data[["gcd"]]))
  )

  hline_aes <- utils::modifyList(list(linetype = "solid"), hline_aes)
  hline_aes <- utils::modifyList(hline_aes, list(yintercept = 0))

  p <- ggplot2::ggplot(dat, ggplot2::aes(.data[["md"]], .data[["change"]]))
  p <- p + do.call(ggplot2::geom_point, point_aes)
  p <- p + do.call(ggplot2::geom_hline, hline_aes)
  p <- p + ggplot2::scale_size_area(name = "gCD", max_size = circle_size)
  p <- p + ggplot2::labs(
    title = "Mean Centrality Change against Mahalanobis Distance,\ngCD as the Size"
  )
  p <- p + ggplot2::xlab("Mahalanobis Distance") +
           ggplot2::ylab(paste("Change in", metric))

  c_change_cut <- Inf
  if (is.numeric(cutoff_change)) {
    cutoff_line_change_aes <- utils::modifyList(list(linetype = "dashed"), cutoff_line_change_aes)
    cutoff_line_change_aes1 <- utils::modifyList(cutoff_line_change_aes,
                                                 list(yintercept = cutoff_change))
    cutoff_line_change_aes2 <- utils::modifyList(cutoff_line_change_aes,
                                                 list(yintercept = -cutoff_change))
    p <- p + do.call(ggplot2::geom_hline, cutoff_line_change_aes1)
    p <- p + do.call(ggplot2::geom_hline, cutoff_line_change_aes2)
    c_change_cut <- abs(cutoff_change)
  }

  c_md_cut <- Inf
  if (isTRUE(cutoff_md)) {
    k <- ncol(data)
    c_md_cut <- stats::qchisq(cutoff_md_qchisq, k)
  } else if (is.numeric(cutoff_md)) {
    c_md_cut <- cutoff_md
  }

  if (is.numeric(c_md_cut) && c_md_cut < Inf) {
    cutoff_line_md_aes <- utils::modifyList(list(linetype = "dashed"), cutoff_line_md_aes)
    cutoff_line_md_aes <- utils::modifyList(cutoff_line_md_aes,
                                            list(xintercept = c_md_cut))
    p <- p + do.call(ggplot2::geom_vline, cutoff_line_md_aes)
  }

  c_gcd_cut <- Inf
  if (is.numeric(cutoff_gcd)) {
    c_gcd_cut <- cutoff_gcd
  }

  label_i <- rep(FALSE, nrow(dat))

  if (largest_change >= 1) {
    o <- order(abs(dat$change), decreasing = TRUE)
    label_i[o[seq_len(min(largest_change, nrow(dat)))]] <- TRUE
  }

  if (largest_md >= 1) {
    o <- order(dat$md, decreasing = TRUE)
    label_i[o[seq_len(min(largest_md, nrow(dat)))]] <- TRUE
  }

  if (largest_gcd >= 1) {
    o <- order(dat$gcd, decreasing = TRUE)
    label_i[o[seq_len(min(largest_gcd, nrow(dat)))]] <- TRUE
  }

  label_i <- label_i |
             (abs(dat$change) >= c_change_cut) |
             (dat$md >= c_md_cut) |
             (dat$gcd >= c_gcd_cut)

  if (any(label_i)) {
    case_label_aes <- utils::modifyList(list(min.segment.length = 0), case_label_aes)
    case_label_aes <- utils::modifyList(
      case_label_aes,
      list(
        data = dat[label_i, , drop = FALSE],
        mapping = ggplot2::aes(
          x = .data[["md"]],
          y = .data[["change"]],
          label = .data[["case_id"]]
        )
      )
    )
    p <- p + do.call(ggrepel::geom_label_repel, case_label_aes)
  }

  p
}