#' netinf: Diagnostic Influetial cases for Network Analysis
#'
#' `netinf` provides tools to identify influential cases in psychological
#' network analysis. It complements [bootnet::bootnet()] by adding a
#' complete workflow for influence diagnostics:
#'
#' * [bootnet_with_indices()] extracts case indices from bootstrap samples,
#'   which is not directly available from `bootnet`.
#' * [calculate_empirical_influence()] computes one influence value per case
#'   via regression on inclusion counts.
#' * [leave_one_out_analysis()] validates candidate cases by removing them
#'   from the data and re-estimating the network, one or several at a time.
#' * [calculate_centrality_gCD()] quantifies centrality changes via a
#'   generalized Cook's distance.
#'
#' All four steps can be run manually, or through a single all-in-one
#' function, [influence_diagnostic()].
#'
#' @section Getting started:
#' The recommended workflow:
#'
#' 1. Bootstrap the network with [bootnet_with_indices()]
#' 2. Compute empirical influence with [calculate_empirical_influence()]
#' 3. Validate candidates with [leave_one_out_analysis()]
#' 4. Diagnose centrality changes with [calculate_centrality_gCD()]
#'
#' Or run all four steps in one call with [influence_diagnostic()].
#'
#' See `vignette("netinf-guide", package = "netinf")` for a full walkthrough.
#'
#' @section Visualization:
#' * [influence_plot()] — plot of empirical influence values
#' * [gcds_plot()] — gCD vs centrality change
#' * [gcds_md_plot()] — bubble plot with Mahalanobis distance
#'
#' @section Related packages:
#' * [bootnet](https://cran.r-project.org/package=bootnet) for network
#'   estimation and bootstrap.
#' * [qgraph](https://cran.r-project.org/package=qgraph) for network
#'   visualization and centrality.
#'
#' @author
#' Borui Yang and Shu Fai Cheung
#'
#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"
