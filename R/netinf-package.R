#' netinf: Diagnostic Influetial cases for Network Analysis
#'
#' `netinf` provides tools to identify influential cases in psychological
#' network analysis. Built on 'bootnet' (Epskamp, Borsboom, and Fried, 2018)
#' <doi:10.3758/s13428-017-0862-1>, it reuses the same bootstrap samples
#' to perform case influence diagnostics, avoiding a separate bootstrap step.
#'
#' * [bootnet_with_indices()] extracts case indices from bootstrap samples.
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
#' @examples
#' # The package ships with a simulated dataset that contains
#' # 99 regular cases and 1 influential case (the 100th row).
#' data("test_data", package = "netinf")
#'
#' # All-in-one workflow
#' set.seed(1)
#' diag <- influence_diagnostic(
#'   data               = test_data,
#'   nBoots             = 100,
#'   top_n              = 3,
#'   centrality_metrics = c("Strength", "Closeness", "Betweenness"),
#'   default            = "EBICglasso",
#'   verbose            = FALSE
#' )
#' diag
#'
#' # Manual workflow
#' set.seed(1)
#' boot_res <- bootnet_with_indices(
#'   test_data,
#'   nBoots  = 100,  # Use nBoots = 5000 or more in practice; 100 here for speed.
#'   default = "EBICglasso"
#' )
#' boot_res
#'
#' empic <- calculate_empirical_influence(boot_res)
#' empic
#'
#' loo_res <- leave_one_out_analysis(
#'   empic, test_data,
#'   top_n   = 3,
#'   default = "EBICglasso",
#'   verbose = FALSE
#' )
#' loo_res
#'
#' gcd_res <- calculate_centrality_gCD(
#'   data        = test_data,
#'   boot_result = boot_res,
#'   case_ids    = 37,
#'   metric      = c("Strength", "Closeness", "Betweenness"),
#'   default     = "EBICglasso"
#' )
#' gcd_res
#'
#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"
