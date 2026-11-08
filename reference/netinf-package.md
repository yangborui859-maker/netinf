# netinf: Diagnostic Influetial cases for Network Analysis

`netinf` provides tools to identify influential cases in psychological
network analysis. Built on 'bootnet' (Epskamp, Borsboom, and Fried,
2018) <doi:10.3758/s13428-017-0862-1>, it reuses the same bootstrap
samples to perform case influence diagnostics, avoiding a separate
bootstrap step.

## Details

- [`bootnet_with_indices()`](https://yangborui859-maker.github.io/netinf/reference/bootnet_with_indices.md)
  extracts case indices from bootstrap samples.

- [`calculate_empirical_influence()`](https://yangborui859-maker.github.io/netinf/reference/calculate_empirical_influence.md)
  computes one influence value per case via regression on inclusion
  counts.

- [`leave_one_out_analysis()`](https://yangborui859-maker.github.io/netinf/reference/leave_one_out_analysis.md)
  validates candidate cases by removing them from the data and
  re-estimating the network, one or several at a time.

- [`calculate_centrality_gCD()`](https://yangborui859-maker.github.io/netinf/reference/calculate_centrality_gCD.md)
  quantifies centrality changes via a generalized Cook's distance.

All four steps can be run manually, or through a single all-in-one
function,
[`influence_diagnostic()`](https://yangborui859-maker.github.io/netinf/reference/influence_diagnostic.md).

## Getting started

The recommended workflow:

1.  Bootstrap the network with
    [`bootnet_with_indices()`](https://yangborui859-maker.github.io/netinf/reference/bootnet_with_indices.md)

2.  Compute empirical influence with
    [`calculate_empirical_influence()`](https://yangborui859-maker.github.io/netinf/reference/calculate_empirical_influence.md)

3.  Validate candidates with
    [`leave_one_out_analysis()`](https://yangborui859-maker.github.io/netinf/reference/leave_one_out_analysis.md)

4.  Diagnose centrality changes with
    [`calculate_centrality_gCD()`](https://yangborui859-maker.github.io/netinf/reference/calculate_centrality_gCD.md)

Or run all four steps in one call with
[`influence_diagnostic()`](https://yangborui859-maker.github.io/netinf/reference/influence_diagnostic.md).

See
[`vignette("netinf-quickstart", package = "netinf")`](https://yangborui859-maker.github.io/netinf/articles/netinf-quickstart.md)
for a quick start, or
[`vignette("netinf-detailedguide", package = "netinf")`](https://yangborui859-maker.github.io/netinf/articles/netinf-detailedguide.md)for
a full walkthrough.

## Visualization

- [`influence_plot()`](https://yangborui859-maker.github.io/netinf/reference/influence_plot.md)
  — plot of empirical influence values

- [`loo_plot()`](https://yangborui859-maker.github.io/netinf/reference/plot.looAnalysis.md)
  — plot of leave one out analysis

- [`gcds_plot()`](https://yangborui859-maker.github.io/netinf/reference/gcds_plot.md)
  — gCD vs centrality change

- [`gcds_md_plot()`](https://yangborui859-maker.github.io/netinf/reference/gcds_md_plot.md)
  — bubble plot with Mahalanobis distance

## Related packages

- [bootnet](https://cran.r-project.org/package=bootnet) for network
  estimation and bootstrap.

- [qgraph](https://cran.r-project.org/package=qgraph) for network
  visualization and centrality.

## See also

Useful links:

- <https://yangborui859-maker.github.io/netinf/>

- <https://github.com/yangborui859-maker/netinf>

- Report bugs at <https://github.com/yangborui859-maker/netinf/issues>

## Author

Borui Yang and Shu Fai Cheung

## Examples

``` r
# The package ships with a simulated dataset that contains
# 99 regular cases and 1 influential case (the 100th row).
data("test_data", package = "netinf")

# All-in-one workflow
if (FALSE) { # \dontrun{
set.seed(1)
diag <- influence_diagnostic(
  data               = test_data,
  nBoots             = 50,
  top_n              = 3,
  centrality_metrics = c("Strength", "Closeness", "Betweenness"),
  default            = "EBICglasso",
  verbose            = FALSE
)
diag
} # }
# Manual workflow
set.seed(1)
boot_res <- bootnet_with_indices(
  test_data,
  nBoots  = 50,  # Use nBoots = 5000 or more in practice; 50 here for speed.
  default = "EBICglasso"
)
#> Note: bootnet will store only the following statistics:  edge, strength, outStrength, inStrength
#> Estimating sample network...
#> Estimating Network. Using package::function:
#>   - qgraph::EBICglasso for EBIC model selection
#>     - using glasso::glasso
#> Bootstrapping...
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   2%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |======                                                                |   8%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  12%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  16%  |                                                                              |=============                                                         |  18%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  22%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  26%  |                                                                              |====================                                                  |  28%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  32%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  36%  |                                                                              |===========================                                           |  38%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  42%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  52%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |=========================================                             |  58%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  62%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  66%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  72%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  76%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  86%  |                                                                              |==============================================================        |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  92%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  96%  |                                                                              |===================================================================== |  98%  |                                                                              |======================================================================| 100%
#> Computing statistics...
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   2%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   6%  |                                                                              |======                                                                |   8%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  12%  |                                                                              |==========                                                            |  14%  |                                                                              |===========                                                           |  16%  |                                                                              |=============                                                         |  18%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  22%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  26%  |                                                                              |====================                                                  |  28%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  32%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  36%  |                                                                              |===========================                                           |  38%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  42%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  46%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  52%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  56%  |                                                                              |=========================================                             |  58%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  62%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  66%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  72%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  76%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  86%  |                                                                              |==============================================================        |  88%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  92%  |                                                                              |==================================================================    |  94%  |                                                                              |===================================================================   |  96%  |                                                                              |===================================================================== |  98%  |                                                                              |======================================================================| 100%
boot_res
#> === bootnet Results ===
#> Number of nodes: 9 
#> Number of non-zero edges in sample: 23 / 36 
#> Mean weight of sample: 0.09643173 
#> Number of bootstrapped networks: 50 
#> Results of original sample stored in x$sample 
#> Table of all statistics from original sample stored in x$sampleTable 
#> Results of bootstraps stored in x$boots 
#> Table of all statistics from bootstraps stored in x$bootTable 
#>  
#> Use plot(x$sample) to plot estimated network of original sample 
#> Use summary(x) to inspect summarized statistics (see ?summary.bootnet for details) 
#> Use plot(x) to plot summarized statistics (see ?plot.bootnet for details) 
#> 
#> Relevant references:
#> 
#>      Friedman, J. H., Hastie, T., & Tibshirani, R. (2008). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9 (3), 432-441.
#>  Foygel, R., & Drton, M. (2010). Extended Bayesian information criteria for Gaussian graphical models. 
#>  Friedman, J. H., Hastie, T., & Tibshirani, R. (2014). glasso: Graphical lasso estimation of gaussian graphical models. Retrieved from https://CRAN.R-project.org/package=glasso
#>  Epskamp, S., Cramer, A., Waldorp, L., Schmittmann, V. D., & Borsboom, D. (2012). qgraph: Network visualizations of relationships in psychometric data. Journal of Statistical Software, 48 (1), 1-18.
#>  Epskamp, S., Borsboom, D., & Fried, E. I. (2018). Estimating psychological networks and their accuracy: a tutorial paper. Multivariate Behavioral Research, 50(1), 195-212.

empic <- calculate_empirical_influence(boot_res)
empic
#> 
#> === Empirical Influence ===
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> Summary Statistics of Influence Values:
#>    Min Median   Mean    Max     SD 
#>    Inf     NA    NaN   -Inf     NA 
#> 
#> * Use `print(x, n = 5)` to view top cases, or `print(x, detailed = TRUE)` to view all.

loo_res <- leave_one_out_analysis(
  empic, test_data,
  top_n   = 3,
  default = "EBICglasso",
  verbose = FALSE
)
loo_res
#> Leave-One-Out Analysis Results
#> ==============================
#> Selection method: top_3
#> Number of cases analyzed: 3
#> 
#> Summary of Structural Changes:
#>  Total_Edges_Changed Number_of_Cases
#>                    0               3
#> 
#> * Use `print(x, n = 5)` to view the top 5 influential cases and their detailed edge changes.
#> * Use `print(x, detailed = TRUE)` to view all cases.

gcd_res <- calculate_centrality_gCD(
  data        = test_data,
  boot_result = boot_res,
  case_ids    = 37,
  metric      = c("Strength", "Closeness", "Betweenness"),
  default     = "EBICglasso"
)
#> Estimating Network. Using package::function:
#>   - qgraph::EBICglasso for EBIC model selection
#>     - using glasso::glasso
gcd_res
#> 
#> === Centrality generalized Cook's Distance (gCD) ===
#> Summary Statistics of gCD by Metric:
#> 
#> Metric: Strength
#>    Min Median   Mean    Max 
#>   0.03   0.03   0.03   0.03 
#> 
#> Metric: Closeness
#>    Min Median   Mean    Max 
#>  0.017  0.017  0.017  0.017 
#> 
#> Metric: Betweenness
#>    Min Median   Mean    Max 
#>  2.396  2.396  2.396  2.396 
#> 
#> * Use `print(x, n = 5)` to view top cases per metric, or `print(x, detailed = TRUE)` to view all.
```
