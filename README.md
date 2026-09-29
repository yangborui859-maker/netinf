<!-- badges: start -->
[![Lifecycle: Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Code size](https://img.shields.io/github/languages/code-size/yangborui859-maker/Influential-cases-in-Network-analysis.svg)](https://github.com/yangborui859-maker/Influential-cases-in-Network-analysis)
[![Last Commit at Main](https://img.shields.io/github/last-commit/yangborui859-maker/Influential-cases-in-Network-analysis.svg)](https://github.com/yangborui859-maker/Influential-cases-in-Network-analysis/commits/main)
[![R-CMD-check](https://github.com/yangborui859-maker/Influential-cases-in-Network-analysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yangborui859-maker/Influential-cases-in-Network-analysis/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Version 0.1.0, updated on 2026-09-29)

# netinf
**(Work-in-progress. Not ready for use)**

`netinf` is an R package for identifying influential cases in network analysis. It complements the `bootnet` package by adding a complete workflow for influence diagnostics: extracting case indices from bootstrap samples, computing empirical influence values, validating candidate cases via leave-one-out and leave-multiple-out analysis, and quantifying centrality changes via a generalized Cook's distance.

For more information on this package, please visit its GitHub page:

https://yangborui859-maker.github.io/Influential-cases-in-Network-analysis/

## Getting started

For a detailed walkthrough, see the
[Getting started guide](articles/netinf-guide.html).

## Installation
```r
install.packages("devtools")
devtools::install_github("yangborui859-maker/Influential-cases-in-Network-analysis")
```

# Issues
If you have any suggestions and found any bugs, please feel
feel to open a GitHub issue. Thanks.
