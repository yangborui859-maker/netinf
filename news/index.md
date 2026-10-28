# Changelog

## netinf 0.1.2

- Added a new S3
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
  (`plot.looAnalysis` /
  [`loo_plot()`](https://yangborui859-maker.github.io/netinf/reference/plot.looAnalysis.md))
  for visualizing structural changes (disappeared, appeared, reversed
  edges) before and after case removal.
- Modified
  [`leave_one_out_analysis()`](https://yangborui859-maker.github.io/netinf/reference/leave_one_out_analysis.md)
  to cache full and reduced network matrices.

## netinf 0.1.1

- Added new S3 [`print()`](https://rdrr.io/r/base/print.html) methods
  for `empiricalInfluence`, `looAnalysis`, `looMultiAnalysis`, and
  `centralityGCD` objects, providing cleaner console output.
- Added the `centralityGCD` class attribute to the output of
  [`calculate_centrality_gCD()`](https://yangborui859-maker.github.io/netinf/reference/calculate_centrality_gCD.md)
  to support custom printing.

## netinf 0.1.0.0

- Initial CRAN submission.
