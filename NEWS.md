# netinf 0.1.2
* Added a new S3 `plot()` method (`plot.looAnalysis` / `loo_plot()`) for visualizing structural changes (disappeared, appeared, reversed edges) before and after case removal.
* Modified `leave_one_out_analysis()` to cache full and reduced network matrices.

# netinf 0.1.1

* Added new S3 `print()` methods for `empiricalInfluence`, `looAnalysis`, `looMultiAnalysis`, and `centralityGCD` objects, providing cleaner console output.
* Added the `centralityGCD` class attribute to the output of `calculate_centrality_gCD()` to support custom printing.

# netinf 0.1.0.0

* Initial CRAN submission.
