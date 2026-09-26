# Influence Diagnostic for Network Models

All-in-one function that performs empirical influence screening,
leave-one-out validation, and centrality gCD computation.

## Usage

``` r
influence_diagnostic(
  data,
  nBoots = 1000,
  remove_cases = NULL,
  top_n = 1,
  threshold = NULL,
  direction = c("both", "positive", "negative"),
  default = "EBICglasso",
  verbose = TRUE,
  centrality_metrics = c("Strength", "Closeness", "Betweenness"),
  run_centrality_gCD = TRUE,
  nCores = 1,
  ...
)
```

## Arguments

- data:

  An object of class `"bootnetWithIndices"`.

- nBoots:

  Number of bootstrap samples. Default is 1000.

- remove_cases:

  Optional numeric vector of case IDs to remove manually.

- top_n:

  Number of top influential cases to select when `remove_cases` and
  `threshold` are not specified.

- threshold:

  Optional threshold for selecting cases based on absolute influence
  values.

- direction:

  Character, direction of selection: `"both"`, `"positive"`, or
  `"negative"`.

- default:

  Network estimation method. Default is `"EBICglasso"`.

- verbose:

  Logical, whether to print progress messages.

- centrality_metrics:

  Character vector of centrality measures to compute in Phase 3. Default
  is `c("Strength", "Closeness", "Betweenness")`.

- run_centrality_gCD:

  Logical, whether to run Phase 3 centrality gCD computation. Default is
  `TRUE`.

- ...:

  Additional arguments passed to
  [`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html).

## Value

An object of class `"influenceDiagnostic"` containing:

- `empirical_influence`: list with influence values and candidate IDs.

- `loo_validation`: output from
  [`leave_one_out_analysis()`](leave_one_out_analysis.md).

- `centrality_gCD`: data frame of centrality gCD results (if computed).

- `boot_result`: original bootstrap result object.

- `data`: original data used.
