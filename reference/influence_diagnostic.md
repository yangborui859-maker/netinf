# Case Influence Diagnostic for Network Models

All-in-one function that performs empirical influence screening,
leave-one-out validation, and centrality gCD computation.

## Usage

``` r
influence_diagnostic(
  data,
  nBoots = 1000,
  remove_cases = NULL,
  threshold = NULL,
  top_n = NULL,
  direction = c("both", "positive", "negative"),
  default = "EBICglasso",
  verbose = TRUE,
  centrality_metrics = c("Strength", "Closeness", "Betweenness"),
  nCores = 1,
  ...
)
```

## Arguments

- data:

  A data frame, matrix, or `bootnetWithIndices` object. If a data
  frame/matrix is provided, bootstrapping is performed internally via
  [`bootnet_with_indices()`](https://yangborui859-maker.github.io/netinf/reference/bootnet_with_indices.md).
  If a `bootnetWithIndices` object is provided, it is used directly and
  `nBoots` is ignored.

- nBoots:

  Number of bootstrap samples. Default is 1000.

- remove_cases:

  Optional numeric vector of case IDs to remove manually for
  [`leave_one_out_analysis()`](https://yangborui859-maker.github.io/netinf/reference/leave_one_out_analysis.md).

- threshold:

  Optional threshold for selecting cases based on absolute influence
  values for
  [`leave_one_out_analysis()`](https://yangborui859-maker.github.io/netinf/reference/leave_one_out_analysis.md).

- top_n:

  Number of top influential cases to select for
  [`leave_one_out_analysis()`](https://yangborui859-maker.github.io/netinf/reference/leave_one_out_analysis.md)
  when `remove_cases` and `threshold` are not specified.

- direction:

  Character. Direction of case selection when `threshold` or `top_n` is
  used: `"both"` (default) selects by absolute influence magnitude;
  `"positive"` selects only cases with positive influence; `"negative"`
  selects only cases with negative influence. Ignored when
  `remove_cases` is supplied.

- default:

  Network estimation method. Default is `"EBICglasso"`.

- verbose:

  Logical, whether to print progress messages.

- centrality_metrics:

  Character vector of centrality measures to compute in Phase 3. Default
  is `c("Strength", "Closeness", "Betweenness")`.

- nCores:

  Integer, number of CPU cores for parallel computation in Phase 3.
  Default is 1.

- ...:

  Additional arguments passed to
  [`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html).

## Value

An object of class `"influenceDiagnostic"` containing:

- `empirical_influence`: list with influence values and candidate IDs.

- `loo_validation`: output from
  [`leave_one_out_analysis()`](https://yangborui859-maker.github.io/netinf/reference/leave_one_out_analysis.md).

- `centrality_gCD`: data frame of centrality gCD results (if computed).

- `boot_result`: original bootstrap result object.

- `data`: original data used.

## Examples
