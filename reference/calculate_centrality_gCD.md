# Calculate Generalized Cook's Distance for Centrality Measures

Computes generalized Cook's distance (gCD) for centrality measures
(Strength, Closeness, Betweenness) based on bootstrap covariance
matrices. For each case, the centrality vector from the full sample is
compared to the centrality vector obtained after removing that case.

## Usage

``` r
calculate_centrality_gCD(
  data,
  boot_result,
  case_ids = NULL,
  metric = c("Strength", "Closeness", "Betweenness"),
  nCores = 1,
  ...
)
```

## Arguments

- data:

  Original data frame used in the bootstrap analysis. The first
  argument, so that it can be passed positionally, matching the
  convention of
  [`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html).

- boot_result:

  An object of class `"bootnetWithIndices"` returned by
  [`bootnet_with_indices()`](https://yangborui859-maker.github.io/Influential-cases-in-Network-analysis/reference/bootnet_with_indices.md).
  Provides the full sample network (`sample$graph`) and the bootstrap
  networks (`boots`) used to estimate the covariance matrix of the
  centrality measures.

- case_ids:

  Numeric vector of case IDs (row indices) to evaluate. If `NULL`
  (default), all cases in `data` are evaluated.

- metric:

  Character vector of centrality measures to compute. Can be one or more
  of `"Strength"`, `"Closeness"`, `"Betweenness"`.

- nCores:

  Integer. Number of CPU cores for parallel estimation of the
  leave-one-out networks. Default is `1` (sequential). Values greater
  than 1 use a SOCK cluster via
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  and
  [`pbapply::pblapply()`](https://peter.solymos.org/pbapply/reference/pbapply.html).

- ...:

  Additional arguments passed to
  [`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html).
  The `default` argument is required, e.g. `default = "EBICglasso"`.

## Value

A data frame with one row per case-metric combination and columns:

- `case_id`: the case identifier

- `metric`: the centrality measure

- `gCD`: the generalized Cook's distance

The list of raw difference vectors (full-sample centrality minus
leave-one-out centrality) is attached as the attribute `"diff_vectors"`.

## Details

For each centrality measure, the covariance matrix of node-level
centrality values is estimated from the bootstrap samples stored in
`boot_result$boots`. The gCD for case \\i\\ is computed as \$\$gCD_i =
(\theta - \theta\_{(-i)})' V^{-1} (\theta - \theta\_{(-i)})\$\$ where
\\\theta\\ is the vector of centrality values in the full sample,
\\\theta\_{(-i)}\\ is the vector after removing case \\i\\, and \\V\\ is
the bootstrap covariance matrix. If \\V\\ is singular, the Moore-Penrose
pseudo-inverse from
[`MASS::ginv()`](https://rdrr.io/pkg/MASS/man/ginv.html) will be used.

## See also

[`bootnet_with_indices()`](https://yangborui859-maker.github.io/Influential-cases-in-Network-analysis/reference/bootnet_with_indices.md),
[`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html)
