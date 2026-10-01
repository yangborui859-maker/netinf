# Plot Case Influence on Centrality Against gCD

Creates a faceted plot with gCD on the x-axis and centrality change on
the y-axis, similar to semfindr's est_change_gcd_plot.

## Usage

``` r
gcds_plot(
  diag,
  metrics = c("Strength", "Closeness", "Betweenness"),
  node = NULL,
  largest_gcd = 1,
  largest_change = 1,
  point_aes = list(),
  hline_aes = list(),
  case_label_aes = list(),
  wrap_aes = list(),
  ...
)
```

## Arguments

- diag:

  An object of class `"influenceDiagnostic"` returned by
  [`influence_diagnostic()`](https://yangborui859-maker.github.io/netinf/reference/influence_diagnostic.md),
  or a data frame returned by
  [`calculate_centrality_gCD()`](https://yangborui859-maker.github.io/netinf/reference/calculate_centrality_gCD.md)
  (with a `"diff_vectors"` attribute).

- metrics:

  Character vector of centrality measures to plot. Can include
  `"Strength"`, `"Closeness"`, `"Betweenness"`.

- node:

  Optional character, if provided, plot change for this node only.

- largest_gcd:

  Integer, number of cases with largest gCD to label.

- largest_change:

  Integer, number of cases with largest absolute centrality change to
  label per metric.

- point_aes:

  Additional aesthetics passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- hline_aes:

  Additional aesthetics passed to
  [`ggplot2::geom_hline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

- case_label_aes:

  Additional aesthetics passed to
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

- wrap_aes:

  Additional aesthetics passed to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- ...:

  Currently unused.

## Value

A `ggplot` object.
