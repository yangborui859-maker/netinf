# Plot gCD Against Mahalanobis Distance and Mean Centrality Change

Creates a bubble plot with Mahalanobis distance on the x-axis, mean
absolute centrality change on the y-axis, and bubble size representing
gCD.

## Usage

``` r
gcds_md_plot(
  diag,
  data,
  metric = c("Strength", "Closeness", "Betweenness"),
  circle_size = 2,
  cutoff_md = FALSE,
  cutoff_md_qchisq = 0.975,
  cutoff_change = NULL,
  cutoff_gcd = NULL,
  largest_gcd = 1,
  largest_md = 1,
  largest_change = 1,
  point_aes = list(),
  hline_aes = list(),
  cutoff_line_md_aes = list(),
  cutoff_line_change_aes = list(),
  case_label_aes = list(),
  ...
)
```

## Arguments

- diag:

  An object of class `"influenceDiagnostic"`, or a data frame with a
  `"diff_vectors"` attribute.

- data:

  Original data frame used in the analysis.

- metric:

  Character, centrality metric to use for bubble size and y-axis. Must
  be one of `"Strength"`, `"Closeness"`, `"Betweenness"`.

- circle_size:

  Numeric, maximum point size.

- cutoff_md:

  Logical or numeric. If TRUE, use a chi-square cutoff.

- cutoff_md_qchisq:

  Numeric, quantile for chi-square cutoff.

- cutoff_change:

  Numeric, optional cutoff for mean absolute change.

- cutoff_gcd:

  Numeric, optional cutoff for gCD.

- largest_gcd:

  Integer, number of cases with largest gCD to label.

- largest_md:

  Integer, number of cases with largest Mahalanobis distance to label.

- largest_change:

  Integer, number of cases with largest mean absolute change to label.

- point_aes:

  Additional aesthetics passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- hline_aes:

  Additional aesthetics passed to
  [`ggplot2::geom_hline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

- cutoff_line_md_aes:

  Additional aesthetics passed to
  [`ggplot2::geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html)
  for MD cutoff.

- cutoff_line_change_aes:

  Additional aesthetics passed to
  [`ggplot2::geom_hline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html)
  for change cutoff.

- case_label_aes:

  Additional aesthetics passed to
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

- ...:

  Currently unused.

## Value

A `ggplot` object.
