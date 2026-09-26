# Plot Empirical Influence Values

Creates a plot of empirical influence values for each case. The plot can
optionally mark cases with the largest absolute influence.

## Usage

``` r
influence_plot(
  object,
  column = NULL,
  plot_title = "Influence Plot",
  x_label = NULL,
  cutoff_x_low = NULL,
  cutoff_x_high = NULL,
  largest_x = NULL,
  absolute = FALSE,
  point_aes = list(),
  vline_aes = list(),
  hline_aes = list(),
  cutoff_line_aes = list(),
  case_label_aes = list(),
  ...
)
```

## Arguments

- object:

  An object of class `"influenceDiagnostic"` or a named numeric vector.

- column:

  If `object` is a matrix or data frame, the column to plot.

- plot_title:

  Title of the plot.

- x_label:

  Label for the y-axis.

- cutoff_x_low:

  Optional lower threshold for highlighting cases.

- cutoff_x_high:

  Optional upper threshold for highlighting cases.

- largest_x:

  Number of cases with largest absolute values to label.

- absolute:

  Logical, whether to plot absolute influence values.

- point_aes:

  Additional arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- vline_aes:

  Additional arguments passed to
  [`ggplot2::geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html).

- hline_aes:

  Additional arguments passed to
  [`ggplot2::geom_hline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

- cutoff_line_aes:

  Additional arguments passed to
  [`ggplot2::geom_hline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html)
  for cutoff lines.

- case_label_aes:

  Additional arguments passed to
  [`ggrepel::geom_label_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html).

- ...:

  Additional arguments (currently unused).

## Value

A `ggplot` object.
