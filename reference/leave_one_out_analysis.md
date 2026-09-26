# Leave-One-Out Analysis for Influential Cases

Performs leave-one-out (or leave-multiple-out) analysis on candidate
influential cases identified by empirical influence values.

## Usage

``` r
leave_one_out_analysis(
  influence_result,
  data,
  remove_cases = NULL,
  threshold = NULL,
  top_n = NULL,
  direction = c("both", "positive", "negative"),
  digits = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- influence_result:

  Numeric vector of empirical influence values.

- data:

  Original data frame.

- remove_cases:

  Optional numeric vector of case IDs to remove manually.

- threshold:

  Optional threshold for selecting cases based on absolute influence
  values.

- top_n:

  Number of top influential cases to select when `remove_cases` and
  `threshold` are not specified.

- direction:

  Character, direction of selection: `"both"`, `"positive"`, or
  `"negative"`.

- digits:

  Optional integer. If supplied, controls the number of decimal places
  used when formatting edge changes. Default `NULL` uses full precision.

- verbose:

  Logical, whether to print progress messages.

- ...:

  Additional arguments passed to
  [`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html).
  The `default` argument is required, e.g. `default = "EBICglasso"`.

## Value

If `remove_cases` has length \> 1, an object of class
`"looMultiAnalysis"`. Otherwise, an object of class `"looAnalysis"`.

## Details

The function validates all inputs before running the analysis:

- `influence_result` must be a numeric vector whose length equals
  `nrow(data)`.

- `remove_cases` must be a vector of unique integers in `1:nrow(data)`.

- `threshold` must be a single non-negative numeric value.

- `top_n` must be a single positive integer; if it exceeds the number of
  cases, a warning is issued and all cases are used.

When multiple selection arguments are supplied, the priority is
`remove_cases` \> `threshold` \> `top_n`. A warning is issued when a
lower-priority argument is ignored.
