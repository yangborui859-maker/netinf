# Bootstrap with Indices Extracted from Stored Data

A lightweight wrapper around
[`bootnet::bootnet()`](https://rdrr.io/pkg/bootnet/man/bootnet.html)
that extracts bootstrap case indices from the row names of the data
stored in each bootstrap result.

## Usage

``` r
bootnet_with_indices(data, keep_data = FALSE, ...)
```

## Arguments

- data:

  A data frame or matrix. Row names are reset to `1:nrow(data)` before
  bootstrapping.

- keep_data:

  If `FALSE` (default), the stored bootstrap data frames are removed
  from each element of `boots` after the indices have been extracted. If
  `TRUE`, the data frames are retained.

- ...:

  Additional arguments passed to
  [`bootnet::bootnet()`](https://rdrr.io/pkg/bootnet/man/bootnet.html).
  Note that these are not listed explicitly in the function signature;
  please refer to
  [`bootnet::bootnet()`](https://rdrr.io/pkg/bootnet/man/bootnet.html)
  for the full list of supported arguments. The `default` argument is
  required, e.g. `default = "EBICglasso"`.

## Value

An object of class `"bootnetWithIndices"` that inherits from
`"bootnet"`. It contains all components returned by
[`bootnet::bootnet()`](https://rdrr.io/pkg/bootnet/man/bootnet.html),
plus:

- `bootIndices`:

  A list of length equal to the number of bootstrap samples. Each
  element is a numeric vector of original case indices drawn in that
  bootstrap sample.

## Details

This function relies on
[`bootnet::bootnet()`](https://rdrr.io/pkg/bootnet/man/bootnet.html)
being called with `memorysaver = FALSE`, which forces the underlying
[`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html)
to store the full bootstrap data in each bootstrap result. The original
case indices are then recovered from the row names of those stored data
frames, after removing the `.1`, `.2`, ... suffixes that R adds for
duplicated row names.

## See also

[`bootnet::bootnet()`](https://rdrr.io/pkg/bootnet/man/bootnet.html),
[`bootnet::estimateNetwork()`](https://rdrr.io/pkg/bootnet/man/estimateNetwork.html)
