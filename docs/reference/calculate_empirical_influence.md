# Calculate Empirical Influence Values

This function estimates the empirical influence of each case on the
global strength of the network using bootstrap inclusion frequencies. It
follows the approach of the `empinf.reg` function in the boot package,
adapted for network analysis.

## Usage

``` r
calculate_empirical_influence(boot_result, ...)
```

## Arguments

- boot_result:

  An object of class `"bootnetWithIndices"` returned by
  [`bootnet_with_indices()`](bootnet_with_indices.md), containing
  bootstrap samples, bootstrap indices, and the original sample network.

- ...:

  Additional arguments (currently unused).

## Value

A numeric vector of length equal to the number of cases in the original
data. Each element represents the centered empirical influence of the
corresponding case on the global strength (sum of absolute edge
weights). Positive values indicate cases that increase global strength
when included more often, while negative values indicate cases that
decrease it.

## Details

The empirical influence is computed by regressing the bootstrap global
strength values on the standardized inclusion frequencies (inclusion
count divided by sample size). To avoid collinearity, the first case is
omitted as the baseline, and the resulting coefficients are
mean-centered.
