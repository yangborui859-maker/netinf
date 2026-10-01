# Simulated Network Data with an Influential Case

A dataset with 100 cases and 9 variables. The first 99 cases are sampled
from the `HolzingerSwineford1939` dataset (Rosseel, 2012) in the lavaan
package; the 100th case is an **influential case**—its first nine
variables are shifted by \\+2\\ standard deviations from the mean of the
first 99 cases. Used in examples, tests, and vignettes of `netinf`.

## Usage

``` r
test_data
```

## Format

A data frame with 100 rows and 9 variables:

- x1, x2, x3, x4, x5, x6, x7, x8, x9:

  Numeric (1–7 Likert scale). The 100th row is the influential case.

## Source

The first 99 cases are a random sample from
[`lavaan::HolzingerSwineford1939`](https://rdrr.io/pkg/lavaan/man/HolzingerSwineford1939.html).
The 100th case is constructed manually as `mean + 2 * sd` on all 9
variables.

## Examples

``` r
data("test_data", package = "netinf")
tail(test_data, 1)   # the influential case
#>           x1       x2       x3       x4       x5       x6       x7       x8
#> 100 7.488946 8.378845 4.457459 5.427352 7.042681 4.390328 6.635857 7.488547
#>           x9
#> 100 7.197295
```
