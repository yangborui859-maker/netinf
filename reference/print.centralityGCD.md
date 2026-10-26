# Print Method for centralityGCD Objects

Prints a summary of the generalized Cook's distance (gCD) results. By
default, it prints summary statistics. If `n` is specified, it prints
the top `n` cases with the largest gCD for each metric. If
`detailed = TRUE`, it prints the full table.

## Usage

``` r
# S3 method for class 'centralityGCD'
print(x, n = NULL, detailed = FALSE, ...)
```

## Arguments

- x:

  An object of class `"centralityGCD"`.

- n:

  If specified, prints the top `n` cases per metric.

- detailed:

  If `TRUE`, prints the full sorted table of all cases.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns `x`.
