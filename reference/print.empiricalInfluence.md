# Print Method for empiricalInfluence Objects

Prints a summary of empirical influence values. By default, it prints
summary statistics. If `n` is specified, it prints the top `n` cases
with the largest absolute influence values. If `detailed = TRUE`, it
prints the full sorted table of all cases.

## Usage

``` r
# S3 method for class 'empiricalInfluence'
print(x, n = NULL, detailed = FALSE, ...)
```

## Arguments

- x:

  An object of class `"empiricalInfluence"`.

- n:

  If setted, only the top "n" cases with absolute values will be
  printed.

- detailed:

  If `TRUE`, prints the full table of all cases. Default is `FALSE`.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns `x`.
