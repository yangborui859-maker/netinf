# Print Method for influenceDiagnostic Objects

Prints a summary of the influence diagnostic results, including
empirical influence values, leave-one-out validation, and centrality gCD
results if available.

## Usage

``` r
# S3 method for class 'influenceDiagnostic'
print(x, n = NULL, detailed = FALSE, ...)
```

## Arguments

- x:

  An object of class `"influenceDiagnostic"`.

- n:

  If specified, prints the top `n` cases. Default is `NULL`.

- detailed:

  If `TRUE`, prints full details. Default is `FALSE`.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns `x`.
