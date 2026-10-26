# Print Method for looAnalysis Objects

Prints a summary of leave-one-out analysis results, including global
strength changes and structural changes for each case.

## Usage

``` r
# S3 method for class 'looAnalysis'
print(x, n = NULL, detailed = FALSE, ...)
```

## Arguments

- x:

  An object of class `"looAnalysis"`or `"looMultiAnalysis"`.

- n:

  The number of top n cases with the most structural changes (Edges) to
  display for single-case LOO. Default is `NULL`.

- detailed:

  If `TRUE`, prints detailed edge weight changes. Default is `FALSE`.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns `x`.
