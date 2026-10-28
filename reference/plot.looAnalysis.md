# Plot Method for looAnalysis and looMultiAnalysis Objects

Visualizes the structural changes in a network before and after removing
an influential case. Topological changes (disappeared, appeared, and
reversed edges) are highlighted with thick blue solid lines, while
unchanged edges are shown in thin black solid lines.

## Usage

``` r
# S3 method for class 'looAnalysis'
plot(x, n = 1, qgraph_args = list(), ...)

loo_plot(x, n = 1, qgraph_args = list(), ...)

# S3 method for class 'looMultiAnalysis'
plot(x, n = 1, qgraph_args = list(), ...)
```

## Arguments

- x:

  An object of class `"looAnalysis"` or `"looMultiAnalysis"`.

- n:

  Integer. Number of top influential cases to plot side-by-side. Default
  is 1.

- qgraph_args:

  Additional arguments passed to `qgraph`.

- ...:

  Additional arguments (currently unused).

## Value

Invisibly returns `NULL`.
