# Bootstrap with Data and Indices

This function extends bootnet's nonparametric bootstrap by saving
bootstrap sample indices and data for influence diagnostics.

## Usage

``` r
bootnet_with_data(
  data,
  nBoots = 1000,
  default = c("none", "EBICglasso", "ggmModSelect", "pcor", "IsingFit", "IsingSampler",
    "huge", "adalasso", "mgm", "relimp", "cor", "TMFG", "LoGo", "SVAR_lavaan",
    "ncvRegularize", "nodeRegresIC"),
  type = c("nonparametric", "parametric", "node", "person", "jackknife", "case"),
  nCores = 1,
  statistics = c("edge", "strength", "outStrength", "inStrength"),
  model = c("detect", "GGM", "Ising", "graphicalVAR"),
  fun,
  verbose = TRUE,
  labels,
  alpha = 1,
  caseMin = 0.05,
  caseMax = 0.75,
  caseN = 10,
  subNodes,
  subCases,
  computeCentrality = TRUE,
  propBoot = 1,
  replacement = TRUE,
  graph,
  sampleSize,
  intercepts,
  weighted,
  signed,
  directed,
  includeDiagonal = FALSE,
  communities,
  useCommunities,
  bridgeArgs = list(),
  library = .libPaths(),
  memorysaver = TRUE,
  ...,
  responses = c(0L, 1L),
  maxErrors = 10
)
```

## Arguments

- data:

  A data frame or bootnetResult object.

- nBoots:

  Number of bootstrap samples.

- default:

  Network estimation method.

- type:

  Character, currently only "nonparametric" is supported.

- statistics:

  Character vector of statistics to compute.

- ...:

  Additional arguments passed to estimateNetwork.

## Value

An object of class `"bootnetWithData"` which inherits from `"bootnet"`
and additionally contains:

- `bootData`: list of bootstrap sample data frames.

- `bootIndices`: list of bootstrap sample indices.
