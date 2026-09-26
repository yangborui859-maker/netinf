# Test of return
test_that("a1: returns influenceDiagnostic object", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 1,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_s3_class(res, "influenceDiagnostic")
})

test_that("a2: has all expected fields", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 1,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_named(res, c(
    "empirical_influence", "loo_validation",
    "centrality_gCD", "centrality_diff_vectors",
    "boot_result", "data",
    "top_n", "threshold", "direction", "remove_cases",
    "centrality_metrics",
    "nBoots"
  ))
})

test_that("a3: empirical_influence contains valid vector", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 2,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  inf <- res$empirical_influence$influence
  expect_s3_class(inf, "empiricalInfluence")
  expect_length(inf, nrow(shared_data_small))
  expect_false(anyNA(inf))
})

test_that("a4: candidate_ids match loo_validation case_ids", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 2,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_equal(res$empirical_influence$candidate_ids,
               res$loo_validation$case_ids)
})

test_that("a5: nBoots reflects actual bootstrap count", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 1,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_equal(res$nBoots, length(shared_boot_small$bootIndices))
})

# Test of input data
test_that("b1: accepts a bootnetWithIndices object", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 1,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_s3_class(res$boot_result, "bootnetWithIndices")
  expect_equal(res$data, shared_boot_small$sample$data)
})

test_that("b2: accepts a data frame and runs bootstrapping", {
  skip_on_cran()

  res <- influence_diagnostic(
    data = shared_data_small,
    nBoots = 50,
    top_n = 1,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_s3_class(res, "influenceDiagnostic")
  expect_s3_class(res$boot_result, "bootnetWithIndices")
  expect_equal(res$nBoots, 50)
})

test_that("b3: accepts a matrix", {
  skip_on_cran()

  mat <- as.matrix(shared_data_small)
  res <- influence_diagnostic(
    data = mat,
    nBoots = 50,
    top_n = 1,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_s3_class(res, "influenceDiagnostic")
})

test_that("b4: invalid data type raises an error", {
  expect_error(
    influence_diagnostic(data = 1:10, verbose = FALSE),
    "must be a data frame, matrix, or bootnetWithIndices"
  )

  expect_error(
    influence_diagnostic(data = "abc", verbose = FALSE),
    "must be a data frame, matrix, or bootnetWithIndices"
  )

  expect_error(
    influence_diagnostic(data = NULL, verbose = FALSE),
    "must be a data frame, matrix, or bootnetWithIndices"
  )
})

# Warning of default
test_that("c1: unsupported default raises an error", {
  expect_error(
    influence_diagnostic(
      data = shared_boot_small,
      default = "pcor",
      verbose = FALSE
    ),
    "only support 'EBICglasso'"
  )
})

test_that("c2: vector default raises an error", {
  expect_error(
    influence_diagnostic(
      data = shared_boot_small,
      default = c("EBICglasso", "pcor"),
      verbose = FALSE
    ),
    "only support 'EBICglasso'"
  )
})

test_that("c3: NA default raises an error", {
  expect_error(
    influence_diagnostic(
      data = shared_boot_small,
      default = NA_character_,
      verbose = FALSE
    ),
    "only support 'EBICglasso'"
  )
})

# Warning of nBoots
test_that("d1: nBoots is ignored with a warning for bootnetWithIndices input", {
  expect_warning(
    influence_diagnostic(
      data = shared_boot_small,
      nBoots = 5000,
      top_n = 1,
      centrality_metrics = "Strength",
      verbose = FALSE
    ),
    "nBoots.*ignored"
  )
})

test_that("d2: no warning when nBoots is not supplied", {
  expect_no_warning(
    influence_diagnostic(
      data = shared_boot_small,
      top_n = 1,
      centrality_metrics = "Strength",
      verbose = FALSE
    )
  )
})

# nCores
test_that("e1: nCores = 0 raises an error", {
  expect_error(
    influence_diagnostic(
      data = shared_boot_small,
      nCores = 0,
      verbose = FALSE
    ),
    "must be a single positive integer"
  )
})

test_that("e2: non-integer nCores raises an error", {
  expect_error(
    influence_diagnostic(
      data = shared_boot_small,
      nCores = 1.5,
      verbose = FALSE
    ),
    "must be a single positive integer"
  )
})

test_that("e3: non-numeric nCores raises an error", {
  expect_error(
    influence_diagnostic(
      data = shared_boot_small,
      nCores = "abc",
      verbose = FALSE
    ),
    "must be a single positive integer"
  )
})

# Parameter transmission
test_that("g1: top_n is passed through", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 2,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_equal(res$top_n, 2)
  expect_length(res$loo_validation$case_ids, 2)
})

test_that("g2: remove_cases is passed through", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    remove_cases = c(1, 2),
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_equal(res$remove_cases, c(1, 2))
  expect_equal(sort(res$loo_validation$case_ids), c(1, 2))
})

test_that("g3: direction is stored", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 1,
    direction = "positive",
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  expect_equal(res$direction, "positive")
})

test_that("g4: centrality_metrics stored correctly", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 1,
    centrality_metrics = c("Strength", "Closeness"),
    verbose = FALSE
  )

  expect_equal(res$centrality_metrics,
               c("Strength", "Closeness"))
})

# Comparison with manual computation
test_that("h1: empirical_influence matches direct call", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 1,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  manual <- calculate_empirical_influence(shared_boot_small)
  expect_equal(as.numeric(res$empirical_influence$influence),
               as.numeric(manual))
})

test_that("h2: loo_validation matches direct call", {
  res <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 1,
    centrality_metrics = "Strength",
    verbose = FALSE
  )

  manual <- leave_one_out_analysis(
    influence_result = calculate_empirical_influence(shared_boot_small),
    data = shared_boot_small$sample$data,
    top_n = 1,
    default = "EBICglasso",
    verbose = FALSE
  )

  expect_equal(res$loo_validation$case_ids, manual$case_ids)
  expect_equal(res$loo_validation$summary, manual$summary)
})

# Test of 'verbose'
test_that("i1: verbose = FALSE does not error", {
  expect_no_error(
    influence_diagnostic(
      data = shared_boot_small,
      top_n = 1,
      centrality_metrics = "Strength",
      verbose = FALSE
    )
  )
})

test_that("i2: verbose = TRUE emits phase messages", {
  expect_message(
    influence_diagnostic(
      data = shared_boot_small,
      top_n = 1,
      centrality_metrics = "Strength",
      verbose = TRUE
    ),
    "Phase 1"
  )
})

