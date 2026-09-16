# print.empiricalInfluence
test_that("print.empiricalInfluence: emits header", {
  expect_output(print(shared_empic_small),
                "Empirical Influence Values")
})

test_that("print.empiricalInfluence: prints case_id and influence columns", {
  expect_output(print(shared_empic_small), "case_id")
  expect_output(print(shared_empic_small), "influence")
})

test_that("print.empiricalInfluence: returns invisibly", {
  expect_invisible(print(shared_empic_small))
})

test_that("print.empiricalInfluence: returns the same object", {
  out <- withVisible(print(shared_empic_small))
  expect_false(out$visible)
  expect_identical(out$value, shared_empic_small)
})

# print.looAnalysis
test_that("print.looAnalysis: emits header", {
  expect_output(print(shared_loo),
                "Leave-One-Out Analysis Results")
})

test_that("print.looAnalysis: prints selection info", {
  expect_output(print(shared_loo), "Selection method:")
  expect_output(print(shared_loo), "Direction:")
  expect_output(print(shared_loo), "Number of cases analyzed:")
})

test_that("print.looAnalysis: prints Summary block", {
  expect_output(print(shared_loo), "Summary:")
  expect_output(print(shared_loo), "case_id")
})

test_that("print.looAnalysis: prints structure changes for each case", {
  expect_output(print(shared_loo), "Structure Changes")
  expect_output(print(shared_loo), "Edges disappeared")
  expect_output(print(shared_loo), "Edges appeared")
  expect_output(print(shared_loo), "Edge sign reversals")
})

test_that("print.looAnalysis: returns invisibly", {
  expect_invisible(print(shared_loo))
})

# print.looMultiAnalysis
test_that("print.looMultiAnalysis: emits header", {
  expect_output(print(shared_loo_multi),
                "Leave-Multiple-Out Analysis Results")
})

test_that("print.looMultiAnalysis: prints removed cases info", {
  expect_output(print(shared_loo_multi), "Removed cases:")
  expect_output(print(shared_loo_multi), "Number of removed cases:")
})

test_that("print.looMultiAnalysis: prints Summary block", {
  expect_output(print(shared_loo_multi), "Summary:")
  expect_output(print(shared_loo_multi), "case_ids")
})

test_that("print.looMultiAnalysis: prints structure changes", {
  expect_output(print(shared_loo_multi), "Structure Changes")
  expect_output(print(shared_loo_multi), "Without Cases")
})

test_that("print.looMultiAnalysis: returns invisibly", {
  expect_invisible(print(shared_loo_multi))
})

# print_structure_changes
test_that("print_structure_changes: single case_id prints 'Without Case'", {
  sc <- shared_loo$results[[1]]$structure_changes

  out <- capture.output(influenceNet:::print_structure_changes(sc, 1))

  expect_true(any(grepl("Without Case 1", out)))
  expect_true(any(grepl("Structure Changes", out)))
})

test_that("print_structure_changes: multiple case_ids prints 'Without Cases'", {
  sc <- shared_loo_multi$structure_changes

  out <- capture.output(
    influenceNet:::print_structure_changes(sc, c(1, 2, 3))
  )

  expect_true(any(grepl("Without Cases 1, 2, 3", out)))
})

test_that("print_structure_changes: NULL case_ids prints generic header", {
  sc <- shared_loo$results[[1]]$structure_changes

  out <- capture.output(influenceNet:::print_structure_changes(sc))

  expect_true(any(grepl("=== Structure Changes ===", out, fixed = TRUE)))
})

test_that("print_structure_changes: always prints three sections", {
  sc <- shared_loo$results[[1]]$structure_changes

  out <- capture.output(influenceNet:::print_structure_changes(sc, 1))

  expect_true(any(grepl("Edges disappeared:", out, fixed = TRUE)))
  expect_true(any(grepl("Edges appeared:", out, fixed = TRUE)))
  expect_true(any(grepl("Edge sign reversals:", out, fixed = TRUE)))
})

test_that("print_structure_changes: empty changes show 'None' three times", {
  sc <- list(
    disappeared   = character(0),
    appeared      = character(0),
    reversed      = character(0),
    n_disappeared = 0,
    n_appeared    = 0,
    n_reversed    = 0
  )

  out <- capture.output(influenceNet:::print_structure_changes(sc, 1))

  n_none <- sum(grepl("None", out))
  expect_equal(n_none, 3)
})

test_that("print_structure_changes: non-empty changes list each edge", {
  sc <- list(
    disappeared   = c("  x1 -- x2 : 0.3 -> 0",
                      "  x1 -- x3 : 0.2 -> 0"),
    appeared      = c("  x2 -- x3 : 0 -> 0.1"),
    reversed      = c("  x3 -- x4 : 0.4 -> -0.4"),
    n_disappeared = 2,
    n_appeared    = 1,
    n_reversed    = 1
  )

  out <- capture.output(influenceNet:::print_structure_changes(sc, 1))

  expect_true(any(grepl("x1 -- x2", out)))
  expect_true(any(grepl("x1 -- x3", out)))
  expect_true(any(grepl("x2 -- x3", out)))
  expect_true(any(grepl("x3 -- x4", out)))

  expect_false(any(grepl("None", out)))
})

# print.influenceDiagnostic
test_that("print.influenceDiagnostic: emits main header", {
  expect_output(print(shared_diag),
                "Influence Diagnostic Results")
})

test_that("print.influenceDiagnostic: prints Phase 1 block", {
  expect_output(print(shared_diag), "Phase 1: Empirical Influence Screening")
  expect_output(print(shared_diag), "Number of cases:")
  expect_output(print(shared_diag), "Selected candidates:")
  expect_output(print(shared_diag), "Selection method:")
})

test_that("print.influenceDiagnostic: prints influence table", {
  expect_output(print(shared_diag),
                "All Case Influence Values")
})

test_that("print.influenceDiagnostic: prints Phase 2 block", {
  expect_output(print(shared_diag), "Phase 2: Leave-One-Out Validation")
  expect_output(print(shared_diag), "Leave-One-Out Analysis Results")
})

test_that("print.influenceDiagnostic: prints Phase 3 block", {
  expect_output(print(shared_diag), "Phase 3: Centrality gCD")
})

test_that("print.influenceDiagnostic: returns invisibly", {
  expect_invisible(print(shared_diag))
})

test_that("print.influenceDiagnostic: does not print Phase 3 when centrality_gCD is NULL", {
  diag_no_gcd <- shared_diag
  diag_no_gcd$centrality_gCD <- NULL

  out <- capture.output(print(diag_no_gcd))

  expect_false(any(grepl("Phase 3", out)))
})

# S3 dispatch check
test_that("S3 dispatch: print() calls the right method", {
  out <- capture.output(print(shared_empic_small))
  expect_true(any(grepl("Empirical Influence", out)))

  out <- capture.output(print(shared_loo))
  expect_true(any(grepl("Leave-One-Out Analysis", out)))

  out <- capture.output(print(shared_loo_multi))
  expect_true(any(grepl("Leave-Multiple-Out Analysis", out)))

  out <- capture.output(print(shared_diag))
  expect_true(any(grepl("Influence Diagnostic Results", out)))
})