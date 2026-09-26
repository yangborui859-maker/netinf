# Test of 'remove_cases'
test_that("a1: single case returns looAnalysis with expected fields", {
  res <- leave_one_out_analysis(
    shared_empic, shared_data,
    remove_cases = 11,
    default = "EBICglasso"
  )

  expect_s3_class(res, "looAnalysis")
  expect_named(res, c("results", "summary", "case_ids", "n_cases",
                      "selection_method", "direction"),
               ignore.order = TRUE)
})

test_that("a2: multiple cases return looMultiAnalysis", {
  res <- leave_one_out_analysis(
    shared_empic, shared_data,
    remove_cases = c(11, 22, 33),
    default = "EBICglasso"
  )

  expect_s3_class(res, "looMultiAnalysis")
})

# b1: influence_result must be numeric
test_that("b1: non-numeric influence_result raises an error", {
  expect_error(
    leave_one_out_analysis(
      as.character(shared_empic), shared_data,
      remove_cases = 1,
      default = "EBICglasso"
    ),
    "must be a numeric vector"
  )
})

# b2: influence_result length must match data 
test_that("b2: wrong-length influence_result raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic[-1], shared_data,
      remove_cases = 1,
      default = "EBICglasso"
    ),
    "must equal the number of rows"
  )
})

# b3: remove_cases out of range
test_that("b3: out-of-range remove_cases raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      remove_cases = nrow(shared_data) + 1,
      default = "EBICglasso"
    ),
    "must be between 1 and"
  )
})

# b4: remove_cases is negative
test_that("b4: negative remove_cases raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      remove_cases = -1,
      default = "EBICglasso"
    ),
    "must be between 1 and"
  )
})

# b5: remove_cases is non-integer
test_that("b5: non-integer remove_cases raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      remove_cases = 1.5,
      default = "EBICglasso"
    ),
    "must be integers"
  )
})

# b6: remove_cases is non-numeric
test_that("b6: non-numeric remove_cases raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      remove_cases = "a",
      default = "EBICglasso"
    ),
    "must be a numeric vector"
  )
})

# b7: remove_cases duplicated 
test_that("b7: duplicated remove_cases raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      remove_cases = c(1, 1, 2),
      default = "EBICglasso"
    ),
    "can not contain duplicated"
  )
})

# b8: threshold is non-numeric 
test_that("b8: non-numeric threshold raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      threshold = "abc",
      default = "EBICglasso"
    ),
    "must be a single numeric"
  )
})

# b9: threshold is a vector
test_that("b9: vector threshold raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      threshold = c(0.1, 0.2),
      default = "EBICglasso"
    ),
    "must be a single numeric"
  )
})

# b10: threshold is negative 
test_that("b10: negative threshold raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      threshold = -0.5,
      default = "EBICglasso"
    ),
    "can not be negative"
  )
})

# b11: top_n = 0
test_that("b11: top_n = 0 raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      top_n = 0,
      default = "EBICglasso"
    ),
    "positive integer"
  )
})

# b12: top_n is negative
test_that("b12: negative top_n raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      top_n = -3,
      default = "EBICglasso"
    ),
    "positive integer"
  )
})

# b13: top_n is non-integer 
test_that("b13: non-integer top_n raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      top_n = 2.5,
      default = "EBICglasso"
    ),
    "positive integer"
  )
})

# b14: top_n exceeds n
test_that("b14: top_n exceeding n warns and selects all cases", {
  expect_warning(
    res <- leave_one_out_analysis(
      shared_empic, shared_data,
      top_n = nrow(shared_data) + 100,
      default = "EBICglasso"
    ),
    "exceeds the number of cases"
  )
  expect_length(res$case_ids, nrow(shared_data))
})

# b15: no topn/removecase/threshold, run all cases
test_that("b15: no selection argument raises an error", {
    res <- leave_one_out_analysis(
      shared_empic, shared_data,
      default = "EBICglasso"
    )
  expect_equal(res$n_cases, nrow(shared_data))
  expect_equal(sort(res$case_ids), seq_len(nrow(shared_data)))
})

#Test of priority(remove_cases/threshold/top_n)
test_that("c1: remove_cases + threshold triggers a warning", {
  expect_warning(
    leave_one_out_analysis(
      shared_empic, shared_data,
      remove_cases = 1, threshold = 0.01,
      default = "EBICglasso"
    ),
    "remove_cases is specified"
  )
})

test_that("c2: threshold + top_n triggers a warning", {
  expect_warning(
    leave_one_out_analysis(
      shared_empic, shared_data,
      threshold = 0.01, top_n = 3,
      default = "EBICglasso"
    ),
    "threshold is specified"
  )
})

#Compare the 'selection' methods with manual computation
test_that("d1: top_n selects the highest absolute influences", {
  res <- leave_one_out_analysis(
    shared_empic, shared_data,
    top_n = 3, default = "EBICglasso"
  )

  expected_ids <- order(abs(shared_empic),
                        decreasing = TRUE)[1:3]
  expect_equal(sort(res$case_ids), sort(expected_ids))
})

test_that("d2: direction = 'positive' only selects positive cases", {
  res <- leave_one_out_analysis(
    shared_empic, shared_data,
    top_n = 3, direction = "positive",
    default = "EBICglasso"
  )

  expect_true(all(shared_empic[res$case_ids] > 0))
})

test_that("d3: direction = 'negative' only selects negative cases", {
  res <- leave_one_out_analysis(
    shared_empic, shared_data,
    top_n = 3, direction = "negative",
    default = "EBICglasso"
  )

  expect_true(all(shared_empic[res$case_ids] < 0))
})

test_that("d4: threshold selects cases above the cutoff", {
  thr <- quantile(abs(shared_empic), 0.9)
  res <- leave_one_out_analysis(
    shared_empic, shared_data,
    threshold = thr, default = "EBICglasso"
  )

  expect_true(all(abs(shared_empic[res$case_ids]) > thr))
})

test_that("d5: invalid direction raises an error", {
  expect_error(
    leave_one_out_analysis(
      shared_empic, shared_data,
      top_n = 1, direction = "invalid",
      default = "EBICglasso"
    ),
    "should be one of"
  )
})

#Compare LOO with manual computation
test_that("e1: single-case result matches manual computation", {
  res <- leave_one_out_analysis(
    shared_empic, shared_data,
    remove_cases = 1, default = "EBICglasso"
  )

  full <- bootnet::estimateNetwork(shared_data,
                                   default = "EBICglasso",
                                   verbose = FALSE)
  without <- bootnet::estimateNetwork(shared_data[-1, ],
                                      default = "EBICglasso",
                                      verbose = FALSE)
  g_full <- full$graph
  g_without <- without$graph
  global_full <- sum(abs(g_full[upper.tri(g_full)]))
  global_without <- sum(abs(g_without[upper.tri(g_without)]))

  r <- res$results[[1]]
  expect_equal(r$global_full, global_full, tolerance = 1e-10)
  expect_equal(r$global_without, global_without, tolerance = 1e-10)
  expect_equal(r$global_weight_strength_change,
               global_full - global_without, tolerance = 1e-10)
})

test_that("e2: multiple-case result matches manual computation", {
  ids <- c(1, 2, 3)
  res <- leave_one_out_analysis(
    shared_empic, shared_data,
    remove_cases = ids, default = "EBICglasso"
  )

  full <- bootnet::estimateNetwork(shared_data,
                                   default = "EBICglasso",
                                   verbose = FALSE)
  without <- bootnet::estimateNetwork(shared_data[-ids, ],
                                      default = "EBICglasso",
                                      verbose = FALSE)
  g_full <- full$graph
  g_without <- without$graph
  global_full <- sum(abs(g_full[upper.tri(g_full)]))
  global_without <- sum(abs(g_without[upper.tri(g_without)]))

  expect_equal(res$global_full, global_full, tolerance = 1e-10)
  expect_equal(res$global_without, global_without, tolerance = 1e-10)
})

#Replication
test_that("f1: same call gives identical result", {
  res1 <- leave_one_out_analysis(
    shared_empic, shared_data,
    remove_cases = 1, default = "EBICglasso"
  )
  res2 <- leave_one_out_analysis(
    shared_empic, shared_data,
    remove_cases = 1, default = "EBICglasso"
  )

  expect_equal(res1$summary, res2$summary)
})


