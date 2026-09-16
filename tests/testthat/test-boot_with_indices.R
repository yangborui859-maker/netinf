#Test of data format
test_that("a1: data.frame input works without error", {
  data <- make_test_data()
  expect_no_error(
    res <- bootnet_with_indices(data, nBoots = 5, default = "EBICglasso")
  )
  expect_length(res$bootIndices, 5)
})

test_that("a2: matrix input is works without error", {
  data <- make_test_data()
  data_mat <- as.matrix(data)
  expect_no_error(
    res <- bootnet_with_indices(data_mat, nBoots = 5, default = "EBICglasso")
  )
  expect_length(res$bootIndices, 5)
})

test_that("a3: tibble input is converted and works without error", {
  data <- make_test_data()
  data_tbl <- tibble::as_tibble(data)
  expect_no_error(
    res <- bootnet_with_indices(data_tbl, nBoots = 5, default = "EBICglasso")
  )
  expect_length(res$bootIndices, 5)
})

test_that("a4: non data.frame / matrix input errors", {
  expect_error(
    bootnet_with_indices(1:10, nBoots = 3, default = "EBICglasso"),
    "'data' must be a data frame or matrix"
  )
  expect_error(
    bootnet_with_indices(list(a = 1, b = 2), nBoots = 3,
                         default = "EBICglasso"),
    "'data' must be a data frame or matrix"
  )
  expect_error(
    bootnet_with_indices(NULL, nBoots = 3, default = "EBICglasso"),
    "'data' must be a data frame or matrix"
  )
})

#Test of row name
test_that("b1: default 1:n row names works silent", {
  data <- make_test_data()
  n <- nrow(data)
  expect_no_error(
   res <- bootnet_with_indices(data, nBoots = 3, default = "EBICglasso")
  )
  expect_equal(rownames(res$sample$data), as.character(seq_len(n)))
})

test_that("b2: character row names trigger warning and are reset", {
  data <- make_test_data()
  n <- nrow(data)
  rownames(data) <- paste0("p", seq_len(nrow(data)))
  expect_warning(
   res <- bootnet_with_indices(data, nBoots = 3, default = "EBICglasso"),
    "Row names are not the default 1:n.*They have been reset to 1:n for bootstrap index extraction."
  )
  expect_equal(rownames(res$sample$data), as.character(seq_len(n)))
})

test_that("b3: duplicated row names trigger warning and are reset", {
  data <- make_test_data()
  n <- nrow(data)
  attr(data, "row.names") <- c(rep("a", 50), rep("b", 50))
  expect_warning(
   res <- bootnet_with_indices(data, nBoots = 3, default = "EBICglasso"),
    "Row names are not the default 1:n.*They have been reset to 1:n for bootstrap index extraction."
  )
  expect_equal(rownames(res$sample$data), as.character(seq_len(n)))
})

test_that("b4: reversed numeric row names trigger warning and are reset", {
  data <- make_test_data()
  n <- nrow(data)
  rownames(data) <- as.character(nrow(data):1)
  expect_warning(
   res <- bootnet_with_indices(data, nBoots = 3, default = "EBICglasso"),
    "Row names are not the default 1:n.*They have been reset to 1:n for bootstrap index extraction."
  )
  expect_equal(rownames(res$sample$data), as.character(seq_len(n)))
})

#Comparison with bootnet
test_that("c1: boots, sample, sampleTable, bootTable match bootnet", {
  data <- make_test_data()
  set.seed(1)
  res1 <- bootnet::bootnet(data, nBoots = 3, default = "EBICglasso")
  
  data <- make_test_data()
  set.seed(1)
  res2 <- bootnet_with_indices(data, nBoots = 3, default = "EBICglasso")

  expect_equal(res1$sample$graph, res2$sample$graph)
  expect_equal(res1$sampleTable,  res2$sampleTable)
  expect_equal(res1$bootTable,    res2$bootTable)
  for (b in seq_len(3)) {
    expect_equal(res1$boots[[b]]$graph, res2$boots[[b]]$graph)
  }
})

test_that("c2: class is bootnetWithIndices and inherits bootnet", {
  data <- make_test_data()
  res <- bootnet_with_indices(data, nBoots = 3, default = "EBICglasso")

  expect_s3_class(res, "bootnetWithIndices")
  expect_s3_class(res, "bootnet")
  expect_true(inherits(res, "bootnet"))
})

#Comparison with manual extraction
test_that("d1: bootIndices match indices extracted from stored data", {
  data <- make_test_data()
  res <- bootnet_with_indices(data, nBoots = 3, keep_data = TRUE,
                              default = "EBICglasso")

  manual_indices <- lapply(res$boots, function(b) {
    as.numeric(gsub("\\.\\d+$", "", rownames(b$data)))
  })

  expect_equal(res$bootIndices, manual_indices)
})

#Test of removing/retaining bootsrap data
test_that("e1: keep_data = FALSE removes data", {
  data <- make_test_data()
  res <- bootnet_with_indices(data, nBoots = 3, keep_data = FALSE,
                              default = "EBICglasso")
  for (b in res$boots) {
    expect_null(b[['data']])
  }
})

test_that("e2: keep_data = TRUE keeps data with correct dimensions", {
  data <- make_test_data()
  res <- bootnet_with_indices(data, nBoots = 3, keep_data = TRUE,
                              default = "EBICglasso")
  for (b in res$boots) {
    expect_s3_class(b$data, "data.frame")
    expect_equal(nrow(b$data), nrow(data))
    expect_equal(ncol(b$data), ncol(data))
  }
})

#Test of bootIndices
test_that("f1: bootIndices has correct structure", {
  data <- make_test_data()
  n <- nrow(data)
  res <- bootnet_with_indices(data, nBoots = 3, default = "EBICglasso")

  expect_length(res$bootIndices, 3)
  for (idx in res$bootIndices) {
    expect_true(is.numeric(idx))
    expect_length(idx, n)
    expect_true(all(idx >= 1 & idx <= n))
  }
})

test_that("g1: bootIndices order matches bootstrap data row order", {
  data <- make_test_data()
  res <- bootnet_with_indices(data, nBoots = 3, keep_data = TRUE,
                              default = "EBICglasso")

  for (b in seq_len(3)) {
    idx <- res$bootIndices[[b]]
    expect_equal(
      as.numeric(res$boots[[b]]$data[1, ]),
      as.numeric(data[idx[1], ])
    )
  }
})

