#Test of input
test_that("a1: invalid metric raises an error", {
  expect_error(
    calculate_centrality_gCD(shared_data, shared_boot,
                             case_ids = 1,
                             metric = "InvalidMetric",
                             default = "EBICglasso"),
    "should be one of"
  )
})

test_that("a2: out-of-range case_ids raise an error", {
  expect_error(
    calculate_centrality_gCD(shared_data, shared_boot,
                             case_ids = nrow(shared_data) + 1,
                             metric = "Strength",
                             default = "EBICglasso"),
    "must be between 1 and"
  )
})

test_that("a3: non-integer case_ids raise an error", {
  expect_error(
    calculate_centrality_gCD(shared_data, shared_boot,
                             case_ids = 2.5,
                             metric = "Strength",
                             default = "EBICglasso"),
    "must be integers"
  )
})

test_that("a4: negative case_ids raise an error", {
  expect_error(
    calculate_centrality_gCD(shared_data, shared_boot,
                             case_ids = -1,
                             metric = "Strength",
                             default = "EBICglasso"),
    "must be between 1 and"
  )
})

test_that("a5: non-numeric case_ids raise an error", {
  expect_error(
    calculate_centrality_gCD(shared_data, shared_boot,
                             case_ids = "a",
                             metric = "Strength",
                             default = "EBICglasso"),
    "'case_ids' must be a numeric vector"
  )
})

#Test of returns (two/three cases)
test_that("b1: returns a data frame with expected columns", {
  res <- calculate_centrality_gCD(shared_data, shared_boot,
                                  case_ids = c(1, 2),
                                  metric = "Strength",
                                  default = "EBICglasso")

  expect_s3_class(res, "data.frame")
  expect_named(res, c("case_id", "metric", "gCD"))
})

test_that("b2: case_ids column matches input", {
  res <- calculate_centrality_gCD(shared_data, shared_boot,
                                  case_ids = c(3, 7, 11),
                                  metric = "Strength",
                                  default = "EBICglasso")

  expect_equal(res$case_id, c(3, 7, 11))
})

test_that("b3: metric column matches input (single)", {
  res <- calculate_centrality_gCD(shared_data, shared_boot,
                                  case_ids = c(1, 2),
                                  metric = "Closeness",
                                  default = "EBICglasso")

  expect_true(all(res$metric == "Closeness"))
})

test_that("b4: metric column matches input (multiple)", {
  res <- calculate_centrality_gCD(shared_data, shared_boot,
                                  case_ids = c(1, 2),
                                  metric = c("Strength", "Closeness", "Betweenness"),
                                  default = "EBICglasso")

  expect_equal(sort(unique(res$metric)),
               sort(c("Strength", "Closeness", "Betweenness")))
  expect_equal(nrow(res), 2 * 3)
})

#Test of 'case_ids =NULL'
test_that("c1: case_ids = NULL evaluates all cases", {
  expect_equal(nrow(shared_gcd_all), nrow(shared_data) * 3)
  expect_equal(sort(unique(shared_gcd_all$case_id)),
               seq_len(nrow(shared_data)))
})

test_that("c2: subset matches rows from the full run", {
  ids <- c(2, 5, 9)
  res_sub <- calculate_centrality_gCD(shared_data, shared_boot,
                                      case_ids = ids,
                                      metric = "Strength",
                                      default = "EBICglasso")

  expected <- shared_gcd_all[shared_gcd_all$case_id %in% ids &
                             shared_gcd_all$metric == "Strength", ]

  res_sub_sorted <- res_sub[order(res_sub$case_id), ]
  expected_sorted <- expected[order(expected$case_id), ]

  expect_equal(res_sub_sorted$gCD, expected_sorted$gCD,
               tolerance = 1e-8)
})

#Test of gCD outputs
test_that("d1: gCD values are non-negative", {
  expect_true(all(shared_gcd_all$gCD >= 0))
})

test_that("d2: no NA in results", {
  expect_false(anyNA(shared_gcd_all$gCD))
  expect_false(anyNA(shared_gcd_all$case_id))
  expect_false(anyNA(shared_gcd_all$metric))
})

test_that("d3: gCD matches manual computation", {
  m   <- "Strength"
  ids <- c(1, 2, 3)

  centrality_list <- lapply(shared_boot$boots, function(net) {
    rowSums(abs(net$graph))
  })
  cent_mat <- do.call(rbind, centrality_list)
  #bootstrap covariance matrix
  V_cent   <- stats::cov(cent_mat)
  V_inv    <- solve(V_cent)

  full_graph <- shared_boot$sample$graph
  cent_full  <- rowSums(abs(full_graph))

  expected_gcd <- sapply(ids, function(id) {
    data_without <- shared_data[-id, , drop = FALSE]
    net_without  <- bootnet::estimateNetwork(data_without,
                                             default = "EBICglasso",
                                             verbose = FALSE)
    cent_without <- rowSums(abs(net_without$graph))
    d <- cent_full - cent_without
    as.numeric(t(d) %*% V_inv %*% d)
  })

  res <- calculate_centrality_gCD(shared_data, shared_boot,
                                  case_ids = ids,
                                  metric = "Strength",
                                  default = "EBICglasso")
  expect_equal(res$gCD, expected_gcd, tolerance = 1e-8)
})

#Test of diff_vector'attributes(also used for ploting)
test_that("e1: diff_vectors attribute exists and has correct length", {
  res <- calculate_centrality_gCD(shared_data, shared_boot,
                                  case_ids = 1:3,
                                  metric = c("Strength", "Closeness"),
                                  default = "EBICglasso")

  dv <- attr(res, "diff_vectors")
  expect_true(is.list(dv))
  expect_length(dv, 3 * 2)
})

test_that("e2: diff_vectors entries have length = number of nodes", {
  res <- calculate_centrality_gCD(shared_data, shared_boot,
                                  case_ids = 1:2,
                                  metric = "Strength",
                                  default = "EBICglasso")

  dv <- attr(res, "diff_vectors")
  for (v in dv) {
    expect_length(v, ncol(shared_data))
    expect_true(is.numeric(v))
  }
})

test_that("e2: out-of-range case_ids raise an error", {
  expect_error(
    calculate_centrality_gCD(shared_data, shared_boot,
                             case_ids = nrow(shared_data) + 1,
                             metric = "Strength",
                             default = "EBICglasso"),
    "must be between 1 and"
  )
})

test_that("e3: negative case_ids raise an error", {
  expect_error(
    calculate_centrality_gCD(shared_data, shared_boot,
                             case_ids = -1,
                             metric = "Strength",
                             default = "EBICglasso"),
    "must be between 1 and"
  )
})

test_that("e4: non-integer case_ids raise an error", {
  expect_error(
    calculate_centrality_gCD(shared_data, shared_boot,
                             case_ids = 2.5,
                             metric = "Strength",
                             default = "EBICglasso"),
    "must be integers"
  )
})

test_that("e5: non-numeric case_ids raise an error", {
  expect_error(
    calculate_centrality_gCD(shared_data, shared_boot,
                             case_ids = "a",
                             metric = "Strength",
                             default = "EBICglasso"),
    "must be a numeric vector"
  )
})
#Replication
test_that("f1: same call gives identical result", {
  res1 <- calculate_centrality_gCD(shared_data, shared_boot,
                                   case_ids = 1:3,
                                   metric = "Strength",
                                   default = "EBICglasso")
  res2 <- calculate_centrality_gCD(shared_data, shared_boot,
                                   case_ids = 1:3,
                                   metric = "Strength",
                                   default = "EBICglasso")

  expect_equal(res1$gCD, res2$gCD)
})

#Test the consistency of output between parallel and serial execution.
test_that("g1: parallel and sequential give identical results", {
  res_seq <- calculate_centrality_gCD(shared_data, shared_boot,
                                      case_ids = 1:3,
                                      metric = "Strength",
                                      nCores = 1,
                                      default = "EBICglasso")
  res_par <- calculate_centrality_gCD(shared_data, shared_boot,
                                      case_ids = 1:3,
                                      metric = "Strength",
                                      nCores = 2,
                                      default = "EBICglasso")
  expect_equal(res_seq$gCD, res_par$gCD, tolerance = 1e-8)
})

#Test of singular matrix
test_that("h1: singular covariance triggers warning and pseudo-inverse", {
#make a singular matrix
  boot_singular <- shared_boot
  for (b in seq_along(boot_singular$boots)) {#b:1-n
    g <- boot_singular$boots[[b]]$graph #take the bth netgraph
    g[] <- 0                       #all edge = 0
    g[1, 2] <- g[2, 1] <- 0.5      #set edge between node1 and node2 = 0.5
    boot_singular$boots[[b]]$graph <- g #return
  }

  expect_warning(
    res <- calculate_centrality_gCD(shared_data, boot_singular,
                                    case_ids = 1,
                                    metric = "Strength",
                                    default = "EBICglasso"),
    "singular"
  )
  expect_true(is.finite(res$gCD))
})
