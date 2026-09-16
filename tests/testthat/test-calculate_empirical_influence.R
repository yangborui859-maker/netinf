#Test of returns(class/lenghth/values)
test_that("a1: returns numeric vector with class empiricalInfluence", {
  empic <- calculate_empirical_influence(shared_boot)

  expect_s3_class(empic, "empiricalInfluence")
  expect_true(is.numeric(empic))
})

test_that("a2: length equals number of cases", {
  empic <- calculate_empirical_influence(shared_boot)

  expect_length(empic, nrow(shared_data))
})

test_that("a3: values are mean-centered", {
  empic <- calculate_empirical_influence(shared_boot)

  expect_equal(mean(empic), 0, tolerance = 1e-10)
})

test_that("a4: all values are finite", {
  empic <- calculate_empirical_influence(shared_boot)

  expect_true(all(is.finite(empic)))
})

#Replication
test_that("b1: same object gives same result on repeated calls", {
  empic1 <- calculate_empirical_influence(shared_boot)
  empic2 <- calculate_empirical_influence(shared_boot)
  
  expect_equal(empic1, empic2)
})

test_that("b2: different nBoots changes nothing in length", {
  empic_lessboot <- calculate_empirical_influence(shared_boot_small)
  empic_moreboot <- calculate_empirical_influence(shared_boot)

  expect_length(empic_lessboot, nrow(shared_data))
  expect_length(empic_moreboot, nrow(shared_data))
})

test_that("b3: influence depends only on bootIndices and boots", {
  empic1 <- calculate_empirical_influence(shared_boot)

  boot_modified <- shared_boot
  boot_modified$sample$graph <- boot_modified$sample$graph*2
  empic2 <- calculate_empirical_influence(boot_modified)

  expect_equal(empic1,empic2)
})

#Test of checking input
test_that("c1: missing bootIndices raises error", {
  boot_missing <- shared_boot
  boot_missing$bootIndices <- NULL

  expect_error(calculate_empirical_influence(boot_missing))
})

test_that("c2: empty bootIndices list raises error", {
  boot_missing2 <- shared_boot
  boot_missing2$bootIndices <- list()

  expect_error(suppressWarnings(calculate_empirical_influence(boot_missing2)))
})

##Comparison with manual calculation
test_that("d1: matches manual implementation from bootIndices and boots", {
  boot <- shared_boot

  nCases <- boot$sampleSize
  nBoots <- length(boot$bootIndices)

  inclusion_matrix <- matrix(0, nrow = nBoots, ncol = nCases)
  for (b in seq_len(nBoots)) {
    inclusion_matrix[b, ] <- tabulate(boot$bootIndices[[b]], nbins = nCases)
  }

  global_boots <- sapply(boot$boots, function(x) {
    g <- x$graph
    sum(abs(g[upper.tri(g)]))
  })

  fins <- which(is.finite(global_boots))
  global_boots <- global_boots[fins]
  inclusion_matrix <- inclusion_matrix[fins, , drop = FALSE]

  X <- inclusion_matrix / nCases
  inc <- 2:nCases
  X <- X[, inc, drop = FALSE]
  beta <- coefficients(glm(global_boots ~ X))[-1L]

  l_manual <- rep(0, nCases)
  l_manual[inc] <- beta
  l_manual <- l_manual - mean(l_manual)

  empic <- calculate_empirical_influence(boot)
  expect_equal(as.numeric(empic), l_manual, tolerance = 1e-10)
})


