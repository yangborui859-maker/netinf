labels3 <- c("x1", "x2", "x3")
g_empty <- matrix(0, 3, 3, dimnames = list(labels3, labels3))

#Test of return
test_that("a1: returns a list with all expected fields", {
  g <- make_g3(w12 = 0.5)
  res <- diagnose_structure_change(g, g, labels3)

  expect_type(res, "list")
  expect_named(res, c("disappeared", "appeared", "reversed",
                      "n_disappeared", "n_appeared", "n_reversed"))
})

test_that("a2: character fields are character vectors", {
  g <- make_g3(w12 = 0.5)
  res <- diagnose_structure_change(g, g, labels3)

  expect_true(is.character(res$disappeared))
  expect_true(is.character(res$appeared))
  expect_true(is.character(res$reversed))
})

test_that("a3: counts match lengths of character fields", {
  g_full    <- make_g3(w12 = 0.5, w13 = 0.3, w23 = 0.2)
  g_without <- make_g3(w12 = 0.5, w23 = 0.2)

  res <- diagnose_structure_change(g_full, g_without, labels3)

  expect_equal(res$n_disappeared, length(res$disappeared))
  expect_equal(res$n_appeared,    length(res$appeared))
  expect_equal(res$n_reversed,    length(res$reversed))
})

#No changes
test_that("b1: identical graphs give all-zero counts", {
  g <- make_g3(w12 = 0.5, w13 = 0.3, w23 = 0.2)

  res <- diagnose_structure_change(g, g, labels3)

  expect_equal(res$n_disappeared, 0)
  expect_equal(res$n_appeared,    0)
  expect_equal(res$n_reversed,    0)
  expect_length(res$disappeared, 0)
  expect_length(res$appeared,    0)
  expect_length(res$reversed,    0)
})

test_that("b2: two identical empty graphs give all-zero counts", {
  res <- diagnose_structure_change(g_empty, g_empty, labels3)

  expect_equal(res$n_disappeared, 0)
  expect_equal(res$n_appeared,    0)
  expect_equal(res$n_reversed,    0)
})

#Test of 'edge disappear/appear/reverse'
test_that("c1: single edge disappears", {
  g_full    <- make_g3(w12 = 0.5, w13 = 0.5, w23 = 0.2)
  g_without <- make_g3(w12 = 0.5, w23 = 0.2)

  res <- diagnose_structure_change(g_full, g_without, labels3)

  expect_equal(res$n_disappeared, 1)
  expect_equal(res$n_appeared,    0)
  expect_equal(res$n_reversed,    0)
  expect_match(res$disappeared, "x1 -- x3")
  expect_match(res$disappeared, "0.5 -> 0")
})

test_that("c2: two edges disappear", {
  g_full    <- make_g3(w12 = 0.5, w13 = 0.3, w23 = 0.2)
  g_without <- make_g3(w23 = 0.2)

  res <- diagnose_structure_change(g_full, g_without, labels3)

  expect_equal(res$n_disappeared, 2)
  expect_equal(res$n_appeared,    0)
  expect_equal(res$n_reversed,    0)
})

test_that("c3: single edge appears", {
  g_full    <- make_g3(w12 = 0.5, w23 = 0.2)
  g_without <- make_g3(w12 = 0.5, w13 = 0.5, w23 = 0.2)

  res <- diagnose_structure_change(g_full, g_without, labels3)

  expect_equal(res$n_disappeared, 0)
  expect_equal(res$n_appeared,    1)
  expect_equal(res$n_reversed,    0)
  expect_match(res$appeared, "x1 -- x3")
  expect_match(res$appeared, "0 -> 0.5")
})

test_that("c4: single edge reverses from positive to negative", {
  g_full    <- make_g3(w12 = 0.5, w13 = 0.5, w23 = 0.2)
  g_without <- make_g3(w12 = 0.5, w13 = -0.5, w23 = 0.2)

  res <- diagnose_structure_change(g_full, g_without, labels3)

  expect_equal(res$n_disappeared, 0)
  expect_equal(res$n_appeared,    0)
  expect_equal(res$n_reversed,    1)
  expect_match(res$reversed, "x1 -- x3")
  expect_match(res$reversed, "0.5 -> -0.5")
})

test_that("c5: two edges reverse in opposite directions", {
  g_full    <- make_g3(w12 = 0.5, w13 = -0.3, w23 = 0.2)
  g_without <- make_g3(w12 = -0.5, w13 = 0.3, w23 = 0.2)

  res <- diagnose_structure_change(g_full, g_without, labels3)

  expect_equal(res$n_reversed, 2)
})

test_that("c6: edge going to zero is 'disappeared', not 'reversed'", {
  g_full    <- make_g3(w12 = 0.5, w13 = 0.3)
  g_without <- make_g3(w12 = 0.5)

  res <- diagnose_structure_change(g_full, g_without, labels3)

  expect_equal(res$n_disappeared, 1)
  expect_equal(res$n_reversed,    0)
})

test_that("c7: edge appearing from zero is 'appeared', not 'reversed'", {
  g_full    <- make_g3(w12 = 0.5)
  g_without <- make_g3(w12 = 0.5, w13 = -0.3)

  res <- diagnose_structure_change(g_full, g_without, labels3)

  expect_equal(res$n_appeared, 1)
  expect_equal(res$n_reversed, 0)
})

test_that("c8: disappear + appear + reverse at the same time", {
  labels4 <- paste0("x", 1:4)

  g_full <- matrix(0, 4, 4, dimnames = list(labels4, labels4))
  g_full["x1", "x2"] <- g_full["x2", "x1"] <- 0.5
  g_full["x1", "x3"] <- g_full["x3", "x1"] <- 0.3
  g_full["x3", "x4"] <- g_full["x4", "x3"] <- 0.4

  g_without <- matrix(0, 4, 4, dimnames = list(labels4, labels4))
  g_without["x1", "x2"] <- g_without["x2", "x1"] <- 0.5
  g_without["x1", "x4"] <- g_without["x4", "x1"] <- 0.1
  g_without["x3", "x4"] <- g_without["x4", "x3"] <- -0.4

  res <- diagnose_structure_change(g_full, g_without, labels4)

  expect_equal(res$n_disappeared, 1)   
  expect_equal(res$n_appeared,    1)   
  expect_equal(res$n_reversed,    1)   
})

#Not influenced by lower triangle of graph
test_that("d1: only upper triangle is used", {
  g_full    <- make_g3(w12 = 0.5)
  g_without <- make_g3(w12 = 0.5)
  g_without[3, 1] <- 999

  res <- diagnose_structure_change(g_full, g_without, labels3)

  expect_equal(res$n_disappeared, 0)
  expect_equal(res$n_appeared,    0)
  expect_equal(res$n_reversed,    0)
})

#Test of digits
test_that("e1: digits = NULL shows full precision", {
  g_full <- make_g3(w12 = 0.12345678901234567)

  res <- diagnose_structure_change(g_full, g_empty, labels3, digits = NULL)

  expect_equal(res$n_disappeared, 1)
  expect_match(res$disappeared, "0.123456789012345")
})

test_that("e2: digits = NULL does not pad trailing zeros", {
  g_full <- make_g3(w12 = 0.1)

  res <- diagnose_structure_change(g_full, g_empty, labels3, digits = NULL)

  expect_match(res$disappeared, "0.1 -> 0")
  expect_false(grepl("0.10000000000000000", res$disappeared))
})

test_that("e3: digits = NULL handles small weights without scientific notation", {
  g_full <- make_g3(w12 = 0.0004)

  res <- diagnose_structure_change(g_full, g_empty, labels3, digits = NULL)

  expect_match(res$disappeared, "0.0004")
  expect_false(grepl("e-", res$disappeared))
})

test_that("e4: digits = NULL handles negative weights", {
  g_full    <- make_g3(w12 = 0.5)
  g_without <- make_g3(w12 = -0.5)

  res <- diagnose_structure_change(g_full, g_without, labels3, digits = NULL)

  expect_match(res$reversed, "0.5 -> -0.5")
})

test_that("e5: digits = 3 shows exactly 3 decimals", {
  g_full <- make_g3(w12 = 0.123456789)

  res <- diagnose_structure_change(g_full, g_empty, labels3, digits = 3)

  expect_match(res$disappeared, "0.123 -> 0")
})

test_that("e6: digits = 6 shows exactly 6 decimals", {
  g_full <- make_g3(w12 = 0.123456789)

  res <- diagnose_structure_change(g_full, g_empty, labels3, digits = 6)

  expect_match(res$disappeared, "0.123457 -> 0")
})

test_that("e7: digits does not affect disappearance detection", {
  g_full <- make_g3(w12 = 0.0004)

  res <- diagnose_structure_change(g_full, g_empty, labels3, digits = 3)

  expect_equal(res$n_disappeared, 1)
  expect_match(res$disappeared, "0.000 -> 0")
})

test_that("e8: digits does not affect reversal detection", {
  g_full    <- make_g3(w12 = 0.0004)
  g_without <- make_g3(w12 = -0.0004)

  res <- diagnose_structure_change(g_full, g_without, labels3, digits = 3)

  expect_equal(res$n_reversed, 1)
  expect_match(res$reversed, "0.000 -> -0.000")
})

test_that("e9: digits does not affect appeared detection", {
  g_without <- make_g3(w12 = 0.0004)

  res <- diagnose_structure_change(g_empty, g_without, labels3, digits = 3)

  expect_equal(res$n_appeared, 1)
  expect_match(res$appeared, "0 -> 0.000")
})

#Test of Non-default labels
test_that("f1: custom labels appear in formatted strings", {
  labs <- c("A", "B", "C")
  g_full <- matrix(0, 3, 3, dimnames = list(labs, labs))
  g_full["A", "C"] <- g_full["C", "A"] <- 0.5

  res <- diagnose_structure_change(g_full, g_empty, labs)

  expect_match(res$disappeared, "A -- C")
})

