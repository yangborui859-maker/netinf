# Simulated data
make_test_data <- function(n = 100, seed = 1) {
  data("test_data", package = "netinf", envir = environment())
  full <- test_data

  set.seed(seed)
  idx <- sample(nrow(full), n)
  out <- full[idx, ]
  rownames(out) <- NULL
  out
}

# Simulate graph with 3 nodes
make_g3 <- function(w12 = 0, w13 = 0, w23 = 0) {
  m <- matrix(c(  0, w12, w13,
                w12,   0, w23,
                w13, w23,   0), nrow = 3, byrow = TRUE)
  dimnames(m) <- list(c("x1", "x2", "x3"), c("x1", "x2", "x3"))
  m
}
