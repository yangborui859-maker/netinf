# Simulated data
make_test_data <- function(n = 100, seed = 1) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    testthat::skip("lavaan not installed")
  }
  set.seed(seed)
  data <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  idx <- sample(seq_len(nrow(data)), n)
  out <- data[idx, ]
  rownames(out) <- NULL

  as.data.frame(out)
}

# Simulate graph with 3 nodes
make_g3 <- function(w12 = 0, w13 = 0, w23 = 0) {
  m <- matrix(c(  0, w12, w13,
                w12,   0, w23,
                w13, w23,   0), nrow = 3, byrow = TRUE)
  dimnames(m) <- list(c("x1", "x2", "x3"), c("x1", "x2", "x3"))
  m
}
