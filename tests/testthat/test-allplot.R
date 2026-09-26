#influence.plot
test_that("influence_plot: returns a ggplot object", {
  p <- influence_plot(named_vec)
  expect_s3_class(p, "ggplot")
})

test_that("influence_plot: accepts empiricalInfluence input", {
  p <- influence_plot(shared_empic_small)
  expect_s3_class(p, "ggplot")
})

test_that("influence_plot: accepts influenceDiagnostic input", {
  p <- influence_plot(shared_diag)
  expect_s3_class(p, "ggplot")
})

test_that("influence_plot: accepts plain numeric vector", {
  p <- suppressMessages(influence_plot(vec_influence))
  expect_s3_class(p, "ggplot")
})

test_that("influence_plot: accepts single-column matrix", {
  p <- suppressMessages(influence_plot(mat_influence))
  expect_s3_class(p, "ggplot")
})

test_that("influence_plot: absolute = TRUE uses abs values", {
  p <- influence_plot(named_vec, absolute = TRUE)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$y, "Absolute")
})

test_that("influence_plot: custom x_label is used", {
  p <- influence_plot(named_vec, x_label = "MyLabel")
  expect_equal(p$labels$y, "MyLabel")
})

test_that("influence_plot: custom plot_title is used", {
  p <- influence_plot(named_vec, plot_title = "Custom Title")
  expect_equal(p$labels$title, "Custom Title")
})

test_that("influence_plot: largest_x adds label layer", {
  p <- influence_plot(named_vec, largest_x = 3)
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true(any(grepl("LabelRepel", layer_classes)))
})

test_that("influence_plot: cutoff_x_low and cutoff_x_high add hline layers", {
  p <- influence_plot(named_vec,
                      cutoff_x_low = -0.05,
                      cutoff_x_high = 0.05)
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  n_hline <- sum(layer_classes == "GeomHline")
  expect_gte(n_hline, 3)
})

test_that("influence_plot: point_aes overrides defaults", {
  p <- influence_plot(named_vec,
                      point_aes = list(color = "red", size = 5))
  expect_s3_class(p, "ggplot")
})

test_that("influence_plot: NULL input raises an error", {
  expect_error(influence_plot(NULL), "Neither a matrix nor a vector")
})

test_that("influence_plot: all-NA input raises an error", {
  expect_error(influence_plot(setNames(c(NA, NA), c("1", "2"))),
               "No cases have valid values")
})

test_that("influence_plot: multi-column matrix with column = NULL raises an error", {
  mat2 <- matrix(1:20, ncol = 2, dimnames = list(NULL, c("a", "b")))
  expect_error(influence_plot(mat2), "must have length 1")
})

test_that("influence_plot: unknown column name raises an error", {
  mat2 <- matrix(1:20, ncol = 2, dimnames = list(NULL, c("a", "b")))
  expect_error(influence_plot(mat2, column = "nonexistent"),
               "not found in 'object'")
})

test_that("influence_plot: unnamed vector emits message", {
  expect_message(
    influence_plot(as.numeric(shared_empic_small)),
    "no row names found"
  )
})

test_that("influence_plot: named vector does not emit message", {
  named <- setNames(as.numeric(shared_empic_small),
                    seq_along(shared_empic_small))
  expect_no_message(influence_plot(named))
})

test_that("influence_plot: matrix without rownames emits message", {
  mat <- matrix(1:20, ncol = 2, dimnames = list(NULL, c("a", "b")))
  expect_message(
    influence_plot(mat, column = "a"),
    "no row names found"
  )
})

test_that("influence_plot: single-column matrix without rownames emits message", {
  mat <- matrix(1:40, ncol = 1)
  expect_message(influence_plot(mat), "no row names found")
})

test_that("influence_plot: valid column name works", {
  mat2 <- matrix(1:20, ncol = 2, dimnames = list(NULL, c("a", "b")))
  p <- suppressMessages(influence_plot(mat2, column = "a"))
  expect_s3_class(p, "ggplot")
})

test_that("influence_plot: matrix with rownames keeps names", {
  mat <- matrix(1:20, ncol = 2,
                dimnames = list(paste0("case", 1:10), c("a", "b")))
  p <- influence_plot(mat, column = "a")
  expect_true(all(grepl("^case", p$data$case_id)))
})

test_that("influence_plot: single-column matrix with rownames keeps names", {
  mat <- matrix(1:40, ncol = 1,
                dimnames = list(paste0("c", 1:40), "x"))
  p <- influence_plot(mat)
  expect_true(all(grepl("^c", p$data$case_id)))
})

#gcds_plot
test_that("gcds_plot: accepts influenceDiagnostic input", {
  p <- gcds_plot(shared_diag, metrics = "Strength")
  expect_s3_class(p, "ggplot")
})

test_that("gcds_plot: accepts data frame with diff_vectors attribute", {
  p <- gcds_plot(shared_gcd_all, metrics = "Strength")
  expect_s3_class(p, "ggplot")
})

test_that("gcds_plot: multiple metrics uses facet_wrap", {
  p <- gcds_plot(shared_diag, metrics = c("Strength", "Closeness"))
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("gcds_plot: single metric still returns ggplot", {
  p <- gcds_plot(shared_diag, metrics = "Strength")
  expect_s3_class(p, "ggplot")
})

test_that("gcds_plot: x axis label is 'Generalized Cook\\'s Distance'", {
  p <- gcds_plot(shared_diag, metrics = "Strength")
  expect_match(p$labels$x, "Generalized Cook")
})

test_that("gcds_plot: y axis label is 'Centrality Change'", {
  p <- gcds_plot(shared_diag, metrics = "Strength")
  expect_equal(p$labels$y, "Centrality Change")
})

test_that("gcds_plot: invalid diag raises an error", {
  expect_error(gcds_plot(1:10),
               "must be an influenceDiagnostic object or a data frame")
})

test_that("gcds_plot: invalid metric raises an error", {
  expect_error(gcds_plot(shared_diag, metrics = "InvalidMetric"),
               "should be one of")
})

test_that("gcds_plot: node argument selects specific node", {
  p <- gcds_plot(shared_diag, metrics = "Strength", node = "x1")
  expect_s3_class(p, "ggplot")
})

test_that("gcds_plot: unknown node raises an error", {
  expect_error(
    gcds_plot(shared_diag, metrics = "Strength", node = "nonexistent_node"),
    "node not found"
  )
})

test_that("gcds_plot: largest_gcd adds label layer", {
  p <- gcds_plot(shared_diag, metrics = "Strength", largest_gcd = 2)
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true(any(grepl("LabelRepel", layer_classes)))
})

#gcds_md_plot
test_that("gcds_md_plot: single metric returns ggplot", {
  p <- gcds_md_plot(shared_diag,
                    data = shared_data_small,
                    metric = "Strength")
  expect_s3_class(p, "ggplot")
})

test_that("gcds_md_plot: multiple metrics returns a named list", {
  p <- gcds_md_plot(shared_diag,
                    data = shared_data_small,
                    metric = c("Strength", "Closeness"))

  expect_type(p, "list")
  expect_length(p, 2)
  expect_named(p, c("Strength", "Closeness"))
  expect_s3_class(p$Strength, "ggplot")
  expect_s3_class(p$Closeness, "ggplot")
})

test_that("gcds_md_plot: accepts data frame with diff_vectors", {
  p <- gcds_md_plot(shared_gcd_all,
                    data = shared_data,
                    metric = "Strength")
  expect_s3_class(p, "ggplot")
})

test_that("gcds_md_plot: title mentions 'Mahalanobis Distance'", {
  p <- gcds_md_plot(shared_diag,
                    data = shared_data_small,
                    metric = "Strength")
  expect_match(p$labels$title, "Mahalanobis Distance")
})

test_that("gcds_md_plot: x axis is 'Mahalanobis Distance'", {
  p <- gcds_md_plot(shared_diag,
                    data = shared_data_small,
                    metric = "Strength")
  expect_equal(p$labels$x, "Mahalanobis Distance")
})

test_that("gcds_md_plot: y axis mentions the metric", {
  p <- gcds_md_plot(shared_diag,
                    data = shared_data_small,
                    metric = "Closeness")
  expect_match(p$labels$y, "Closeness")
})

test_that("gcds_md_plot: cutoff_md = TRUE adds vline", {
  p <- gcds_md_plot(shared_diag,
                    data = shared_data_small,
                    metric = "Strength",
                    cutoff_md = TRUE)
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true(any(layer_classes == "GeomVline"))
})

test_that("gcds_md_plot: cutoff_change adds two hlines", {
  p <- gcds_md_plot(shared_diag,
                    data = shared_data_small,
                    metric = "Strength",
                    cutoff_change = 0.05)
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  n_hline <- sum(layer_classes == "GeomHline")
  expect_gte(n_hline, 3)  
})

test_that("gcds_md_plot: invalid diag raises an error", {
  expect_error(gcds_md_plot(1:10, data = shared_data_small,
                            metric = "Strength"),
               "must be an influenceDiagnostic")
})

test_that("gcds_md_plot: metric not in results raises an error", {
  diag_strength_only <- influence_diagnostic(
    data = shared_boot_small,
    top_n = 1,
    centrality_metrics = "Strength",
    verbose = FALSE
  )
  expect_error(
    gcds_md_plot(diag_strength_only,
                 data = shared_data_small,
                 metric = "Closeness"),
    "not found in gCD results"
  )
})

test_that("gcds_md_plot: invalid metric raises an error", {
  expect_error(
    gcds_md_plot(shared_diag, data = shared_data_small,
                 metric = "InvalidMetric"),
    "should be one of"
  )
})