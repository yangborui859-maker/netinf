# data-raw/test_data.R
#
# Generates `test_data` — 99 regular cases from `psychTools::bfi` plus
# 1 influential case. Used in examples, tests, and vignettes of netinf.

library(psychTools)
data(bfi)

set.seed(1)

bfi_sub <- na.omit(bfi[, 1:10])
colnames(bfi_sub) <- paste0("x", 1:10)

idx <- sample(nrow(bfi_sub), 99)
data_99 <- bfi_sub[idx, ]
rownames(data_99) <- NULL

means_99 <- colMeans(data_99)
sds_99   <- apply(data_99, 2, sd)

case_100 <- means_99
high_vars <- paste0("x", 1:9)
case_100[high_vars] <- means_99[high_vars] + 2 * sds_99[high_vars]

test_data <- rbind(data_99, case_100)
rownames(test_data) <- seq_len(nrow(test_data))

usethis::use_data(test_data, overwrite = TRUE)
