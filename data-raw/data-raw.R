# data-raw/test_data.R
set.seed(123)
dat <- lavaan::HolzingerSwineford1939
datused <- dat[, paste0("x", 1:9)]

#Following Fried's suggestion(2017),N/P > 10
sample_idx <- sample(1:nrow(datused), 99)
data_99 <- datused[sample_idx, ]
means_99 <- colMeans(data_99)
sds_99 <- sapply(data_99, sd)
case_100 <- numeric(9)

#One influential case
names(case_100) <- paste0("x", 1:9)
case_100 <- means_99 + 2 * sds_99
test_data <- rbind(data_99, case_100)
rownames(test_data) <- seq_len(nrow(test_data))

usethis::use_data(test_data, overwrite = TRUE)
