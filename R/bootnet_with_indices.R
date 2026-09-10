#' Bootstrap with indices extracted from stored data
#'
#' A wrapper around `bootnet::bootnet()` that extracts bootstrap case indices
#' from the row names of the data stored in each bootstrap result. This avoids
#' copying the full `bootnet` source code.
#'
#' @param data A data frame or tibble. Row names will be reset to `1:nrow(data)`
#'   if they are not already `1:n`. If row names are not the default numeric
#'   sequence, a warning is issued.
#' @param nBoots Number of bootstrap samples. Default is 1000.
#' @param default Network estimation method. Default is `"EBICglasso"`.
#' @param keep_data Logical. If `FALSE`, the stored `data` in each bootstrap
#'   result is removed after extracting indices to save memory. Default is
#'   `FALSE`.
#' @param ... Additional arguments passed to [bootnet::bootnet()].
#'
#' @return An object of class `"bootnet"` (and `"bootnetWithIndices"`) with an
#'   additional component `bootIndices`: a list of length `nBoots`, each element
#'   being a numeric vector of original case indices used in that bootstrap
#'   sample.
#'
#' @details
#' This function uses `bootnet::bootnet(..., memorysaver = FALSE)` so that each
#' bootstrap result stores the full bootstrap data. The original case indices
#' are recovered from the row names of those stored data frames. After
#' extraction, the data frames are removed (unless `keep_data = TRUE`) to
#' conserve memory.
#'
#' @export
bootnet_with_indices <- function(data,
                                  nBoots = 1000,
                                  keep_data = FALSE,
                                  ...) {

  if (inherits(data, "tbl_df")) {
    data <- as.data.frame(data)
  }

  original_rownames <- rownames(data)

  if (!is.null(original_rownames)) {
    expected_rownames <- as.character(seq_len(nrow(data)))
    if (!identical(original_rownames, expected_rownames)) {
      warning(
        "Row names are not the default 1:n. ",
        "They have been reset to 1:n for bootstrap index extraction."
      )
    }
  }

  rownames(data) <- NULL


  boot_res <- bootnet::bootnet(
    data,
    nBoots = nBoots,
    memorysaver = FALSE,          
    ...
  )


  boot_indices <- lapply(boot_res$boots, function(boot_i) {
    rn <- rownames(boot_i$data)

    if (is.null(rn)) {
      stop(
        "Bootstrap data has no row names. ",
        "This should not happen after resetting input row names."
      )
    }

    idx <- as.numeric(gsub("\\.\\d+$", "", rn))
    idx
  })


  if (!keep_data) {
    boot_res$boots <- lapply(boot_res$boots, function(boot_i) {
      boot_i$data <- NULL
      boot_i
    })
  }

  boot_res$bootIndices <- boot_indices

  class(boot_res) <- c("bootnetWithIndices", class(boot_res))

  return(boot_res)
}