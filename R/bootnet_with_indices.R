#' Bootstrap with Indices Extracted from Stored Data
#'
#' A lightweight wrapper around [bootnet::bootnet()] that extracts bootstrap
#' case indices from the row names of the data stored in each bootstrap result.
#'
#' @param data A data frame or matrix. Row names are reset to `1:nrow`
#'   before bootstrapping.
#' @param keep_data If `FALSE` (default), the stored bootstrap data
#'   frames and the estimation internals (`results`, `.input`,
#'   `estimator`)  are removed from each element of `boots` after the indices have
#'   been extracted. If `TRUE`, all of them are retained.
#' @param ... Additional arguments passed to [bootnet::bootnet()]. Note that
#'   these are not listed explicitly in the function signature; please refer to
#'   [bootnet::bootnet()] for the full list of supported arguments.
#'   **The `default` argument is required, e.g. `default = "EBICglasso"`**.
#' @return An object of class `"bootnetWithIndices"` that inherits from
#'   `"bootnet"`. It contains all components returned by
#'   [bootnet::bootnet()], plus:
#'   \describe{
#'     \item{`bootIndices`}{A list of length equal to the number of bootstrap
#'       samples. Each element is a numeric vector of original case indices
#'       drawn in that bootstrap sample.}
#'   }
#'
#' @details
#' This function relies on [bootnet::bootnet()] being called with
#' `memorysaver = FALSE`, which forces the underlying
#' [bootnet::estimateNetwork()] to store the full bootstrap data in each
#' bootstrap result. The original case indices are then recovered from the row
#' names of those stored data frames, after removing the `.1`, `.2`, ... suffixes
#' that R adds for duplicated row names.
#'
#' @seealso [bootnet::bootnet()], [bootnet::estimateNetwork()]
#'
#' @examples
#' data("test_data", package = "netinf")
#'
#' boot_res <- bootnet_with_indices(
#'   test_data,
#'   nBoots  = 100,       # 100 For demonstration (5000 or more is recommended in practice)
#'   default = "EBICglasso"
#' )
#'
#' boot_res
#'
#' @export
bootnet_with_indices <- function(data,
                                  keep_data = FALSE,
                                  ...) {
  if(!is.data.frame(data)&&!is.matrix(data)){
    stop("'data' must be a data frame or matrix")
  }

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
    memorysaver = FALSE,
    ...
  )

  boot_indices <- lapply(boot_res$boots, function(boot_i) {
    rn <- rownames(boot_i$data)
    idx <- as.numeric(gsub("\\.\\d+$", "", rn))
    idx
  })


  if (!keep_data) {
    for (i in seq_along(boot_res$boots)){
      boot_res$boots[[i]]$data <- NULL
      boot_res$boots[[i]]$results   <- NULL
      boot_res$boots[[i]]$.input    <- NULL
      boot_res$boots[[i]]$estimator <- NULL
    }
  }

  boot_res$bootIndices <- boot_indices

  class(boot_res) <- c("bootnetWithIndices", class(boot_res))

  return(boot_res)
}
