#' Bootstrap with Data and Indices
#'
#' This function extends bootnet's nonparametric bootstrap by
#' saving bootstrap sample indices and data for influence diagnostics.
#'
#' @param data A data frame or bootnetResult object.
#' @param nBoots Number of bootstrap samples.
#' @param default Network estimation method.
#' @param type Character, currently only "nonparametric" is supported.
#' @param statistics Character vector of statistics to compute.
#' @param ... Additional arguments passed to estimateNetwork.
#' @return An object of class `"bootnetWithData"` which inherits from `"bootnet"`
#'   and additionally contains:
#'   - `bootData`: list of bootstrap sample data frames.
#'   - `bootIndices`: list of bootstrap sample indices.
#' @export
bootnet_with_data <- function(data, nBoots = 1000, default = c("none", "EBICglasso",
    "ggmModSelect", "pcor", "IsingFit", "IsingSampler", "huge",
    "adalasso", "mgm", "relimp", "cor", "TMFG", "LoGo", "SVAR_lavaan",
    "ncvRegularize", "nodeRegresIC"), type = c("nonparametric",
    "parametric", "node", "person", "jackknife", "case"), nCores = 1,
    statistics = c("edge", "strength", "outStrength", "inStrength"),
    model = c("detect", "GGM", "Ising", "graphicalVAR"), fun,
    verbose = TRUE, labels, alpha = 1, caseMin = 0.05, caseMax = 0.75,
    caseN = 10, subNodes, subCases, computeCentrality = TRUE,
    propBoot = 1, replacement = TRUE, graph, sampleSize, intercepts,
    weighted, signed, directed, includeDiagonal = FALSE, communities,
    useCommunities, bridgeArgs = list(), library = .libPaths(),
    memorysaver = TRUE, ..., responses = c(0L, 1L), maxErrors = 10)
{
    construct <- "function"
    if (default[[1]] == "glasso")
        default <- "EBICglasso"
    if (!missing(default) && identical(default[[1]], "graphicalVAR") &&
        !is(data, "bootnetResult")) {
        stop("default = 'graphicalVAR' only supported for output of estimateNetwork()")
    }
    default <- match.arg(default)
    if (default != "EBICglasso") {
    warning("Only 'EBICglasso' is currently supported. Other methods may produce invalid results.")
    stop("Unsupported default method.")
  }
    if (any(statistics == "all")) {
        if (missing(communities)) {
            statistics <- c("intercept", "edge", "length", "distance",
                "closeness", "betweenness", "strength", "expectedInfluence",
                "outStrength", "outExpectedInfluence", "inStrength",
                "inExpectedInfluence", "rspbc", "hybrid", "eigenvector")
        }
        else {
            statistics <- c("intercept", "edge", "length", "distance",
                "closeness", "betweenness", "strength", "expectedInfluence",
                "outStrength", "outExpectedInfluence", "inStrength",
                "inExpectedInfluence", "rspbc", "hybrid", "eigenvector",
                "bridgeStrength", "bridgeCloseness", "bridgeBetweenness",
                "bridgeInDegree", "bridgeOutDegree", "bridgeExpectedInfluence")
        }
    }
    else {
        message(paste("Note: bootnet will store only the following statistics: ",
            paste0(statistics, collapse = ", ")))
    }
    type <- match.arg(type)
    if (type != "nonparametric") {stop("'bootnet_with_data' currently only supports type = 'nonparametric'")}
    if (type == "case")
        type <- "person"
    model <- match.arg(model)
    if (identical(list(...)[["missing"]], "stackedMI") && type !=
        "parametric") {
        message("============================================================\n",
            "============================================================\n",
            "Combining multiple imputation with bootstrapping, while potentially providing the most robust estimation, can be computationally very demanding. It is strongly recommended to first test the procedure using a small number of bootstraps ('nBoot') and imputations ('nimp') to ensure that the estimation runs without errors and to get an impression of the expected runtime. Once verified, the full analysis should be executed on a machine that can run uninterrupted for an extended period, potentially even several days.",
            "============================================================\n",
            "============================================================\n")
    }
    usedCorMethod <- list(...)[["corMethod"]]
    if (is.null(usedCorMethod) && !missing(data) && is(data,
        "bootnetResult")) {
        usedCorMethod <- data$arguments$corMethod
    }
    if (identical(usedCorMethod, "cor_mantar") && type == "node") {
        stop("corMethod = 'cor_mantar' is not supported for node-wise bootstrapping. Please use a different correlation method or a different bootstrap type.")
    }
    if (!missing(communities)) {
        bridgeArgs$communities <- communities
    }
    else {
        bridgeArgs$communities <- communities <- NULL
    }
    if (!missing(useCommunities)) {
        bridgeArgs$useCommunities <- useCommunities
    }
    else {
        bridgeArgs$useCommunities <- useCommunities <- "all"
    }
    sampleResult <- NULL
    if (missing(data)) {
        if (type != "parametric") {
            warning("'data' can only be missing if type = 'parametric'. Setting type = 'parametric' and performing parametric bootstrap instead.")
            type <- "parametric"
        }
        if (missing(graph)) {
            stop("'graph' may not be missing in parametric bootstrap when 'data' is missing.")
        }
        if (missing(sampleSize)) {
            stop("'sampleSize' may not be missing in parametric bootstrap when 'data' is missing.")
        }
        N <- ncol(graph)
        Np <- sampleSize
        datatype <- "normal"
        if (missing(intercepts)) {
            intercepts <- rep(0, N)
        }
        if (!missing(data)) {
            warning("'data' is ignored when using manual parametric bootstrap.")
            data <- NULL
        }
        manual <- TRUE
        dots <- list(...)
    }
    else {
        manual <- FALSE
        if (is(data, "bootnetResult")) {
            if (isTRUE(data$thresholded)) {
                stop("Network has already been thresholded using bootstraps.")
            }
            if (isTRUE(data$bootInclude)) {
                stop("Network is based on bootstrap include probabilities.")
            }
            sampleResult <- data
            default <- data$default
            inputCheck <- data$.input
            datatype <- data$datatype
            if (missing(labels)) {
                labels <- data$labels
            }
            fun <- data$estimator
            dots <- data$arguments
            if (missing(weighted)) {
                weighted <- data$weighted
            }
            if (missing(signed)) {
                signed <- data$signed
            }
            if (missing(directed)) {
                directed <- data$directed
            }
            N <- data$nNode
            Np <- data$nPerson
            data <- data$data
        }
        else {
            if (missing(directed)) {
                if (default == "SVAR_lavaan") {
                  directed <- list(contemporaneous = TRUE, temporal = TRUE)
                }
                else if (default != "relimp") {
                  directed <- FALSE
                }
                else {
                  directed <- TRUE
                }
            }
            datatype <- "normal"
            dots <- list(...)
            N <- ncol(data)
            Np <- nrow(data)

            if (missing(fun)) {
                fun <- NULL
            }
            if (!manual) {
                goodColumns <- sapply(data, function(x) is.numeric(x) |
                  is.ordered(x) | is.integer(x))
                if (!all(goodColumns)) {
                  if (verbose) {
                    warning(paste0("Removing non-numeric columns: ",
                      paste(which(!goodColumns), collapse = "; ")))
                  }
                  data <- data[, goodColumns, drop = FALSE]
                }
            }
        }
    }
    if (datatype != "normal")
        {stop("'bootnet_with_data' currently only supports cross-sectional data (datatype = 'normal')")}
    if (datatype == "graphicalVAR" && (!is.list(data) || is.null(data$vars))) {
        stop("graphicalVAR data object lacks 'vars' field.")
    }
    if (missing(subNodes)) {
        if (datatype == "normal") {
            subNodes <- 2:(N - 1)
        }
        else if (datatype == "graphicalVAR") {
            subNodes <- 2:(length(data$vars) - 1)
        }
    }
    if (missing(subCases)) {
        if (datatype == "normal") {
            subCases <- round((1 - seq(caseMin, caseMax, length = caseN)) *
                Np)
        }
        else if (datatype == "graphicalVAR") {
            subCases <- round((1 - seq(caseMin, caseMax, length = caseN)) *
                nrow(data$data_c))
        }
    }
    inputCheck <- checkInput(default = default, fun = fun, .dots = dots)
    if (missing(weighted)) {
        weighted <- TRUE
    }
    if (missing(signed)) {
        signed <- TRUE
    }
    if (missing(directed)) {
        if (!default %in% c("graphicalVAR", "relimp", "DAG"))
            directed <- FALSE
    }
    if (type == "jackknife") {
        message("Jacknife overwrites nBoot to sample size")
        nBoots <- Np
    }
    if (type == "node" & N < 3) {
        stop("Node-wise bootstrapping requires at least three nodes.")
    }
    if (datatype == "normal" && !manual && !(is.data.frame(data) ||
        is.matrix(data))) {
        stop("'data' argument must be a data frame")
    }
    if (!manual && is.matrix(data)) {
        data <- as.data.frame(data)
    }
    if (missing(labels)) {
        if (manual) {
            labels <- colnames(graph)
            if (is.null(labels)) {
                labels <- seq_len(ncol(graph))
            }
        }
        else {
            labels <- colnames(data)
            if (is.null(labels)) {
                labels <- seq_len(ncol(data))
            }
        }
    }
    if (type == "parametric" & model == "detect") {
        if (manual) {
            stop("'model' must be set in parametric bootstrap without 'data'.")
        }
        if (default == "graphicalVAR") {
            model <- "graphicalVAR"
        }
        else if (default != "none") {
            model <- ifelse(grepl("ising", default, ignore.case = TRUE),
                "Ising", "GGM")
        }
        else {
            stop("'none' default set not supported for graphicalVAR data.")
        }
        message(paste0("model set to '", model, "'"))
    }
    if (!manual) {
        if (is.null(sampleResult)) {
            if (verbose) {
                message("Estimating sample network...")
            }
            sampleResult <- bootnet::estimateNetwork(data, default = default,
                fun = inputCheck$estimator, .dots = inputCheck$arguments,
                labels = labels, verbose = verbose, weighted = weighted,
                signed = signed, .input = inputCheck, datatype = datatype,
                directed = directed)
        }
    }
    else {
        sampleResult <- list(graph = graph, intercepts = intercepts,
            labels = labels, nNode = N, nPerson = Np, estimator = inputCheck$estimator,
            arguments = inputCheck$arguments, default = default,
            weighted = weighted, signed = signed)
        class(sampleResult) <- c("bootnetResult", "list")
    }
    if (type == "parametric") {
        if (model == "Ising" && !manual && !is.null(data)) {
            enc <- sort(unique(unlist(data)))
            if (length(enc) == 2 && !identical(as.numeric(enc),
                c(0, 1))) {
                responses <- enc
            }
        }
        if (model == "GGM" && !is.null(sampleResult$default) &&
            sampleResult$default %in% c("cor", "TMFG") && !(sampleResult$default ==
            "TMFG" && identical(sampleResult$arguments$graphType,
            "pcor"))) {
            stop("Parametric bootstrap with model = 'GGM' requires a partial-correlation network; default = '",
                sampleResult$default, "' estimates a correlation network.")
        }
    }
    totalRetries <- 0
    if (nCores == 1) {
        bootResults <- vector("list", nBoots)
        boot_indices_list <- vector("list", nBoots)
        boot_data_list <- vector("list", nBoots)
        if (verbose) {
            message("Bootstrapping...")
            pb <- utils::txtProgressBar(0, nBoots, style = 3)
        }
        for (b in seq_len(nBoots)) {
            tryLimit <- maxErrors
            tryCount <- 0
            repeat {
                if (!type %in% c("node", "person")) {
                  nNode <- N
                  inSample <- seq_len(N)
                  if (type == "jackknife") {
                    if (datatype == "normal") {
                      bootData <- data[-b, , drop = FALSE]
                    }
                    else {
                      bootData <- data
                      bootData$data_c <- bootData$data_c[-b,
                        , drop = FALSE]
                      bootData$data_l <- bootData$data_l[-b,
                        , drop = FALSE]
                    }
                    nPerson <- Np - 1
                  }
                  else if (type == "parametric") {
                    nPerson <- Np
                    if (model == "Ising") {
                      if (identical(as.numeric(responses), c(0,
                        1))) {
                        bootData <- IsingSampler::IsingSampler(round(propBoot *
                          Np), noDiag(sampleResult$graph), sampleResult$intercepts)
                      }
                      else {
                        bootData <- IsingSampler::IsingSampler(round(propBoot *
                          Np), noDiag(sampleResult$graph), sampleResult$intercepts,
                          responses = responses)
                      }
                    }
                    else if (model == "GGM") {
                      g <- -sampleResult$graph
                      diag(g) <- 1
                      bootData <- mvtnorm::rmvnorm(round(propBoot *
                        Np), sigma = corpcor::pseudoinverse(g))
                      colnames(bootData) <- sampleResult$labels
                      inputCheck$arguments$corArgs$auxiliary_vars <- inputCheck$arguments$auxiliary_vars <- NULL
                    }
                    else if (model == "graphicalVAR") {
                      stop("model = 'graphicalVAR' not yet supported")
                    }
                    else stop(paste0("Model '", model, "' not supported."))
                  }
                  else {
                    nPerson <- Np
                    if (datatype == "normal") {
                      boot_idx <- sample(seq_len(Np), round(propBoot * Np), replace = replacement) #added
                      boot_indices_list[[b]] <- boot_idx
                      bootData <- data[boot_idx, , drop = FALSE]
                      boot_data_list[[b]] <- bootData
                    }
                    else {
                      bootData <- data
                      bootSample <- sample(seq_len(Np), round(propBoot *
                        Np), replace = replacement)
                      bootData$data_c <- bootData$data_c[bootSample,
                        ]
                      bootData$data_l <- bootData$data_l[bootSample,
                        ]
                    }
                  }
                }
                else if (type == "node") {
                  nPerson <- Np
                  nNode <- sample(subNodes, 1)
                  inSample <- sort(sample(seq_len(N), nNode))
                  if (datatype == "normal") {
                    bootData <- data[, inSample, drop = FALSE]
                  }
                  else if (datatype == "graphicalVAR") {
                    bootData <- data
                    bootData$data_c <- bootData$data_c[, data$vars[inSample],
                      drop = FALSE]
                    keep <- names(data$data_l) == "1" | sub("_lag[0-9]+$",
                      "", names(data$data_l)) %in% data$vars[inSample]
                    bootData$data_l <- bootData$data_l[, keep,
                      drop = FALSE]
                    bootData$vars <- data$vars[inSample]
                  }
                }
                else {
                  if (length(subCases) == 1) {
                    nPerson <- subCases
                  }
                  else {
                    nPerson <- sample(subCases, 1)
                  }
                  inSample <- 1:N
                  persSample <- sort(sample(seq_len(Np), nPerson))
                  if (datatype == "normal") {
                    bootData <- data[persSample, , drop = FALSE]
                  }
                  else if (datatype == "graphicalVAR") {
                    bootData <- data
                    bootData$data_c <- bootData$data_c[persSample,
                      , drop = FALSE]
                    bootData$data_l <- bootData$data_l[persSample,
                      , drop = FALSE]
                  }
                }
                res <- suppressWarnings(try({
                  bootnet::estimateNetwork(bootData, default = default,
                    fun = inputCheck$estimator, .dots = inputCheck$arguments,
                    labels = labels[inSample], verbose = FALSE,
                    weighted = weighted, signed = signed, .input = inputCheck,
                    memorysaver = memorysaver, directed = directed)
                }, silent = TRUE))
                if (is(res, "try-error")) {
                  if (tryCount == tryLimit) {
                    cond <- attr(res, "condition")
                    stop("Maximum number of retries reached in bootstrap ",
                      b, ". Last estimation error: ", if (is.null(cond))
                        "unknown error"
                      else conditionMessage(cond))
                  }
                  tryCount <- tryCount + 1
                  totalRetries <- totalRetries + 1
                }
                else {
                  break
                }
            }
            bootResults[[b]] <- res
            if (verbose) {
                utils::setTxtProgressBar(pb, b)
            }
        }
        if (verbose) {
            close(pb)
        }
    }
    else {
        if (verbose) {
            message("Bootstrapping...")
        }
        if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) &&
            Sys.info()["sysname"] == "Darwin" && gsub("\\..*",
            "", getRversion()) == "4") {
            snow::setDefaultClusterOptions(setup_strategy = "sequential")
        }
        if (nCores < 2) {
            stop("nCores must be >= 2 to use parallel computation")
        }
        nClust <- nCores
        cl <- snow::makeSOCKcluster(nClust)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        if (missing(graph)) {
            graph <- matrix(0, N, N)
        }
        if (missing(data)) {
            data <- matrix(0, Np, N)
        }
        if (missing(intercepts)) {
            intercepts <- rep(0, N)
        }
        if (missing(sampleSize)) {
            sampleSize <- Np
        }
        excl <- c("prepFun", "prepArgs", "estFun", "estArgs",
            "graphFun", "graphArgs", "intFun", "intArgs", "fun")
        parallel::clusterExport(cl, ls()[!ls() %in% c(excl, "cl")], envir = environment())
        bootResults <- pbapply::pblapply(seq_len(nBoots), function(b) {
            .libPaths(library)
            tryLimit <- maxErrors
            tryCount <- 0
            repeat {
                if (!type %in% c("node", "person")) {
                  nNode <- N
                  inSample <- seq_len(N)
                  if (type == "jackknife") {
                    if (datatype == "normal") {
                      bootData <- data[-b, , drop = FALSE]
                    }
                    else {
                      bootData <- data
                      bootData$data_c <- bootData$data_c[-b,
                        , drop = FALSE]
                      bootData$data_l <- bootData$data_l[-b,
                        , drop = FALSE]
                    }
                    nPerson <- Np - 1
                  }
                  else if (type == "parametric") {
                    nPerson <- Np
                    if (model == "Ising") {
                      if (identical(as.numeric(responses), c(0,
                        1))) {
                        bootData <- IsingSampler::IsingSampler(round(propBoot *
                          Np), noDiag(sampleResult$graph), sampleResult$intercepts)
                      }
                      else {
                        bootData <- IsingSampler::IsingSampler(round(propBoot *
                          Np), noDiag(sampleResult$graph), sampleResult$intercepts,
                          responses = responses)
                      }
                    }
                    else if (model == "GGM") {
                      g <- -sampleResult$graph
                      diag(g) <- 1
                      bootData <- mvtnorm::rmvnorm(round(propBoot *
                        Np), sigma = corpcor::pseudoinverse(g))
                      colnames(bootData) <- sampleResult$labels
                      inputCheck$arguments$corArgs$auxiliary_vars <- inputCheck$arguments$auxiliary_vars <- NULL
                    }
                    else if (model == "graphicalVAR") {
                      stop("model = 'graphicalVAR' not yet supported")
                    }
                    else stop(paste0("Model '", model, "' not supported."))
                  }
                  else {
                    nPerson <- Np
                    if (datatype == "normal") {
                      boot_idx <- sample(seq_len(Np), round(propBoot * Np), replace = replacement)
                      bootData <- data[boot_idx, , drop = FALSE]
                    }
                    else {
                      bootData <- data
                      boot_idx <- sample(seq_len(Np), round(propBoot *
                        Np), replace = replacement) #added
                      bootData$data_c <- bootData$data_c[boot_idx,] #added
                      bootData$data_l <- bootData$data_l[boot_idx,] #added
                    }
                  }
                }
                else if (type == "node") {
                  nPerson <- Np
                  nNode <- sample(subNodes, 1)
                  inSample <- sort(sample(seq_len(N), nNode))
                  if (datatype == "normal") {
                    bootData <- data[, inSample, drop = FALSE]
                  }
                  else if (datatype == "graphicalVAR") {
                    bootData <- data
                    bootData$data_c <- bootData$data_c[, data$vars[inSample],
                      drop = FALSE]
                    keep <- names(data$data_l) == "1" | sub("_lag[0-9]+$",
                      "", names(data$data_l)) %in% data$vars[inSample]
                    bootData$data_l <- bootData$data_l[, keep,
                      drop = FALSE]
                    bootData$vars <- data$vars[inSample]
                  }
                }
                else {
                  if (length(subCases) == 1) {
                    nPerson <- subCases
                  }
                  else {
                    nPerson <- sample(subCases, 1)
                  }
                  inSample <- 1:N
                  persSample <- sort(sample(seq_len(Np), nPerson))
                  if (datatype == "normal") {
                    bootData <- data[persSample, , drop = FALSE]
                  }
                  else if (datatype == "graphicalVAR") {
                    bootData <- data
                    bootData$data_c <- bootData$data_c[persSample,
                      , drop = FALSE]
                    bootData$data_l <- bootData$data_l[persSample,
                      , drop = FALSE]
                  }
                }
                res <- suppressWarnings(try({
                  bootnet::estimateNetwork(bootData, default = default,
                    fun = inputCheck$estimator, .dots = inputCheck$arguments,
                    labels = labels[inSample], verbose = FALSE,
                    weighted = weighted, signed = signed, .input = inputCheck,
                    memorysaver = memorysaver, directed = directed)
                }, silent = TRUE))
                if (is(res, "try-error")) {
                  if (tryCount == tryLimit) {
                    cond <- attr(res, "condition")
                    stop("Maximum number of retries reached in bootstrap ",
                      b, ". Last estimation error: ", if (is.null(cond))
                        "unknown error"
                      else conditionMessage(cond))
                  }
                  tryCount <- tryCount + 1
                }
                else {
                  break
                }
            }
            return(list(res = res, tries = tryCount,
              boot_idx = boot_idx,
              boot_data = bootData))
        }, cl = cl)
        totalRetries <- sum(vapply(bootResults, function(o) o$tries,
            numeric(1)))
        boot_indices_list <- lapply(bootResults, function(o) o$boot_idx)
        boot_data_list <- lapply(bootResults, function(o) o$boot_data)
        bootResults <- lapply(bootResults, "[[", "res")
    }
    if (totalRetries > 0) {
        warning(sprintf("%d bootstrap estimation(s) failed and were resampled.",
            totalRetries))
    }
    if (verbose) {
        message("Computing statistics...")
    }
    statTableOrig <- statTable(sampleResult, name = "sample",
        alpha = alpha, computeCentrality = computeCentrality,
        statistics = statistics, directed = directed, includeDiagonal = includeDiagonal,
        bridgeArgs = bridgeArgs)
    if (nCores == 1) {
        if (verbose) {
            pb <- utils::txtProgressBar(0, nBoots, style = 3)
        }
        statTableBoots <- vector("list", nBoots)
        for (b in seq_len(nBoots)) {
            statTableBoots[[b]] <- statTable(bootResults[[b]],
                name = paste("boot", b), alpha = alpha, computeCentrality = computeCentrality,
                statistics = statistics, directed = directed,
                bridgeArgs = bridgeArgs, includeDiagonal = includeDiagonal)
            if (verbose) {
                 utils::setTxtProgressBar(pb, b)
            }
        }
        if (verbose) {
             close(pb)
        }
    }
    else {
        statTableBoots <- pbapply::pblapply(seq_len(nBoots), function(b) {
            .libPaths(library)
            statTable(bootResults[[b]], name = paste("boot",
                b), alpha = alpha, computeCentrality = computeCentrality,
                statistics = statistics, directed = directed,
                bridgeArgs = bridgeArgs, includeDiagonal = includeDiagonal)
        }, cl = cl)
    }
    Result <- list(sampleTable = dplyr::ungroup(statTableOrig),
                   bootTable = dplyr::ungroup(dplyr::bind_rows(statTableBoots)),
                   sample = sampleResult,
                   boots = bootResults,
                   type = type,
                   sampleSize = Np,
                   bootData = boot_data_list,
                   bootIndices = boot_indices_list,
                   nBoots = nBoots)
             class(Result) <- c("bootnetWithData", "bootnet")
    return(Result)
}
