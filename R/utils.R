noDiag <- function(x) {
    diag(x) <- 0
    return(x)
}

checkInput <- function (default = c("none", "EBICglasso", "ggmModSelect", "pcor",
    "IsingFit", "IsingSampler", "huge", "adalasso", "mgm", "relimp",
    "cor", "TMFG", "ggmModSelect", "LoGo", "graphicalVAR", "piecewiseIsing",
    "SVAR_lavaan", "ncvRegularize", "nodeRegresIC"), fun, verbose = TRUE,
    .dots = list(), ...)
{
    construct <- "function"
    if (default[[1]] == "glasso")
        default <- "EBICglasso"
    if (default[[1]] == "IsingSampler")
        default <- "IsingSampler"
    default <- match.arg(default)
    if (missing(fun)) {
        fun <- NULL
    }
    dots <- c(.dots, list(...))
    argNames <- character(0)
    if (construct == "function") {
        Args <- dots
        if (default == "none") {
            Function <- fun
        }
        else if (default == "EBICglasso") {
            Function <- bootnet::bootnet_EBICglasso
        }
        else if (default == "ggmModSelect") {
            Function <- bootnet::bootnet_ggmModSelect
        }
        else if (default == "IsingFit") {
            Function <- bootnet::bootnet_IsingFit
        }
        else if (default == "IsingSampler") {
            Function <- bootnet::bootnet_IsingSampler
        }
        else if (default == "pcor") {
            Function <- bootnet::bootnet_pcor
        }
        else if (default == "cor") {
            Function <- bootnet::bootnet_cor
        }
        else if (default == "adalasso") {
            Function <- bootnet::bootnet_adalasso
        }
        else if (default == "huge") {
            Function <- bootnet::bootnet_huge
        }
        else if (default == "mgm") {
            Function <- bootnet::bootnet_mgm
        }
        else if (default == "relimp") {
            Function <- bootnet::bootnet_relimp
        }
        else if (default == "TMFG") {
            Function <- bootnet::bootnet_TMFG
        }
        else if (default == "LoGo") {
            Function <- bootnet::bootnet_LoGo
        }
        else if (default == "graphicalVAR") {
            Function <- bootnet::bootnet_graphicalVAR
        }
        else if (default == "piecewiseIsing") {
            Function <- bootnet::bootnet_piecewiseIsing
        }
        else if (default == "SVAR_lavaan") {
            Function <- bootnet::bootnet_SVAR_lavaan
        }
        else if (default == "ncvRegularize") {
            Function <- bootnet::bootnet_ncvRegularize
        }
        else if (default == "nodeRegresIC") {
            Function <- bootnet::bootnet_nodeRegresIC
        }
        else stop("Currently not supported.")
    }
    Output <- list(default = default, estimator = Function, arguments = Args)
    return(Output)
}

statTable <- function (x, name, alpha = 1, computeCentrality = TRUE, statistics = c("edge",
    "strength", "closeness", "betweenness"), directed = FALSE,
    bridgeArgs = list(), includeDiagonal = FALSE, ...)
{
    if (is.list(x$graph)) {
        Tables <- list()
        for (i in seq_len(length(x$graph))) {
            dummyobject <- x
            dummyobject$graph <- x$graph[[i]]
            dummyobject$directed <- x$directed[[i]]
            Tables[[i]] <- statTable(dummyobject, name = name,
                alpha = alpha, computeCentrality = computeCentrality,
                statistics = statistics, directed = dummyobject$directed,
                includeDiagonal = includeDiagonal)
            Tables[[i]]$graph <- names(x$graph)[[i]]
        }
        return(dplyr::bind_rows(Tables))
    }
    bridgeCentralityNames <- x[["labels"]]
    substr(statistics, 0, 1) <- tolower(substr(statistics, 0,
        1))
    validStatistics <- c("intercept", "edge", "length", "distance",
        "closeness", "betweenness", "strength", "expectedInfluence",
        "outStrength", "outExpectedInfluence", "inStrength",
        "inExpectedInfluence", "rspbc", "hybrid", "eigenvector",
        "bridgeStrength", "bridgeCloseness", "bridgeBetweenness",
        "bridgeInDegree", "bridgeOutDegree", "bridgeExpectedInfluence")
    if (!all(statistics %in% validStatistics)) {
        stop(paste0("'statistics' must be one of: ", paste0("'",
            validStatistics, "'", collapse = ", ")))
    }
    type <- NULL
    value <- NULL
    stopifnot(is(x, "bootnetResult"))
    tables <- list()
    if (is.null(x[["labels"]])) {
        x[["labels"]] <- seq_len(ncol(x[["graph"]]))
    }
    if (!directed) {
        index <- upper.tri(x[["graph"]], diag = FALSE)
        ind <- which(index, arr.ind = TRUE)
    }
    else {
        if (!includeDiagonal) {
            index <- diag(ncol(x[["graph"]])) != 1
        }
        else {
            index <- matrix(TRUE, ncol(x[["graph"]]), ncol(x[["graph"]]))
        }
        ind <- which(index, arr.ind = TRUE)
    }
    Wmat <- qgraph::getWmat(x)
    if ("edge" %in% statistics) {
        tables$edges <- tibble::as_tibble(data.frame(name = name,
            type = "edge", node1 = x[["labels"]][ind[, 1]], node2 = x[["labels"]][ind[,
                2]], value = Wmat[index], stringsAsFactors = FALSE))
    }
    if ("length" %in% statistics) {
        tables$length <- tibble::as_tibble(data.frame(name = name,
            type = "length", node1 = x[["labels"]][ind[, 1]],
            node2 = x[["labels"]][ind[, 2]], value = abs(1/abs(Wmat[index])),
            stringsAsFactors = FALSE))
    }
    if (!is.null(x[["intercepts"]])) {
        tables$intercepts <- tibble::as_tibble(data.frame(name = name,
            type = "intercept", node1 = x[["labels"]], node2 = "",
            value = x[["intercepts"]], stringsAsFactors = FALSE))
    }
    if (any(is.na(Wmat)) && computeCentrality) {
        warning("NAs found in weights matrix; skipping centrality computation.")
        computeCentrality <- FALSE
    }
    if (computeCentrality) {
        if (all(x[["graph"]] == 0)) {
            cent <- list(OutDegree = rep(0, ncol(x[["graph"]])),
                InDegree = rep(0, ncol(x[["graph"]])), Closeness = rep(0,
                  ncol(x[["graph"]])), Betweenness = rep(0, ncol(x[["graph"]])),
                ShortestPathLengths = matrix(Inf, ncol(x[["graph"]]),
                  ncol(x[["graph"]])), RSPBC = rep(0, ncol(x[["graph"]])),
                Hybrid = rep(0, ncol(x[["graph"]])), step1 = rep(0,
                  ncol(x[["graph"]])), expectedInfluence = rep(0,
                  ncol(x[["graph"]])), OutExpectedInfluence = rep(0,
                  ncol(x[["graph"]])), InExpectedInfluence = rep(0,
                  ncol(x[["graph"]])), bridgeStrength = rep(0,
                  ncol(x[["graph"]])), bridgeCloseness = rep(0,
                  ncol(x[["graph"]])), bridgeBetweenness = rep(0,
                  ncol(x[["graph"]])), bridgeInDegree = rep(0,
                  ncol(x[["graph"]])), bridgeOutDegree = rep(0,
                  ncol(x[["graph"]])), bridgeExpectedInfluence = rep(0,
                  ncol(x[["graph"]])))
            bridgeCentralityNames <- x[["labels"]]
        }
        else {
            cent <- qgraph::centrality(Wmat, alpha = alpha, all.shortest.paths = FALSE)
            bridgecen <- c("bridgeInDegree", "bridgeOutDegree",
                "bridgeStrength", "bridgeBetweenness", "bridgeCloseness",
                "bridgeExpectedInfluence")
            if (any(bridgecen %in% statistics)) {
                bridgeArgs <- c(list(network = Wmat), bridgeArgs)
                if (is.null(bridgeArgs$communities)) {
                  warning("If bridge statistics are to be bootstrapped, the communities argument should be provided")
                }
                b <- do.call(networktools::bridge, args = bridgeArgs)
                rename <- function(x, from, to) {
                  if (from %in% x) {
                    x[x == from] <- to
                  }
                  x
                }
                names(b) <- rename(names(b), "Bridge Indegree",
                  "bridgeInDegree")
                names(b) <- rename(names(b), "Bridge Outdegree",
                  "bridgeOutDegree")
                names(b) <- rename(names(b), "Bridge Strength",
                  "bridgeStrength")
                names(b) <- rename(names(b), "Bridge Betweenness",
                  "bridgeBetweenness")
                names(b) <- rename(names(b), "Bridge Closeness",
                  "bridgeCloseness")
                names(b) <- rename(names(b), "Bridge Expected Influence (1-step)",
                  "bridgeExpectedInfluence")
                names(b) <- rename(names(b), "Bridge Expected Influence (2-step)",
                  "bridgeExpectedInfluence2step")
                b$communities <- NULL
            }
            else {
                b <- NULL
            }
            if (!is.null(bridgeArgs$useCommunities) && bridgeArgs$useCommunities[1] !=
                "all") {
                b <- lapply(b, function(cen) {
                  cen[bridgeArgs$communities %in% bridgeArgs$useCommunities]
                })
                bridgeCentralityNames <- x[["labels"]][bridgeArgs$communities %in%
                  bridgeArgs$useCommunities]
            }
            else {
                bridgeCentralityNames <- x[["labels"]]
            }
            cent <- c(cent, b)
        }
        if ("strength" %in% statistics & !directed) {
            tables$strength <- tibble::as_tibble(data.frame(name = name,
                type = "strength", node1 = x[["labels"]], node2 = "",
                value = cent[["OutDegree"]], stringsAsFactors = FALSE))
        }
        if ("outStrength" %in% statistics && directed) {
            tables$outStrength <- tibble::as_tibble(data.frame(name = name,
                type = "outStrength", node1 = x[["labels"]],
                node2 = "", value = cent[["OutDegree"]], stringsAsFactors = FALSE))
        }
        if ("inStrength" %in% statistics && directed) {
            tables$inStrength <- tibble::as_tibble(data.frame(name = name,
                type = "inStrength", node1 = x[["labels"]], node2 = "",
                value = cent[["InDegree"]], stringsAsFactors = FALSE))
        }
        if ("closeness" %in% statistics) {
            tables$closeness <- tibble::as_tibble(data.frame(name = name,
                type = "closeness", node1 = x[["labels"]], node2 = "",
                value = cent[["Closeness"]], stringsAsFactors = FALSE))
        }
        if ("betweenness" %in% statistics) {
            tables$betweenness <- tibble::as_tibble(data.frame(name = name,
                type = "betweenness", node1 = x[["labels"]],
                node2 = "", value = cent[["Betweenness"]], stringsAsFactors = FALSE))
        }
        if ("distance" %in% statistics) {
            tables$sp <- tibble::as_tibble(data.frame(name = name,
                type = "distance", node1 = x[["labels"]][ind[,
                  1]], node2 = x[["labels"]][ind[, 2]], value = cent[["ShortestPathLengths"]][index],
                stringsAsFactors = FALSE))
        }
        if ("expectedInfluence" %in% statistics && !directed) {
            tables$expectedInfluence <- tibble::as_tibble(data.frame(name = name,
                type = "expectedInfluence", node1 = x[["labels"]],
                node2 = "", value = cent[["OutExpectedInfluence"]],
                stringsAsFactors = FALSE))
        }
        if ("rspbc" %in% statistics) {
            tryrspbc <- try({
                tables$rspbc <- tibble::as_tibble(data.frame(name = name,
                  type = "rspbc", node1 = x[["labels"]], node2 = "",
                  value = as.vector(NetworkToolbox::rspbc(abs(Wmat))),
                  stringsAsFactors = FALSE))
            })
            if (is(tryrspbc, "try-error")) {
                tables$rspbc <- tibble::as_tibble(data.frame(name = name,
                  type = "rspbc", node1 = x[["labels"]], node2 = "",
                  value = NA, stringsAsFactors = FALSE))
            }
        }
        if ("hybrid" %in% statistics) {
            tryhybrid <- try({
                tables$hybrid <- tibble::as_tibble(data.frame(name = name,
                  type = "hybrid", node1 = x[["labels"]], node2 = "",
                  value = as.vector(NetworkToolbox::hybrid(abs(Wmat),
                    BC = "random")), stringsAsFactors = FALSE))
            })
            if (is(tryhybrid, "try-error")) {
                tables$hybrid <- tibble::as_tibble(data.frame(name = name,
                  type = "hybrid", node1 = x[["labels"]], node2 = "",
                  value = NA, stringsAsFactors = FALSE))
            }
        }
        if ("eigenvector" %in% statistics) {
            tryeigenvector <- try({
                tables$eigenvector <- tibble::as_tibble(data.frame(name = name,
                  type = "eigenvector", node1 = x[["labels"]],
                  node2 = "", value = as.vector(NetworkToolbox::eigenvector(Wmat)),
                  stringsAsFactors = FALSE))
            })
            if (is(tryeigenvector, "try-error")) {
                tables$eigenvector <- tibble::as_tibble(data.frame(name = name,
                  type = "eigenvector", node1 = x[["labels"]],
                  node2 = "", value = NA, stringsAsFactors = FALSE))
            }
        }
        if ("outExpectedInfluence" %in% statistics && directed) {
            tables$outExpectedInfluence <- tibble::as_tibble(data.frame(name = name,
                type = "outExpectedInfluence", node1 = x[["labels"]],
                node2 = "", value = cent[["OutExpectedInfluence"]],
                stringsAsFactors = FALSE))
        }
        if ("inExpectedInfluence" %in% statistics && directed) {
            tables$inExpectedInfluence <- tibble::as_tibble(data.frame(name = name,
                type = "inExpectedInfluence", node1 = x[["labels"]],
                node2 = "", value = cent[["InExpectedInfluence"]],
                stringsAsFactors = FALSE))
        }
        if ("bridgeStrength" %in% statistics) {
            tables$bridgeStrength <- tibble::as_tibble(data.frame(name = name,
                type = "bridgeStrength", node1 = bridgeCentralityNames,
                node2 = "", value = cent[["bridgeStrength"]],
                stringsAsFactors = FALSE))
        }
        if ("bridgeCloseness" %in% statistics) {
            tables$bridgeCloseness <- tibble::as_tibble(data.frame(name = name,
                type = "bridgeCloseness", node1 = bridgeCentralityNames,
                node2 = "", value = cent[["bridgeCloseness"]],
                stringsAsFactors = FALSE))
        }
        if ("bridgeBetweenness" %in% statistics) {
            tables$bridgeBetweenness <- tibble::as_tibble(data.frame(name = name,
                type = "bridgeBetweenness", node1 = bridgeCentralityNames,
                node2 = "", value = cent[["bridgeBetweenness"]],
                stringsAsFactors = FALSE))
        }
        if ("bridgeInDegree" %in% statistics) {
            tables$bridgeInDegree <- tibble::as_tibble(data.frame(name = name,
                type = "bridgeInDegree", node1 = bridgeCentralityNames,
                node2 = "", value = cent[["bridgeInDegree"]],
                stringsAsFactors = FALSE))
        }
        if ("bridgeOutDegree" %in% statistics) {
            tables$bridgeOutDegree <- tibble::as_tibble(data.frame(name = name,
                type = "bridgeOutDegree", node1 = bridgeCentralityNames,
                node2 = "", value = cent[["bridgeOutDegree"]],
                stringsAsFactors = FALSE))
        }
        if ("bridgeExpectedInfluence" %in% statistics) {
            tables$bridgeExpectedInfluence <- tibble::as_tibble(data.frame(name = name,
                type = "bridgeExpectedInfluence", node1 = bridgeCentralityNames,
                node2 = "", value = cent[["bridgeExpectedInfluence"]],
                stringsAsFactors = FALSE))
        }
    }
    for (i in seq_along(tables)) {
        tables[[i]]$id <- ifelse(tables[[i]]$node2 == "", tables[[i]]$node1,
            paste0(tables[[i]]$node1, ifelse(directed, "->",
                "--"), tables[[i]]$node2))
    }
    tab <- dplyr::bind_rows(tables)
    tab$nNode <- x$nNode
    tab$nPerson <- x$nPerson
    tab <- dplyr::group_by(tab, .data[["type"]])
    tab <- dplyr::mutate(tab,
                     rank_avg = rank(value, ties.method = "average"),
                     rank_min = rank(value, ties.method = "min"),
                     rank_max = rank(value, ties.method = "max"))
    tab$graph <- "1"
    return(tab)
}

diagnose_structure_change <- function(full_graph, graph_without, labels) {

  full_edges <- full_graph != 0
  without_edges <- graph_without != 0
  upper_idx <- upper.tri(full_edges)

  disappeared <- which(full_edges & !without_edges & upper_idx, arr.ind = TRUE)

  appeared <- which(!full_edges & without_edges & upper_idx, arr.ind = TRUE)

  common <- which(full_edges & without_edges & upper_idx, arr.ind = TRUE)
  reversed_idx <- which(sign(full_graph[common]) != sign(graph_without[common]))
  reversed <- common[reversed_idx, , drop = FALSE]

  format_edge_list <- function(edge_matrix, type) {
    if (is.null(edge_matrix) || nrow(edge_matrix) == 0) {
      return(character(0))
    }

    edge_strings <- character(nrow(edge_matrix))
    for (i in 1:nrow(edge_matrix)) {
      from <- labels[edge_matrix[i, 1]]
      to <- labels[edge_matrix[i, 2]]
      weight_from <- full_graph[edge_matrix[i, 1], edge_matrix[i, 2]]
      weight_to <- graph_without[edge_matrix[i, 1], edge_matrix[i, 2]]

      if (type == "disappeared") {
        edge_strings[i] <- sprintf("  %s -- %s : %.3f -> 0",
                                   from, to, weight_from)
      } else if (type == "appeared") {
        edge_strings[i] <- sprintf("  %s -- %s : 0 -> %.3f",
                                   from, to, weight_to)
      } else if (type == "reversed") {
        edge_strings[i] <- sprintf("  %s -- %s : %.3f -> %.3f",
                                   from, to, weight_from, weight_to)
      }
    }
    return(edge_strings)
  }

  disappeared_edges <- format_edge_list(disappeared, "disappeared")
  appeared_edges <- format_edge_list(appeared, "appeared")
  reversed_edges <- format_edge_list(reversed, "reversed")

  list(
    disappeared = disappeared_edges,
    appeared = appeared_edges,
    reversed = reversed_edges,
    n_disappeared = length(disappeared_edges),
    n_appeared = length(appeared_edges),
    n_reversed = length(reversed_edges)
  )
}

print_structure_changes <- function(structure_changes, case_ids = NULL) {

  if (!is.null(case_ids)) {
    if (length(case_ids) == 1) {
      cat(sprintf("\n=== Without Case %d: Structure Changes ===\n\n", case_ids))
    } else {
      cat(sprintf("\n=== Without Cases %s: Structure Changes ===\n\n",
                  paste(case_ids, collapse = ", ")))
    }
  } else {
    cat("\n=== Structure Changes ===\n\n")
  }

  cat("Edges disappeared:\n")
  if (structure_changes$n_disappeared > 0) {
    for (edge in structure_changes$disappeared) {
      cat(edge, "\n")
    }
  } else {
    cat("  None\n")
  }

  cat("\nEdges appeared:\n")
  if (structure_changes$n_appeared > 0) {
    for (edge in structure_changes$appeared) {
      cat(edge, "\n")
    }
  } else {
    cat("  None\n")
  }

  cat("\nEdge sign reversals:\n")
  if (structure_changes$n_reversed > 0) {
    for (edge in structure_changes$reversed) {
      cat(edge, "\n")
    }
  } else {
    cat("  None\n")
  }

  cat("\n")
}
