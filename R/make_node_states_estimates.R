#' Create \code{\link[decisionSupport]{estimate}} for Bayesain network node
#'
#' create a multivariate \code{\link[decisionSupport]{estimate}} object for Bayesian network node.
#' The function samples from the posterior distribution of the Bayesian network to construct an
#' object of class \code{\link[decisionSupport]{estimate}} accounting for each state of the node.
#'
#' @author Issoufou Liman
#' @inheritParams sample_cpdist
#' @inheritParams guess_decisionSupport_estimates
#' @inheritParams fitdistrplus::fitdist
#' @inheritParams guess_decisionSupport_estimates
#' @param state_effects numeric vector specifying the relative weight factor of each state in the final estimate.
#' @param fit character specifying whether the estimates should be derived from either empirical ('emp', default) or theoritical distribution ('theo').
#' @param emp_estimates list alternative estimates specification.Default to NULL. This is only used when fit = 'theo'.
#' @seealso \code{\link[decisionSupport]{estimate}}.
#' @details see \code{\link[decisionSupport]{estimate}}.
#' @references
#' Eike Luedeling and Lutz Goehring (2018). decisionSupport: Quantitative Support of Decision Making under Uncertainty.
#' R package version 1.103.8. https://CRAN.R-project.org/package=decisionSupport
#' @examples
#' library (gRain)
#' library(bnlearn)
#' ## setting a bayesian network with gRain
#' Soil_type <- cptable (~Soil_type, values = c(0.05, 0.55, 0.4),
#' levels = c('Sandy', 'Loamy', 'Clayey'))
#' Manure_application <- cptable(~Manure_application, values = c(0.3, 0.7),
#' levels = c('FALSE', 'TRUE'))
#' Soil_water_holding_capacity_tmp <- make_gRain_CPT(
#'  parent_effects = list(c(0, 2.5, 3), c(0, 2)),
#'  parent_weights = c(2,1),
#'  b = 3,
#'  child_prior = c(0.2,0.5,0.3),
#'  child_states = c('Low', 'Medium', 'High'),
#'  parent_states = list(c('Sandy', 'Loamy', 'Clayey'), c('FALSE', 'TRUE'))
#' )
#' Soil_water_holding_capacity_values <- Soil_water_holding_capacity_tmp$values
#' Soil_water_holding_capacity_levels <- Soil_water_holding_capacity_tmp$levels
#' Soil_water_holding_capacity <- cptable (
#' ~Soil_water_holding_capacity|Soil_type:Manure_application,
#' values = Soil_water_holding_capacity_values,
#' levels = Soil_water_holding_capacity_levels)
#' ## Compile conditional probability tables
#' network <- compileCPT(list(Soil_type, Manure_application, Soil_water_holding_capacity))
#' ## Graphical Independence Network ####
#' network <- grain(network)
#' ## Use grain object (gRain package)
#' make_node_states_estimates (bn = network, node = 'Soil_water_holding_capacity')
#'
#' ## converting the grain bayesian network to bn.fit
#' network_bn_fit <- as.bn.fit(network)
#' ## Use bn.fit object (bnlearn package)
#' make_node_states_estimates (bn = network_bn_fit,
#' node = 'Soil_water_holding_capacity', distr = c('beta', 'norm', 'gamma'))
#' @importFrom stats na.omit
#' @importFrom fitdistrplus descdist
#' @import gRain
#' @export
make_node_states_estimates <- function(bn, node, op = "proba", distr = "beta", n_run=1000,
                                       state_effects = NULL, evidence = NULL,
                                       n_generation = NULL, include_relatives = TRUE,
                                       estimate_method = "fit", percentiles = c(0.025, 0.5, 0.975),
                                       plot = FALSE, show.output = FALSE, fit = c('emp', 'theo'), emp_estimates=NULL) {
    bn <- check_bn(bn,include_cpt = TRUE)

    fit <- match.arg(fit)

    if (length(distr) == 1) {
        # distr <- rep(distr, ncol(node))
        distr <- rep(distr, length(nodeStates(as.grain(bn), node)[[1]]))
        warning("A single distribution specified, using it for all nodes states")
    }
    if(length(distr) != length(nodeStates(as.grain(bn), node)[[1]])){
        stop(paste('A vector of distribution of length', length(nodeStates(as.grain(bn), node)[[1]]), 'was expected but only', length(distr), 'were provided.'))
    }

    if (length(estimate_method) == 1) {
        estimate_method <- rep(estimate_method, length(nodeStates(as.grain(bn), node)[[1]]))
        warning("A single estimate_method specified, using it for all nodes states")
    }

    if(length(estimate_method) != length(nodeStates(as.grain(bn), node)[[1]])){
        stop(paste('A vector of estimate_method of length', length(nodeStates(as.grain(bn), node)[[1]]), 'was expected but only', length(estimate_method), 'were provided.'))
    }

    # query_set <- rownames(node)
    if(is.null(state_effects)){
        state_effects <- rep(1, length(nodeStates(as.grain(bn), node)[[1]]))
    }
    if (length(state_effects) != length(nodeStates(as.grain(bn), node)[[1]])){
        stop(paste(deparse(substitute(state_effects)), "should of length", length(nodeStates(as.grain(bn), node)[[1]])))
    }
    tag <- node

    if(fit == 'emp'){
        # , state_effects = c(1/3, 1/2, 1)
        node <- sample_cpdist(bn = bn, node = node, op = op,
                              evidence = evidence, n_generation = n_generation, include_relatives = include_relatives, n_run = n_run)
        node <- na.omit(node$posterior)
        node <- as.data.frame(node)
        node <- mapply("*", node, state_effects, SIMPLIFY = FALSE)
        node <- as.data.frame(node)
        names(node) <- paste0(tag, "=", names(node))
        out <- guess_decisionSupport_estimates(data = node, distr = distr, percentiles = percentiles,
                                               plot = plot, show.output = show.output, estimate_method = estimate_method)
        as.list.estimate(out)
    } else if (fit == 'theo') {
        if(!is.null(emp_estimates)){
            if(is.null(names(emp_estimates))){
                names(emp_estimates) <- paste0(tag, "=", nodeStates(as.grain(bn), node)[[1]])
            }
            emp_estimates <- mapply("*", emp_estimates, state_effects, SIMPLIFY = FALSE)
            tmp <- 1:length(emp_estimates); names(tmp) <- names(emp_estimates)
            out <- sapply(tmp, function(i){
                out <- c(emp_estimates[[i]], distr[i], estimate_method[i])
                out <- data.frame(t(out), stringsAsFactors = FALSE)
                rownames(out) <- names(emp_estimates)[[i]]
                names(out) <- c('lower', 'median', 'upper', 'distribution', 'method')
                as.estimate(out)
            }, simplify = FALSE)
        } else {
            row_names <- paste0(tag, "=", nodeStates(as.grain(bn), node)[[1]])
            node <- sample_cpdist(bn = bn, node = node, op = op,
                                  evidence = evidence, n_generation = n_generation, include_relatives = include_relatives, n_run = n_run)
            node <- na.omit(node$posterior)

            tmp <- 1:ncol(node); names(tmp) <- colnames(node)
            out <- sapply(tmp, function(i){
                node <- mapply("*", node[, i], state_effects[i], SIMPLIFY = FALSE)
                node <- descdist(as.numeric(node), boot=1000, graph=FALSE)
                node <- data.frame(node$min, node$median, node$max, distr[i], estimate_method[i], stringsAsFactors = FALSE)
                rownames(node) <- row_names[i]
                colnames(node) <- c('lower', 'median', 'upper', 'distribution', 'method')
                as.estimate(node)
            }, simplify = FALSE)
        }
    }
}
