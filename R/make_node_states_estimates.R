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
#' @export
make_node_states_estimates <- function(bn, node, op = "proba", distr = "beta", n_run=1000,
                                       state_effects = NULL, evidence = NULL,
                                       n_generation = NULL, include_relatives = TRUE,
                                       estimate_method = "fit", percentiles = c(0.025, 0.5, 0.975),
                                       plot = FALSE, show.output = FALSE) {
    # , state_effects = c(1/3, 1/2, 1)
    bn <- check_bn(bn,include_cpt = TRUE)
    tag <- node
    node <- sample_cpdist(bn = bn, node = node, op = op,
                          evidence = evidence, n_generation = n_generation, include_relatives = include_relatives, n_run = n_run)
    # print(head(node))
    node <- na.omit(node$posterior)
    node <- as.data.frame(node)
    # query_set <- rownames(node)
    if(is.null(state_effects)){
        state_effects <- rep(1, ncol(node))
    }
    if (length(state_effects) != ncol(node)){
        stop(paste(deparse(substitute(state_effects)), "should of length", ncol(node)))
    }

    node <- mapply("*", node, state_effects, SIMPLIFY = FALSE)

    # scale_states <- function(proba, state_effects){ tmp0 <- mapply('*', proba, state_effects) tmp1 <-
    # sum(tmp0) tmp0/tmp1 } node <- t(apply(X = node, MARGIN = 1, FUN = scale_states, state_effects =
    # state_effects))
    node <- as.data.frame(node)

    # node <- data.frame(query_set=query_set, node)
    names(node) <- paste0(tag, "=", names(node))
    if (length(distr) == 1) {
        distr <- rep(distr, ncol(node))
        warning("A single distribution specified, using it for all nodes states")
    }
    out <- guess_decisionSupport_estimates(data = node, distr = distr, percentiles = percentiles,
        plot = plot, show.output = show.output, estimate_method = estimate_method)
    as.list.estimate(out)
}
