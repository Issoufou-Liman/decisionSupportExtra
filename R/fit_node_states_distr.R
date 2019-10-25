#' Fitting univariate distributions to Bayesain network nodes
#'
#' Fitting univariate distributions to the posterior distribution of a Bayesian network node.
#'
#' @author Issoufou Liman
#' @inheritParams ggplot_descdist_bn
#' @inheritParams sample_cpdist
#' @inheritParams fitdistrplus::fitdist
#' @seealso \code{\link[fitdistrplus]{fitdist}}.
#' @details see \code{\link[fitdistrplus]{fitdist}}.
#' @references
#' Marie Laure Delignette-Muller, Christophe Dutang (2015). fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software, 64(4), 1-34. http://www.jstatsoft.org/v64/i04/.
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
#' fit_node_states_distr (bn = network, node = 'Soil_water_holding_capacity', gof='KS')
#'
#' ## converting the grain bayesian network to bn.fit
#' network_bn_fit <- as.bn.fit(network)
#' ## Use bn.fit object (bnlearn package)
#' fit_node_states_distr (bn = network_bn_fit,
#' node = 'Soil_water_holding_capacity', distr = c('beta', 'norm', 'gamma'))
#' @export
fit_node_states_distr <- function(bn, node, op = "proba", distr = "beta", method = "mme", start = NULL,
    fix.arg = NULL, discrete, keepdata = TRUE, keepdata.nb = 100, include_relatives = TRUE, n_run = 1000, ...) {

    bn <- check_bn(bn, include_cpt = TRUE)

    node <- sample_cpdist(bn = bn, node = node, op = op,
                          evidence = NULL, include_relatives = include_relatives, n_run = n_run)
    node <- node$posterior
    if (missing(discrete)) {
        if (is.element(distr, c("binom", "nbinom", "geom", "hyper", "pois"))) {
            discrete <- TRUE
        } else {
            discrete <- FALSE
        }
    }

    distr <- check_fitdist_args(distr, node)
    method <- check_fitdist_args(method, node)
    start <- check_fitdist_args(start, node)
    fix.arg <- check_fitdist_args(fix.arg, node)
    # discrete <- check_fitdist_args (discrete, node)
    keepdata <- check_fitdist_args(keepdata, node)
    keepdata.nb <- check_fitdist_args(keepdata.nb, node)

    tmp <- lapply(as.list(1:ncol(node)), function(i) {
        fitdist(node[, i], distr = distr[i], method = method[i], start = start[i], fix.arg = fix.arg[i],
            discrete = discrete[i], keepdata = keepdata[i], keepdata.nb = keepdata.nb[i], ...)
    })
    names(tmp) <- colnames(node)
    return(tmp)
}
