#' Orchestrate conditional probability table for Bayesain network node based on gross evidence
#'
#' Create conditional probability table for a node based on a set of known nodes
#' @author Issoufou Liman
#' @inheritParams extract_bn
#' @param target_child character string specifying the node for which
#' the conditional probability table is requested.
#' @param target_parents character vector specifying  the nodes based on which
#' evidence are to be constructed to query bn at target_child.
#' @details While invalid nodes are silently dropped,
#' bn is retuned as is, with a warning, when both target_child and target_parents are set to NULL.
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
#' seduce_CPT (network, "Soil_water_holding_capacity", c( "Soil_type"))
#' seduce_CPT (network, "Soil_water_holding_capacity", c( "Manure_application"))
#' seduce_CPT (network, "Soil_water_holding_capacity", c( "Soil_type", "Manure_application"))
#'
#' ## converting the grain bayesian network to bn.fit
#' network_bn_fit <- as.bn.fit(network)
#' ## Use bn.fit object (bnlearn package)
#' seduce_CPT (network_bn_fit, "Soil_water_holding_capacity", c( "Soil_type"))
#' seduce_CPT (network_bn_fit, "Soil_water_holding_capacity", c( "Manure_application"))
#' seduce_CPT (network_bn_fit, "Soil_water_holding_capacity", c( "Soil_type", "Manure_application"))
#' @importFrom bnlearn as.grain
#' @importFrom gRain nodeStates querygrain setEvidence nodeNames
#' @export
seduce_CPT <- function(bn, target_child = NULL, target_parents = NULL){
  # checking and converting bn.fit object to grain object
  if(inherits(bn, "bn.fit")){
    bn <- as.grain(bn)
  }
  if (is.null(target_child) & is.null(target_parents)){
    warning(paste(deparse(substitute(bn)), "returned as is: ", "both arguments target_child and target_child were set to NULL"))
    out <- bn
  } else if (length(target_child) > 1){
    stop("multiple target child nodes supplied: target_child must be of length 1")
  } else if ((length(target_child) == 1)){
    # getting the parent states
    # reorganizing target_parents to match bn config
    # silently dropping invalid node in bn
    target_parents <- nodeNames(bn)[nodeNames(bn) %in% target_parents]
    parent_states <- nodeStates(bn, target_parents)

    # getting the child states
    child_states <- nodeStates(bn, target_child)

    # getting the CPTs from bn given what is known as the states target_parentss
    evid <- as.matrix(expand.grid(parent_states))
    evid <- sapply(1:nrow(evid), function (i){
      as.list(evid[i, ])
    }, simplify = FALSE)

    # querying the network evidence by evidence
    query_set <- sapply(evid, function (i){
      querygrain(setEvidence(object = bn,
                             nodes = target_child,
                             evidence = i))
    }, simplify = FALSE, USE.NAMES = TRUE)

    # extract only the target child
    query_set <- sapply(query_set, function(i){
      i[[target_child]]
    }, simplify = FALSE)

    # create an appropriate size array for gRain CPTs
    dimens <- sapply(c(child_states, parent_states), length)
    out <- array(data = unlist(query_set, recursive = TRUE, use.names = FALSE),
          dim = dimens,
          dimnames = c(child_states, parent_states))
  }
  return(out)
}
