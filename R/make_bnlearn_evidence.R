#' Create evidence for Bayesain network query
#'
#' Take a fully specified bayesian network, a list nodes states names after the node names
#' or character vector of node names, and return a list of query formated following the
#' bnlearn package specifications.
#' @author Issoufou Liman
#' @inheritParams extract_bn
#' @param evidence list or character vector specify the states or nodes based on which the evidences
#' are to be formated.
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
#' make_bnlearn_evidence (bn = network, evidence = c('Soil_type', 'Manure_application'))
#' make_bnlearn_evidence (bn = network,
#' evidence = list(Soil_type = c('Sandy', 'Loamy', 'Clayey'), Manure_application = c('FALSE', 'TRUE')))
#' make_bnlearn_evidence (bn = network,
#' evidence = list(Soil_type = 'Sandy', Manure_application = c('FALSE', 'TRUE')))
#'
#' ## converting the grain bayesian network to bn.fit
#' network_bn_fit <- as.bn.fit(network)
#' ## Use bn.fit object (bnlearn package)
#' make_bnlearn_evidence (bn = network_bn_fit, evidence = c('Soil_type', 'Manure_application'))
#' make_bnlearn_evidence (bn = network_bn_fit,
#' evidence = list(Soil_type = c('Sandy', 'Loamy', 'Clayey'), Manure_application = c('FALSE', 'TRUE')))
#' make_bnlearn_evidence (bn = network_bn_fit,
#' evidence = list(Soil_type = 'Sandy', Manure_application = c('FALSE', 'TRUE')))
#' @export
make_bnlearn_evidence <- function(bn, evidence){
  bn <- check_bn(bn, include_cpt = TRUE)
  evidence <- check_bn_nodes(bn, evidence)
  if (is.list(evidence)){
    evidence <- check_bn_node_states(bn, evidence)
  }
  if (is.character(evidence)){
    evidence <- nodeStates(as.grain(bn), evidence)
  }

  evidence <- expand.grid(evidence, stringsAsFactors = FALSE)
  evidence <- sapply(1:nrow(evidence), function (i){
    evidence[i, ]
  }, simplify = FALSE)

  format_bnlearn_evidence <- function(x){
    # parse(text=paste0('(',names(x),'==',"'", x,"'",")",collapse = '&'))
    paste0('(',names(x),'==',"'", x,"'",")",collapse = '&')
  }
  evidence <- sapply(X = evidence, FUN = format_bnlearn_evidence, simplify = FALSE)
  return(evidence)
}
