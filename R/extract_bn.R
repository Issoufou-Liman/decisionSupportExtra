#' Extract a Bayesian network based on string model specifications
#'
#' Take a fully specified bayesian network, a sub string model, and return a subset of former matching the configuration of the latter.
#'
#' @author Issoufou Liman
#' @param bn an object of class \code{\link[bnlearn]{bn.fit class}} or \code{\link[gRain]{grain-main}}.
#' @param string_model Character string describing the configuration of the Bayesian network as return by \code{\link[bnlearn]{modelstring}}.
#' @return An object of class \code{\link[bnlearn]{bn.fit class}}
#' @details Currently only the classes of the arguments are checked. The user should take care of providing the right inputs.
#' @seealso \code{\link[bnlearn]{bn.fit}}
#' @seealso \code{\link[bnlearn]{modelstring}}
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
#' ## converting the grain bayesian network to bn.fit
#' network_bn_fit <- as.bn.fit(network)
#'
#' ## extract the entire Bayesian network as it is
#' extract_bn(network_bn_fit, modelstring(as.bn.fit(network)))
#'
#' ## Dropping all but soil type and Manure application nodes from the Bayesian network
#' extract_bn(network_bn_fit, "[Soil_type][Manure_application]")
#' @importFrom bnlearn nodes
#' @importFrom bnlearn model2network
#' @importFrom bnlearn root.nodes
#' @importFrom gRain querygrain
#' @importFrom bnlearn as.grain
#' @importFrom bnlearn custom.fit
#' @importFrom bnlearn as.bn.fit
#' @export
extract_bn <- function(bn, string_model){
  is_grain_bn <- inherits(bn, "grain")
  if (is.character(string_model)){
    string_model <- model2network (string_model)
  }
  if (!(inherits(bn, "bn.fit") | is_grain_bn)){
    stop("bn must an object of class bn.fit or grain")
    if (inherits(bn, "grain")){
      bn <- as.bn.fit(bn)
    }
  }
  if (!(all(nodes(string_model) %in% nodes(bn)))){
    stop(paste("Nodes:", string_model[!(nodes(string_model) %in% nodes(bn))], "are not valid nodes in deparse(substituate(bn))"))
  }
  root_nodes_names <- root.nodes(string_model)
  root_nodes<- lapply(root_nodes_names, function(i){
      querygrain(as.grain(bn), i)
  })
  root_nodes <- unlist(root_nodes, recursive = FALSE)
  names(root_nodes) <- root_nodes_names
  names_other_nodes <- nodes(string_model)[!(nodes(string_model) %in% root.nodes(string_model))]
  other_nodes <- lapply(names_other_nodes, function(i){
    # out <- bn[[i]][["prob"]]
    # out[parents(bn, i) %in% parents(string_model, i)]
    bn[[i]][["prob"]]
  })
  names(other_nodes) <- names_other_nodes
  nods <- c(root_nodes, other_nodes)
  if (is_grain_bn){
    return(as.grain(custom.fit(string_model, dist =nods)))
  } else {
    return(custom.fit(string_model, dist =nods))
  }
}
