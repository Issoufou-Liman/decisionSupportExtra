#' Sampling from posterior distribution of a Bayesian network at a node of interest
#'
#' Take a bayesian network, a node of interst, and a return value type option and returns either the
#' probability/sampled data (depending on the op argument specified) of the different  states of the
#' node of interst given the different conbinaition of its and its parents states.
#'
#' @author Issoufou Liman
#' @param bn an object of class bn.fit.
#' @param node character string, the label of the node which conditional distribution is of interest.
#' @param op a vector of character strings, the type of returned value: either probabilities or raw data sampled from the posterior distribution
#' @param evidence a name value pair:a named character vector which values are node states and names are node names.
#' @param n_generation how far to go in the network topology for building the conditionning specification for the query?
#' @return A matrix containing either the probabilities or raw data sampled from the posterior distribution
#' @details Each row consists in a comditional probability of a state of the node of interest given the combination of the states of the parent nodes.
#' @seealso \code{\link[bnlearn]{cpdist}}
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
#' network <- as.bn.fit(network)
#' sample_cpdist (network, "Soil_water_holding_capacity")
#' @importFrom bnlearn nodes cpdist as.grain parents
#' @importFrom gRain nodeStates
#' @export
sample_cpdist <- function(bn, node, op=c("sampler", "proba"), evidence = NULL, n_generation = NULL){
  op <- match.arg(op)
  # getting all nodes before the target
  query_list <- parents(x = bn, node = node)
  # removing nodes corresponding to evidence
  if(!is.null(evidence)){
    # sanity check on evidence
    if(length(names(evidence)) == 0){
      warning('evidence does not map to any node in the network. evidence will be ignored.')
      evidence = NULL
    } else {
      # names exists but are they correct?
      wrong_evid_names <- names(evidence)[!(names(evidence) %in% nodes(bn))]
      if(length(wrong_evid_names) > 0){
        warning(paste('Node names', wrong_evid_names, 'in evidence: ignored as inconsistant with nodes in the network.'))
        evidence <- evidence[!(names(evidence) %in% wrong_evid_names)]
      }
      out_evid_names <- names(evidence)[!(names(evidence) %in% query_list)]
      in_evid_names <- names(evidence)[(names(evidence) %in% query_list)]
      query_list <- c(query_list, out_evid_names)
      if(length(in_evid_names) != 0){
        query_list[names(in_evid_names)] <- in_evid_names
      }
    }
  }
  query_list <- nodeStates(x = as.grain(x=bn), query_list)
  query_list <- expand.grid(query_list, stringsAsFactors = FALSE)
  prior <- query_list

  g <- function(x){
    parse(text=paste0('(',colnames(x),'==',"'", x,"'",")",collapse = '&'))
  }

  query_list <- lapply(1:nrow(query_list), function (i){
    evidence <- g(query_list[i, ])
    cmd <- paste0("cpdist", "(", 'fitted', '=' ,  'bn', ',', "nodes", "=", 'node', ',', 'evidence', '=', evidence, ")" )
    out <- eval(parse(text= cmd))
    if(op == 'proba'){
      out <- table(out)
      somme <- sum(out)
      if(somme > 0){
        out <- out/somme
        out <- t(as.matrix(out))
        rownames(out) <- paste(node, '|', evidence)
      }
    } else {
      colnames(out) <- paste(node, '|', evidence)
    }
    return(out)
  })
  if(op == 'proba'){
    query_list <- do.call (rbind, query_list)
  }
  query_list <- list(prior = prior, posterior = query_list)
  class(query_list) <- "sample_cpdist"
  query_list
}
