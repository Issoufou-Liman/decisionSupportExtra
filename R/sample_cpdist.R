#' Take a bayesian network, a node of interst, and a return value type option and returns either the
#' probability/sampled data (depending on the op argument specified) of the different  states of the
#' node of interst given the different conbinaition of its and its parents states.
#' @param bn an object of class bn.fit.
#' @param node a vector of character strings, the labels of the nodes whose conditional distribution we are interested in.
#' @param op a vector of character strings, the type of returned value: either probabilities or raw data sampled from the posterior distribution
#' @param evidence a name value pair:a named character vector which values are node states and names are node names.
#' @param n_generation how far to go in the network topology for building the conditionning specification for the query?
#' @return a matrix containing either the probabilities or raw data sampled from the posterior distribution
#' @details the rownames are specify the conditionning used for each query.
#' @example run the bayesian network and the function, then try using the code below the function.
sample_cpdist <- function(bn, node, op=c("sampler", "proba"), evidence = NULL, n_generation = NULL){
  op <- match.arg(op)
  # getting all nodes before the target
  query_list <- bnlearn::parents(x = bn, node = node)
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
  query_list <- gRain::nodeStates(x = bnlearn::as.grain(x=bn), query_list)
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
