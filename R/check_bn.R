check_bn <- function(bn, include_cpt = FALSE){
  if (include_cpt){
    if (!(inherits(bn, "grain") | inherits(bn, "bn.fit"))){
      stop(paste (deparse(substitute(bn)), "must be an object of class grain or bn.fit."))
    }
    if (inherits(bn, "grain")){
      bn <- as.bn.fit(bn)
    }
    return(bn)
  } else {
    if (!(is.character(bn) | inherits(bn, "bn") | inherits(bn, "grain") | inherits(bn, "bn.fit"))){
      stop(paste (deparse(substitute(bn)), "must be a character string specifying the netowork configuration, or an object of class bn, grain or bn.fit."))
    }
    if (inherits(bn, "grain")){
      bn <- as.bn.fit(bn)
    } else if (is.character(bn)){
      bn <- model2network(bn)
    }
    return(bn)
  }
  return(bn)
}

check_fitdist_args <- function(arg, node){
  if (!is.null(arg)){
    if (length(arg) == 1){
      arg <- rep(arg, ncol(node))
    }
    if (length(arg) != ncol(node)){
      stop(paste(deparse(substitute(arg)), " must be a vector of length "), ncol(node))
    }
    return(arg)
  }
}

#' @importFrom utils lsf.str
rriskdistributions_get_pars <- function(){
  funs <- unclass(lsf.str(envir = asNamespace("rriskDistributions"), all = T))
  out <- funs[grepl("^get..*.par$",funs)]
  paste("#' @importFrom", "rriskDistributions", out)
}

