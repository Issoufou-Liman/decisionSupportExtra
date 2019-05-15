check_bn <- function(bn, include_cpt = FALSE) {
    if (include_cpt) {
        if (!(inherits(bn, "grain") | inherits(bn, "bn.fit"))) {
            stop(paste(deparse(substitute(bn)), "must be an object of class grain or bn.fit."))
        }
        if (inherits(bn, "grain")) {
            bn <- as.bn.fit(bn)
        }
        return(bn)
    } else {
        if (!(is.character(bn) | inherits(bn, "bn") | inherits(bn, "grain") | inherits(bn, "bn.fit"))) {
            stop(paste(deparse(substitute(bn)), "must be a character string specifying the netowork configuration, or an object of class bn, grain or bn.fit."))
        }
        if (inherits(bn, "grain")) {
            bn <- as.bn.fit(bn)
        } else if (is.character(bn)) {
            bn <- model2network(bn)
        }
        return(bn)
    }
    return(bn)
}

check_fitdist_args <- function(arg, node) {
    if (!is.null(arg)) {
        if (length(arg) == 1) {
            arg <- rep(arg, ncol(node))
        }
        if (length(arg) != ncol(node)) {
            stop(paste(deparse(substitute(arg)), " must be a vector of length "), ncol(node))
        }
        return(arg)
    }
}

#' @importFrom utils lsf.str
rriskdistributions_get_pars <- function() {
    funs <- unclass(lsf.str(envir = asNamespace("rriskDistributions"), all = T))
    out <- funs[grepl("^get..*.par$", funs)]
    paste("#' @importFrom", "rriskDistributions", out)
}

split_labels_accross_lines <- function(x, line_leng = 10) {
    out <- gsub(paste0("(.{1,", line_leng, "})(\\s|$)"), "\\1\n", x)
    substring(out, 1, (nchar(out) - 1))
}

check_bn_nodes <- function(bn, evidence) {
  if(is.list(evidence)){
    if(!(all(names(evidence) %in% nodes(bn)))){
      warning(paste0(names(evidence)[!(names(evidence) %in% nodes(bn))],
                     " are not not valid node names in ",
                     deparse(substitute(bn)),
                     ". These will be dropped!"))
      evidence <- evidence[names(evidence) %in% nodes(bn)]
    }
    if(any(names(evidence) == "") | any(is.na(names(evidence)))){
      stop("evidence list should be a named list")
    }
    return(evidence)
  } else if (is.character(evidence)){
    if(!(all(evidence %in% nodes(bn)))){
      warning(paste(evidence[!(evidence %in% nodes(bn))],
                    " are not valid node names in ", deparse(substitute(bn)),
                    ". These will be dropped!"))
      evidence <- evidence[evidence %in% nodes(bn)]
    }
    if (any(evidence == "") | any(is.na(evidence))){
      stop("evidence vector should not contain null character")
    }
    names(evidence) <- evidence
    return(evidence)
  }
  return(evidence)
}

check_bn_node_states <- function(bn, evidence) {
  if(is.list(evidence)){
    node_states <- nodeStates(as.grain(bn), names(evidence))
    check_it <- mapply(function(x, y) sort(x) %in% sort(y), evidence, node_states)
    check_it <- unlist(check_it, recursive = TRUE)
    if(!all(check_it == TRUE)){
      stop("invalid node states detected in evidence, please check it!")
    }
  }
  return(evidence)
}
