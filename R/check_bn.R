Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)

nodeNames  <- function(x) UseMethod("nodeNames")

nodeNames.grain  <- function(x)
  getgrain(x, "universe")$nodes

nodeStates <- function(x, nodes=nodeNames(x)) UseMethod("nodeStates")

nodeStates.grain <- function(x, nodes=nodeNames(x)){
  getgrain(x, "universe")$levels[nodes]
}

# `%:::%` = function(pkg, fun) get(fun, envir = asNamespace(pkg),
#                                  inherits = FALSE)

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
  ## evidence that are not named are maybe node names
  if (is.null(names(evidence))){
    names(evidence) <- evidence
  }
  ## Some may not be named
  names_evid <- names(evidence)
  names_evid[names_evid == ""] <- evidence[names_evid == ""]
  names(evidence) <- names_evid
  ## check these names are valid node names in bn
  if(!(all(names(evidence) %in% nodes(bn)))){
    warning(paste0(names(evidence)[!(names(evidence) %in% nodes(bn))],
                   " are not not valid node names in ",
                   deparse(substitute(bn)),
                   ". These will be dropped!"))
    evidence <- evidence[names(evidence) %in% nodes(bn)]
  } else if(any(names(evidence) == "") | any(is.na(names(evidence)))){
    stop("evidence list should be a named list")
  }
  return(evidence)
}

check_bn_node_states <- function(bn, evidence) {
  ## check node names validity
  evidence <- check_bn_nodes(bn, evidence)
  ## get all node states for all vailid node names
  node_states <- nodeStates(as.grain(bn), names(evidence))
  ## check those nodes that seem to have states: if node names is different from it value,
  ## there maybe node states in play. if so, keep those explicitly specified states.
  check_it <- mapply(function(x, y) identical(sort(x), sort(y)), evidence, names(evidence))
  evidence[check_it] <- node_states[check_it]
  ## Checking invalid node states
  check_it <- mapply(function(x, y) sort(x) %in% sort(y), evidence, node_states)
  check_it <- unlist(check_it, recursive = TRUE)
  if(!all(check_it == TRUE)){
    stop("invalid node states detected in evidence, please check it!")
  }
  return(evidence)
}

#' # The following comes from rowr package which is no longer available on CRAN
#'
#' #'A more robust form of the R \code{\link{as}} function.
#' #'
#' #' Alternative to \code{as} that allows any data object to be converted to any other.
#' #'
#' #' @param object any \code{R} object
#' #' @param class the name of the class to which \code{object} should be coerced
#' as2<-function(object,class)
#' {
#'   object<-as.matrix(object)
#'   if(class=='factor')
#'     return(as.factor(as.character(object)))
#'   if(class=='data.frame')
#'     return(as.data.frame(object))
#'   else
#'     return(methods::as(object,class))
#' }
#'
#' vert<-function(object)
#' {
#'   #result<-as.data.frame(cbind(as.matrix(object)))
#'   if(is.list(object))
#'     object<-cbind(object)
#'   object<-data.frame(object)
#'
#'   return(object)
#' }
#'
#' #'Pads an object to a desired length, either with replicates of itself or another repeated object.
#' #'
#' #'@param x an R object
#' #'@param length.out the desired length of the final output
#' #'@param fill R object to fill empty rows in columns below the max size.  If unspecified, repeats input rows in the same way as \code{cbind}.
#' #'@param preserveClass determines whether to return an object of the same class as the original argument.  Otherwise, returns a matrix.
#' #'@examples
#' #'buffer(c(1,2,3),20)
#' #'buffer(matrix(c(1,2,3,4),nrow=2),20)
#' #'buffer(list(1,2,3),20)
#' #'df<-data.frame(as.factor(c('Hello','Goodbye')),c(1,2))
#' #'buffer(df,5)
#' #'buffer((factor(x=c('Hello'))),5)
#' buffer<-function(x,length.out=len(x),fill=NULL,preserveClass=TRUE)
#' {
#'   xclass<-class(x)
#'   input<-lapply(vert(x),unlist)
#'   results<-as.data.frame(lapply(input,rep,length.out=length.out))
#'   if(length.out>len(x) && !is.null(fill))
#'   {
#'     results<-t(results)
#'     results[(length(unlist(x))+1):length(unlist(results))]<-fill
#'     results<-t(results)
#'   }
#'   if(preserveClass)
#'     results<-as2(results,xclass)
#'   return(results)
#' }
#'
#' #'Allows finding the 'length' without knowledge of dimensionality.
#' #'
#' #'@param data any \code{R} object
#' #'@examples
#' #'len(list(1,2,3))
#' #'len(c(1,2,3,4))
#' #'df<-data.frame(a=c(1,2,3),b=c(1,2,3))
#' #'len(df)
#' len <- function(data)
#' {
#'   result<-ifelse(is.null(nrow(data)),length(data),nrow(data))
#'   return(result)
#' }
#'
#' #' Combine arbitrary data types, filling in missing rows.
#' #'
#' #' Robust alternative to \code{\link{cbind}} that fills missing values and works
#' #' on arbitrary data types.  Combines any number of R objects into a single matrix, with each input
#' #' corresponding to the greater of 1 or ncol.  \code{cbind} has counterintuitive
#' #' results when working with lists, cannot handle certain inputs of differing
#' #' length, and does not allow the fill to be specified.
#' #'
#' #' @param ... any number of R data objects
#' #' @param fill R object to fill empty rows in columns below the max size.  If unspecified, repeats input rows in the same way as \code{cbind}. Passed to \code{\link{buffer}}.
#' #' @examples
#' #' cbind.fill(c(1,2,3),list(1,2,3),cbind(c(1,2,3)))
#' #' cbind.fill(rbind(1:2),rbind(3:4))
#' #'df<-data.frame(a=c(1,2,3),b=c(1,2,3))
#' #' cbind.fill(c(1,2,3),list(1,2,3),cbind(c('a','b')),'a',df)
#' #' cbind.fill(a=c(1,2,3),list(1,2,3),cbind(c('a','b')),'a',df,fill=NA)
#' cbind.fill<-function(...,fill=NULL)
#' {
#'   inputs<-list(...)
#'   inputs<-lapply(inputs,vert)
#'   maxlength<-max(unlist(lapply(inputs,len)))
#'   bufferedInputs<-lapply(inputs,buffer,length.out=maxlength,fill,preserveClass=FALSE)
#'   return(Reduce(cbind.data.frame,bufferedInputs))
#' }
