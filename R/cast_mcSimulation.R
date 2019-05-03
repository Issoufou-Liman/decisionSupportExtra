#' Extract the simulated output from \code{\link[decisionSupport]{mcSimulation}} object
#'
#' Take an object of class \code{\link[decisionSupport]{mcSimulation}} and return the simulated values
#' as specified in the model function.
#'
#' @author Issoufou Liman
#' @param x \code{\link[decisionSupport]{mcSimulation}} or a list of \code{\link[decisionSupport]{mcSimulation}}
#' from which to extract the simulated output.
#' @return
#' If x is a list, then a list of simulated output is returned. Otherwise, a dataframe is returned.
#' @export
cast_mcSimulation <- function(x){
  f <- function(mcSim){
    mcSim$y
  }
  if((inherits(x, 'mcSimulation', which = TRUE) == 1)) {
    out <- f(x)
  } else if ((inherits(x, 'list', which = TRUE) == 1)) {
    out <- sapply(x, FUN = cast_mcSimulation, simplify = FALSE, USE.NAMES = TRUE)
  } else {
    stop(paste(deparse(substitute(x)), 'must be of class mcSimulation,
               list of mcSimulation, or a list of arbitratry depth of mcSimulation objects.'))
  }
}
