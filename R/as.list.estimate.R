#' Construct a list of \code{\link[decisionSupport]{estimate}} from \code{\link[decisionSupport]{estimate}}
#'
#' as.list method for estimate class from decisionSupport package.
#'
#' @author Issoufou Liman
#' @param x estimate object to be coerced.
#' @param ... Additional arguments non implemented.
#' @details Each row of the marginal attribute of the estimate object, representing a variable, will be coerced to an object of class estimate to provide a list of these. correlation_matrix attribute will be, hence, discarded.
#' @examples
#' ## This example is taken fom decisionSupport
#' library (decisionSupport)
#' # Create an estimate from text (with correlated components):
#' estimateTextMarg<-"variable,  distribution, lower, upper
#' revenue1,  posnorm,      100,   1000
#' revenue2,  posnorm,      50,    2000
#' costs1,    posnorm,      50,    2000
#' costs2,    posnorm,      100,   1000"
#' estimateTextCor<-",         revenue1, costs2
#' revenue1,        1,   -0.3
#' costs2,       -0.3,      1"
#' estimateCor <- as.estimate (read.csv (header=TRUE, text=estimateTextMarg,
#' strip.white = TRUE, stringsAsFactors = FALSE),
#' correlation_matrix = data.matrix (read.csv (text = estimateTextCor,
#' row.names = 1, strip.white = TRUE)))
#' as.list (estimateCor)
#' @importFrom decisionSupport estimate
#' @importFrom decisionSupport as.estimate
#' @export
as.list.estimate <- function(x, ...){
  # removing the class attr
  unclassed_x <- unclass(x)
  # extracting the data.frame embedded in the estimate
  x <- unclassed_x$marginal
  n <- 1:nrow(x)
  names(n) <- rownames(x)
  sapply(n, function (i) x[i, ], USE.NAMES = TRUE, simplify = FALSE)
}
