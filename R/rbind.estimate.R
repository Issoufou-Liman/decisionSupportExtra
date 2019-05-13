#' Combine \code{\link[decisionSupport]{estimate}} objects by rows.
#'
#' rbind method for estimate class from decisionSupport package.
#'
#' @author Issoufou Liman
#' @param ... estimate object to combine
#' @examples
#' ## This example is taken fom decisionSupport
#' library (decisionSupport)
#'
#' # Create an estimate with optional columns (only marginal information supplied):
#' estimateMarg<-estimate(           c("posnorm", "lnorm"),
#' c(        4,       4),
#' c(       50,      10),
#' variable=c("revenue", "costs"),
#' median = c(   "mean",      NA),
#' method = c(    "fit",      ""))
#' print(estimateMarg)
#' print(corMat(estimateMarg))
#'
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
#'
#' estimateMarg; estimateCor
#' rbind (estimateMarg, estimateCor)
#' @importFrom decisionSupport estimate
#' @importFrom decisionSupport as.estimate
#' @importFrom plyr rbind.fill
#' @method rbind estimate
#' @export
rbind.estimate <- function(...){
  if(
    any(
      sapply(
        list(...), function(i){
          !is(i, "estimate")
        }
      )
    )
  )
  {
    stop('all objects should be of class estimates')
  }
  estimates_list <- list(...)
  x <- lapply(
    estimates_list, function (i){
      out <- unclass(i)$marginal
      out_nam <- row.names(out)
      list (out, out_nam)
    }
  )
  out_nam <- sapply(x, "[[", 2, simplify = FALSE)
  x <- sapply(x, "[[", 1, simplify = FALSE)
  x <- do.call(rbind.fill, x)
  rownames(x) <- do.call("c", out_nam)
  x <- as.estimate(x)
  return(x)
}
