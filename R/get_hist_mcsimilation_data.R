#' Format \code{\link[decisionSupport]{mcSimulation}} data for ggplot
#'
#' Take an object of class \code{\link[decisionSupport]{mcSimulation}} or a list of these and return the simulated values
#' such that, when converted into molten data frame, can be easily understood by ggplot.
#'
#' @author Issoufou Liman
#' @param x \code{\link[decisionSupport]{mcSimulation}} or a list of \code{\link[decisionSupport]{mcSimulation}}
#' from which to extract the simulated output.
#' @inheritParams cast_mcSimulation
#' @inheritParams decisionSupport::hist.mcSimulation
#' @details
#' The specification are made for \code{\link[ggplot2]{geom_col}}.
#' @seealso \code{\link[decisionSupportExtra]{get_dens_mcsimilation_data}} \code{\link[decisionSupport]{mcSimulation}}
#' @return
#' If x is a list, then a list of simulated output is returned. Otherwise, a dataframe is returned.
#' @importFrom graphics hist
#' @export
get_hist_mcsimilation_data <- function(x, breaks = 100,
                                       colorQuantile = c("GRAY48", "YELLOW", "ORANGE", "DARK GREEN", "ORANGE1", "YELLOW1", "GRAY49"),
                                       colorProbability = c(1.00,    0.95,     0.75,     0.55,         0.45,     0.25,     0.05)) {
  f <- function(x,   colorQuantile, colorProbability) {
    dt <- as.data.frame(x$y)
    out <- sapply(dt, function (i){
      histPrepare <- hist(i, breaks=breaks, plot=FALSE)
      breaks <- histPrepare$breaks
      df <- data.frame(x = histPrepare$mids, y = histPrepare$counts)
      probability <- cumsum(histPrepare$density * diff(histPrepare$breaks))
      df$color <- cut(probability, breaks = c(colorProbability, 0), labels = colorQuantile)
      df$color_equiv <- cut(probability, breaks = c(colorProbability, 0), labels = colorProbability)
      df$quant <- paste0(as.numeric(as.character(df$color_equiv))*100, "%")
      return(list(result = df, breaks = breaks))
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  if((inherits(x, 'mcSimulation', which = TRUE) == 1)) {
    out <- f(x, colorQuantile, colorProbability)
  } else if ((inherits(x, 'list', which = TRUE) == 1)) {
    out <- sapply(X = x, FUN = get_hist_mcsimilation_data, colorQuantile = colorQuantile, colorProbability = colorProbability, simplify = FALSE, USE.NAMES = TRUE)
  } else {
    stop(paste(deparse(substitute(x)), 'must be of class mcSimulation, list of mcSimulation, or a list of arbitratry depth of mcSimulation objects.'))
  }
}
