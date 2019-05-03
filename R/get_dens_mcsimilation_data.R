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
#' The specification are made for \code{\link[ggplot2]{geom_density}}.
#' @seealso \code{\link[decisionSupportExtra]{get_dens_mcsimilation_data}} \code{\link[decisionSupport]{mcSimulation}}
#' @return
#' If x is a list, then a list of simulated output is returned. Otherwise, a dataframe is returned.
#' @importFrom stats density
#' @export
get_dens_mcsimilation_data <- function(x, colorQuantile = c("GRAY48", "YELLOW", "ORANGE", "DARK GREEN", "ORANGE1", "YELLOW1", "GRAY49"),
                                       colorProbability = c(1.00,    0.95,     0.75,     0.55,         0.45,     0.25,     0.05)) {
  colorQuantile <- rev(colorQuantile); colorProbability <- rev(colorProbability)
  f <- function(x,   colorQuantile, colorProbability) {
    dt <- as.data.frame(x$y)
    sapply(dt, function(i){
      dens <- density(i)
      df <- data.frame(x=dens$x, y=dens$y)
      quantiles <- quantile(i, prob=colorProbability)
      df$quant <- factor(findInterval(df$x,quantiles[-length(quantiles)]))
      df$quant_x_breaks <- rep(NA, nrow(df))
      df$color_equiv <- rep(NA, nrow(df))
      df$color <- rep(NA, nrow(df))
      for(i in unique(df$quant)){
        df[df$quant == i, 'quant_x_breaks'] <- max(df[df$quant == i, 'x'], na.rm = TRUE)
      }
      for(i in 1:length(unique(df$quant_x_breaks))){
        df[df$quant_x_breaks == unique(df$quant_x_breaks)[i], 'color_equiv'] <- colorProbability[i]
      }
      for(i in 1:length(unique(df$quant_x_breaks))){
        df[df$quant_x_breaks == unique(df$quant_x_breaks)[i], 'color'] <- colorQuantile[i]
      }
      df$quant <- paste0(df$color_equiv*100, "%")
      return(df)
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  if((inherits(x, 'mcSimulation', which = TRUE) == 1)) {
    out <- f(x, colorQuantile, colorProbability)
  } else if ((inherits(x, 'list', which = TRUE) == 1)) {
    out <- sapply(X = x, FUN = get_dens_mcsimilation_data, colorQuantile = colorQuantile, colorProbability = colorProbability, simplify = FALSE, USE.NAMES = TRUE)
  } else {
    stop(paste(deparse(substitute(x)), 'must be of class mcSimulation, list of mcSimulation, or a list of arbitratry depth of mcSimulation objects.'))
  }
}
