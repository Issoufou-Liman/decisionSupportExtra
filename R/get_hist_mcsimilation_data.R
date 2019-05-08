#' Compute kernel density and histogram data, from \code{\link[decisionSupport]{mcSimulation}} object, for \code{\link[ggplot2]{geom_bar}} and \code{\link[ggplot2]{geom_density}}
#'
#' Take an object of class \code{\link[decisionSupport]{mcSimulation}} or a list of these and return a ggplot object
#' or the simulated values such that, when converted into molten data frame, can be easily
#' understood by ggplot.
#'
#' @author Issoufou Liman
#' @param x \code{\link[decisionSupport]{mcSimulation}} or a list of \code{\link[decisionSupport]{mcSimulation}}
#' @inheritParams decisionSupport::hist.mcSimulation
#' @section get_hist_mcsimilation_data:
#' get_hist_mcsimilation_data, which specification are made for \code{\link[ggplot2]{geom_bar}},
#' takes care of extracting the simulated values as histogram
#' from the \code{\link[decisionSupport]{mcSimulation}} object.
#' @section get_dens_mcsimilation_data:
#' get_dens_mcsimilation_data, which specification are made for \code{\link[ggplot2]{geom_density}},
#' takes care of extracting the simulated values as kernel density
#' from the \code{\link[decisionSupport]{mcSimulation}} object.
#' @section ggplot_mc_hist and ggplot_mc_dens:
#' These are generics for which methods are defined for \code{\link[decisionSupport]{mcSimulation}}
#' object or a list of these.Internally, these methods rely on get_hist_mcsimilation_data and get_dens_mcsimilation_data
#' respectively to generate the data required for \code{\link[ggplot2]{geom_bar}} and \code{\link[ggplot2]{geom_density}}
#' and hand over these data to ggplot.
#' @seealso \code{\link[decisionSupportExtra]{ggplot_mc_hist}} \code{\link[decisionSupportExtra]{get_dens_mcsimilation_data}} \code{\link[decisionSupport]{mcSimulation}}
#' @return
#' \itemize{
#'  \item{list: }{If x is a list object.}
#'  \item{dataframe: }{If x is an mcSimulation object.}
#' }
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

#' Generic \code{\link[decisionSupport]{mcSimulation}} histogram plotting with ggplot
#'
#' generate histogram for \code{\link[decisionSupport]{mcSimulation}} using \code{\link[ggplot2]{geom_bar}}
#'
#' @author Issoufou Liman
#' @inheritParams get_hist_mcsimilation_data
#' @param ... Additional Arguments such as breaks, colorQuantile and colorProbability passed to get_hist_mcsimilation_data
#' @return A ggplot object
#' @seealso \code{\link[decisionSupportExtra]{ggplot_mc_dens}} \code{\link[decisionSupportExtra]{get_hist_mcsimilation_data}} \code{\link[decisionSupport]{mcSimulation}}
#' @export
ggplot_mc_hist <- function (x, ...) {
  UseMethod("ggplot_mc_hist", x)
}

#' @rdname ggplot_mc_hist
#' @export
ggplot_mc_hist.mcSimulation <- function(x, ...) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    bars <- get_hist_mcsimilation_data(x, ...)
    breaks <- sapply (bars, function(i) i$breaks, simplify = FALSE)
    bars <- sapply (bars, function(i) i$result, simplify = FALSE)
    id.var <- c("x", "y", 'color', 'color_equiv', 'quant')
    result <- reshape2::melt(bars, id.var = id.var)
    ggplot2::ggplot(data = result[order(result$L1, decreasing = T), ],
                    aes_string(x = 'x', y = 'y', fill = 'color')) +
      geom_col(stat = "identity")+
      scale_x_continuous(expand = c(0,0))+
      scale_y_continuous(expand = c(0,0))+
      scale_fill_identity(NULL, labels = result$color_equiv, breaks = result$color, guide = "legend", drop = FALSE)
  } else {
    stop("ggplot2 is required for the ggplot_mc_hist method.")
  }
}

#' @rdname ggplot_mc_hist
#' @export
ggplot_mc_hist.list <- ggplot_mc_hist.mcSimulation
