#' @rdname get_hist_mcsimilation_data
#' @importFrom stats density
#' @export
get_dens_mcsimilation_data <- function(x, colorQuantile = c("GRAY48", "YELLOW", "ORANGE", "DARK GREEN",
    "ORANGE1", "YELLOW1", "GRAY49"), colorProbability = c(1, 0.95, 0.75, 0.55, 0.45, 0.25, 0.05)) {

    f <- function(x, colorQuantile, colorProbability) {
      if (length(colorQuantile) != length(colorProbability)){
        stop("colorQuantile should have the same length as colorProbability")
      }
      ids <- match(colorProbability, sort(colorProbability))
      colorProbability <- colorProbability[ids]
      colorQuantile <- colorQuantile[ids]
        dt <- as.data.frame(x$y)
        sapply(dt, function(i) {
            dens <- density(i)
            df <- data.frame(x = dens$x, y = dens$y)
            quantiles <- quantile(i, prob = colorProbability)
            df$quant <- factor(findInterval(df$x, quantiles[-length(quantiles)]))
            df$quant_x_breaks <- rep(NA, nrow(df))
            df$color_equiv <- rep(NA, nrow(df))
            df$color <- rep(NA, nrow(df))
            for (i in unique(df$quant)) {
                df[df$quant == i, "quant_x_breaks"] <- max(df[df$quant == i, "x"], na.rm = TRUE)
            }
            for (i in 1:length(unique(df$quant_x_breaks))) {
                df[df$quant_x_breaks == unique(df$quant_x_breaks)[i], "color_equiv"] <- colorProbability[i]
            }
            for (i in 1:length(unique(df$quant_x_breaks))) {
                df[df$quant_x_breaks == unique(df$quant_x_breaks)[i], "color"] <- colorQuantile[i]
            }
            df$quant <- paste0(df$color_equiv * 100, "%")
            return(df)
        }, simplify = FALSE, USE.NAMES = TRUE)
    }
    if ((inherits(x, "mcSimulation", which = TRUE) == 1)) {
        out <- f(x, colorQuantile, colorProbability)
    } else if ((inherits(x, "list", which = TRUE) == 1)) {
        out <- sapply(X = x, FUN = get_dens_mcsimilation_data, colorQuantile = colorQuantile, colorProbability = colorProbability,
            simplify = FALSE, USE.NAMES = TRUE)
    } else {
        stop(paste(deparse(substitute(x)), "must be of class mcSimulation, list of mcSimulation, or a list of arbitratry depth of mcSimulation objects."))
    }
}

#' Generic \code{\link[decisionSupport]{mcSimulation}} density plotting with ggplot
#'
#' generate density for \code{\link[decisionSupport]{mcSimulation}} using \code{\link[ggplot2]{geom_density}}
#'
#' @author Issoufou Liman
#' @inheritParams get_dens_mcsimilation_data
#' @param ... Additional Arguments such as colorQuantile and colorProbability passed to get_dens_mcsimilation_data
#' @return A ggplot object
#' @seealso \code{\link[decisionSupportExtra]{ggplot_mc_hist}} \code{\link[decisionSupportExtra]{get_dens_mcsimilation_data}} \code{\link[decisionSupport]{mcSimulation}}
#' @export
ggplot_mc_dens <- function(x, ...) {
    UseMethod("ggplot_mc_dens", x)
}

#' @rdname ggplot_mc_dens
#' @export
ggplot_mc_dens.mcSimulation <- function(x, ...) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
        bars <- get_dens_mcsimilation_data(x, ...)
        # bars <- sapply (bars, function(i) i$result, simplify = FALSE)
        id.var <- c("x", "y", "color", "color_equiv", "quant")
        result <- reshape2::melt(bars, id.var = id.var)
        ggplot(data = result[order(result$L1, decreasing = T), ], aes_string("x", "y", fill = "color")) +
            geom_density(stat = "identity") + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0,
            0)) + scale_fill_identity(NULL, labels = result$color_equiv, breaks = result$color, guide = "legend",
            drop = FALSE)
    } else {
        stop("ggplot2 is required for the ggplot_mc_hist method.")
    }
}

#' @rdname  ggplot_mc_dens
#' @export
ggplot_mc_dens.list <- ggplot_mc_dens.mcSimulation
