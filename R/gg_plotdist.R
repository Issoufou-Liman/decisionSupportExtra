#' ggplot empirical and theoretical distributions for non-censored data following the \code{\link[fitdistrplus]{descdist}} function.
#'
#' Generate compact plot of Bayesian network node states following the \code{\link[fitdistrplus]{descdist}} function as customizable ggplot.
#'
#' @author Issoufou Liman
#' @param fitted A list of \code{\link[fitdistrplus]{fitdist}} object
#' @param title character string to be used as plot title
#' @param hist_bar_size numeric size of histogram bars
#' @param dist_geom_pts_size,dist_geom_smooth_size,dist_geom_abline_size numeric. The size of the geom_point, smoothing line and abline, respectively.
#' @inheritParams ggplot_descdist
#' @seealso \code{\link[fitdistrplus]{plotdist}}.
#' @details see \code{\link[fitdistrplus]{plotdist}}.
#' @references
#' Marie Laure Delignette-Muller, Christophe Dutang (2015). fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software, 64(4), 1-34. http://www.jstatsoft.org/v64/i04/.
#' @examples
#' library (gRain)
#' library(bnlearn)
#' ## setting a bayesian network with gRain
#' Soil_type <- cptable (~Soil_type, values = c(0.05, 0.55, 0.4),
#' levels = c('Sandy', 'Loamy', 'Clayey'))
#' Manure_application <- cptable(~Manure_application, values = c(0.3, 0.7),
#' levels = c('FALSE', 'TRUE'))
#' Soil_water_holding_capacity_tmp <- make_gRain_CPT(
#'  parent_effects = list(c(0, 2.5, 3), c(0, 2)),
#'  parent_weights = c(2,1),
#'  b = 3,
#'  child_prior = c(0.2,0.5,0.3),
#'  child_states = c('Low', 'Medium', 'High'),
#'  parent_states = list(c('Sandy', 'Loamy', 'Clayey'), c('FALSE', 'TRUE'))
#' )
#' Soil_water_holding_capacity_values <- Soil_water_holding_capacity_tmp$values
#' Soil_water_holding_capacity_levels <- Soil_water_holding_capacity_tmp$levels
#' Soil_water_holding_capacity <- cptable (
#' ~Soil_water_holding_capacity|Soil_type:Manure_application,
#' values = Soil_water_holding_capacity_values,
#' levels = Soil_water_holding_capacity_levels)
#' ## Compile conditional probability tables
#' network <- compileCPT(list(Soil_type, Manure_application, Soil_water_holding_capacity))
#' ## Graphical Independence Network ####
#' network <- grain(network)
#' ## Use grain object (gRain package)
#' fitted <- fit_node_states_distr (bn = network,
#' node = "Soil_water_holding_capacity", gof="KS")
#' gg_plotdist(fitted, title = "Soil_water_holding_capacity")

#' ## converting the grain bayesian network to bn.fit
#' network_bn_fit <- as.bn.fit(network)
#' ## Use bn.fit object (bnlearn package)
#' fitted <- fit_node_states_distr (bn = network_bn_fit,
#' node = "Soil_water_holding_capacity", distr = c("beta", "beta", "beta"))
#' gg_plotdist(fitted, title = "Soil_water_holding_capacity")
#' @importFrom graphics hist
#' @importFrom stats ppoints
#' @importFrom grid grid.newpage textGrob gpar
#' @importFrom gridExtra grid.arrange
#' @export gg_plotdist
gg_plotdist <- function(fitted, title = NULL, hist_bar_size = 0.3, dist_geom_pts_size = 0.03,
                        dist_geom_smooth_size = 0.3, dist_geom_abline_size = 0.3,
                        axis_text_size = 9, axis_title_size = 9,
                        plot_title_size = 10, plot_subtitle_size = 8,
                        strip_text_size = 9){
  if (is.null(title)){
    title <- deparse(substitute(fitted))
  }
  title <- gsub('_', ' ', title)

  fmt_dcimals <- function(decimals=0){
    # return a function responpsible for formatting the
    # axis labels with a given number of decimals
    function(x) as.character(round(x,decimals))
  }

  my_theme <- theme_minimal() +
    theme(
      text = element_text(family = 'sans', size = 14, face = 'plain'),
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      panel.spacing=unit(0.075, "lines"),
      panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.75),
      axis.ticks = element_line(colour = 'black', size = 0.1),
      axis.text = element_text(size = axis_text_size),
      axis.title = element_text(size = axis_title_size),
      # legend.title = element_blank(),
      # legend.text = element_text(size = legend_text_size),
      # legend.position="top",
      # legend.justification = 'right',
      # legend.margin=margin(0, 0, 0, 0),
      # legend.box.margin=margin(-22, 0, -10, 0),
      strip.text = element_text(size = strip_text_size),
      # strip.background = element_rect(color = "gray", size = 0.075),
      strip.background = element_rect(size = 0.075, fill='lightgoldenrodyellow'),

      plot.title = element_text(size=plot_subtitle_size, face="italic", color="yellow3"),
      # plot.subtitle = element_text(size=plot_subtitle_size, face="italic", color="blue"),
      plot.background = element_rect(size=0.13,linetype="solid", color="black")
    )

  ddistname <- 'dbeta'
  qdistname <- 'qbeta'
  pdistname <- 'pbeta'

  f1 <- function(x) {
    h <- hist(x[['data']], plot=FALSE)
    data <- data.frame(x=x[['data']])
    xhist <- seq(min(h$breaks), max(h$breaks), length=nrow(data))
    yhist <- do.call(ddistname, c(list(x=xhist), as.list(x[['estimate']])))
    if(length(yhist) != length(xhist)){
      stop("problem when computing densities.")
    }
    ymax <- ifelse(is.finite(max(data$yhist)), max(max(h$density), max(data$yhist)), max(h$density))
    list(data.frame(data, xhist, yhist), ymax)
  }
  data <- sapply(X=fitted, FUN = f1, USE.NAMES = TRUE, simplify = FALSE)
  ymax <- max(sapply(data, '[[', 2), na.rm = T)
  data <- sapply(data, '[[', 1, USE.NAMES = T, simplify = F)
  data <- sapply(X=1:length(data), function(i){
    states <- rep(names(data)[i], nrow(data[[i]]))
    data.frame(data[[i]], states = states)
  }, USE.NAMES = TRUE, simplify = FALSE)
  data <- do.call("rbind", data)
  # # PLOT 1 - plot of empirical and  theoretical density
  p1 <- ggplot(data = data, aes_string(x = "x")) +
    geom_histogram(aes_string(y = "..density.."),
                   bins = 10,
                   breaks=hist(data$x, plot=FALSE)$breaks,
                   size=hist_bar_size,
                   col="black",
                   fill="white",
                   alpha = .2) +
    # Add of theoretical density
    geom_line(aes_string(x = "xhist", y = "yhist"), size=dist_geom_smooth_size, colour='red')+
    coord_cartesian(ylim = c(0, ymax))+
    facet_grid(.~states)+
    my_theme+
    scale_x_continuous(labels = fmt_dcimals(1), breaks = scales::pretty_breaks(n=3), expand = c(0, 0.015)) +
    scale_y_continuous(labels = fmt_dcimals(1), breaks = scales::pretty_breaks(n=4), expand = c(0, 0.015)) +

    labs(title=paste("Empirical and theoretical dens."), x="Data", y = 'Density')
  # plot 2 - plot of the qqplot
  f2 <- function(x){
    s <- sort(x[['data']])
    n <- length(x[['data']])
    obsp <- ppoints(s)
    theoq <- do.call(qdistname, c(list(p=obsp), as.list(x[['estimate']])))
    if(length(theoq) != length(obsp))
      stop("problem when computing quantities.")
    data.frame(theoq=theoq, s=s)
  }
  data <- sapply(X=fitted, FUN = f2, USE.NAMES = TRUE, simplify = FALSE)
  data <- sapply(X=1:length(data), function(i){
    states <- rep(names(data)[i], nrow(data[[i]]))
    data.frame(data[[i]], states = states)
  }, USE.NAMES = TRUE, simplify = FALSE)
  data <- do.call("rbind", data)

  p2 <- ggplot(data = data, aes_string ("theoq", "s"))+
    geom_point(shape=".", size=dist_geom_pts_size, fill='white', color='black')+
    geom_abline(intercept = 0, slope = 1, size=dist_geom_abline_size, alpha = 0.5)+
    labs(title = " Q-Q plot",
         x="Theoretical quantiles",
         y="Empirical quantiles")+
    facet_grid(.~states)+
    scale_x_continuous(labels = fmt_dcimals(1), breaks = scales::pretty_breaks(n=3), expand = c(0, 0.015)) +
    scale_y_continuous(labels = fmt_dcimals(1), breaks = scales::pretty_breaks(n=4), expand = c(0, 0.015)) +

    my_theme
  # PLOT 3 - plot of the cumulative probability distributions

  f3 <- function(x) {
    s <- sort(x[['data']])
    n <- length(x[['data']])
    obsp <- ppoints(s)
    h <- hist(x[['data']], plot=FALSE)
    xmin <- h$breaks[1]
    xmax <- h$breaks[length(h$breaks)]
    if(length(s) != length(obsp)){
      stop("problem when computing probabilities.")
    }
    sfin <- seq(xmin, xmax, by=(xmax-xmin)/100)
    theopfin <- do.call(pdistname, c(list(q=sfin), x[['estimate']]))
    list(data.frame(s=s, obsp = obsp), data.frame(theopfin=theopfin, sfin), xlim=c(xmin, xmax))
  }
  data <- sapply(X=fitted, FUN = f3, USE.NAMES = TRUE, simplify = FALSE)
  data2 <- sapply(data, '[[', 2, USE.NAMES = T, simplify = F)
  data2 <- sapply(X=1:length(data2), function(i){
    states <- rep(names(data2)[i], nrow(data2[[i]]))
    data.frame(data2[[i]], states = states)
  }, USE.NAMES = TRUE, simplify = FALSE)
  xlim <- sapply(data, '[[', 3)
  xlim <- c(min(xlim), max(xlim))
  data2 <- do.call("rbind", data2)

  data <- sapply(data, '[[', 1, USE.NAMES = T, simplify = F)
  data <- sapply(X=1:length(data), function(i){
    states <- rep(names(data)[i], nrow(data[[i]]))
    data.frame(data[[i]], states = states)
  }, USE.NAMES = TRUE, simplify = FALSE)
  data <- do.call("rbind", data)

  p3 <- ggplot(data = data, aes_string("s", "obsp"))+
    geom_point(shape=".", size=dist_geom_pts_size, fill='white', color='black')+
    geom_line(data = data2, aes_string("sfin", "theopfin"), size=dist_geom_smooth_size, col="red" )+
    facet_grid(.~states)+
    coord_cartesian(xlim=xlim)+

    labs(title = paste("Empirical and theoretical CDFs"),
         x="Data",
         y="CDF")+
    scale_x_continuous(labels = fmt_dcimals(1), breaks = scales::pretty_breaks(n=3), expand = c(0, 0.015)) +
    scale_y_continuous(labels = fmt_dcimals(1), breaks = scales::pretty_breaks(n=4), expand = c(0, 0.015)) +

    my_theme
  # PLOT 4 - plot of the ppplot
  f4 <- function(x){
    s <- sort(x[['data']])
    obsp <- ppoints(s)
    theop <- do.call(pdistname, c(list(q=s), x[['estimate']]))
    if(length(theop) != length(obsp)){
      stop("problem when computing probabilities.")
    }
    data.frame(theop = theop, obsp = obsp)
  }
  data <- sapply(X=fitted, FUN = f4, USE.NAMES = TRUE, simplify = FALSE)
  data <- sapply(X=1:length(data), function(i){
    states <- rep(names(data)[i], nrow(data[[i]]))
    data.frame(data[[i]], states = states)
  }, USE.NAMES = TRUE, simplify = FALSE)
  data <- do.call("rbind", data)

  p4 <- ggplot(data = data, aes_string("theop", "obsp"))+
    geom_point(shape=".", size=dist_geom_pts_size, fill='white', color='black')+
    geom_abline(intercept = 0, slope = 1, size=dist_geom_abline_size, alpha = 0.5)+
    facet_grid(.~states)+

    labs(title = "P-P plot",
         x="Theoretical probabilities",
         y="Empirical probabilities")+
    scale_x_continuous(labels = fmt_dcimals(1), breaks = scales::pretty_breaks(n=3), expand = c(0, 0.015)) +
    scale_y_continuous(labels = fmt_dcimals(1), breaks = scales::pretty_breaks(n=4), expand = c(0, 0.015)) +

    my_theme

  grid.newpage()
  g <- grid.arrange(gridExtra::arrangeGrob(p1,p2,p3,p4, nrow=2),
                    ncol=1, top=textGrob(title, x=0, hjust=0, gp=gpar(fontsize=plot_title_size, fontfamily='sans', fontface = 'bold', col="grey")))
}
