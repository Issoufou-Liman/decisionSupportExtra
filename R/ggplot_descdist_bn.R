#' ggplot the posterior distribution of a Bayesian network node as in \code{\link[fitdistrplus]{descdist}}
#'
#' Generate compact plot of Bayesian network node states following the \code{\link[fitdistrplus]{descdist}} function as customizable ggplot.
#'
#' @author Issoufou Liman
#' @inheritParams sample_cpdist
#' @inheritParams ggplot_descdist
#' @seealso \code{\link[fitdistrplus]{descdist}}.
#' @details see \code{\link[fitdistrplus]{descdist}}.
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
#' ggplot_descdist_bn (bn = network, node = 'Soil_water_holding_capacity')
#'
#' ## converting the grain bayesian network to bn.fit
#' network_bn_fit <- as.bn.fit(network)
#' ## Use bn.fit object (bnlearn package)
#' ggplot_descdist_bn (bn = network, node = 'Soil_water_holding_capacity')
#' @export ggplot_descdist_bn
ggplot_descdist_bn <- function(bn, node, boot = 1000, obs.col = "darkblue", boot.col = "orange", title = "Cullen and Frey graph",
    subtitle = node, xlab = "square of skewness", ylab = "kurtosis", obs_geom_size = 4, boot_geom_size = 0.02,
    dist_geom_pts_size = 5, dist_geom_line_size = 0.6, axis_text_size = 12, axis_title_size = 12, plot_title_size = 20,
    plot_subtitle_size = 17, strip_text_size = 18, legend_text_size = 12, evidence = NULL, n_generation = NULL, include_relatives = TRUE, n_run=1000) {
    bn <- check_bn(bn, include_cpt = TRUE)
    subtitle <- gsub(pattern = "_", " ", subtitle)
    data <- sample_cpdist(bn, node, op = "proba", evidence = evidence, n_generation = n_generation, include_relatives = include_relatives, n_run = n_run)
    data <- data$posterior
    ggplot_descdist(data = data, boot = boot, obs.col = obs.col, boot.col = boot.col, title = title, subtitle = subtitle,
        xlab = xlab, ylab = ylab, obs_geom_size = obs_geom_size, boot_geom_size = boot_geom_size, dist_geom_pts_size = dist_geom_pts_size,
        dist_geom_line_size = dist_geom_line_size, axis_text_size = axis_text_size, axis_title_size = axis_title_size,
        plot_title_size = plot_title_size, plot_subtitle_size = plot_subtitle_size, strip_text_size = strip_text_size,
        legend_text_size = legend_text_size)
}
