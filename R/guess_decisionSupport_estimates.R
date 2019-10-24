#' guessing decisionSupport's estimates from imperfect information.
#'
#' Take some data along with other tuning parameters to return the formated estimates as returned by \code{\link[decisionSupport]{estimate}} to be used in the \code{\link[decisionSupport]{mcSimulation}}.

#' @author Issoufou Liman
#' @param data The base data or list of base data such that fun (data), or do.call (fun, data) will either return a data.frame or a numeric vector coercible to data.frame. see example.
#' @param fun Thefunction to be applied to data.
#' @param distr charater vector. The expected probability distribution function.
#' @param method Distribution fitting method (see method argument in \code{\link[fitdistrplus]{fitdist}}). default to "mge" (maximum goodness-of-fit estimation).
#' @param estimate_method character vector.	Method to be used for estimating the marginal distribution parameters as in \code{\link[decisionSupport]{estimate}}. If null, the default, method is set to "fit" (see \code{\link[decisionSupport]{estimate}}).
#' @param percentiles numeric vector of percentiles based on which to define a quantile function to be used for fitting distr with rriskDistributions package.
#' @param plot logical. Should the quantile function be ploted? default to FALSE
#' @param show.output Should the computation be verbose? default to FALSE
#' @return estimate object as returned by \code{\link[decisionSupport]{estimate}} or a list of thereof depending on the data provided.
#' @details In decisonSupport package, \code{\link[decisionSupport]{estimate}} assumes the case where hard data are not available and expert can agree on a set of estimated benchmarking (i.e. minimum, mediam, maximun) values and distributions based on which data can generated to support decision making under uncertainity. guess_decisionSupport_estimates () function provide additional ways to derive such estimates from the persepectives where some sort of hard data are available. The function is more useful when intermediate computations are needed to generate the desired the data. For example it can use the data generating function (fun argument) to construct intermediate outputs upon which it derives the recipies required by \code{\link[decisionSupport]{estimate}}. This could be useful for variables that difficult to be directly estimated by experts.
#' @seealso \code{\link[decisionSupport]{estimate}}.
#' @examples
#' ## - Suppose we have data on yield potential of different crops grown in a given area
#' ## distributed following a gamma distribution function.
#' ## - Suppose also we can estimates the proportion of biomass accumulation following the
#' ## crop development stages
#' ## - Suppose we also have an idea of the proportion of grains relative to the biomass
#' ## towards the harvest period.
#' ## Now we want to construct the decisionSupport estimates of the actual grain yield
#' ## from these.
#' ## generate some hypothetical data reporting yield potential of different crops varietie
#' potential_grain_yield <- rgamma(100, shape=1, rate = 1)
#' ## checking the distribution shape
#' hist(potential_grain_yield)
#' ## proportion of grain yield relative to biomass yield
#' harvest_index = c(0.2, 0.6)
#' ## relative biomass accumulation accross over time.
#' stage_ratio = c(initial_stage = 0.27, mid_stage = 0.68, development_stage = 1, late_stage = 0.86)
#' ## A function for making an informed guess of biomass yield at each stage
#' guess_biomass_yield <-  function(observed_grain_yield, harvest_index = c(0.2, 0.6), stage_ratio){
#'   harvest_index <- runif(1000, harvest_index[1], harvest_index[2])
#'   actu_bomass <- sapply(harvest_index, function(i){
#'     observed_grain_yield/i
#'   })
#'   actu_bomass <- sapply(stage_ratio, function (i){
#'     i * actu_bomass
#'   }, simplify = TRUE, USE.NAMES = TRUE)
#'   as.data.frame(na.omit(actu_bomass))
#' }
#'
#' ## Try it with gamma distribution and fit method
#' library (rriskDistributions)
#' fit_gamma <- guess_decisionSupport_estimates (data = list(potential_grain_yield,
#'   harvest_index = harvest_index, stage_ratio = stage_ratio),
#'   fun = guess_biomass_yield, distr = 'gamma', percentiles = c(0.025, 0.5, 0.975),
#'   plot = FALSE, show.output = FALSE)
#'
#'  ## Try it with gamma distribution and calculate method
#' calc_gamma <- guess_decisionSupport_estimates (data = list(potential_grain_yield,
#'   harvest_index = harvest_index, stage_ratio = stage_ratio), estimate_method = 'calculate',
#'   fun = guess_biomass_yield, distr = 'gamma', percentiles = c(0.025, 0.5, 0.975),
#'   plot = FALSE, show.output = FALSE)
#'
#'  ## Try it with normal distribution and calculate method
#' calc_norm <- guess_decisionSupport_estimates (data = list(potential_grain_yield,
#'   harvest_index = harvest_index, stage_ratio = stage_ratio), estimate_method = 'calculate',
#'   fun = guess_biomass_yield, distr = 'norm', percentiles = c(0.025, 0.5, 0.975),
#'   plot = FALSE, show.output = FALSE)
#'
#'  ## Try with a constant with a function
#'  calc_const_with_fun <- guess_decisionSupport_estimates(data = 1, fun = "sum")
#'
#'  ## Try with a constant without function
#'  calc_const_without_fun <- guess_decisionSupport_estimates(data = 100)
#'
#' ## Check the difference
#' calc_gamma; fit_gamma; calc_norm; calc_const_with_fun; calc_const_without_fun
#' @import rriskDistributions
#' @importFrom methods is
#' @importFrom fitdistrplus fitdist
#' @importFrom decisionSupport as.estimate
#' @importFrom graphics par
#' @export
guess_decisionSupport_estimates <- function(data, fun = NULL, distr = 'norm',
                                            method="mge", estimate_method = NULL,
                                            percentiles = c(0.025, 0.975),
                                            plot = FALSE, show.output = FALSE){
  default_par <- par(no.readonly = TRUE)
  if(!is.null(fun) & inherits(data, 'list')){
    data <- do.call(fun, data)
    if (!is(data, 'data.frame') & !is.numeric(data)){
      stop('fun must return a matrix or data.frame')
    }
  }
  if(is.numeric(data)|is.matrix(data)){
    data <- as.data.frame(data)
  }
  tmp <- 1:ncol(data)
  names(tmp) <- names(data)
  if(length(distr) == 1){
    distr = rep(distr, ncol(data))
  }
  fitted <- sapply(X = tmp, function (i){
    if((length(data[, i]) == 1) | (min(data[, i], na.rm = TRUE) == max(data[, i], na.rm = TRUE))){
      estimates <- rep(data[, i], 3)
      distr <- "const"
    } else {
      fitted <- data [, i]
      distr <- distr[i]
      fitted <- fitdist(data = fitted, distr = distr, method = method)$estimate
      q_dist <- paste0 ('q', distr)
      q_dist_args <- list(percentiles)
      q_dist_args <- c(q_dist_args, as.list(fitted))
      q <- do.call(q_dist, q_dist_args)
      fonction <- get(paste('get', distr, 'par', sep = '.'))
      dist_par <-  fonction(q = q, plot = plot, show.output = show.output)
      r_dist <- paste0 ('r', distr)
      estimates <- summary(do.call(r_dist, as.list(c(1000, dist_par))))[c(1, 3, 6)]
    }

    estimates <- c(estimates, distr)
    names(estimates) <- c('lower', 'median', 'upper', 'distribution')
    return (estimates)
  }, simplify = FALSE, USE.NAMES = TRUE)
  estimates <- sapply(names(fitted), function(i) fitted[[i]])
  estimates <- as.data.frame(t(estimates), stringsAsFactors = FALSE)
  if(is.null(estimate_method)){
    estimate_method <- rep('fit', nrow(estimates))
  } else if (length(estimate_method) == 1){
    estimate_method <- rep(estimate_method, nrow(estimates))
  } else if(length(estimate_method) != nrow(estimates)){
    stop(paste('A vector of distribution of length', nrow(estimates), 'was expected but only', length(estimate_method), 'were provided.'))
  }
  estimates$method <- estimate_method
  par(default_par)
  as.estimate(estimates)
}
