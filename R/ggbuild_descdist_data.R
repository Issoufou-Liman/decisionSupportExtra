#' Format \code{\link[fitdistrplus]{descdist}} data to ggplot2
#'
#' Generate a list of \code{\link[fitdistrplus]{descdist}} outputs to be understood by ggplot2.
#'
#' @author Issoufou Liman
#' @inheritParams fitdistrplus::descdist
#' @param title,subtitle Title and Subtitle
#' @seealso \code{\link[fitdistrplus]{descdist}}.
#' @details see \code{\link[fitdistrplus]{descdist}}.
#' @references
#' Marie Laure Delignette-Muller, Christophe Dutang (2015). fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software, 64(4), 1-34. http://www.jstatsoft.org/v64/i04/.
#' @examples
#' ggbuild_descdist_data(rbeta(100, shape1 = 0.05, shape2 = 1), boot = 500, obs.col = 'blue',
#' boot.col = 'yellow')
#' @importFrom stats median sd
#' @export ggbuild_descdist_data
ggbuild_descdist_data <- function(data, discrete = FALSE, boot = NULL, method = "unbiased", graph = TRUE, 
    title = "Cullen and Frey graph", subtitle = NULL, obs.col = "darkblue", boot.col = "orange") {
    if (missing(data) || !is.vector(data, mode = "numeric")) 
        stop("data must be a numeric vector")
    if (length(data) < 4) 
        stop("data must be a numeric vector containing at least four values")
    moment <- function(data, k) {
        m1 <- mean(data)
        return(sum((data - m1)^k)/length(data))
    }
    if (method == "unbiased") {
        skewness <- function(data) {
            # unbiased estimation (Fisher 1930)
            sd <- sqrt(moment(data, 2))
            n <- length(data)
            gamma1 <- moment(data, 3)/sd^3
            unbiased.skewness <- sqrt(n * (n - 1)) * gamma1/(n - 2)
            return(unbiased.skewness)
        }
        kurtosis <- function(data) {
            # unbiased estimation (Fisher 1930)
            n <- length(data)
            var <- moment(data, 2)
            gamma2 <- moment(data, 4)/var^2
            unbiased.kurtosis <- (n - 1)/((n - 2) * (n - 3)) * ((n + 1) * gamma2 - 3 * (n - 1)) + 3
            return(unbiased.kurtosis)
        }
        standdev <- function(data) {
            sd(data)
        }
    } else if (method == "sample") {
        skewness <- function(data) {
            sd <- sqrt(moment(data, 2))
            return(moment(data, 3)/sd^3)
        }
        kurtosis <- function(data) {
            var <- moment(data, 2)
            return(moment(data, 4)/var^2)
        }
        standdev <- function(data) {
            sqrt(moment(data, 2))
        }
    } else stop("The only possible value for the argument method are 'unbiased' or 'sample'")
    
    res <- list(min = min(data), max = max(data), median = median(data), mean = mean(data), sd = standdev(data), 
        skewness = skewness(data), kurtosis = kurtosis(data), method = method)
    
    
    skewdata <- res$skewness
    kurtdata <- res$kurtosis
    dist_graph <- function() {
        # Cullen and Frey graph
        if (graph) {
            # bootstrap sample for observed distribution and computation of kurtmax from this sample
            if (!is.null(boot)) {
                if (!is.numeric(boot) || boot < 10) {
                  stop("boot must be NULL or a integer above 10")
                }
                n <- length(data)
                
                databoot <- matrix(sample(data, size = n * boot, replace = TRUE), nrow = n, ncol = boot)
                s2boot <- sapply(1:boot, function(iter) skewness(databoot[, iter])^2)
                kurtboot <- sapply(1:boot, function(iter) kurtosis(databoot[, iter]))
                
                kurtmax <- max(10, ceiling(max(kurtboot)))
                xmax <- max(4, ceiling(max(s2boot)))
            } else {
                kurtmax <- max(10, ceiling(kurtdata))
                xmax <- max(4, ceiling(skewdata^2))
            }
            
            ymax <- kurtmax - 1
            yax <- as.character(kurtmax - 0:ymax)
            
            par_skew_kurto_data = list(xmax = xmax, ymax = ymax, yax = yax)
            
            skew_kurto_data <- data.frame(x = (skewdata^2), y = (kurtmax - kurtdata))
            data_list <- list(skew_kurto_data = skew_kurto_data)
            if (!discrete) {
                # beta dist
                p <- exp(-100)
                lq <- seq(-100, 100, 0.1)
                q <- exp(lq)
                s2a <- (4 * (q - p)^2 * (p + q + 1))/((p + q + 2)^2 * p * q)
                ya <- kurtmax - (3 * (p + q + 1) * (p * q * (p + q - 6) + 2 * (p + q)^2)/(p * q * (p + 
                  q + 2) * (p + q + 3)))
                p <- exp(100)
                lq <- seq(-100, 100, 0.1)
                q <- exp(lq)
                s2b <- (4 * (q - p)^2 * (p + q + 1))/((p + q + 2)^2 * p * q)
                yb <- kurtmax - (3 * (p + q + 1) * (p * q * (p + q - 6) + 2 * (p + q)^2)/(p * q * (p + 
                  q + 2) * (p + q + 3)))
                s2 <- c(s2a, s2b)
                y <- c(ya, yb)
                beta_dist <- data.frame(x = s2, y = y)
                data_list$beta_dist <- beta_dist
                # gamma dist
                lshape <- seq(-100, 100, 0.1)
                shape <- exp(lshape)
                s2 <- 4/shape
                y <- kurtmax - (3 + 6/shape)
                gamma_dist <- data.frame(x = s2, y = y)
                data_list$gamma_dist <- gamma_dist
                # log-normal dist
                lshape <- seq(-100, 100, 0.1)
                shape <- exp(lshape)
                es2 <- exp(shape^2)
                s2 <- (es2 + 2)^2 * (es2 - 1)
                y <- kurtmax - (es2^4 + 2 * es2^3 + 3 * es2^2 - 3)
                lnorm_dist <- data.frame(x = s2, y = y)
                data_list$lnorm_dist <- lnorm_dist
            } else {
                # negative binomial dist
                p <- exp(-10)
                lr <- seq(-100, 100, 0.1)
                r <- exp(lr)
                s2a <- (2 - p)^2/(r * (1 - p))
                ya <- kurtmax - (3 + 6/r + p^2/(r * (1 - p)))
                p <- 1 - exp(-10)
                lr <- seq(100, -100, -0.1)
                r <- exp(lr)
                s2b <- (2 - p)^2/(r * (1 - p))
                yb <- kurtmax - (3 + 6/r + p^2/(r * (1 - p)))
                s2 <- c(s2a, s2b)
                y <- c(ya, yb)
                negbin_dist <- data.frame(x = s2, y = y)
                data_list$negbin_dist <- negbin_dist
                
                # poisson dist
                llambda <- seq(-100, 100, 0.1)
                lambda <- exp(llambda)
                s2 <- 1/lambda
                y <- kurtmax - (3 + 1/lambda)
                poisson_dist <- data.frame(x = s2, y = y)
                data_list$poisson_dist <- poisson_dist
            }
            # bootstrap sample for observed distribution
            if (!is.null(boot)) {
                boot_data <- data.frame(x = s2boot, y = kurtmax - kurtboot)
                data_list$boot_data <- boot_data
            }
            # observed dist
            observed_dist <- data.frame(x = skewness(data)^2, y = kurtmax - kurtosis(data))
            data_list$observed_dist <- observed_dist
            # normal dist
            norm_dist <- data.frame(x = 0, y = kurtmax - 3)
            data_list$norm_dist <- norm_dist
            # uniform dist
            unif_dist <- data.frame(x = 0, y = kurtmax - 9/5)
            data_list$unif_dist <- unif_dist
            # exponential dist
            exp_dist <- data.frame(x = 2^2, y = kurtmax - 9)
            data_list$exp_dist <- exp_dist
            # logistic dist
            logistic_dist <- data.frame(x = 0, y = kurtmax - 4.2)
            data_list$logistic_dist <- logistic_dist
        }
        data_list <- list(data_list = data_list, par_skew_kurto_data = par_skew_kurto_data)
    }
    data_list <- dist_graph()
    data_list
}
