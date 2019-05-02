#' ggplot Empirical distribution as in \code{\link[fitdistrplus]{descdist}}
#'
#' Generate compact plot following the \code{\link[fitdistrplus]{descdist}} function as customizable ggplot.
#'
#' @author Issoufou Liman
#' @inheritParams fitdistrplus::descdist
#' @param title,subtitle Title and Subtitle
#' @param xlab,ylab These are respectively x and y labels.
#' @param obs_geom_size,boot_geom_size,dist_geom_pts_size The size of the geom_point to be used for the empirical distributoion (default to 4), bootstrapping (default to 0.02), theoritical distribution (default to 5), respectively.
#' @param dist_geom_line_size The size of the geom_line to be used for the empirical distributoion. The default is 0.6.
#' @param axis_text_size,axis_title_size,plot_title_size,plot_subtitle_size,strip_text_size,legend_text_size = 12,
#' Text size respectively corresponding to axis text (default to 12), axis title (default to 12), plot title (default to 20), subtitle (default to 17), strip text (default to 18), and legend (default to 12).
#' @seealso \code{\link[fitdistrplus]{descdist}}.
#' @details see \code{\link[fitdistrplus]{descdist}}.
#' @references
#' Marie Laure Delignette-Muller, Christophe Dutang (2015). fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software, 64(4), 1-34. http://www.jstatsoft.org/v64/i04/.
#' @examples
#' ggplot_descdist(rbeta(100, shape1 = 0.05, shape2 = 1), boot = 500, obs.col = "blue",
#' boot.col = "yellow")
#' @importFrom stats median sd
#' @importFrom scales rescale_none
#' @importFrom reshape2 melt
#' @export ggplot_descdist
ggplot_descdist <- function(data, boot = 1000, obs.col = "darkblue", boot.col = "orange",
                            title = "Cullen and Frey graph", subtitle = NULL,
                            xlab = "square of skewness", ylab = "kurtosis",
                            obs_geom_size = 4, boot_geom_size = 0.02, dist_geom_pts_size = 5,
                            dist_geom_line_size = 0.6, axis_text_size = 12, axis_title_size = 12,
                            plot_title_size = 20, plot_subtitle_size = 17,strip_text_size = 18,
                            legend_text_size = 12){

  # data <- ifelse(is.data.frame(data), data, as.data.frame(data)) # not sure why it is not working
  if(!is.data.frame(data)) data <- as.data.frame(data)
  plot_lims <- sapply(data, function(i){
    ggbuild_descdist_data(i, boot = boot, graph = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)

  fit_data <- lapply(plot_lims, '[[', 1)
  plot_lims <- lapply(plot_lims, '[[', 2)
  yax <- unique(unlist(lapply(plot_lims, '[[', 'yax')))
  ymax <- max(unlist(lapply(plot_lims, '[[', 'ymax')))
  xmax <- max(unlist(lapply(plot_lims, '[[', 'xmax')))


  # names_plot_lims <- as.character(sapply(plot_lims, names))
  # plot_lims <- unlist(plot_lims, use.names = TRUE)
  # names(plot_lims) <- names_plot_lims
  # plot_lims

  fit_data <- melt(fit_data, id.var=c("x", "y"))
  fit_data$group <- sapply(fit_data$L2, function(i){
    check <- unique(fit_data$L2)
    which(check == i)
  })

  my_theme <- theme_minimal() +
    theme(
      text = element_text(family = 'sans', size = 16, face = 'plain'),
      #panel.grid.major = element_blank(),
      #panel.grid.minor = element_blank(),
      panel.spacing=unit(0.075, "lines"),
      panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.75),
      axis.ticks = element_line(colour = 'black', size = 0.075),
      axis.text = element_text(size = axis_text_size),
      axis.title = element_text(size = axis_title_size),
      # legend.title = element_blank(),
      legend.text = element_text(size = legend_text_size),
      legend.position="top",
      legend.justification = 'right',
      legend.margin=margin(0, 0, 0, 0),
      legend.box.margin=margin(-22, 0, -10, 0),
      strip.text = element_text(size = strip_text_size),
      # strip.background = element_rect(color = "gray", size = 0.075),
      strip.background = element_rect(size = 0.075, fill='lightgoldenrodyellow'),

      plot.title = element_text(size=plot_title_size, color = 'grey'),
      plot.subtitle = element_text(size=plot_subtitle_size, face="italic", color="yellow3"),
      plot.background = element_rect(size=0.13,linetype="solid", color="black")
    )
  # fit_data$L1 <- factor(fit_data$L1, levels = unique(fit_data$L1))
  ggplot() +
    geom_polygon(data = fit_data[fit_data$L2 == 'beta_dist', ],
                 aes(x, y, group = L1, alpha = factor("beta"))) +
    geom_line(data = fit_data[fit_data$L2 == 'lnorm_dist', ],
              aes(x, y, linetype = 'lognormal'), size = dist_geom_line_size)+
    geom_line(data = fit_data[fit_data$L2 == 'gamma_dist', ],
              aes(x, y, linetype = 'gamma'), size = dist_geom_line_size)+

    geom_point(data = fit_data[fit_data$L2 == 'boot_data', ],
               aes(x, y, color = "Bootstrapped values"), size = boot_geom_size)+
    geom_point(data = fit_data[fit_data$L2 == 'observed_dist', ],
               aes(x, y, color = "Observation"), size = obs_geom_size)+

    # geom_point(data = fit_data[fit_data$L2 == 'skew_kurto_data', ],
    #            aes(x, y, shape = 'a'), colour = 'black', size = 5)+

    geom_point(data = fit_data[fit_data$L2 == 'norm_dist', ],
               aes(x, y, shape = "normal"), size = dist_geom_pts_size)+
    geom_point(data = fit_data[fit_data$L2 == 'unif_dist', ],
               aes(x, y, shape = "uniform"), size = dist_geom_pts_size)+
    geom_point(data = fit_data[fit_data$L2 == 'exp_dist', ],
               aes(x, y, shape = "exponential"), size = dist_geom_pts_size)+
    geom_point(data = fit_data[fit_data$L2 == 'logistic_dist', ],
               aes(x, y, shape = "logistic"), size = dist_geom_pts_size)+


    scale_colour_manual(values=c(boot.col, obs.col)) +
    scale_linetype_manual(values=c('dashed', 'dotted'))+
    scale_alpha_manual(values = 0.3) +
    scale_shape_manual(values = c(8, 2, 7, 3))+
    # scale_size_identity()+ # this would remove cause the legend to not show if size was supplied as aes

    guides(color = guide_legend(title = NULL, order = 1, ncol = 1,
                                override.aes = list(color = c(boot.col, obs.col))),
           alpha = guide_legend(title = NULL, order = 3),
           shape = guide_legend(title = NULL, order = 2, ncol = 2,
                                override.aes = list(shape = c(8, 2, 7, 3))),
           linetype = guide_legend(title = "(Weibull is close to gamma and lognormal)",
                                   title.position = "bottom",
                                   title.theme = element_text(size = legend_text_size-1),
                                   order = 4, ncol = 1,
                                   override.aes = list(linetype = c('dashed', 'dotted'))))+

    facet_wrap(.~L1)+
    labs(title = title, subtitle = subtitle,
         x = xlab,
         y = ylab)+
    my_theme +
    scale_x_continuous(breaks = 0:ymax, limits = c(0,xmax), oob = rescale_none)+
    scale_y_continuous(breaks = as.numeric(yax), labels = rev(yax), limits = c(0, ymax), oob = rescale_none)
}
