% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_descdist.R
\name{ggplot_descdist}
\alias{ggplot_descdist}
\title{ggplot Empirical distribution as in \code{\link[fitdistrplus]{descdist}}}
\usage{
ggplot_descdist(
  data,
  boot = 1000,
  obs.col = "darkblue",
  boot.col = "orange",
  title = "Cullen and Frey graph",
  subtitle = NULL,
  xlab = "square of skewness",
  ylab = "kurtosis",
  obs_geom_size = 4,
  boot_geom_size = 0.02,
  dist_geom_pts_size = 5,
  dist_geom_line_size = 0.6,
  axis_text_size = 12,
  axis_title_size = 12,
  plot_title_size = 20,
  plot_subtitle_size = 17,
  strip_text_size = 18,
  legend_text_size = 12
)
}
\arguments{
\item{data}{ A numeric vector.  }

\item{boot}{ If not \code{NULL}, \code{boot} values of skewness and kurtosis are plotted from bootstrap
 samples of \code{data}. \code{boot} must be fixed in this case to an integer above 10. }

\item{obs.col}{ Color used for the observed point on the skewness-kurtosis graph. }

\item{boot.col}{ Color used for bootstrap sample of points on the skewness-kurtosis graph. }

\item{title, subtitle}{Title and Subtitle}

\item{xlab, ylab}{These are respectively x and y labels.}

\item{obs_geom_size, boot_geom_size, dist_geom_pts_size}{The size of the geom_point to be used for the empirical distributoion (default to 4), bootstrapping (default to 0.02), theoritical distribution (default to 5), respectively.}

\item{dist_geom_line_size}{The size of the geom_line to be used for the empirical distributoion. The default is 0.6.}

\item{axis_text_size, axis_title_size, plot_title_size, plot_subtitle_size, strip_text_size, legend_text_size}{= 12,
Text size respectively corresponding to axis text (default to 12), axis title (default to 12), plot title (default to 20), subtitle (default to 17), strip text (default to 18), and legend (default to 12).}
}
\description{
Generate compact plot following the \code{\link[fitdistrplus]{descdist}} function as customizable ggplot.
}
\details{
see \code{\link[fitdistrplus]{descdist}}.
}
\examples{
ggplot_descdist(rbeta(100, shape1 = 0.05, shape2 = 1), boot = 500, obs.col = "blue",
boot.col = "yellow")
}
\references{
Marie Laure Delignette-Muller, Christophe Dutang (2015). fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software, 64(4), 1-34. http://www.jstatsoft.org/v64/i04/.
}
\seealso{
\code{\link[fitdistrplus]{descdist}}.
}
\author{
Issoufou Liman
}
