\name{plot.cpfpo}
\alias{plot.cpfpo}
\title{Plot Method for a cpfpo Object}
\description{
  Displays the coefficients obtained when fitting the proportional-odds
  model for the conditional probability function, along with possible
  exponential transformation and pointwise confidence intervals.
}
\usage{
\S3method{plot}{cpfpo}(x, layout, conf.int = TRUE, level = 0.95,
                       odds = TRUE, intercept = TRUE,
                       ylab, xlab, title, lty = c(1, 3, 3),
                       col = c(1, 1, 1), ...)
}
\arguments{
  \item{x}{A \code{cpfpo} object}
  \item{layout}{A vector of integer specifying the layout of the
    plots. Works like \code{par(mfrow)}}
  \item{conf.int}{A logical specifying whether to draw pointwise
    confidence intervals. Default is TRUE}
  \item{level}{Level of the two-sided confidence interval. Default is 0.95}
  \item{odds}{A logical indicating whether or not display the
    exponential of the coefficients, \emph{i.e} the odds-ratios. Default
    is TRUE}
  \item{intercept}{A logical indicating whether or not plot the
    intercept. Default is TRUE}
  \item{ylab}{Label of the Y-axis. Default is "odds-ratio"}
  \item{xlab}{Label of the X-axis. Default is "time"}
  \item{title}{Titles of the plots. If missing, the names of the
    covariates --if given-- will be used}
  \item{lty}{Vector of line type}
  \item{col}{ector of colours}
  \item{\dots}{Further graphical parameters}
}
\value{
  No value returned
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{\code{\link{cpfpo}}}
\keyword{hplot}