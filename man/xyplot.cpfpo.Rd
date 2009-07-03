\name{xyplot.cpfpo}
\Rdversion{1.1}
\alias{xyplot.cpfpo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
xyplot.cpfpo(x, data = NULL, conf.int = TRUE, level = 0.95, odds = TRUE, intercept = TRUE, ylab, xlab, lty = c(1, 3, 3), col = c(1, 1, 1), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{conf.int}{
%%     ~~Describe \code{conf.int} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{odds}{
%%     ~~Describe \code{odds} here~~
}
  \item{intercept}{
%%     ~~Describe \code{intercept} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, data = NULL, conf.int = TRUE, level = 0.95,
                         odds = TRUE, intercept = TRUE, ylab, xlab,
                         lty = c(1,3,3), col = c(1,1,1), ...) {
    if (!inherits(x, "cpfpo")) {
        stop("'x' must be of class 'cpfpo'")
    }
    if (missing(ylab)) {
        if (odds==TRUE)
            ylab <- "odds-ratio"
        else
            ylab <- "log-odds-ratio"
    }
    if (missing(xlab))
        xlab <- "time"
##     if (missing(title))
##         title <- colnames(x$alpha)
    ncov <- if (intercept) 1:dim(x$alpha)[2] else 2:dim(x$alpha)[2]
    dat <- lapply(ncov, function(i) {
        temp <- data.frame(coef = x$alpha[, i],
                           var = x$valpha[, i],
                           time = x$tis,
                           cov = colnames(x$alpha)[i])
        temp
    })
    dat <- do.call(rbind, dat)
    if (conf.int) {
        z <- qnorm(0.975)
        dat$lower <- with(dat, coef - z * var)
        dat$upper <- with(dat, coef + z * var)
        if (odds) {
            with(dat, upper <- exp(upper))
            with(dat, lower <- exp(lower))
            with(dat, coef <- exp(coef))
        }
        aa <- xyplot(coef + lower + upper ~ time | cov, dat, type = "s",
                     col = col, lty = lty, xlab = xlab, ylab = ylab, ...)
    }
    else {
        if (odds) {
            with(dat, coef <- exp(coef))
        }
        aa <- xyplot(coef ~ time | cov, dat, type = "s",
                     col = col, lty = lty, xlab = xlab, ylab = ylab, ...)
    }
    aa
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
