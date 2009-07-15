\name{pseudocpf}
\Rdversion{1.1}
\alias{pseudocpf}
\title{
  Pseudo values for the conditional probability function
}
\description{
  The function computes pseudo values and then fit a proportional-odds
  model to the conditional probability function using GEE
}
\usage{
pseudocpf(formula, data, id, subset, na.action, timepoints,
failcode = 1, ...)
}
\arguments{
  \item{formula}{A formula object, whose terms are on the right of a ~
    operator and the response, a \code{Hist} object, on the left}
  \item{data}{A data frame in which to interpret the formula}
  \item{id}{Individual patient id}
  \item{subset}{expression specifying that only a subset of the data set
    should be used}
  \item{na.action}{A missing data filter funtion applied to the
    model.frame, after any subset argument has been used. Default is
    options()$na.action}
  \item{timepoints}{Time points at which to compute the pseudo values}
  \item{failcode}{Integer that specifies which event is of interest}
  \item{\dots}{Other arguments for the \code{\link[geepack]{geese}}
    function}
}
\details{
  TO DO
}
\value{
  Returns an object of class \code{pseudocpf} containing the following
  components:
  \item{fit}{A \code{geese} object}
  \item{pseudo}{The pseudo values computed at the specified time points}
  \item{timepoints}{Same as in the function call}
  \item{call}{The matched call}
}
\references{
  TO DO
}
\author{
  Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}
}
\note{
  The function returns the pseudo values, therefore, you can fit the
  model you want, if necessary (to rephrase)
}
\seealso{
  \code{\link[geepack]{geese}}, \code{\link{summary.pseudocpf}}
}
\examples{
data(mgus)

cutoffs <- quantile(mgus$time, probs = seq(0, 1, 0.05))[-1]

### with fancy variance estimation
fit1 <- pseudocpf(Hist(time, ev) ~ age + creat, mgus, id = id,
                  timepoints = cutoffs, corstr = "independence",
                  scale.value = TRUE)
summary(fit1)

### with jackknife variance estimation
fit2 <- pseudocpf(Hist(time, ev) ~ age + creat, mgus, id = id,
                  timepoints = cutoffs, corstr = "independence",
                  scale.value = TRUE, jack = TRUE)
summary(fit2)
}
\keyword{survival}
\keyword{regression}
