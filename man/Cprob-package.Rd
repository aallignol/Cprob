\name{Cprob-package}
\alias{Cprob-package}
\alias{Cprob}
\docType{package}
\title{
Conditional Probability Function of a Competing Event
}
\description{
Estimates the conditional probability function of a competing event, and fit, using tpr, a proportional-odds model.
}
\details{
\tabular{ll}{
Package: \tab Cprob\cr
Version: \tab 0.7-0\cr
Depends: \tab prodlim, tpr\cr
License: \tab GPL >= 2\cr
Packaged: \tab Thu Oct 23 18:27:30 2008; arthur\cr
Built: \tab R 2.7.1; ; 2008-10-23 18:27:41; unix\cr
}

Index:
\preformatted{
Cprob-package           Conditional probability function of a competing
                        event
cpf                     Conditional Probability Function of a Competing
                        Event
cpfpo                   Proportional-odds Model for the Conditional
                        Probability Function
mgus                    Monoclonal Gammopathy of Undetermined
                        Significance
plot.cpf                Plot pethod for cpf objects
plot.cpfpo              Plot Method for a cpfpo Object
predict.cpf             Conditional Probability Estimates at chosen
                        timepoints
print.cpf               Print method for a cpf object
print.cpfpo             Print Method for cpfpo objects
summary.cpf             Summary method for cpf
}
\code{cpf} computes the conditional probability estimates, and return a
\code{cpf} object with appropriate \code{print}, \code{summary},
\code{plot} and \code{predict} methods.

The \code{cpfpo} function fits, using \pkg{tpr}, a proportional-odds
model to the conditional probability function. 
}
\author{
Arthur Allignol

Maintainer: Arthur Allignol <arthur.allignol@fdm.uni-freiburg.de>
}
\references{
  J.P. Fine, J. Yan and M.R. Kosorok (2004). Temporal Process
  Regression, \emph{Biometrika}, 91(3):683-703.

  M.S. Pepe and M. Mori, Kaplan-Meier, marginal or conditional
  probability curves in summarizing competing risks failure time data?
  \emph{Statistics in Medicine}, 12(8):737--751.
}
\keyword{ package }
\seealso{
  \code{\link[prodlim]{prodlim}}, \code{\link[prodlim]{Hist}},
  \code{\link[tpr]{tpr}}
}
\examples{
data(mgus)
mgus$A <- ifelse(mgus$age < 64, 0, 1)

fit <- cpf(Hist(time, ev)~A, mgus)

plot(fit, curvlab = c("Age < 64", "Age >= 64"),
     main = "Conditional Probability of Cancer", xlab = "Years")

## fit the model for 2 covariates
fit.cpfpo <- cpfpo(Hist(time, ev)~factor(A) + creat,
                   data = mgus, tis=seq(10, 30, 0.3),
                   w=rep(1,67))
## and plot the odds-ratios
plot(fit.cpfpo, layout = c(1, 3))
}
