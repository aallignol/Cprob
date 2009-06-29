\name{mgus}
\docType{data}
\alias{mgus}
\title{Monoclonal Gammopathy of Undetermined Significance}
\description{
  All 241 patients diagnosed with monoclonal gammopathy of undetermined
  significance at the Mayo Clinic before Jan. 1, 1971, and followed
  forward through 1992. The interest lies in the possible transformation
  of mgus towards a cancer of the plasma cells, death from other causes
  acting as competing risks.
}
\usage{
data(mgus)
}
\format{
  \item{id}{Patient identification number}
  \item{time}{Event time}
  \item{ev}{Event type. 0=censored, 1=cancer of the plasma cells, 2=other}
  \item{age}{Age at diagnostic of mgus}
  \item{sex}{Sex. 1=male, 2=female}
  \item{y.diag}{Calendar year of diagnosis}
  \item{albu}{Albumine level at mgus diagnosis}
  \item{creat}{Serum creatinine level at mgus diagnosis}
  \item{Hb}{Hemoglobine level at mgus diagnosis}
  \item{size}{Size of the monoclonal protein peak at mgus diagnosis}
}
\source{
  T.M. Thernean and P.M. Grambsch (2001). \emph{Modelling Survival Data:
    Extending the Cox Model}. Springer, New York.
  }
\examples{data(mgus)}
\keyword{datasets}