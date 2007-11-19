\name{autocorTest}
\alias{autocorTest}
\title{ Box-Ljung autocorrelation test }
\description{
  Ljung-Box test for autocorrelation 
}
\usage{
autocorTest(x, lag=log(length(x)))
}
\arguments{

  \item{x}{
    a numeric vector or a univariate time series 
  }  
  \item{lag}{
    the statistic will be based on 'lag' autocorrelation coefficients.
    Tsay (p. 27-28) says, "Simulation studies suggest that the choice of
    [lag = log(length(x))] provides better power performance.  This
    general rule needs modification in analysis of seasonal time series
    for which autocorrelations with lags at multiples of the seasonality
    are more important."  
  }

}
\details{
  This is provided for compatibility with the S-Plus script in Tsay
  (p. 30).  
}
\source{
  \url{
    http://faculty.chicagogsb.edu/ruey.tsay/teaching/fts2
  }
}
\references{
  Ruey Tsay (2005) Analysis of Financial Time Series, 2nd ed. (Wiley)
}
\seealso{
  \code{\link[stats]{BoxTest}} }
\examples{
data(m.ibm2697)
autocorTest(m.ibm2697, 5)
}
\keyword{ts}

