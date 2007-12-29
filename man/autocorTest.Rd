\name{AutocorTest}
\alias{AutocorTest}
\title{ Box-Ljung autocorrelation test }
\description{
  Ljung-Box test for autocorrelation 
}
\usage{
AutocorTest(x, lag=ceiling(log(length(x))),
            type=c("Ljung-Box", "Box-Pierce", "rank") )
}
\arguments{
  \item{x}{
    a numeric vector or a univariate time series 
  }
  \item{lag}{
    the statistic will be based on 'lag' autocorrelation coefficients.
    Tsay (p. 27-28) says, 'Simulation studies suggest that the choice of
    [lag = log(length(x))] provides better power performance.  This
    general rule needs modification in analysis of seasonal time series
    for which autocorrelations with lags at multiples of the seasonality
    are more important.'
  }
  \item{type}{
    which Box.test 'type' should be used?  Partial matching is used.

    The 'rank' alternative computes 'Ljung-Box' on rank(x);  see Burns
    (2002) and references therein.    

    NOTE:  The default 'Ljung-Box' type generally seems to be more
    accurate and popular than the earlier 'Box-Pierce', which is however
    the default for 'Box.test'. 
  }
}
\details{
  This is provided for compatibility with 'autocorTest' in the S-Plus
  script in Tsay (p. 30).  It is a wrapper for the R function
  'Box.test'. 
}
\source{
  \url{
    http://faculty.chicagogsb.edu/ruey.tsay/teaching/fts2
  }
}
\references{
  Ruey Tsay (2005) Analysis of Financial Time Series, 2nd ed. (Wiley)

  Patrick Burns (2002) 'Robustness of the Ljung-Box Test and its Rank
  Equivalent',
  \url{http://www.burns-stat.com/pages/Working/ljungbox.pdf}, accessed
  2007.12.29.  
}
\seealso{
  \code{\link[stats]{Box.test}} }
\examples{
data(m.ibm2697)
AutocorTest(m.ibm2697, 5)
}
\keyword{ts}

