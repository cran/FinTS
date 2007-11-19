\name{Unitroot}
\alias{Unitroot}
\alias{summary.fHTEST}
\title{ unit root tests }
\description{
  Test for a unit root, comparable to 'unitroot' from S-PLUS Finmetrics
  used in the examples on pp. 70-72 of Tsay (2005).  

  NOTE:  This help page is written without access to S-PLUS Finmetrics,
  and functionality beyond that in those two examples could change in
  the future.
%  Rd files with unknown encoding: ??????????
}
\usage{
Unitroot(x, trend=c("c", "nc", "ct"), method=c('adf', 'McKinnon'),
        lags=2)
\method{summary}{fHTEST}(object, ...) 
}
\arguments{
  \item{x}{
    a numvariate time series or numeric vector 
  }
  \item{trend}{
    a character string describing the type of the unit root
    regression. Valid choices are '"nc"' for a regression  with no
    intercept (constant) nor time trend, and '"c"'  for a regression
    with an intercept (constant) but no time  trend, '"ct"' for a
    regression with an intercept (constant) and a time trend. 
  }
  \item{method}{
    character string, 'adf' to use 'adfTest' and "McKinnon" to use
    'unitrootTest' in 'fUnitRoots' package.  
  }
  \item{lags}{
    the maximum number of lags used for error term correction.

    NOTE:  This is one more than the 'lags' argument used in 'adfTest',
    'unitrootTest', 'ur.df', 'ADF.test' and 'ur.df'.  See the comparison
    in the examples.  The default was copied from 'ur.df' and
    'UnitrootTests', noting that the 'lags' argument for other R
    functions are one less than that of 'unitroot' in S-PLUS
    Finmetrics.  
  }
  \item{object}{
    an object of class 'fHTEST', as returned by 'unitroot'.  
  }
  \item{...}{
    optional arguments for 'summary';  not currently used.  
  }
}
\details{
  There are 4 functions in different contributed packages in R for the
  Augmented Dickey-Fuller test (as of 2007.12.11):
  
  adf.test{tseries} by A. Trapletti
  
  adfTest{fUnitRoots} by Diethelm Wuertz, based on Trapletti's algorithm 

  ur.df{urca} by Bernhard Pfaff

  ADF.test{uroot} by Javier Lopez-de-Lacalle

  This 'Unitroot' function and the companion 'summary.fHTEST' use
  'adfTest'. It is provided for partial compatibility with the S-PLUS
  Finmetrics examples on pp. 70-72 of Tsay (2005).  As noted in the
  examples below, this function produces a very close match for the 
  numbers on pp. 70-72, except for the ADF p-value in the first
  example.  For this, 'adfTest' (and hence 'Uniroot') uses linear
  interpolation in a crude table.  This could be improved.  See the
  examples below. 
}
\value{
  an object of class '"fHTEST"' as described with 'UnitrootTests' in the
  'fUnitRoots' package, except that the returned vallue may not have a
  slot 'data.name' described with 'Unitroottests'.  
}
\references{
  Ruey Tsay (2005) Analysis of Financial Time Series, 2nd ed. (Wiley)
}
\author{
  Adrian Trapletti and Diethelm Wuertz for the 'fUnitRoots' functions
  used, and Spencer Graves for the 'FinTS' interface, with help from
  Javier Lopez de Lacalle and Bernard Pfaff.  
}
\seealso{
  \code{\link[fUnitRoots]{UnitrootTests}}
  \code{\link[fUnitRoots]{adfTest}}
  \code{\link[fUnitRoots]{unitrootTest}}
  \code{\link[ur.df]{urca}}
  \code{\link[uroot]{ADF.test}}
  \code{\link[urca]{ur.df}}
}
\examples{
##
## Tsay, pp. 69-71
##
data(q.gdp4703)
adft.gdp <- Unitroot(log(q.gdp4703), trend='c', method='adf', lags=10)
summary(adft.gdp)
# Except for the p-value and degrees of freedom for residual std error,
# all numbers matched the S-Plus Finmetrics answers.  

##
## Tsay, pp. 71-72
##
data(d.sp9003lev)
adft.sp <- Unitroot(log(d.sp9003lev), trend='ct', method='adf', lags=14)
summary(adft.sp)

##
## Using adfTest{fUnitRoots} directly
##
adfTest(log(q.gdp4703), lags=9, type='c')
# Gives the ADF statistic and p-value but not the table.  

##
## Using ur.df{urca}
##
if(require(urca))ur.df(log(q.gdp4703), type='drift', lags=9)
# prints 2 numbers:
# The first is the ADF statistic on Tsay, p. 70.
# It's not obvious what the second number is.  

##
## Using ADF.test{uroot} 
##
if(require(uroot)){
   gdp.ts <- ts(log(q.gdp4703), frequency=4, start=c(1947,1))
   ADF.test(gdp.ts, c(1,0,0), selectlags=list(mode=as.numeric(1:9)))
#  Gives the ADF statistic on p. 71, but the p-value is wrong.
}

##
## Using adf.test{tseries} 
##
if(require(tseries))  
  adf.test(log(as.numeric(q.gdp4703)), alternative="stationary", k=9)
# None of the numbers match;  I don't know why.  
}
\keyword{ts}

