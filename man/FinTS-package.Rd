\name{FinTS-package}
\alias{FinTS-package}
\alias{FinTS}
\docType{package}
\title{
Companion to Tsay (2005) Analysis of Financial
Time Series
}
\description{
R companion to Tsay (2005)
Analysis of Financial Time Series, 2nd ed. (Wiley).
Includes data sets, functions and script files
required to work some of the examples.  Version 0.2-x
includes R objects for all data files used in the text
and script files to recreate most of the analyses in
chapters 1-3 and 9 plus parts of chapters 4 and 11.
}
\details{
\tabular{ll}{
Package: \tab FinTS\cr
Type: \tab Package\cr
Version: \tab 0.3-3\cr
Date: \tab 2008-05-23\cr
License: \tab GPL (>= 2)\cr
URL: \tab http://www.r-project.org\cr
Depends: \tab zoo, graphics\cr
Suggests: \tab moments, distrEx, tseries, urca,
  lmtest, sandwich, psych, GPArotation, chron,
  polynom, fUnitRoots, e1071\cr
Packaged: \tab Sat May 24 11:32:56 2008; spencerg\cr
Built: \tab R 2.7.0; ; 2008-05-24 11:36:17; windows\cr
}

Index:
\preformatted{
ARIMA                   Arima with Ljung-Box
Acf                     Autocorrelation Function
ArchTest                ARCH LM Test
AutocorTest             Box-Ljung autocorrelation test
FinTS.stats             Financial Time Series summary statistics
TsayFiles               List of the names of files downloaded from the
                        "Analysis of Financial Data" web site.
Unitroot                unit root tests
apca                    Asymptotic PCA
as.yearmon2             Conditionally convert x to yearmon if the
                        conversion is unqique, retaining x as names.
ch01data                financial time series for Tsay (2005, chapter
                        1[text])
compoundInterest        compute compound interest
findConjugates          Find complex conjugate pairs
package.dir             Directory of a package
plot.loadings           Plot loadings
plotArmaTrueacf         plot the theoretical ACF corresponding to an
                        ARMA model
read.yearmon            Reading Monthly zoo Series
runscript               Run a package script
url2data                Create local copies of files read from urls.
}

    See the \code{scripts} subdirectory of the \code{FinTS} installation
    directory =  system.file(package='FinTS').

    Corrections to the script files provided and contributions to script
    files for other chapters will be graciously accepted.
}
\author{
Spencer Graves

Maintainer: Spencer Graves <spencer.graves@prodsyse.com>
}
\references{
    Ruey Tsay (2005) Analysis of Financial Time Series, 2nd ed. (Wiley)
}
\keyword{ package }
\keyword{ ts }
\seealso{
  \url{
    http://faculty.chicagogsb.edu/ruey.tsay/teaching/fts2
  }
}
\examples{
# Where is the 'FinTS' directory?
system.file(package='FinTS')

# View the script file 'ch01.R', which is in the 'scripts'
# subdirectory of the system.file(package='FinTS') directory:
runscript(1, 'view')

# SP statistics in Table 1.2 of Tsay
data(d.ibmvwewsp6203)
FinTS.stats(100*d.ibmvwewsp6203[, "SP"])

}
