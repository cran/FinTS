\name{findConjugates}
\alias{findConjugates}
\title{ Find complex conjugate pairs }
\description{
  Find all complex conjugate pairs in a vector of complex numbers and
  return one number from each pair.  
}
\usage{
findConjugates(x, complex.eps=.Machine[["double.eps"]]) 
}
\arguments{
  \item{x}{
    a vector of complex numbers 
  }
  \item{complex.eps}{
    a small positive number used to identify complex conjugates:
    x[i] and x[j] are considered conjugates if

    (abs(x-Conj(x)) / max(abs(x[i], x[j]))) < complex.eps)

    and

    (abs(x[i]-x[j]) > complex.eps.

    The latter condition excludes repeated roots.  
  }
}
\details{
  1.  Compute normalization m2 = outer(abs(x), abs(x), max) 

  2.  Compute complex differences c2 = abs(outer(x, Conj(x), "-"))/m2 

  3.  If any abs(c2) < complex.eps, make sure the numbers are not
  duplicate reals via (d2 = abs(outer(x, x, "-"))) > complex.eps 
}
\value{
  a complex vector with one representative of each complex pair found
}
\author{
  Spencer Graves and Ravi Varadhan
}
\seealso{
  \code{\link{plotArmaTrueacf}}
}
\examples{
# none
findConjugates(NULL)
findConjugates(numeric(0))
findConjugates(0:4)
findConjugates(rep(0:1,each=3))

# some 
findConjugates(c(1+1i, 0, 1-1i, 2-2i, 3, 2+2i, NA))

# Testing with polyroot and solve(polynomial(...))
set.seed(1234)
if(require(polynomial)){
  p <- polynomial(sample(1:10, size=45, rep=TRUE)) # degree 44
  z <- solve(p)
  findConjugates(z, complex.eps=.Machine$double.eps)
# this identifies all 21 conjugate pairs, R 2.6.0 for Windows

z1 <- polyroot(p)
findConjugates(z1, complex.eps=.Machine$double.eps)
# this only identifies only 3 conjugate pairs, R 2.6.0 for Windows
}
}
\keyword{arith}
