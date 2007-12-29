###
### 
### Ruey S. Tsay (2005)
### Analysis of Financial Time Series, 2nd ed.
### (Wiley)
###
### 

# p. 97
###
### ch. 3.  Conditional Heteroscedastic Models 
###
library(FinTS)

# p. 98 
##
## sec. 3.1.  Characteristics of volatility 
##

# p. 99
##
## sec. 3.2.  Structure of a model 
##

# Figure 3.1
data(m.intc7303)
ml.intc <- log(1+m.intc7303)

op <- par(mfcol=c(2,2))
acf(as.numeric(ml.intc), main="(a) Log returns", ylim=c(-.2, .4), acfLag0=FALSE)
acf(as.numeric(ml.intc)^2, main="(b) Squared log returns", ylim=c(-.2, .4),
    acfLag0=FALSE)
acf(abs(as.numeric(ml.intc)), main="(c) Absolute log returns", ylim=c(-.2, .4),
    acfLag0=FALSE) 
pacf(as.numeric(ml.intc)^2, main="(b) Squared log returns", ylim=c(-.2, .4))
par(op) 

# p. 101 
##
## sec. 3.3.  Model Building 
##

# sec. 3.3.1.  Testing for ARCH effect 

# p. 102
data(m.intc7303)
str(m.intc7303)

AutocorTest(log(1+as.numeric(m.intc7303)), lag=12)

#archTest(log(1+as.numeric(m.intc7303)), lag=12)
# doesn't work.  


##??????????????
##
## Question sent to a package maintainer.  2007.12.23
##
##??????????????

##
## sec. 3.4.  The ARCH model
##

# p. 103 
# Figure 3.2
data(exch.perc)
str(exch.perc)

op <- par(mfrow=c(2,1))
plot(exch.perc, type="l", xlab="", ylab="fx",
     main="(a) Percentage change in exchange rate")
plot(exch.perc^2, type="l", xlab="", ylab="sq-fx",
     main="(b) Squared series")
par(op)

# p. 104 
# Figure 3.3

op <- par(mfrow=c(2,1))
acf(exch.perc, ylim=c(-.1, .1), main="(a) Sample ACF", acfLag0=FALSE)

pacf(exch.perc^2, ylim=c(-.1, .1),
     main="(b) Partial ACF of the squared series")
par(op)

# sec. 3.4.1.  Properties of ARCH models   

# p. 106
# sec. 3.4.2.  Weaknesses of ARCH models

# sec. 3.4.3.  Building an ARCH model 

# p. 109
# sec. 3.4.4.  Some Examples

# Example 3.1
data(m.intc7303)
ml.intc <- log(1+m.intc7303)

# Possibiliities:
#garch {tseries} Fit GARCH Models to Time Series
#GarchFitting {fGarch} Univariate GARCH Time Series Fitting

library(tseries)
#arch3.fit <- garch(ml.intc, order=c(1, 3))
#Error in garch(ml.intc, order = c(1, 3)) : NAs in x

arch3.fit <- garch(as.numeric(ml.intc), order=c(1, 3))
summary(arch3.fit)
# very different from Tsay

##??????????????
##
## Question sent to a package maintainer.  20007.12.24
##
##??????????????

library(fGarch)
arch3.Fit <- garchFit(~garch(3, 0), data=ml.intc)

arch3.Fit <- garchFit(~garch(3, 0), data=as.numeric(ml.intc))

##??????????????
##
## Question sent to a package maintainer.  2007.12.24
##
##??????????????

# p. 112
# Figure 3.4
# plot of residuals from a fit that I don't know yet how to get


# p. 113 
##
## sec. 3.5.  The GARCH Model
##

# p. 116
#  sec. 3.5.1.  An Illustrative Example
data(sp500)

(spFit03 <- arima(sp500, c(0, 0, 3)))
coef(spFit03)
(spFit03. <- arima(sp500, c(0, 0, 3),
                 fixed=c(ma1=NA, ma2=0, ma3=NA, intercept=NA)))
names(spFit03.)
sqrt(spFit03.$sigma2)

str(sp500)
(spFit30 <- arima(sp500, c(3, 0, 0)))

library(fGarch)
spFit30.11 <- garchFit(sp500~arma(3,0)+garch(1,1),
                       data=sp500)
# Difference from the book could be minor differences in
# the accuracy of the nonlinear optimizer used.  

# p. 117
# Figure 3.5
plot(sp500, xlab="year", ylab="rtn")
abline(h=0, lty="dashed")

# Figure 3.6 
op <- par(mfrow=c(2,1))
acf(sp500, lag.max=30, main="(a)", acfLag0=FALSE)
pacf(sp500^2, main="(b)")
par(op)

# p. 118 
spFit00.11 <- garchFit(sp500~garch(1,1), data=sp500)
summary(spFit00.11)

str(spFit00.11)

# Figure 3.7 
op <- par(mfrow=c(2,1))
plot(index(sp500), spFit00.11@sigma.t, type="l", xlab="year",
     ylab="sigma.t", main="(a) Estimated volatility process")
std.res <- residuals(spFit00.11)/spFit00.11@sigma.t
plot(index(sp500), std.res, type="l", xlab="year",
     ylab="std-resi", main="(b) Standardized residuals")
par(op)

# p. 119
# Figure 3.8
op <- par(mfrow=c(2,1))
acf(std.res, ylim=c(-.2, .2), main="(a)", lag.max=24, acfLag0=FALSE)
acf(std.res^2, ylim=c(-.2, .2), main="(b)", lag.max=24, acfLag0=FALSE)
par(op)

AutocorTest(std.res, 12)
AutocorTest(std.res, 24)

AutocorTest(std.res^2, 12)
AutocorTest(std.res^2, 24)

# p. 120
# Since the 'arma' part of the model is nothing,
# the "mean return" forecast is constant 'mu',
# estimated here as 0.0074497 vs. 0.0076 in the book.  
#str(spFit00.11)
names(spFit00.11@fit)
spFit00.11@fit$coef['mu']

# For the one-step ahead volatility forecast,
# we need the formula on the bottom of p. 119:

#sigma2.h[1] = omega + alpha1*resid[h]^2 + beta1*sigma2.h
(varCoef <- spFit00.11@fit$coef[-1])

resids <- spFit00.11@residuals
sigma.t <- spFit00.11@sigma.t
N <- length(resids)

pred.sigma2.t <- rep(NA, 6)
names(pred.sigma2.t) <- c(1:5, Inf) 

pred.sigma2.t[1] <- (varCoef["omega"] + varCoef["alpha1"] * resids[N]^2
               + varCoef["beta1"] * sigma.t[N])
# per expression (3.16), p. 115:
for(i in 2:5)
  pred.sigma2.t[i] <- (varCoef["omega"] +
             (varCoef["alpha1"] + varCoef["beta1"]) * pred.sigma2.t[i-1])

# per the last formula on the bottom of p. 115:
pred.sigma2.t["Inf"] <- (varCoef["omega"] /
                (1-(varCoef["alpha1"] + varCoef["beta1"])) ) 
round(sqrt(pred.sigma2.t), 5)

# compare with the book's numbers
vCoef <- c(omega=0.000086, alpha1=0.1216, beta1=0.8511)
pred.s2.t <- rep(NA, 6)
names(pred.s2.t) <- c(1:5, Inf)
pred.s2.t[1] <- (vCoef["omega"] + vCoef["alpha1"] * resids[N]^2
               + vCoef["beta1"] * sigma.t[N])
# per expression (3.16), p. 115:
for(i in 2:5)
  pred.s2.t[i] <- (vCoef["omega"] +
             (vCoef["alpha1"] + vCoef["beta1"]) * pred.s2.t[i-1])

# per the last formula on the bottom of p. 115:
pred.s2.t["Inf"] <- (vCoef["omega"] /
                (1-(vCoef["alpha1"] + vCoef["beta1"])) ) 
round(sqrt(pred.s2.t), 5)

