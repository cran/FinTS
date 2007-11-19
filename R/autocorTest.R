autocorTest <- function(x, lag=log(length(x))){
  Box.test(x, lag, "Ljung-Box")
}
