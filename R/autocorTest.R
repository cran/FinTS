AutocorTest <- function(x, lag=ceiling(log(length(x))),
            type=c("Ljung-Box", "Box-Pierce", "rank") ){
  type <- match.arg(type)
  if(type=="rank"){
    x <- rank(x) 
    type <- "Ljung-Box"
  }
#  
  Box.test(x, lag, type)
}
