FinTS.stats <- function(x){
  Start <- min(index(x))
  N <- sum(!is.na(x))
  Mean <- mean(x, na.rm=TRUE)
  Sd <- sd(x, na.rm=TRUE)
  sk <- skewness(x, na.rm=TRUE)
  kurt <- kurtosis(x, na.rm=TRUE)
  Min <- min(x, na.rm=TRUE)
  Max <- max(x,na.rm=TRUE)
#  
  data.frame(Start=Start, Size=N, Mean=Mean, Standard.Deviation=Sd,
    Skewness=sk, Excess.Kurtosis=kurt, Minimum=Min,
    Maximum=Max)
}
