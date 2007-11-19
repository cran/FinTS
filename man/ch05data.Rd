\name{ch05data}
\docType{data}
\alias{ch05data}
\alias{ibm}
\alias{ibm9912.tp}
\alias{ibmdurad}
\alias{ibm1to5.dur}
\alias{ibm91.ads}
\alias{ibm91.adsx}
\alias{day15.ori}
\alias{day15}
\title{ financial time series for Tsay (2005, chapter 5[text]) }
\description{
  Financial time series used in examples in chapter 5.  
}
\usage{
data(ibm)
data(ibm9912.tp)
data(ibmdurad)
data(ibm1to5.dur)
data(ibm91.ads)
data(ibm91.adsx)
data(day15.ori)
data(day15)
}
\format{

  \item{ibm}{
    IBM transactions data (11/1/1990 - 1/31/1991)
    
    data.frame of date.time, volume, bid, ask, and price of IBM stock
    transactions.  date.time is of class 'chron', while volume, bid,
    ask, and price are all numeric.  Some tranactions have the same
    date.time values, which is why this is a data.frame and not a zoo
    object.      
  }  
  \item{ibm9912.tp}{
    IBM transactions data of December 1999:  data.frame of date.time and
    price.  
  }
  \item{ibmdurad}{
    Adjusted time durations between trades of IBM stock 
    (11/01/1990-1/31/1991).  

    Format:  data.frame with columns date.time and adjusted.duration 
  }
  \item{ibm1to5.dur}{
    subset of 'ibmdurad' limited to positive durations in the first 5
    trading days. 
  }
  \item{ibm91.ads}{
    a data.frame on the changes in the price of IBM stock transactions
    between November 1, 1990 and January 31, 1991.  This period includes
    63 trading days, during which 59,838 transactions were recorded
    during normal trading hours.  The first transcation for each day was
    dropped leaving the 59,775 transactions in this data.frame.  

    \itemize{
      \item{A.priceChange}{
	1 if a price change from the previous trade, 0 otherwise
      }
      \item{DirectionOfChg}{
	1 if positive, -1 if negative, 0 if no change
      }
      \item{SizeInTicks}{
	Size of the price change in number of ticks of 1/8 of a US
	dollar.

	NOTE:  There are 10 anomalous records for which A.priceChange
	!=0 but SizeInTicks == 0 in this data.frame.  These correspond
	to price changes of half a tick, which got rounded down to 0.  
      }
    }
  }  
  \item{ibm91.adsx}{
    a data.frame with 6 variables the same transactions as in
    'ibm91.ads':  

    \itemize{
      \item{volume.thousands}{thousands of shares traded}
      \item{time.betw.trades}{seconds between the previous two trades}
      \item{bid.ask.spread}{
	the bid-ask spread in USD of the current transaction.
      }
      \item{A.priceChange}{
	1 if the previous trade involved a price change from its
	predacessor, and 0 otherwise
      }
      \item{DirectionOfChg}{
	1 if the previous change was positive, -1 if negative, 0 if no
	change 
      }
      \item{SizeInTicks}{
	Size of the price change in the previous trade in number of
	ticks of 1/8 of a US dollar.

	NOTE:  The last three columns are ibm91.ads lagged one
	transaction, so ibm91.adsx[-1, 4:5] == ibm91.ads[-59775, ], with
	24 exceptions. 	
      }
    }    
  }
  \item{day15.ori}{
    data.frame with the transaction time and the stock price for the 728
    IBM stock transactions that occurred during normal trading hours on
    November 21, 1990.      
  }
  \item{day15}{
    a zoo object with the following columns supposedly summarizing only
    the price changes in day15.ori:  

    \itemize{
      \item{timeBetwPriceChg}{
	time in seconds since the last price
	change
      }
      \item{DirectionOfChange}{
	1 if the price increased, -1 if it decreased
      }
      \item{priceChgTicks}{
	price change in number of ticks of USD 1/8.
      }
      \item{nTradesWoChg}{
	number of trades without a price change since the previous price
	change ... supposedly.  These numbers do not match a manual
	extraction of these data from 'day15.ori'.  
      }
      \item{multTrans}{
	1 if there were multiple transactions within the same one second
	interval, 0 if not.  
      }
      \item{dailyCumChg}{
	cumulative price change in USD since the start of normal trading
	on November 21, 1990.
      }
    }    
  }
}
%\details{
%}
\source{
  \url{
    http://faculty.chicagogsb.edu/ruey.tsay/teaching/fts2
  }
}
\references{
  Ruey Tsay (2005)
  Analysis of Financial Time Series, 2nd ed. (Wiley, ch. 5)
}
\keyword{datasets}
\seealso{
  \code{\link{ch01data}} 
  \code{\link{ch02data}}
}
%\examples{
%}

