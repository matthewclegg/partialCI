\name{multigetYahooPrices}
\alias{multigetYahooPrices}
\title{
Fetches closing prices of multiple stock tickers
}
\description{
Fetches a \code{zoo} \code{data.frame} of daily closing prices of multiple stock tickers.
}
\usage{
multigetYahooPrices(components, start, end, quiet = FALSE, adjust = TRUE)
}
\arguments{
  \item{components}{Character vector of Yahoo ticker symbols}
  \item{start}{First date of desired data in YYYYMMDD format.  Default is
    earliest date of all series}
  \item{end}{Last date of desired data in YYYYMMDD format.  Default is the
    last date for which data is available}
  \item{quiet}{If \code{FALSE}, then information is printed about the 
    progress of the fetch operation}
  \item{adjust}{If \code{TRUE}, then adjusted closing prices are returned.
    Otherwise, unadjusted prices are returned.}
}
% \details{
%%  ~~ If necessary, more details than the description above ~~
% }
\value{
Returns a \code{\link{zoo}} \code{\link{data.frame}} containing the
closing prices of the series listed in the \code{components} parameter,
one column per price series.
}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}
}

\seealso{\code{\link[TTR]{getYahooData}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

### Note: you must have a working internet
### connection for these examples to work!
spy.voo <- multigetYahooPrices(c("SPY","VOO"))
}
