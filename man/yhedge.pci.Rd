\name{yhedge.pci}
\alias{yhedge.pci}
\title{
Hedge portfolio for a stock price series
}
\description{
Computes the hedge of a stock price series fetched form Yahoo!
using one or more other stock price series also fetched form Yahoo!
}
\usage{
yhedge.pci(target, factors, start, end, na.rm=FALSE, ...)
}
\arguments{
  \item{target}{The ticker symbol of the stock price series that is to be modeled. }
  \item{factors}{A list of ticker symbols of stock price series to be used in modeling
    \code{target}}
  \item{start}{The starting date for which data is to be fetched, given in the
    format YYYYMMDD.  Default: 2 years ago today.}
  \item{end}{The ending date for which data is to be fetched, given in the
    format YYYYMMDD.  Default: today.}
  \item{na.rm}{If TRUE, then \code{NA}'s will be removed from the \code{data.frame}
    of fetched prices.  A heuristic approach is used to decide between deleting
    securities versus deleting days.}
  \item{...}{Additional optional parameters to be passed to \code{\link{fit.pci}}}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
An \code{S3} object of class \code{pci.hedge} representing the best fit that
was found.
}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}

Christopher Krauss \email{christopher.krauss@fau.de}

Jonas Rende \email{jonas.rende@fau.de}
}

\seealso{
\code{\link{fit.pci}}
}

\examples{
# Compute the best hedge of Coca-Cola using sector ETFS.
# sectorETFS <- c("XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY")
# hedge <- yhedge.pci("KO", sectorETFS)
# hedge
# test.pci(hedge$pci)
# plot(hedge)
}
\keyword{ ts }
\keyword{ models }

