\name{which.hypothesis.pcitest}
\alias{which.hypothesis.pcitest}
\title{
Returns the preferred hypothesis when testing for partial cointegration
}
\description{Returns the preferred hypothesis when testing for partial cointegration}
\usage{
which.hypothesis.pcitest(AT)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AT}{
An object of class \code{"pcitest"} that has been returned by a 
previous call to \code{\link{test.pci}}.
}
}
\details{
Based upon the critical value \code{alpha} that was given in the
call to \code{test.pci} and the p-value that was computed, 
determines which hypothesis best fits the data.  
}
\value{
If a non-robust fit was used, then one of the following values is returned:
\item{\code{"PCI"}}{Partially cointegrated.  Both the random walk hypothesis
  and the AR(1) hypothesis were rejected.}
\item{\code{"RW"}}{Random walk.}
\item{\code{"AR1"}}{Autoregressive of order one.}

If a robust fit was used, then one of the following values is returned:
\item{\code{"RPCI"}}{Partially cointegrated.  Both the random walk hypothesis
  and the AR(1) hypothesis were rejected.}
\item{\code{"RRW"}}{Random walk.}
\item{\code{"RAR1"}}{Autoregressive of order one.}

}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}

Christopher Krauss \email{christopher.krauss@fau.de}

Jonas Rende \email{jonas.rende@fau.de}
}
\seealso{
\code{\link{fit.pci}} Fits a partially cointegrated model

\code{\link{likelihood_ratio.pci}} Computes the likelihood ratio of a PCI model
  versus a null model
}
\examples{
\dontrun{
# The following should usually return "PCI"
YX <- rpci(n=1000, alpha=1, beta=c(2,3), sigma_C=c(0.1,0.1), rho=0.8, sigma_M=1, sigma_R=1)
which.hypothesis.pcitest(test.pci(YX[,1], YX[,2:ncol(YX)]))

# The following should usally return "RW"
YX.rw <- rpci(n=1000, alpha=1, beta=c(2,3), sigma_C=c(0.1,0.1), rho=0.8, sigma_M=0, sigma_R=1)
which.hypothesis.pcitest(test.pci(YX.rw[,1], YX.rw[,2:ncol(YX.rw)]))

# The following should usually return "AR1"
YX.mr <- rpci(n=1000, alpha=1, beta=c(2,3), sigma_C=c(0.1,0.1), rho=0.8, sigma_M=1, sigma_R=0)
which.hypothesis.pcitest(test.pci(YX.mr[,1], YX.mr[,2:ncol(YX.mr)]))
}
}
\keyword{ ts }
\keyword{ models }

