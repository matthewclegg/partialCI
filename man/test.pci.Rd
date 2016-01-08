\name{test.pci}
\alias{test.pci}
\title{Tests the goodness of fit of a partial cointegration model}
\description{
Tests the goodness of fit of a partial cointegration model
}
\usage{
test.pci(Y, X, alpha = 0.05, 
  null_hyp = c("rw", "ar1"), 
  robust = FALSE, 
  pci_opt_method = c("jp", "twostep"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
  The time series that is to be modeled.  A plain or \code{\link{zoo}} vector of length \code{n}.
}
  \item{X}{
  A (possibly \code{\link{zoo}}) matrix of dimensions \code{n} x \code{k}.  
  If \code{k=1}, then this may be a plain or \code{\link{zoo}} vector.
}
  \item{alpha}{
The cutoff value to be used for determining whether or not to accept the
null hypothesis.  If the the \code{p-value} computed through the likelihood
ratio test is below this value, then the null hypothesis is rejected.
Default: 0.05.
}
  \item{null_hyp}{
This specifies the null hypothesis.
This can be either \code{"rw"}, \code{"ar1"} or \code{c("rw","ar1")}.
If \code{"rw"}, then the null hypothesis is a random walk.
If \code{"ar1"}, then the null hypothesis is an autoregressive process
of order 1.  (In this case, the null hypothesis calls for Y and X to be
cointegrated.)  If \code{(c("rw","ar1")}, then the null hypothesis is
either a random walk or AR(1) process.  Default: both.
}

\item{robust}{
  If \code{TRUE}, then the residuals are assumed to follow a t-distribution.  Default: \code{FALSE}.
}
\item{pci_opt_method}{
The method that will be used for fitting a partially cointegrated
model to \code{X} and \code{Y}.  This can be either \code{"jp"}
(joint penalty) or \code{"twostep"} (Engle-Granger two-step).
See \code{\link{fit.pci}} for a complete explanation.
Default: \code{"jp"}.
}
}
\details{
The likelihood ratio test is used to determine whether 
the null hypothesis should be accepted or whether the alternative
of partial cointegration should be accepted.  That is to say,
a search is performed for the best fitting model under the null
hypothesis, and the log likelihood score of this model is computed.
Then a search is performed for the best fitting model under the
alternative hypothesis of partial cointegration, and the log
likelihood score of this model is computed.  The associated
p-values have been computed through simulation.

If the null hypothesis is the union \code{c("rw", "ar1")}, then
a table of p-values is used that has been pre-computed, however
this table is not unique.
}
\value{An object of class \code{"pcitest"} containing the
results of the hypothesis test.
}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}
}
\seealso{
\code{\link{fit.pci}} Fits a partially cointegrated model

\code{\link{likelihood_ratio.pci}} Computes the likelihood ratio of a PCI model
  versus a null model
}
\examples{
# The following should reject both the random walk and AR(1) models
YX <- rpci(n=1000, alpha=1, beta=c(2,3), sigma_C=c(0.1,0.1), rho=0.8, sigma_M=1, sigma_R=1)
test.pci(YX[,1], YX[,2:ncol(YX)])

# The following should accept the random walk model and reject the AR(1) model
YX.rw <- rpci(n=1000, alpha=1, beta=c(2,3), sigma_C=c(0.1,0.1), rho=0.8, sigma_M=0, sigma_R=1)
test.pci(YX.rw[,1], YX.rw[,2:ncol(YX.rw)])

# The following should reject the random walk model and accept the AR(1) model
YX.mr <- rpci(n=1000, alpha=1, beta=c(2,3), sigma_C=c(0.1,0.1), rho=0.8, sigma_M=1, sigma_R=0)
test.pci(YX.mr[,1], YX.mr[,2:ncol(YX.mr)])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ models }
