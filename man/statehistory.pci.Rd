\name{statehistory.pci}
\alias{statehistory.pci}
\title{
Generates the sequence of inferred states of a partial cointegration model
}
\description{Generates the sequence of inferred states of a partial cointegration model}
\usage{
statehistory.pci(A, data = A$data, basis = A$basis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
An object returned by \code{\link{fit.pci}} representing a partial cointegration fit.
}
  \item{data}{
The data history for which the inferred states are to be computed.  This should
be a \code{(k+1) x n} matrix, where \code{k} is the number of independent variables
and \code{n} is the number of observations.  If this is omitted, then uses the
data history that was used in fitting the model \code{A}.
}
  \item{basis}{
The coefficients of the independent variables.  This is a vector
of length \code{k}.  If this is omitted,
then uses the coefficients that were computed in fitting the model \code{A}.
}
}
\details{
Computes the expected internal states of the model over the course
of the data history.
}
\value{
Returns a \code{data.frame} with the following columns:
\item{Y}{The variable being modeled}
\item{X1,...,X_N}{The independent variables}
\item{Z}{The residual series \code{Y - beta \%*\% X}}
\item{M}{The estimated state of the mean reverting component}
\item{R}{The estimated state of the random walk component}
\item{eps_M}{The innovation to the mean reverting component}
\item{eps_R}{The innovation to the random walk component}
}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}

Christopher Krauss \email{christopher.krauss@fau.de}

Jonas Rende \email{jonas.rende@fau.de}
}
\seealso{
\code{\link[egcm:egcm]{egcm}} Engle-Granger cointegration model

\code{\link[partialAR:partialAR]{partialAR}} Partially autoregressive models
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

YX <- rpci(n=1000, alpha=1, beta=c(2,3), sigma_C=c(0.1,0.1), rho=0.9, sigma_M=1, sigma_R=2)
f <- fit.pci(YX[,1], YX[,2:ncol(YX)])
statehistory.pci(f)

}
\keyword{ ts }
\keyword{ models }
