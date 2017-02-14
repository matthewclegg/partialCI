\name{likelihood_ratio.pci}
\alias{likelihood_ratio.pci}
\title{
Computes the likelihood ratio of the partially cointegrated model vs the null model
}
\description{
Computes the likelihood ratio of the partially cointegrated model vs the null model
}
\usage{
likelihood_ratio.pci(Y, X, 
  robust = FALSE, 
  null_model = c("rw", "ar1"), 
  pci_opt_method = c("jp", "twostep"), 
  nu = 5)
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
  \item{robust}{
If \code{TRUE}, then the residuals are assumed to follow a t-distribution with
\code{nu} degrees of freedom.  Default: \code{FALSE}.
}

  \item{null_model}{
This specifies the model that is assumed under the null hypothesis.
\itemize{
\item{rw}{Random walk.  Assumes sigma_M = rho = 0. Default.}
\item{ar1}{Autoregressive of order one.  Assumes sigma_R=0.}
}
}
  \item{pci_opt_method}{
Method to be used for fitting Y to X.
\itemize{
\item{jp}{The coefficients of Y are jointly optimized
with the parameters of the AAR fit of the residuals.  Default.}
\item{twostep}{A modified Engle-Granger procedure is used, where
the coefficients of Y are first estimated, and then an AAR
model is fit to the residuals.}
}}
  \item{nu}{
If \code{robust} is \code{TRUE}, then this is the degrees of freedom
parameter used in fitting the t-distribution.  Default: 5.
}
}
\details{
First searches for the optimal fit under the null model, and computes
the log of the likelihood score of this fit.  Then, searches for the optimal
fit under the full model, and computes the log of the likelihood score of
this fit.  Returns the difference of the two likelihood scores.  Since the
null model is nested in the full model, the log likelihood ratio score
is guaranteed to be negative.
}
\value{The log of the ratio of the likelihoods of the two models.
}
\references{
  Clegg, Matthew, 2015.
  Modeling Time Series with Both Permanent and Transient Components
  using the Partially Autoregressive Model.
  \emph{Available at SSRN: http://ssrn.com/abstract=2556957}
}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}

Christopher Krauss \email{christopher.krauss@fau.de}

Jonas Rende \email{jonas.rende@fau.de}
}
\seealso{
\code{\link{fit.pci}} Fitting partially cointegrated models
}
\examples{
YX <- rpci(n=1000, alpha=1, beta=c(2,3,4), sigma_C=c(1,1,1), rho=0.9, sigma_M=0.1, sigma_R=0.2)
likelihood_ratio.pci(YX[,1], YX[,2:ncol(YX)])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ models }% __ONLY ONE__ keyword per line
