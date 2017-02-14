\name{loglik.pci}
\alias{loglik.pci}
\title{
Computes the log likelihood of a partially cointegrated model
}
\description{
Computes the log likelihood of a partially cointegrated model
}
\usage{
loglik.pci(Y, X, alpha, beta, rho, sigma_M, sigma_R, 
  M0 = 0, R0 = 0, 
  calc_method = c("css", "fkf", "ss", "sst", "csst"), 
  nu = pci.nu.default())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The time series that is to be modeled.  A plain or \code{\link{zoo}} vector of length \code{n}.
}
  \item{X}{
A (possibly \code{\link{zoo}}) matrix of dimensions \code{n} x \code{k}.  If \code{k=1}, then this may be a plain or \code{\link{zoo}} vector.
}
  \item{alpha}{
The constant term to be used in the fit.
}
  \item{beta}{
A vector of length \code{k} representing the weightings to be given to the components of \code{X}.
}
  \item{rho}{
The coefficient of mean reversion.
}
  \item{sigma_M}{
The standard deviation of the innovations of the mean-reverting component of the model.
}
  \item{sigma_R}{
The standard deviation of the innovations of the random walk component of the model.
}
  \item{M0}{
The initial value of the mean-reverting component.  Default = 0.
}
  \item{R0}{
The initial value of the random walk component.  Default = 0.
}
  \item{calc_method}{
  Specifies the Kalman filter implementation that will be used for computing
  the likelihood score:
\itemize{
\item "ss" Steady-state Kalman filter
\item "css" C++ implementation of steady-state Kalman filter 
\item "fkf" Kalman filter implementation of the FKF package
\item "sst" Steady-state Kalman filter using t-distributed innovations
\item "csst" C++ implementation of steady-state Kalman filter using t-distributed innovations
}
Default: \code{css}
}

\item{nu}{
The degrees-of-freedom parameter to be used if \code{calc_method} is "sst" or "csst".
}
}
\details{
The partial cointegration model is given by the equations:

\deqn{
  Y_t = \alpha + \beta_1 * X_{t,1} + beta_2 * X_{t,2} + ... + beta_k * X_{t,k} + M_t + R_t}{
  Y[t] = alpha + beta[1] * X[t,1] + beta[2] * X[t,2] + ... + beta[k] * X[t,k] + M[t] + R[t]
}
\deqn{M_t = \rho M_{t-1} + \epsilon_{M,t}}{M[t] = rho * M[t-1] + epsilon_M[t]}
\deqn{R_t = R_{t-1} + \epsilon_{R,t}}{R[t] = R[t-1] + epsilon_R[t]}
\deqn{-1 < \rho < 1}{-1 < rho < 1}
\deqn{\epsilon_{M,t} \sim N(0,\sigma_M^2)}{epsilon_M[t] ~ N(0, sigma_M^2)}
\deqn{\epsilon_{R,t} \sim N(0,\sigma_R^2)}{epsilon_R[t] ~ N(0, sigma_R^2)}

Given the input series 
  \code{Y} and \code{X}, 
and given the parameter values 
  \code{alpha}, \code{beta}, \code{rho}, \code{M0} and \code{R0}, 
the innovations \code{epsilon_M[t]} and \code{epsilon_R[t]} are calculated
using a Kalman filter.  Based upon these values, the log-likelihood score
is then computed and returned.
}
\value{
The log of the likelihood score of the Kalman filter
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
\code{\link[egcm:egcm]{egcm}} Engle-Granger cointegration model

\code{\link[partialAR:partialAR]{partialAR}} Partially autoregressive models
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

    set.seed(1)
    YX <- rpci(n=500, alpha=1, beta=c(2,3,4), sigma_C=c(1,1,1), rho=0.9, sigma_M=0.1, sigma_R=0.2)
    loglik.pci(YX[,1], YX[,2:ncol(YX)], alpha=1, beta=c(2,3,4), rho=0.9, sigma_M=0.1, sigma_R=0.2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ models }
