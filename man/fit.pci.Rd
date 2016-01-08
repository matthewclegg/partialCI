\name{fit.pci}
\alias{fit.pci}
\title{
Fits the partial cointegration model to a collection of time series
}
\description{
Fits the partial cointegration model to a collection of time series
}
\usage{
fit.pci(Y, X, 
  pci_opt_method = c("jp", "twostep"), 
  par_model = c("par", "ar1", "rw"), 
  lambda = 0, 
  robust = FALSE, nu = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
The time series that is to be modeled.  A plain or \code{\link{zoo}} vector of length \code{n}.
}
  \item{X}{
A (possibly \code{\link{zoo}}) matrix of dimensions \code{n} x \code{k}.  If \code{k=1}, then this may be a plain or \code{\link{zoo}} vector.
}
  \item{pci_opt_method}{
Specifies the method that will be used for finding the best fitting model.  One of the following:
\itemize{
\item "jp" The joint-penalty method (see below)
\item "twostep" The two-step method (see below)
}
Default: \code{jp}

}
  \item{par_model}{
The model used for the residual series.  One of the following:
\itemize{
\item "par"  The residuals are assumed to follow a partially autoregressive model.
\item "ar1"  The residuals are assumed to be autoregressive of order one.
\item "rw"   The residuals are assumed to follow a random walk.
}
Default: \code{par}
}
\item{lambda}{
The penalty parameter to be used in the joint-penalty (\code{jp}) estimation method.
Default: 0.
}
\item{robust}{
If \code{TRUE}, then the residuals are assumed to follow a t-distribution with
\code{nu} degrees of freedom.  Default: \code{FALSE}.
}
  \item{nu}{
The degrees-of-freedom parameter to be used in robust estimation.  Default: 5.
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
this function searches for the parameter values 
  \code{alpha}, \code{beta}, \code{rho}
that give the best fit of this model when using a Kalman filter.

If \code{pci_opt_method} is \code{twostep}, then a two-step procedure is used.
In the first step, a linear regression is performed of \code{X} on \code{Y} to determine
the parameters \code{alpha} and \code{beta}.  From this regression, a series of residuals
is determined.  In the second step, a model is fit to the residual series.  If 
\code{par_model} is \code{par}, then a partially autoregressive model is fit to
the residual series. If \code{par_model} is \code{ar1}, then an autoregressive model
is fit to the residual series.  If \code{par_model} is \code{rw} then a random walk
model is fit to the residual series.  Note that if \code{pci_opt_method} is \code{twostep}
and \code{par_model} is \code{ar1}, then this reduces to the Engle-Granger two-step
procedure.

If \code{pci_opt_method} is \code{jp}, then the joint-penalty procedure is used.
In this method, the parameters \code{alpha} and \code{beta} are estimated jointly
with the parameter \code{rho} using a gradient-search optimization function.
In addition, a penalty value of 
  \eqn{\lambda * \sigma_R^2}{lambda * sigma_R^2}
is added to the Kalman filter likelihood score when searching for the
optimum solution.  By choosing a positive value for \code{lambda}, you can drive
the solution towards a value that places greater emphasis on the mean-reverting
component.  

Because the joint-penalty method uses gradient search, the final parameter
values found are dependent upon the starting point.  There is no guarantee
that a global optimum will be found.  However, the joint-penalty method chooses
several different starting points, so as to increase the chance of finding a
global optimum.  One of the chosen starting points consists of the parameters
found through the two-step procedure.  Because of this, the joint-penalty method
is guaranteed to find parameter values which give a likelihood score at least
as good as those found using the two-step procedure.  Sometimes the improvement
over the two-step procedure is substantial.
}
\value{
An object of class \code{pci.fit} containing the fit that was found.  The following components
may be of interest
\item{alpha}{The constant term of the fit}
\item{alpha.se}{The estimated standard error of \code{alpha}}
\item{beta}{The vector of weights}
\item{beta.se}{The standard errors of the components of \code{beta}}
\item{rho}{The estimated coefficient of mean reversion}
\item{rho.se}{The standard error of \code{rho}}
\item{negloglik}{The negative of the log likelihood}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  Clegg, Matthew.
  Modeling Time Series with Both Permanent and Transient Components
  using the Partially Autoregressive Model.
  \emph{Available at SSRN: http://ssrn.com/abstract=2556957}
}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}
}
\seealso{
\code{\link[egcm:egcm]{egcm}} Engle-Granger cointegration model

\code{\link[partialAR:partialAR]{partialAR}} Partially autoregressive models
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

YX <- rpci(n=1000, alpha=1, beta=c(2,3,4), sigma_C=c(1,1,1), rho=0.9, sigma_M=0.1, sigma_R=0.2)
fit.pci(YX[,1], YX[,2:ncol(YX)])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ models }

