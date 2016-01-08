\name{hedge.pci}
\alias{hedge.pci}
\title{
Searches for a partially cointegrated hedge for a given time series
}
\description{
Given a time series and a collection of possible factors, finds a subset of the
factors that provides the best fit to the given time series using the partially
cointegrated model.
}
\usage{
hedge.pci(Y, X, 
  maxfact = 10, 
  lambda = 0, 
  use.multicore = TRUE, 
  minimum.stepsize = 0, 
  verbose = TRUE, 
  exclude.cols = c(), 
  search_type = c("lasso", "full", "limited"), 
  pci_opt_method=c("jp", "twostep"), 
  ...)
}
\arguments{
\item{Y}{
  An \code{N x 1} column vector or data \code{data.frame}, 
  representing the series that is to be hedged.
}
\item{X}{
  An \code{N x L} \code{data.frame}, where each column represents a possible factor to
  be used in a partially cointegrated fit.
}
\item{maxfact}{
  The maximum number of columns from \code{X} that will be selected for modeling \code{Y}.
  Default: 10
}
\item{lambda}{
  A penalty to be applied to the random walk
  portion of the partialAR model.  A positive value for \code{lambda}
  will drive the model towards a solution with a smaller random walk component.
  Default: 0
}
\item{use.multicore}{
  If \code{TRUE}, parallel processing will be used to improve performance.
  See \code{\link[parallel:mclapply]{parallel:mclapply}}
  Default: \code{TRUE}
}
\item{minimum.stepsize}{
  If this is non-NA, then the search stops if an improvement
  cannot be found of at least this much.
  Default: 0
}
\item{verbose}{
  If \code{TRUE}, then detailed information is printed about 
  the execution. 
  Default: \code{TRUE}
}
\item{exclude.cols}{
  A list of column indexes specifying columns from \code{X} which
  should be excluded from consideration.  Alternatively, the list of
  excluded columns may be given as a list of strings, in which case
  they are interepreted as column names.
  Default: \code{c()}
}
  \item{search_type}{
  If "lasso", then the lasso algorithm (see \code{\link{glmnet}}) is
  used to identify the factors that provide the best linear fit to
  the target sequence.
  If "full", then a greedy algorithm is used to search for
  factors to be used in the hedge.  At each step, all possible additions
  to the portfolio are considered, and the best one is chosen for inclusion.
  If "limited", then at each iteration, a preliminary screening step is performed
  to identify the securities with the highest correlations to the residuals
  of the currently selected portfolio.  The top securities from this list are
  then checked for whether they would improve the portfolio, and the best one
 included.
}
  \item{pci_opt_method}{
Specifies the method that will be used for finding the best fitting model.  One of the following:
\itemize{
\item \code{"jp"} The joint-penalty method (see \code{\link{fit.pci}})
\item \code{"twostep"} The two-step method (see \code{\link{fit.pci}})
}
Default: \code{jp}

}
  \item{\dots}{
Other parameters to be passed onto the search function.  See the source
code.
}
}
\details{
The hedge is constructed by searching for column indices \code{i1,i2, ..., iN} 
from among the columns of \code{X} which yield the best fit to the partially
cointegrated fit:

\deqn{
  Y_t = \alpha + \beta_1 * X_{t,i1} + beta_2 * X_{t,i2} + ... + beta_N * X_{t,iN} + M_t + R_t}{
  Y[t] = alpha + beta[1] * X[t,i1] + beta[2] * X[t,i2] + ... + beta[k] * X[t,iN] + M[t] + R[t]
}
\deqn{M_t = \rho M_{t-1} + \epsilon_{M,t}}{M[t] = rho * M[t-1] + epsilon_M[t]}
\deqn{R_t = R_{t-1} + \epsilon_{R,t}}{R[t] = R[t-1] + epsilon_R[t]}
\deqn{-1 < \rho < 1}{-1 < rho < 1}
\deqn{\epsilon_{M,t} \sim N(0,\sigma_M^2)}{epsilon_M[t] ~ N(0, sigma_M^2)}
\deqn{\epsilon_{R,t} \sim N(0,\sigma_R^2)}{epsilon_R[t] ~ N(0, sigma_R^2)}

if \code{search_type="lasso"} is specified, then the lasso algorithm
(see \code{\link{glmnet}}) is used to search for the factors that give
the best linear fit to the target sequence \code{Y}.  Having determined
the list of factors, the cutoff point is determined based successive
improvements to the likelihood score of the fitted model.

Otherwise, a greedy algorithm (\code{search_type="full"}) or a modified greedy algorithm
(\code{search_type="limited"}) is used.  This proceeds by searching through all
columns of \code{X} (except those listed in \code{exclude.cols}) to find the
column that gives the best fit to \code{Y}, as determined by 
the likelihood score of the partially cointegrated model. This column becomes the initial
hedging portfolio.  Having selected columns \code{i1, i2, ..., iK}, the next
column is found by searching through all remaining columns of \code{X} (except those
listed in \code{exclude.cols}) for the column which gives the best improvement
to the partially cointegrated fit.  However, if the best improvement is less than
\code{minimum.stepsize}, or if \code{maxfact} columns have already been added,
then the search terminates.

In the case of the modified greedy algorithm (\code{search_type="limited"}), a
preprocessing step is used at the beginning of each iteration.  In this preprocessing
step, the correlation is computed between each unused column of \code{X} and the
residual series of the currently computed best fit.  The top \code{B} choices are then
considered for inclusion in the portfolio, where \code{B} is a branching factor.
The branching factor can be controlled by setting the value of the optional parameter
\code{max.branch}.  Its default value is 10.

The \code{lasso} algorithm is by far the fastest, followed by the \code{limited} greedy search.
So, the best strategy is probably to start by using the \code{lasso}.  If it fails to
produce acceptable results, then move on to the \code{limited} greedy algorithm and finally
the \code{full} search.
}
\value{
Returns an S3 object of class \code{pci.hedge} containing the following fields
\item{pci}{The best partially cointegrated fit that was found}
\item{indexes}{The indexes of the columns from \code{X} that were selected}
\item{index_names}{The names of the columns from \code{X} that were selected}
}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}
}
\seealso{
\code{\link{fit.pci}} Fitting of partially cointegrated models

\code{\link[partialAR:partialAR]{partialAR}} Partially autoregressive models

\code{\link[egcm:egcm]{egcm}} Engle-Granger cointegration model
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

YX <- rpci(n=1000, alpha=1, beta=c(2,3,4,5,6), 
  sigma_C=c(0.1,0.1,0.1,0.1,0.1), rho=0.9, sigma_M=1, sigma_R=1)
YXC <- cbind(YX, matrix(rnorm(5000), ncol=5))
hedge.pci(YX[,1], YX[,2:ncol(YX)])
hedge.pci(YXC[,1], YXC[,2:ncol(YXC)])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ models }
