\name{rpci}
\alias{rpci}
\title{
Generates a random instance of a partial cointegration model
}
\description{Generates a random instance of a partial cointegration model}
\usage{
rpci(n, alpha, beta, sigma_C, rho, sigma_M, sigma_R, 
  include.state = FALSE, robust = FALSE, nu = 5)
}
\arguments{
  \item{n}{
Number of observations to generate
}
  \item{alpha}{
Constant term of the model
}
  \item{beta}{
A vector of factor loadings
}
  \item{sigma_C}{
A vector of standard deviations
}
  \item{rho}{
The coefficient of mean reversion
}
  \item{sigma_R}{
The standard deviation of the innovations of the random walk portion of the residual series
}
  \item{sigma_M}{
The standard deviation of the innovations of the mean-reverting portion of the residual series
}
  \item{include.state}{
If TRUE, then the output data.frame contains the innovations to the factors and residual 
series, as well as the state of the residual series.  Default: FALSE
}
  \item{robust}{
If TRUE, then a t-distribution is used to generate the innovations.
Otherwise, the innovations are normally distributed.  Default: FALSE.
}
  \item{nu}{
The degrees of freedom parameter used for t-distributed innovations.  Default: 5.
}
}
\details{
Generates a random set of partially cointegrated vectors.  On input, \code{n} is the
length of the sequence to be generated.  \code{beta} is a vector of length k
representing the coefficients of the factor loadings, and \code{sigma_C} is a
vector of length k representing the standard deviations of the increments
of the factor loadings.  
  
Generates a random realization of the sequence
    \deqn{Y_t = \alpha + \beta_1 F_{1,t} + \beta_2 F_{2,t} + ... + \beta_k F_{k,t} + M_t + R_t}
    \deqn{F_{i,j} = F_{i,j-1} + \delta_{i,j}}
    \deqn{M_t = \rho m_{t-1} + \epsilon_{M,t}}
    \deqn{R_t = r_{t-1} + \epsilon_{R,t}}
    \deqn{\delta_{i,j} ~ N(0, \sigma_{C,i}^2)}
    \deqn{\epsilon_{M,t} ~ N(0, \sigma_M^2)}
    \deqn{\epsilon_{R,t} ~ N(0, \sigma_R^2)}
    
}
\value{
A \code{data.frame} of \code{n} rows representing the realization of the partially
cointegrated sequence.

If \code{include.state} is \code{FALSE}, returns an \code{n x (k+1)} matrix whose columns
are \code{y, F_1, F_2, ..., F_k}.  If include.state is \code{TRUE}, returns an
\code{n x (2k + 6)} matrix whose columns are 
 \code{y, F_1, F_2, ..., F_k, x, M, R, delta_1, delta_2, ..., delta_k, epsilon_M, epsilon_R}.
}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}
}

\seealso{
\code{\link{fit.pci}}
}
\examples{
rpci(10, alpha =1, beta=1, sigma_C=1, rho=0.9, sigma_R=1, sigma_M=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ models }% __ONLY ONE__ keyword per line
