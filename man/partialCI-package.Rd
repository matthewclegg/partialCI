\name{partialCI-package}
\alias{partialCI-package}
\alias{partialCI}
\docType{package}
\title{
\packageTitle{partialCI} % Partial Cointegration Models
}
\description{
A collection of time series is said to be partially cointegrated if they have a linear
combination that is partially autoregressive, e.g., that can be represented as a sum of an
autoregressive series and a random walk.  This may be useful in modeling
certain sets of financial time series.

To find the partially cointegrated model that best fits two series X and Y, use:

\preformatted{
> fit.pci(Y, X)
}

An interface to Yahoo! Finance permits you to find the best fits for two particular 
stocks of interest:

\preformatted{
> yfit.pci("RDS-B", "RDS-A")
Fitted values for PCI model
  Y[t] = alpha + X[t] %*% beta + M[t] + R[t]
  M[t] = rho * M[t-1] + eps_M [t], eps_M[t] ~ N(0, sigma_M^2)
  R[t] = R[t-1] + eps_R [t], eps_R[t] ~ N(0, sigma_R^2)

           Estimate Std. Err
alpha        0.2063   0.8804
beta_RDS-A   1.0531   0.0133
rho          0.9055   0.0355
sigma_M      0.2431   0.0162
sigma_R      0.0993   0.0350

-LL = 41.30, R^2[MR] = 0.863
}

This example was run on 1/7/2016. RDS-A and RDS-B are two classes of shares offered by 
Royal Dutch Shell that differ slightly in aspects of their tax treatment. The above fit 
shows that the spread between the two shares is mostly mean-reverting but that it contains 
a small random walk component. The mean-reverting component accounts for 86.3\% of 
the variance of the daily returns. The value of 0.9055 for rho corresponds to a half-life 
of mean reversion of about 7 trading days.

To test the goodness of fit, the \code{test.pci} function can be used:

\preformatted{
> h <- yfit.pci("RDS-B", "RDS-A")
> test.pci(h)

	Likelihood ratio test of [Random Walk or CI(1)] vs Almost PCI(1) (joint penalty method)

data:  h

Hypothesis              Statistic    p-value
Random Walk                 -4.94      0.010
AR(1)                       -4.08      0.010
Combined                               0.010
}

The \code{test.pci} function tests each of two different null hypotheses:
(a) the residual series is purely a random walk, and (b) the residual series is
purely autoregressive.  In addition, the union of these hypothesis is
also tested.  For practical applications, one is usually most interested in
rejecting the first of these null hypotheses, e.g., that the residual series
is purely a random walk.

The partialCI package also contains a function for searching for hedging portfolios. 
Given a particular stock (or time series), a search can be conducted to find the set 
of stocks that best replicate the target stock. In the following example, a hedge is 
sought for SPY using sector ETF's.

\preformatted{
> sectorETFS <- c("XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY")
> prices <- multigetYahooPrices(c("SPY", sectorETFS), start=20140101)
> hedge.pci(prices[,"SPY"], prices)
     -LL   LR[rw]    p[rw]    p[mr]      rho  R^2[MR]   Factor |   Factor coefficients
  490.67  -1.7771   0.1782   0.0100   0.9587   0.8246      XLF |   6.8351 
  283.26  -4.3988   0.0137   0.0786   0.9642   1.0000      XLK |   3.6209   2.2396 
  168.86  -6.4339   0.0100   0.0100   0.7328   0.6619      XLI |   2.3191   1.6542   1.1391 

Fitted values for PCI model
  Y[t] = alpha + X[t] %*% beta + M[t] + R[t]
  M[t] = rho * M[t-1] + eps_M [t], eps_M[t] ~ N(0, sigma_M^2)
  R[t] = R[t-1] + eps_R [t], eps_R[t] ~ N(0, sigma_R^2)

           Estimate Std. Err
alpha       14.2892   1.5598
beta_XLF     2.3191   0.1439
beta_XLK     1.6542   0.0804
beta_XLI     1.1391   0.0662
rho          0.7328   0.1047
sigma_M      0.2678   0.0315
sigma_R      0.2056   0.0401

-LL = 168.86, R^2[MR] = 0.662
}

The top table displays the quality of the fit that is found as each new factor is 
added to the fit. The best fit consisting of only one factor is found by using XLF 
(the financials sector). The negative log likelihod score for this model is 490.67. 
However, the random walk hypothesis (p[rw]) cannot be rejected at the 5\% level. 
When adding XLK (the technology sector), the negative log likelihood drops to 283.26 
and the random walk hypothesis for the spread can now be rejected. This means that SPY 
is at least partially cointegrated and possibly fully cointegrated with a portfolio 
consisting of XLF and XLK in the right proportions. The best overall fit is obtained 
by also adding XLI (industrials) to the hedging portfolio. The final fit is

\preformatted{
  SPY = $14.29 + 2.32 XLF + 1.65 XLK + 1.14 XLI
}

For this fit, the proportion of variance attributable to the mean reverting 
component is 66.2\%, and the half life of mean reversion is about 2.2 days.

Please feel free to write to me if you have questions or suggestions.

Matthew Clegg  

Jan 7, 2016
}
\author{
Matthew Clegg \email{matthewcleggphd@gmail.com}
}

\seealso{
\code{\link{fit.pci}} 
\code{\link{yfit.pci}}
\code{\link{test.pci}} 
\code{\link{hedge.pci}}
\code{\link{yhedge.pci}}
}

\keyword{ ts }
\keyword{ models }% __ONLY ONE__ keyword per line
