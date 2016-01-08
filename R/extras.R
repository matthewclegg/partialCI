# extras.R
# Copyright (C) 2016 by Matthew Clegg

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/


# A few extra helper routines for the egcm module.

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

price_matrix.rm.na <- function (DF, target=1) {
  # Heuristically removes NA's from a data.frame representing a matrix of
  # prices. 
  
  # Remove all rows where the target has an NA
  DF <- DF[!is.na(DF[,target]),]
  
  # Remove all rows where there are two or more NA's
  DF <- DF[apply(DF, 1, function(x) sum(is.na(x)) <= 1),]
  
  # Remove all columns containing two or more NA's
  DF <- DF[,apply(DF, 2, function(x) sum(is.na(x)) <= 1),]
  
  # Remove all rows containing an NA
  DF <- DF[apply(DF, 1, function(x) all(!is.na(x))), ]
  
  DF
}

multigetYahooPrices <- function (
  components,     # Character vector of Yahoo ticker symbols
  start,          # First date of desired data in YYYYMMDD format.
  end,            # Last date of desired data in YYYYMMDD format.
  quiet=FALSE,    # If TRUE, prints information as symbols are fetched
  adjust=TRUE     # If TRUE, adjusted closing prices are returned.
) {
  # Given a list of ticker symbols, fetches the
  # adjusted closing prices of each of the symbols in the
  # list from Yahoo!, and creates a zoo data.frame.
  
  yahooParams <- list(symbol="NA", adjust=adjust, quiet=quiet)
  if (!missing(start)) yahooParams["start"] <- start
  if (!missing(end)) yahooParams["end"] <- end
  
  get_ticker <- function (t) { 
    if (!quiet) cat(t, " ")
    p <- NULL
    #    try (p <- getYahooData (t, startdate, enddate, adjust=adjust) )
    yahooParams["symbol"] <- t
    try (p <- do.call("getYahooData", yahooParams))
    if (is.null(dim(p))) return(NULL)
    zdf <- zoo(data.frame(p$Close), index(p))
    colnames(zdf) <- t
    zdf
  }
  
  all_prices <- lapply(components, get_ticker)
  if (!quiet) cat("\n")
  not_null <- !c(lapply(all_prices, is.null), recursive=TRUE)
  all_prices <- do.call("cbind", all_prices[not_null])
  all_prices
}

yfit.pci <- function(target, factors, 
                  start=as.numeric(format(Sys.Date()-365*2, "%Y%m%d")),   # Starting date 
                  end=as.numeric(format(Sys.Date(), "%Y%m%d")),         # Ending date 
                  na.rm=FALSE,
                  ...) { # Additional parameters to be passed to fit.pci
  # Fetches the price series of a target security and set of potential
  # factor securities from Yahoo and constructs a partial cointegration 
  # model from them.
  
  #    require(TTR)
  prices <- multigetYahooPrices(c(target, factors), start, end, quiet=TRUE)
  if (any(is.na(prices))) {
    if (!na.rm && any(is.na(prices))) {
      has.any <- colnames(prices)[apply(prices,2,function(x) any(is.na(x)))]
      stop("The following tickers have NAs: ", has.any, "\nTry re-running with na.rm=TRUE")
    }
  } else {
    prices <- price_matrix.rm.na(prices)
  }
    
  e <- fit.pci(prices[,1,drop=FALSE], prices[,2:ncol(prices),drop=FALSE], ...)
  e
}

yhedge.pci <- function(target, factors, 
                     start=as.numeric(format(Sys.Date()-365*2, "%Y%m%d")),   # Starting date 
                     end=as.numeric(format(Sys.Date(), "%Y%m%d")),         # Ending date 
                     na.rm=FALSE,
                     ...) { # Additional parameters to be passed to hedge.pci
  # Fetches the price series of a target security and set of potential
  # factor securities from Yahoo and constructs a partial cointegration 
  # model from them.
  
  #    require(TTR)
  prices <- multigetYahooPrices(c(target, factors), start, end, quiet=TRUE)
  if (any(is.na(prices))) {
    if (!na.rm && any(is.na(prices))) {
      has.any <- colnames(prices)[apply(prices,2,function(x) any(is.na(x)))]
      stop("The following tickers have NAs: ", has.any, "\nTry re-running with na.rm=TRUE")
    }
  } else {
    prices <- price_matrix.rm.na(prices)
  }
  
  e <- hedge.pci(prices[,1,drop=FALSE], prices[,2:ncol(prices),drop=FALSE], ...)
  e
}

