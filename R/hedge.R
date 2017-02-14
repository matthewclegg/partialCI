# hedge.R -- functions for finding the optimal hedging portfolio
#  for a given security, using the partially AR(1) and partially cointegrated models.

# Copyright (C) 2016 Matthew Clegg

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


LARGECAPS <- c("TLT", "AAPL", "GOOGL", "GOOG", "MSFT", "BRK-B", "XOM", "AMZN", 
               "GE", "FB", "JNJ", "WFC", "JPM", "PG", "T", "PFE", "WMT", "DIS", 
               "KO", "VZ", "BAC", "V", "HD", "CVX", "ORCL", "INTC", "C", "GILD", 
               "MRK", "CMCSK", "CMCSA", "PEP", "CSCO", "PM", "IBM", "AGN", "AMGN", 
               "BMY", "MO", "NKE", "UNH", "MA", "MDT", "MCD", "CVS", "BA", "MMM", 
               "LLY", "SLB", "ABBV", "WBA", "UPS", "SBUX", "KHC", "CELG", "UTX", 
               "GS", "HON", "AIG", "QCOM", "USB", "COST", "MDLZ", "LOW", "AXP", 
               "MS", "ACN", "ABT", "LMT", "UNP", "RAI", "DHR", "PCLN", "BIIB", 
               "COP", "DOW", "TXN", "CL", "DD", "BLK", "FOX", "ESRX", "REGN", 
               "SPG", "FOXA", "GM", "MET", "TWX", "F", "NFLX", "OXY", "TMO", 
               "CRM", "TWC", "EMC", "PNC", "BK", "PSX", "TJX", "ADBE", "DUK", 
               "TGT", "GD", "SCHW", "NEE", "EOG", "KMB", "FDX", "MCK", "PYPL", 
               "LYB", "MON", "COF", "PSA", "AMT", "SO", "CAT", "AVGO", "ADP", 
               "D", "ALXN", "KR", "DAL", "PRU", "CTSH", "ACE", "KMI", "RTN", 
               "ABC", "AET", "CI", "SYK", "EBAY", "ECL", "BRCM", "GIS", "TRV", 
               "ANTM", "VLO", "ITW", "NOC", "CME", "YHOO", "YUM", "BDX", "HAL", 
               "EMR", "LUV", "PCP", "MNST", "EL", "PX", "VRTX", "BBT", "CB", 
               "CCL", "APD", "STT", "MMC", "MPC", "CAH", "EQR", "ICE", "LB", 
               "APC", "PPG", "ATVI", "STZ", "CCI", "AAL", "JCI", "NSC", "VFC", 
               "HCA", "AEP", "HPE", "AFL", "SYF", "MHFI", "AON", "CSX", "ILMN", 
               "DE", "PCG", "INTU", "ETN", "TEL", "ORLY", "SHW", "HUM", "MYL", 
               "ALL", "AVB", "BSX", "K", "BEN", "DLPH", "EXC", "BXLT", "SRE", 
               "DFS", "CBS", "WM", "WMB", "GGP", "SYY", "AZO", "ZTS", "BHI", 
               "AMAT", "PPL", "STI", "HCN", "PLD", "HPQ", "ROST", "UAL", "EA", 
               "PXD", "FISV", "PRGO", "BF-B", "ADM", "GLW", "FIS", "RCL", "CERN", 
               "MCO", "BAX", "HRL", "ZBH", "DG", "AMP", "EIX", "PEG", "ISRG", 
               "PAYX", "BXP", "HIG", "ROP", "TROW", "TSN", "UA", "HSY", "ADI", 
               "LVLT", "VNO", "VIAB", "PGR", "MAR", "OMC", "ED", "NVDA", "DLTR", 
               "EQIX", "CAG", "XEL", "VTR", "STJ", "PCAR", "TAP", "CMG", "EW", 
               "NTRS", "DPS", "ADS", "APA", "NLSN", "APH", "MU", "SWKS", "MTB", 
               "FITB", "IP", "DVN", "HCP", "CLX", "SWK", "CPB", "CMI", "WY", 
               "EXPE", "ALTR", "ES", "SE", "MJN", "HES", "WEC", "ESS", "RSG", 
               "IR", "DVA", "SJM", "SNDK", "RHT", "WDC", "ADSK", "CTL", "NBL", 
               "DTE", "PFG", "TYC", "IVZ", "ENDP", "ROK", "MHK", "PH", "FE", 
               "BCR", "LNC", "NOV", "A", "L", "SYMC", "TSO", "GPC", "RF", "VMC", 
               "EFX", "DISCA", "AME", "NUE", "HSIC", "WRK", "VRSK", "MSI", "CAM", 
               "DISCK", "LH", "GWW", "XLNX", "WHR", "MAC", "O", "DHI", "M", 
               "LRCX", "HST", "CBG", "AA", "HBI", "TRIP", "CA", "COL", "KMX", 
               "NWL", "HOT", "TSCO", "UHS", "ETR", "SLG", "CTXS", "XL", "FAST", 
               "TXT", "JNPR", "AAP", "CCE", "KEY", "MKC", "LLTC", "MRO", "EMN", 
               "MOS", "NEM", "LEN", "XEC", "WAT", "BBY", "XRX", "GPS", "KIM", 
               "KLAC", "AEE", "JWN", "HRS", "VRSN", "CF", "SIG", "TSS", "STX", 
               "MLM", "HRB", "RL", "MAS", "SNA", "CINF", "WFM", "AKAM", "PNR", 
               "DOV", "CTAS", "ARG", "SRCL", "TIF", "DGX", "IFF", "LLL", "CMS", 
               "BWA", "NDAQ", "MCHP", "WU", "IPG", "BLL", "AMG", "GT", "HBAN", 
               "KSS", "NTAP", "HOG", "BBBY", "SEE", "FCX", "CHRW", "ETFC", "UNM", 
               "KSU", "EXPD", "JBHT", "WYN", "XRAY", "CVC", "PCL", "MAT", "MNK", 
               "HAS", "COH", "SCG", "QRVO", "EQT", "NWS", "CMA", "SPLS", "KORS", 
               "VAR", "GMCR", "SNI", "GAS", "TMK", "FTI", "FFIV", "DRI", "GRMN", 
               "COG", "CNP", "HAR", "AN", "PNW", "URI", "FLR", "PHM", "PVH", 
               "XYL", "RHI", "POM", "WYNN", "LUK", "ALLE", "LEG", "AES", "TGNA", 
               "TE", "ZION", "NI", "AIV", "FSLR", "CPGX", "AVY", "FTR", "NFX", 
               "FLS", "PKI", "IRM", "ADT", "HP", "AIZ", "FMC", "JEC", "NWSA", 
               "PBCT", "OKE", "RIG")

SECTORETFS <- c("XLB",      # Materials Select Sector SPDR
                "XLE",      # Energy Select Sector SPDR
                "XLF",      # Financial Select Sector SPDR
                "XLI",      # Industrial Select Sector SPDR
                "XLK",      # Technology Select Sector SPDR
                "XLP",      # Consumer Staples Select Sector SPDR
                "XLU",      # Utilities Select Sector SPDR
                "XLV",      # Health Care Select Sector SPDR
                "XLY"       # Consumer Discretionary SPDR
              )

hedge.pci <- function (Y, X, 
    maxfact=10,   # Maximum number of factors to include in hedge
    lambda=0,     # Penalty factor to be applied to random walk portion of fit
    use.multicore=TRUE, # TRUE if parallel processing should be used
    minimum.stepsize=0, # Required amount by which the score of the fit should be improved
    verbose = TRUE,     # TRUE if information should be displayed as fitting progresses
    exclude.cols=c(),    # A list of columns from X that should be excluded from consideration
    search_type=c("lasso","full", "limited"), # The type of search that is to be performed
    pci_opt_method=c("jp", "twostep"), # The method used to perform PCI fitting
    ...
    ) {
    # Searches for an optimal hedge of Y using the securities given in X.
    #  
    # Input values
    #   Y:   An N x 1 column vector or data data.frame, 
    #     representing the series that is to be hedged.
    #   X:  An N x L data.frame, where each column represents a possible factor to
    #     be used in an partially cointegrated fit.
    #   maxfact:  The maximum number of factors to be used in modeling Y.
    #   lambda:  This specifies a penalty to be applied to the random walk
    #     portion of the AR(1) model, driving the solution towards one with a minimal value
    #     of sigma_RW
    #   use.multicore:  If TRUE, parallel processing will be used to improve performance.
    #   minimum.stepsize:  If this is non-NA, then the search stops if an improvement
    #     cannot be found of at least this much.
    #   verbose:  If TRUE, information is printed about the search as it progresses.
    #   exclude.cols:  Specifies the indices of columns from X which should be excluded
    #     from consideration as part of the hedge.  Alternative, the list of excluded
    #     columns may be given as a set of strings, in which case they are interpreted
    #     as column names.
    #   search_type:  If "lasso", then the lasso algorithm is used to find the factors
    #     that give the best linear fit.  If "full", then a greedy algorithm is 
    #     used to search for factors to be used in the hedge.  At each step, 
    #     all possible additions to the portfolio are considered, and the best one 
    #     is chosen for inclusion.  If "limited", then at each iteration, a preliminary
    #     screening step is performed to identify the securities with the highest 
    #     correlations to the residuals of the currently selected portfolio.  The 
    #     top securities from this list are then checked for whether they would 
    #     improve the portfolio, and the best one is included.
    #
    #  Fits the model
    #
    #  Y[t] = alpha + beta[1] X_i1[t,1] + beta[2] X_i2[t,2] + ... + beta [m] X_iM[t,k]
    #    + m[t] + r[t]
	#
	# where
	#
    #   X_ij is a column from X, 1 <= ij <= M
    #   m[t] = rho * m[t-1] + eps_M[t]
    #   r[t] = r[t-1] + eps_R[t]
	#   eps_M,t ~ N(0,sigma_M^2)
    #   eps_R,t ~ N(0,sigma_R^2)
    #   r[0] = 0
    #
    # Searches for up to maxfact factors from X which provided the best
    # fit of an partially cointegrated model to Y.  
    #
    # Returns an S3 object of class pci.hedge which describes the hedge that was found.

    pci_opt_method <- match.arg(pci_opt_method)    
    if (length(exclude.cols) > 0 && is(exclude.cols, "character")) {
      if (!all(exclude.cols %in% colnames(X))) {
        missing.cols <- exclude.cols[!(exclude.cols %in% colnames(X))]
        stop("exclude.cols contains column names not found in X: ", missing.cols)
      } 
      ch.exclude.cols <- exclude.cols
      exclude.cols <- match(ch.exclude.cols, colnames(X))
    }

    target_in_source <- which(sapply(1:ncol(X), function(k) all(Y == X[,k])))
    if (length(target_in_source) > 0) {
      #       cat("Found target at column ", target_in_source, "\n\n")
      exclude.cols <- union(exclude.cols, target_in_source)
    }
    
    Imap <- setdiff(1:ncol(X), exclude.cols)
    Xexcl <- X[,Imap]
    
    fit <- switch(match.arg(search_type),
        lasso = multihedge.pci.lasso (Y, Xexcl, maxfact=maxfact, 
           lambda=lambda, use.multicore=use.multicore,
           minimum.stepsize=minimum.stepsize,
           verbose=verbose,  
           pci_opt_method=pci_opt_method, ...),
        full = multihedge.pci.greedy (Y, Xexcl, maxfact=maxfact, 
            lambda=lambda, use.multicore=use.multicore,
            minimum.stepsize=minimum.stepsize,
            verbose=verbose, 
            pci_opt_method=pci_opt_method, ...),
        limited = multihedge.pci.branching (Y, Xexcl, maxfact=maxfact, 
            lambda=lambda, use.multicore=use.multicore,
            minimum.stepsize=minimum.stepsize,
            verbose=verbose, 
            pci_opt_method=pci_opt_method, ...)
        )
    
    fit$indexes <- Imap[fit$indexes]
    fit$target <- Y
    fit$factors <- X
    fit
}

print.pci.hedge <- function (x, ...) {
  
  print.internal.pci.hedge(x)
}

print.internal.pci.hedge <- function (AH) {
    # Prints summary information for an partially cointegrated hedge model
    print(AH$pci)
}

plot.pci.hedge <- function (x, ...) {
 
  
  plot.internal.pci.hedge(x)
}


plot.internal.pci.hedge <- function (AH) {
    plot(AH$pci)
}

multihedge.pci.general <- function (
  Y,                     # Target that is to be hedged
  X,                     # A matrix of factors to be considered for use in hedging
  find_branches,         # A function which returns a list of branches to be
                         # examined from the current node.
  maxfact=10,            # Maximum number of factors to include in hedge
  use.multicore=TRUE,    # TRUE if multiple CPU's should be used
  minimum.stepsize=0.5,  # Cutoff score used to terminate early
  verbose = TRUE,        # TRUE if information should be printed about execution
  lambda=0,              # Penalty factor to be applied to random walk portion of fit
  ...                    # Additional parameters to be passed to PCI routines
) {  
  # Constructs a sequence of hedges of Y in terms of securities chosen from X.
  # Uses a modified greedy algorithm to search for the best hedge.  Given a
  # current node specified as a collection I of columns from X, the function
  # find_branches(I) returns a list of nodes that should be explored next.
  # The best of these nodes is added to I and then the search continues.

  
  factor_score <- function (I) {
    # On input, I is a collection of indexes.  Computes the hedge score
    # for Y hedged against X[,I].  Lower scores are better.
    #       tf <- test.pci(Y, X[,I,drop=FALSE], null_hyp="rw")
    #       tf$statistic
    score <- likelihood_ratio.pci(Y, X[,I,drop=FALSE], null_model="rw", ...)
    score
  }

  find_factor <- function (I) {
    # On input, I is a collection of indexes.  For each index j not in I,
    # computes the hedge score of that index.  Returns I \cup {j*}, where j*
    # is the index of that factor that most improves the factor score.
    J <- find_branches(I)
    if (length(J) == 0) return(I)
    if (use.multicore) {
      jscores <- mclapply(J, function(js) factor_score(c(I,js)))
    } else {
      #           jscores <- sapply(J, function (js) factor_score(c(I, js)))
      jscores <- lapply(J, function (js) factor_score(c(I, js)))
    }
    jscores <- c(jscores, recursive=TRUE)
    jstar <- J[which.min(jscores)]
    c(I, jstar)
  }
  
  if (verbose && !is.null(names(Y))) {
    cat(sprintf("Hedges computed for %s\n\n", names(Y)[1]))
  }
  
  if (verbose) cat(sprintf("%8s %8s %8s %8s %8s %8s %8s |   Factor coefficients\n", "-LL", "LR[rw]", "p[rw]", "p[mr]", "rho", "R^2[MR]", "Factor"))
  
  I <- c()
  #   debug (find_factor)
  score <- 0
  pci.fit <- NULL
  while (length(I) < maxfact && (length(I) < ncol(X))) {
    Inew <- find_factor(I)
    if (length(Inew) == length(I)) break
    pci.fit <- fit.pci(Y, X[,Inew,drop=FALSE], lambda=lambda, ...)
    tr <- test.pci(Y, X[,Inew,drop=FALSE], null_hyp="rw", ...)
    tm <- test.pci(Y, X[,Inew,drop=FALSE], null_hyp="ar1", ...)
    score.new <- factor_score(Inew)
    if ((length(I) > 0) && !is.na(minimum.stepsize) && 
        (score.new + minimum.stepsize > score)) break;
    if (verbose) {
      cat(sprintf("%8.2f %8.4f %8.4f %8.4f %8.4f %8.4f %8s | ", pci.fit$negloglik, score.new,
                  tr$p.value, tm$p.value,
                  pci.fit$rho, pci.fit$pvmr, colnames(X)[Inew[length(Inew)]]))
      for (j in 1:length(Inew)) {
        betaj_str <- paste("beta_", colnames(X)[Inew[j]], sep="")
        betaj <- pci.fit$beta[[betaj_str]]
        cat(sprintf("%8.4f ", betaj))
      }
      cat("\n")
    }
    score <- score.new
    I <- Inew
  }
  
  pci.fit <- fit.pci(Y, X[,I,drop=FALSE], lambda=lambda, ...)
  
  fit <- structure(list(pci = pci.fit,
                        indexes = I,
                        index_names = colnames(X)[I]
  ),
  class="pci.hedge")
  
  if (verbose) printf("\n")
  fit
}

multihedge.pci.lasso <- function (
  Y, 
  X, 
  ...
) {  
  # Constructs a sequence of hedges of Y in terms of securities chosen from X
  # Uses the lasso to search for a hedge of Y in terms of securities
  # from X that is significant as an PCI model.  
  
  lasso <- glmnet(X, Y)
  
  find_branches <- function (I) {
    # On input, I is a collection of indexes.  For each index j not in I,
    # computes the hedge score of that index.  Returns I \cup {j*}, where j*
    # is the index of that factor that most improves the factor score.
    J <- c()
    ix <- 1
    while (all(J %in% I)) {
      ix <- ix + 1
      J <- which(lasso$beta[,ix] != 0)
    }
    return(setdiff(J, I)[1])
  }
  
  multihedge.pci.general(Y, X, find_branches, ...)  
}

multihedge.pci.branching <- function (
  Y, 
  X, 
  max.branch = 10,
  lambda=0,     # Penalty factor to be applied to random walk portion of fit
  pci_opt_method=c("jp", "twostep"), # The method used to perform PCI fitting
  robust=FALSE, # TRUE if robust fitting should be used
  ...
) {  
  # Constructs a sequence of hedges of Y in terms of securities chosen from X
  # Uses a modified greedy algorithm to search for a hedge of Y in terms of securities
  # from X that is significant as an PCI model.  

  pci_opt_method <- match.arg(pci_opt_method)  
  DX <- diff(X)
  
  find_branches <- function (I) {
    # On input, I is a collection of indexes.  For each index j not in I,
    # calculates a ranking for that index.  Returns the top max.branch
    # indexes.
    J <- setdiff(1:ncol(X), I)
    if (length(J) <= max.branch) return(J)
    
    if (length(I) == 0) {
      Z <- Y
    } else {
      pci.fit <- fit.pci(Y, X[,I], lambda=lambda, 
                         pci_opt_method=pci_opt_method, robust=robust)
      Z <- pci.fit$residuals
    }
    
    DZ <- diff(Z)
    scores <- cor(DZ, DX[,J])
    best.scores <- J[order(scores, decreasing=TRUE)][1:max.branch]
    best.scores
  }

  multihedge.pci.general(Y, X, find_branches, lambda=lambda, 
                         pci_opt_method=pci_opt_method, robust=robust, ...)  
}

multihedge.pci.greedy <- function (
  Y, 
  X, 
  ...
) {
   
  # Constructs a sequence of hedges of Y in terms of securities chosen from X
  # Uses a greedy algorithm to search for a hedge of Y in terms of securities
  # from X that is significant as an PCI model.  
  
  find_branches <- function (I) {
    setdiff(1:ncol(X), I)
  }

  multihedge.pci.general(Y, X, find_branches, ...)  
}

