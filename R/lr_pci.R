# Functions for computing likelihoods, likelihood ratios and likelihood ratio
# tests for partially cointegrated (PCI) series

pci.nu.default <- function () 5

loglik.pci.fkf <- function (Y, X, alpha, beta, rho, sigma_M, sigma_R, M0=0, R0=0) {
    # Given a sequence Y, a basis X, and a parameterization 
    # (beta, rho, sigma_M, sigma_R) of an
    # associated PCI process, calculates the negative log likelihood that
    # Y would be observed under these process parameters.  

    if (is.null(dim(X)) && (length(beta) == 1)) {
      Z <- as.numeric(Y - X * beta - alpha)
    } else {
      Z <- as.numeric(Y - X %*% beta - alpha)
    }
    if (missing(R0)) R0 <- Z[1]
    partialAR:::loglik.par.fkf (Z, rho, sigma_M, sigma_R, M0, R0)
}

loglik.pci.ss <- function (Y, X, alpha, beta, rho, sigma_M, sigma_R, M0=0, R0=0) {
    # Given a sequence Y, basis X, and a parametrization 
    # (beta, rho, sigma_M, sigma_R) of an 
    # associated PCI process, calculates the negative log likelihood that Y would
    # be observed under these process parameters, using a steady state 
    # Kalman filter.

    if (is.null(dim(X)) && (length(beta) == 1)) {
      Z <- as.numeric(Y - X * beta - alpha)
    } else {
      Z <- as.numeric(Y - X %*% beta - alpha)
    }
    if (missing(R0)) R0 <- Z[1]
    partialAR:::loglik.par.ss (Z, rho, sigma_M, sigma_R, M0, R0)
}

loglik.pci.css <- function (Y, X, alpha, beta, rho, sigma_M, sigma_R, M0=0, R0=0) {
    # Given a sequence Y, basis X, and a parametrization 
    # (beta, rho, sigma_M, sigma_R) of an 
    # associated PCI process, calculates the negative log likelihood that Y would
    # be observed under these process parameters, using a steady state 
    # Kalman filter.  Uses a C implementation.

    if (is.null(dim(X)) && (length(beta) == 1)) {
      Z <- as.numeric(Y - X * beta - alpha)
    } else {
      Z <- as.numeric(Y - X %*% beta - alpha)
    }
    if (missing(R0)) R0 <- Z[1]
    partialAR:::loglik_par_c (Z, rho, sigma_M, sigma_R, M0, R0)
}

loglik.pci.sst <- function (Y, X, alpha, beta, rho, sigma_M, sigma_R, M0=0, R0=0, nu=pci.nu.default()) {
    # Given a sequence Y, basis X, and a parametrization 
    # (beta, rho, sigma_M, sigma_R) of an 
    # associated PCI process, calculates the negative log likelihood that Y would
    # be observed under these process parameters, using a steady state 
    # Kalman filter and a robust probability distribution.

    if (is.null(dim(X)) && (length(beta) == 1)) {
      Z <- as.numeric(Y - X * beta - alpha)
    } else {
      Z <- as.numeric(Y - X %*% beta - alpha)
    }
    if (missing(R0)) R0 <- Z[1]
    partialAR:::loglik.par.ss.t (Z, rho, sigma_M, sigma_R, M0, R0, nu=nu)
}

loglik.pci.csst <- function (Y, X, alpha, beta, rho, sigma_M, sigma_R, M0=0, R0=0, nu=pci.nu.default()) {
    # Given a sequence Y, basis X, and a parametrization 
    # (beta, rho, sigma_M, sigma_R) of an 
    # associated PCI process, calculates the negative log likelihood that Y would
    # be observed under these process parameters, using a steady state 
    # Kalman filter and a robust probability distribution.  Uses a C implementation.

    if (is.null(dim(X)) && (length(beta) == 1)) {
      Z <- as.numeric(Y - X * beta - alpha)
    } else {
      Z <- as.numeric(Y - X %*% beta - alpha)
    }
    if (missing(R0)) R0 <- Z[1]
    partialAR:::loglik_par_t_c (Z, rho, sigma_M, sigma_R, M0, R0, nu)
}

loglik.pci <- function (Y, X, alpha, beta, rho, sigma_M, sigma_R, M0=0, R0=0, 
    calc_method=c("css", "fkf", "ss", "sst", "csst"), nu=pci.nu.default()) {
    # Given a sequence Y, basis X, and a parameterization (beta, rho, sigma_M, sigma_R) of an
    # associated PCI process, calculates the negative log likelihood that
    # Y would be observed under these process parameters.  The method used
    # for calculating the log likelihood is determined by "par_method":
    #   fkf:  Uses the Fast Kalman Filter (fkf) package
    #   ss:   Uses a steady state Kalman filter
    #   css:  Uses a steady state Kalman filter coded in C
    
    switch(match.arg(calc_method),
      fkf=loglik.pci.fkf(Y, X, alpha, beta, rho, sigma_M, sigma_R, M0, R0),
      ss=loglik.pci.ss(Y, X, alpha, beta, rho, sigma_M, sigma_R, M0, R0),
      css=loglik.pci.css(Y, X, alpha, beta, rho, sigma_M, sigma_R, M0, R0),
      sst=loglik.pci.sst(Y, X, alpha, beta, rho, sigma_M, sigma_R, M0, R0, nu=nu),
      csst=loglik.pci.csst(Y, X, alpha, beta, rho, sigma_M, sigma_R, M0, R0)
      )
    
}

likelihood_ratio.pci <- function (
    Y,                       # The series which is being fit
    X,                       # The basis used for hedging X 
    robust=FALSE,            # If TRUE, robust estimations are performed                  
    null_model=c("rw", "ar1"),  # Specifies the null hypothesis
                             # rw = null model estimates sigma_R, assuming rho = sigma_M = 0.
                             #      This is the default.
                             # ar1 = null model estimates rho and sigma_M, assuming sigma_R = 0.
    pci_opt_method=c("jp", "twostep"),
                             # Method to be used for fitting Y to X.
                             #   jp:  The coefficients of Y are jointly optimized
                             #     with the parameters of the AAR fit of the residuals
                             #   twostep: A modified Engle-Granger procedure is used, where
                             #     the coefficients of Y are first estimated, and then an AAR
                             #     model is fit to the residuals.
    nu=5                     # If robust is TRUE, the degrees of freedom parameter                          
) {
    null_model <- match.arg(null_model)
    pci_opt_method <- match.arg(pci_opt_method)
        
    f.alt <- fit.pci(Y, X, robust=robust, pci_opt_method=pci_opt_method, nu=nu)
    f.null <- fit.pci(Y, X, robust=robust, par_model=null_model, pci_opt_method=pci_opt_method, nu=nu)
    f.alt$negloglik - f.null$negloglik   
}

sample.likelihood_ratio.pci <- function (n=500, 
    alpha=1.0, beta=1.0, sigma_C=1.0,
    rho=0.8, sigma_M=1.0, sigma_R=1.0, nrep=1000, use.multicore=TRUE,
    pci_opt_method=c("jp", "twostep"),
    robust=FALSE, nu=pci.nu.default(), seed.start=0) {
    # Generates random PCI sequences with the specified parameters for
    #  alpha, beta, sigma_C, rho, sigma_M, sigma_R and nu
    # For each such random sequence, performs RW, AR(1) and AAR fits to the residuals
    # of an PCI model using the optimization method pci_opt_method.  Creates a 
    # data.frame containing the statistics obtained for each of the fits.
    
    pci_opt_method <- match.arg(pci_opt_method)
    
    sample1 <- function (seed) {
        set.seed(seed)
        XY <- rpci(n, alpha, beta, sigma_C, rho, sigma_M, sigma_R, robust=robust)
        Y <- XY[,1,drop=FALSE]
        X <- XY[,2:ncol(XY),drop=FALSE]
        frw <- fit.pci(Y, X, robust=robust, pci_opt_method=pci_opt_method, par_model="rw", nu=nu)
        fmr <- fit.pci(Y, X, robust=robust, pci_opt_method=pci_opt_method, par_model="ar1", nu=nu)
        fpar <- fit.pci(Y, X, robust=robust, pci_opt_method=pci_opt_method, par_model="par", nu=nu)
        c(n, alpha, beta, sigma_C, 
          rho, sigma_M, sigma_R, pci_opt_method, robust, nu, seed,
          frw$rho, frw$sigma_M, frw$sigma_R, frw$negloglik,
          fmr$rho, fmr$sigma_M, fmr$sigma_R, fmr$negloglik,
          fpar$rho, fpar$sigma_M, fpar$sigma_R, fpar$negloglik, 
          fpar$negloglik - frw$negloglik, fpar$negloglik - fmr$negloglik,
          fpar$pvmr)
    }
#    debug(sample1)

    pvec_sample <- function(N) {
        lapply(N, function(k) sample1(k))
    }
    
    if (use.multicore) {
        samples <- parallel::pvec(seed.start + 1:nrep, pvec_sample)
    } else {
        samples <- lapply(seed.start + 1:nrep, sample1)
    }
    df <- as.data.frame(do.call("rbind", samples))
    beta_names <- paste("beta", 1:length(beta), sep="")
    sigma_C_names <- paste("sigma_C", 1:length(sigma_C), sep="")
    colnames(df) <- c("n", "alpha", beta_names, sigma_C_names,
                      "rho", "sigma_M", "sigma_R", "pci_opt", "robust", "nu", "seed",
                      "rw_rho", "rw_sigma_M", "rw_sigma_R", "rw_negloglik",
                      "mr_rho", "mr_sigma_M", "mr_sigma_R", "mr_negloglik",  
                      "pci_rho", "pci_sigma_M", "pci_sigma_R", "pci_negloglik",
                      "rw_lrt", "mr_lrt", "pvmr")
    df 
}

pci.generate.likelihood_ratio.samples <- function (sample_dir = "samples",
    nrep = 1000,
    sample_size=c(50, 100, 150, 200, 250, 300, 400, 500, 600, 700, 750, 800, 900, 1000, 1250, 
        1500, 1750, 2000, 2250, 2500), 
#    rho=c(0.5, 0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1.0),
    rho=c(0.9),
    sigma_M=c(0.0, 1.0),
    sigma_R=c(0.0, 1.0),
    pci_opt_method=c("jp", "twostep"),
    robust=c(FALSE, TRUE), 
    skip.existing=FALSE,
    ...
) {
    # For each combination of sample size, rho and robust, generates
    # nrep likelihood_ratio samples with those parameters,
    # and writes the result to a file whose name is in the form
    #  sample_dir/PCI.SAMPLES.nn.rr[.ROB]
    params.df <- expand.grid(sample_size, robust, rho, sigma_M, sigma_R, pci_opt_method, stringsAsFactors=FALSE)
    colnames(params.df) <- c("sample_size", "robust", "rho", "sigma_M", "sigma_R", "pci_opt")
    # Ensure that exactly one of (sigma_M, sigma_R) is non-zero
    params.df <- params.df[params.df$sigma_M != 0.0 | params.df$sigma_R != 0.0,]
    params.df <- params.df[params.df$sigma_M == 0.0 | params.df$sigma_R == 0.0,]
    dir.create(sample_dir, showWarnings=FALSE, recursive=TRUE)
    seed.start <- 1
    for (i in 1:nrow(params.df)) {
        robstring <- if (params.df$robust[i]) ".ROB" else ""
        fn <- sprintf("%s/PCI.SAMPLES.%d.%02d.%02d.%02d.%s%s", sample_dir, 
            params.df$sample_size[i], 
            floor(100.0 * params.df$rho[i]),
            floor(100.0 * params.df$sigma_M[i]),
            floor(100.0 * params.df$sigma_R[i]),
            params.df$pci_opt[i],
            robstring)
        if (!skip.existing || !file.exists(fn)) {
            printf("%s ", Sys.time())
            lrdf <- sample.likelihood_ratio.pci(n=params.df$sample_size[i], 
                alpha = 1.0, beta=1.0, sigma_C=1.0,
                rho=params.df$rho[i],
                sigma_M=params.df$sigma_M[i], sigma_R=params.df$sigma_R[i],
                nrep=nrep, robust=params.df$robust[i], 
                pci_opt_method=params.df$pci_opt[i],
                seed.start=seed.start, ...)
            write.table(lrdf, fn)
            printf("%s\n", fn)
        }
        seed.start <- seed.start + nrep
    }
}

pci.load.likelihood_ratio.samples <- function (sample_dir = "samples") {
    # Reads all of the samples in the specified samples dir, and returns
    # a data.frame containing the results
    
    files <- list.files(sample_dir, "PCI.SAMPLES", full.names=TRUE)
    PCI.SAMPLES <<- do.call("rbind", lapply(files, 
        function(f) read.table(f, header=TRUE, stringsAsFactors=FALSE)))
    "Samples loaded into PCI.SAMPLES"
}

sample.pci.lrt.nullrw.likelihood_ratio <- function (n=500, nrep=1000, ncol=1, use.multicore=TRUE) {
    # Performs a likelihood ratio test of random walk null hypothesis on nrep randomly generated 
    # PCI sequences and returns the log likelihood ratios that are found.

    do_rep <- function (dummy=0) {        
        # I tried two different approaches to generating random PCI sequences.
        # In the first approach, all of the coefficients and s.d.'s are set identically to 1.
        # In the second approach, coefficients and s.d.'s are chosen randomly.
        # With the second approach, the likelihood scores were slightly more tightly 
        # distributed.  So, the first approach was taken, as this seems more conservative.
        #
        # Also, I looked at the distribution of likelihood scores as a function of the number
        # of columns.  No detectable difference was found.
#        beta <- rnorm(ncol)*10
#        sigma_C <- rnorm(ncol)^2
#        Y <- rpci(n, 1, beta, sigma_C, 0, 0, 1)
        Y <- rpci(n, 1, rep(1, ncol), rep(1, ncol), 0, 0, 1)
        likelihood_ratio.pci(Y[,1], Y[,2:ncol(Y),drop=FALSE], null_model="rw")
    }
    
    multirep <- function (X) {
        sapply(X, do_rep)
    }

    if (use.multicore) {
        parallel::pvec(1:nrep, multirep)
    } else {
        replicate(nrep, do_rep())
    }
}

sample.pci.lrt.nullmr.likelihood_ratio <- function (n=500, rho=1.0, nrep=1000, ncol=1, use.multicore=TRUE) {
    # Performs a likelihood ratio test of pure AR(1) null hypothesis on nrep randomly generated 
    # pure mean-reverting sequences and returns the log likelihood ratios that are found.

    do_rep <- function (dummy=0) {
#        x <- rpar(n, rho, 1, 0)
#        likelihood_ratio.par.rw0(x)
        Y <- rpci(n, 1, rep(1, ncol), rep(1, ncol), rho, 1, 0)
        likelihood_ratio.pci(Y[,1], Y[,2:ncol(Y),drop=FALSE], null_model="ar1")        
    }
    
    multirep <- function (X) {
        sapply(X, do_rep)
    }

    if (use.multicore) {
        parallel::pvec(1:nrep, multirep)
    } else {
        replicate(nrep, do_rep())
    }
}

pci.find.joint.critical.values <- function (alpha, n=500, robust=FALSE, nest=TRUE,
                                            pci_opt_method=c("jp","twostep")) {
  # Given an acceptance value alpha (or a vector of such acceptance values), 
  # calculates likelihood scores Lambda_R and Lambda_M such that the probability
  # is no more than alpha that a random sample will have likelihood ratio for the 
  # random walk no more than Lambda_R and likelihood ratio for the AR(1) hypothesis
  # no more than Lambda_M.  For the inverse, see par.joint.dist.
  
  pci_opt_method <- match.arg(pci_opt_method)
  pn <- n
  probust <- robust
  rw <- NULL  # Make R CMD check happy
  mr <- NULL  # Make R CMD check happy
  sigma_M <- NULL
  sigma_R <- NULL
  pci_opt <- NULL 
  
  if (!exists("PCI.SAMPLES.DT")) {
    if (!exists("PCI.SAMPLES")) pci.load.likelihood_ratio.samples()
    PCI.SAMPLES.DT <<- data.table(PCI.SAMPLES)
  }
  setkey(PCI.SAMPLES.DT, n, sigma_M, sigma_R, robust, pci_opt)
  AM <- PCI.SAMPLES.DT[list(pn, 1, 0, probust, pci_opt_method),]
  AR <- PCI.SAMPLES.DT[list(pn, 0, 1, probust, pci_opt_method),]
  
  if (nrow(AM) == 0 || nrow(AR) == 0) stop ("No matching samples found") 
  
  AM <- AM[,c("rw_lrt", "mr_lrt"),with=FALSE]
  AR <- AR[,c("rw_lrt", "mr_lrt"),with=FALSE]
  setnames(AM, c("rw", "mr"))
  setnames(AR, c("rw", "mr"))
  
  LR_min <- -Inf
  LM_min <- -Inf
  
  find1 <- function(a) {
    cv <- function(p) {
      LR <- p[1]
      LM <- p[2]
      if (nest && ((LR < LR_min) || (LM < LM_min))) return(3)
      am <- AM[,sum(rw < LR & mr < LM)] / nrow(AM)
      ar <- AR[,sum(rw < LR & mr < LM)] / nrow(AR)
      (am - a)^2 + (ar - a)^2
    }
    
    LR0 <- max(quantile(AR$rw, a), LR_min)
    LM0 <- max(quantile(AM$mr, a), LM_min)
    p <- optim(c(LR0, LM0), cv)
    if (nest) {
      LR_min <<- p$par[1]
      LM_min <<- p$par[2]
    }
    QM <- AM[,sum(rw < p$par[1] & mr < p$par[2])] / nrow(AM)
    QR <- AR[,sum(rw < p$par[1] & mr < p$par[2])] / nrow(AR)
    v <- data.frame(n, robust, pci_opt_method, a, LR0, LM0, p$par[1], p$par[2], QR, QM)
    colnames(v) <- c("n", "robust", "pci_opt", "alpha", "C_R0", "C_M0", "C_R", "C_M", "QR", "QM")
    v
  }
  
  do.call("rbind", lapply(alpha, find1))
}

pci.findall.joint.critical.values <- function (alpha=seq(0.01,0.99,by=0.01),
                                               nest=TRUE,
                                               use.multicore=TRUE, ...) {
  if (!exists("PCI.SAMPLES")) pci.load.likelihood_ratio.samples()
  idf <- expand.grid(n=sort(unique(PCI.SAMPLES$n)), robust=sort(unique(PCI.SAMPLES$robust)),
                     pci_opt=sort(unique(PCI.SAMPLES$pci_opt)), stringsAsFactors=FALSE)
  if (use.multicore) lapply <- mclapply
  do.call("rbind", lapply(1:nrow(idf), function(k)
    pci.find.joint.critical.values(alpha, n=idf$n[k], robust=idf$robust[k], nest=nest, 
                                   pci_opt_method=idf$pci_opt[k], ...)))
}

pci.rw.pvalue <- function (
  stat.rw,                  # Statistic used for testing RW hypothesis.
                            # This is a likelihood ratio
  n,                        # Sample size
  pci_opt_method=c("jp", "twostep"), # Optimization method
  robust=FALSE              # TRUE if robust models should be used
) {
  # Given a statistic on the RW hypothesis (e.g., the value of the
  # likelihood ratio function), returns the corresponding p-value.

  pci_opt_method <- match.arg(pci_opt_method)
  if ((pci_opt_method == "jp") && robust) { 
    p.value <- quantile_table_interpolate(PCI.RWNULL.ROB.JP.LRQT, n, stat.rw)
  } else if (pci_opt_method == "jp") {
    p.value <- quantile_table_interpolate(PCI.RWNULL.JP.LRQT, n, stat.rw)    
  } else if ((pci_opt_method == "twostep") && robust) {
    p.value <- quantile_table_interpolate(PCI.RWNULL.ROB.TWOSTEP.LRQT, n, stat.rw)    
  } else if (pci_opt_method == "twostep") {
    p.value <- quantile_table_interpolate(PCI.RWNULL.TWOSTEP.LRQT, n, stat.rw)    
  }
  
  p.value
}

pci.mr.pvalue <- function (
  stat.mr,                  # Statistic used for testing AR(1) hypothesis
  n,                        # Sample size
  pci_opt_method=c("jp", "twostep"), # Optimization method
  robust=FALSE             # TRUE if robust models should be used
) {
  # Given a statistic on the AR(1) hypothesis, returns the corresponding 
  # p-value.  
  
  pci_opt_method <- match.arg(pci_opt_method)
  if ((pci_opt_method == "jp") && robust) { 
    p.value <- quantile_table_interpolate(PCI.MRNULL.ROB.JP.LRQT, n, stat.mr)
  } else if (pci_opt_method == "jp") {
    p.value <- quantile_table_interpolate(PCI.MRNULL.JP.LRQT, n, stat.mr)    
  } else if ((pci_opt_method == "twostep") && robust) {
    p.value <- quantile_table_interpolate(PCI.MRNULL.ROB.TWOSTEP.LRQT, n, stat.mr)    
  } else if (pci_opt_method == "twostep") {
    p.value <- quantile_table_interpolate(PCI.MRNULL.TWOSTEP.LRQT, n, stat.mr)    
  }

  p.value
}

pci.joint.pvalue <- function (
  stat.rw,                  # Statistic used for testing RW hypothesis.
                            # This is a likelihood ratio
  stat.mr,                  # Statistic used for testing AR(1) hypothesis
                            # This is also a likelihood ratio.
  n,                        # Sample size
  robust=FALSE,             # TRUE if robust models should be used
  pci_opt_method=c("jp", "twostep") # Optimization method
) {
  # Given statistics on the RW hypothesis and AR(1) hypothesis, calculates
  # a p-value associated with those statistics.  The p-value is an estimate
  # of the probability that a random sequence satisfying the null hypothesis
  # of either RW or AR(1) would have statistics more extreme than the
  # statistics (stat.rw, stat.mr)

  pci_opt_method <- match.arg(pci_opt_method)
  
  if (!exists("PCI.JOINT.CRITICAL.VALUES")) stop ("Could not find table of critical values")
  AJCV <- PCI.JOINT.CRITICAL.VALUES
    
  nvals <- unique(AJCV[,"n"])
  if (!any(nvals <= n)) {
    warning("Sample size too small (", n, ") to provide accurate p-value")
    nlower <- 1
    plower <- 1
  } else {
    nlower <- max(nvals[nvals <= n])
    lower.matches <- (AJCV[,"n"] == nlower) & 
      (AJCV[,"robust"] == robust) &
      (AJCV[,"pci_opt"] == pci_opt_method) &
      (stat.rw <= AJCV[,"C_R"]) &
      (stat.mr <= AJCV[,"C_M"])
    if (!any(lower.matches)) {
      plower <- 1
    } else {
      plower <- min(AJCV[lower.matches,"alpha"])
    }
  }
  
  if (any(nvals >= n)) {
    nupper <- min(nvals[nvals > n])
    upper.matches <- (AJCV[,"n"] == nlower) & 
      (AJCV[,"robust"] == robust) &
      (AJCV[,"pci_opt"] == pci_opt_method) &
      (stat.rw <= AJCV[,"C_R"]) &
      (stat.mr <= AJCV[,"C_M"])
    if (!any(upper.matches)) {
      pupper <- 1
    } else {
      pupper <- min(AJCV[upper.matches,"alpha"])
    }
  } else {
    nupper <- n
    pupper <- plower
  }
  
  pval.joint <- (nupper - n)/(nupper - nlower) * plower +
    (n - nlower) / (nupper - nlower) * pupper
  
  pval.joint
}

test.pci.nullrw <- function (Y, X, pci_opt_method=c("jp", "twostep"), robust=FALSE) {
    # Tests the hypothesis that Y with basis X is a random walk vs. the alternative
    # that Y,X is an PCI series.

    DNAME <- paste(deparse(substitute(Y)), "/", deparse(substitute(X)))
    pci_opt_method = match.arg(pci_opt_method)
    STAT <- likelihood_ratio.pci (Y, X, null_model="rw", pci_opt_method=pci_opt_method, robust=robust)
    PVAL <- pci.rw.pvalue(STAT, length(Y), pci_opt_method, robust)
    if (robust) {
        METHOD <- "Likelihood ratio test of Robust Random Walk vs Robust PCI(1)"
        alternative <- "RPCI(1)"
    } else {
        METHOD <- "Likelihood ratio test of Random Walk vs PCI(1)"
        alternative <- "PCI(1)"
    }
    if (pci_opt_method == "jp") {
        METHOD <- paste(METHOD, "(joint penalty method)")
    } else {
        METHOD <- paste(METHOD, "(two step method)")
    }
    names(STAT) <- "LL"
    structure(list(statistic = STAT, alternative = alternative, 
        p.value = PVAL, method = METHOD, data.name = DNAME),
        class = "htest")
}

test.pci.nullmr <- function (Y, X, pci_opt_method=c("jp", "twostep"), robust=FALSE) {
    # Tests whether a sequence Y with basis X conforms to the PCI model.  The null
    # hypothesis is that Y,X is AR(1).  

    DNAME <- paste(deparse(substitute(Y)), "/", deparse(substitute(X)))
    pci_opt_method = match.arg(pci_opt_method)
    STAT <- likelihood_ratio.pci (Y, X, null_model="ar1", pci_opt_method=pci_opt_method, robust=robust)
    PVAL <- pci.mr.pvalue(STAT, length(Y), pci_opt_method, robust)
    if (robust) {
        METHOD <- "Likelihood ratio test of Robust Cointegrated vs Robust PCI(1)"
        alternative <- "RPCI(1)"
    } else {
        METHOD <- "Likelihood ratio test of cointegrated vs partially cointegrated"
        alternative <- "PCI(1)"
    }
    if (pci_opt_method == "jp") {
        METHOD <- paste(METHOD, "(joint penalty method)")
    } else {
        METHOD <- paste(METHOD, "(two step method)")
    }
    names(STAT) <- "LL"
    structure(list(statistic = STAT, alternative = alternative, 
        p.value = PVAL, method = METHOD, data.name = DNAME),
        class = "htest")
}

test.pci <- function (Y, 
    X,
    alpha=0.05,               # Critical value to be used in deciding whether to reject the null.
    null_hyp=c("rw", "ar1"),  # Specifies the null hypothesis.  Can be either or both.
                              # rw = null model estimates sigma_R, assuming rho = sigma_M = 0.
                              # ar1 = null model estimates rho and sigma_M, assuming sigma_R = 0.
                              # Default is both
    robust=FALSE,             # TRUE if robust models should be used
    pci_opt_method=c("jp", "twostep")  # Method used to search for partially cointegrating relations
) {
    # Tests the null hypothesis that Y is either a random walk or an AR(1)
    # series (or both) against the alternative hypothesis that Y is AAR.

    DNAME <- deparse(substitute(Y))
    pci_opt_method <- match.arg(pci_opt_method)

    if (is(Y, "pci.hedge")) {
        Yhedge <- Y
        Y <- Yhedge$pci
    }
    
    if (is(Y, "pci.fit")) {
        Yfit <- Y
        Y <- Yfit$data
        X <- Yfit$basis
        if (missing(robust)) robust <- Yfit$robust
        if (missing(pci_opt_method)) {
          pci_opt_method <- Yfit$pci.fit
        }
    } 
    
    if (length(null_hyp) == 1) {
        if (null_hyp == "rw") return(test.pci.nullrw(Y, X, pci_opt_method=pci_opt_method, robust=robust))
        if ((null_hyp == "ar1") || (null_hyp == "mr")) return(test.pci.nullmr(Y, X, pci_opt_method=pci_opt_method, robust=robust))
        stop("Error in parameter null_hyp: ", null_hyp)
    }

    Yorig <- Y
    if (!is.null(dim(Y))) {
        if (dim(Y)[2] > 1) {
            if (missing(X)) {
                X <- Y[,2:ncol(Y),drop=FALSE]
            } else {
                stop("Y must be a single column")
            }
        }
        Y <- Y[,1]
    }
    Y <- coredata(Y)
    X <- as.matrix(coredata(X))
    
    frw <- fit.pci(Y, X, par_model="rw", pci_opt_method=pci_opt_method, robust=robust)
    fmr <- fit.pci(Y, X, par_model="ar1", pci_opt_method=pci_opt_method, robust=robust)
    fpar <- fit.pci(Y, X, par_model="par", pci_opt_method=pci_opt_method, robust=robust)
    
    stat.mr <- fpar$negloglik - fmr$negloglik
    stat.rw <- fpar$negloglik - frw$negloglik
    STAT <- c(stat.rw, stat.mr)

    pval.mr <- pci.mr.pvalue(stat.mr, length(Y), pci_opt_method=pci_opt_method, robust=robust)
    pval.rw <- pci.rw.pvalue(stat.rw, length(Y), pci_opt_method=pci_opt_method, robust=robust)
    pval.joint <- pci.joint.pvalue(stat.rw, stat.mr, length(Y), pci_opt_method = pci_opt_method, 
                                   robust=robust)
    
    if (!robust) {
        METHOD <- "Likelihood ratio test of [Random Walk or CI(1)] vs Almost PCI(1)"
        alternative <- "Almost PCI(1)"
    } else {
        METHOD <- "Likelihood ratio test of [Robust Random Walk or Robust CI(1)] vs Robust Almost PCI(1)"
        alternative <- "Robust Almost PCI(1)"
    }

    if (pci_opt_method == "jp") {
        METHOD <- sprintf("%s (joint penalty method)", METHOD)
    } else {
        METHOD <- sprintf("%s (two step method)", METHOD)
    }
    
    PVAL <- c(RW=pval.rw, MR=pval.mr, JOINT=pval.joint)
    
    names(STAT) <- "LL"
    structure(list(statistic = STAT, alternative = alternative, 
        p.value = PVAL, method = METHOD, data.name = DNAME, alpha=alpha, robust=robust),
        class = c("pcitest", "htest"))
}

print.pcitest <- function (AT, alpha=0.05) {
  # See stats:::print.htest
  cat("\n")
  cat(strwrap(AT$method, prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", AT$data.name, "\n", sep = "")
  cat("\n")
  
  if (AT$robust) {
    h0a <- "Robust RW"
    h0b <- "Robust AR(1)"
    h1 <- "Robust PAR"
  } else {
    h0a <- "Random Walk"
    h0b <- "AR(1)"
    h1 <- "PAR"
  }
  
  cat(sprintf("%-12s %20s %10s\n", "Hypothesis", "Statistic", "p-value"))
  cat(sprintf("%-12s %20.2f %10.3f\n", h0a, AT$statistic[1], AT$p.value[1]))
  cat(sprintf("%-12s %20.2f %10.3f\n", h0b, AT$statistic[2], AT$p.value[2]))
  cat(sprintf("%-12s %20s %10.3f\n", "Combined", "", AT$p.value[3]))
  
  cat("\n")
  invisible(AT)
}

which.hypothesis.pcitest <- function(AT) {
    if (is.na(AT$alpha)) {
        return(NA)
    } 
    
    if (AT$p.value[3] < AT$alpha) {
        result <- "PCI"
    } else if (AT$alpha < AT$p.value[1]) {
        result <- "RW"
    } else {
        result <- "AR1"
    }
    
    if (AT$robust) result <- paste("R", result, sep="")
    
    result
}
