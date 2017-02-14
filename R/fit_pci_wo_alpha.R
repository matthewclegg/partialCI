# Functions pertaining to fitting partially cointegrated (PCI) series
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

utils::globalVariables("p0")

fit.pci.twostep.a0 <- function (Y, X, par_model=c("par", "ar1", "rw"), robust=FALSE, nu=5) {
  #  On input, Y is an n x 1 column vector, and X is an n x k matrix.
  #  Fits the model
  #
  #  Y[t] = beta[1] X[t,1] + beta[2] X[t,2] + ... + beta [k] X[t,k]
  #    + m[t] + r[t]
  #
  # where
  #
  #   m[t] = rho * m[t-1] + eps_M[t]
  #   r[t] = r[t-1] + eps_R[t]
  #   eps_M,t ~ N(0,1)
  #   eps_R,t ~ N(0,1)
  #   m[0] = r[0] = 0
  # 
  # Estimates the values of beta, rho, sigma_M and sigma_R using
  # the two step procedure:
  #   1.  Perform a linear regression of Y on X to obtain beta
  #   2.  Determine rho, sigma_M and sigma_R through fitting a PAR model
  #       to the residuals of the linear regression performed in step 1.
  #
  # The parameter par_model specifies which parameters of the PAR model are
  # to be optimized.  
  
    par_model <- match.arg(par_model)
    if (robust) {
        L <- summary(rlm(Y~X))
    } else {
        L <- summary(lm(Y~X))
    }
    PAR <- fit.par(L$residuals, model=par_model, robust=robust, nu=nu)

    fit <- structure(list(
        data=Y,
        basis=X,
        residuals=L$residuals,
        index=1:length(Y),
        alpha = 0,
        beta = coef(L)[2:nrow(coef(L)),1],
        rho = PAR$rho,
        sigma_M = PAR$sigma_M,
        sigma_R = PAR$sigma_R,
        M0 = PAR$par[4],
        R0 = PAR$par[5],
        beta.se = coef(L)[2:nrow(coef(L)),2],
        rho.se = PAR$stderr[1],
        sigma_M.se = PAR$stderr[2],
        sigma_R.se = PAR$stderr[3],
        M0.se = PAR$stderr[4],
        R0.se = PAR$stderr[5],
        negloglik = c(negloglik=PAR$negloglik),
        pvmr = PAR$pvmr,
        par.fit = PAR,
        par_model=par_model,
        robust=robust,
        pci.fit="twostep"), 
    class="pci.fit")
    
    names(fit$beta) <- paste("beta_", colnames(X), sep="")
    names(fit$beta.se) <- paste(paste("beta_", colnames(X), sep=""), ".se", sep="")
    
    fit
}

pci.jointpenalty.guess.a0 <- function (Y, X) {
    # On input, Y is an n x 1 column vector, and X is an n x k matrix.
    # Generates a guess for the starting point of optimization for an PCI
    # fit as follows:
    #   1.  Estimates beta by fitting the differenced X series to the
    #       differenced Y series.
    #   2.  Estimates rho, sigma_M and sigma_R by fitting a PAR series
    #       to  Y - X %*% beta - alpha
    # Returns the guess that is found

    X <- as.matrix(X)
    DY <- diff(Y)
    DX <- apply(X, 2, diff)
    beta0 <- coef(lm(DY ~ DX + 0))
    res0 <- Y - X %*% beta0
    PAR0 <- fit.par(res0)
    rho0 <- PAR0$rho
    sigma_M0 <- PAR0$sigma_M
    sigma_R0 <- PAR0$sigma_R
    M00 <- 0
    p0 <- c(beta0, rho0, sigma_M0, sigma_R0, M00, res0[1])
    names(p0)[1:ncol(X)] <- paste("beta_", colnames(X), sep="")
    names(p0)[length(p0)-1] <- "M0"
    names(p0)[length(p0)] <- "R0"
    p0
}

fit.pci.jointpenalty.rw.a0 <- function (Y, X, lambda=0, p0=pci.jointpenalty.guess(Y,X), 
                                     robust=FALSE, nu=5, pgtol=1e-8) {
    # On input, Y is an n x 1 column vector, and X is an n x k matrix.
    # Fits an PCI model to Y,X where the residual series is modeled as
    # a random walk.  Returns a three-component list:
    #   par: The parameter estimates
    #   se:  The estimated standard error of the parameter estimates
    #   nll: The negative log likelihood of the fit

    if (is.null(dim(Y))) Y <- as.matrix(Y, ncol=1)
    if (is.null(dim(X))) X <- as.matrix(X, ncol=1)
    
    n <- ncol(X) 
    highval <- NA
    ll_calc_method <- if (robust) "csst" else "css"
    
    rw.ofun <- function (p) {
        beta <- p[1:n]
        sigma_R <- p[n+1]
        if (sigma_R <= 0.0) return(highval)
        Z <- Y - X %*% beta
        R0 <- Z[1]
        loglik.par (Z, 0, 0, sigma_R, 0, R0, ll_calc_method, nu=nu) + lambda * sigma_R^2
    }

    beta0 <- p0[1:n]
    res0 <- Y - X %*% beta0
    sdmax <- 2.0 * sd(diff(Y))
    rw.p0 <- c(beta0, sdmax * 0.5)
    rw.pmin <- c(rep(-Inf, n), 0.0)
    rw.pmax <- c(rep(Inf, n), sdmax)
    highval <- rw.ofun(rw.p0) + 1.0

    rw.val <- optim(rw.p0, rw.ofun, method="L-BFGS-B", lower=rw.pmin, upper=rw.pmax, hessian=TRUE, control=list(pgtol=pgtol))
    
    R0.par <- (Y - X %*% rw.val$par[1:n]) [1]
    rw.par <- c(rw.val$par[1:n], rho=0, sigma_M=0, sigma_R=rw.val$par[n+1], M0=0, R0=R0.par)
    names(rw.par)[1:n] <- paste("beta_", colnames(X), sep="")
    rw.se <- rep(NA_real_, nrow(rw.val$hessian))
    suppressWarnings(try(rw.se <- sqrt(diag(solve(rw.val$hessian))), silent=TRUE))
    rw.se <- c(rw.se[1:n], rho=NA, sigma_M=NA, sigma_R=rw.se[n+1], M0=NA, R0=NA)
    names(rw.se)[1:n] <- paste("beta_", colnames(X), sep="")

    return(list(par=rw.par, se=rw.se, nll=c(negloglik=rw.val$value)))
}

fit.pci.jointpenalty.mr.a0 <- function (Y, X, lambda=0, p0=pci.jointpenalty.guess.a0(Y,X), 
                                     robust=FALSE, nu=5, pgtol=1e-8) {
    # On input, Y is an n x 1 column vector, and X is an n x k matrix.
    # Fits an PCI model to Y,X where the residual series is modeled as
    # a pure AR(1) series.  Returns a three-component list:
    #   par: The parameter estimates
    #   se:  The estimated standard error of the parameter estimates
    #   nll: The negative log likelihood of the fit

    if (is.null(dim(Y))) Y <- as.matrix(Y, ncol=1)
    if (is.null(dim(X))) X <- as.matrix(X, ncol=1)
  
    n <- ncol(X) 
    highval <- NA
    ll_calc_method <- if (robust) "csst" else "css"

    mr.ofun <- function (p) {
        beta <- p[1:n]
        rho <- p[n+1]
        sigma_M <- p[n+2]
        if (rho < -1 || rho > 1 || sigma_M <= 0.0) return(highval)
        Z <- Y - X %*% beta
        loglik.par (Z, rho, sigma_M, 0, 0, Z[1], ll_calc_method, nu=nu)
    }

    rw.fit <- fit.pci.jointpenalty.rw.a0 (Y, X, lambda, p0, robust=robust, nu=nu)
    rw.p0 <- c(rw.fit$par[1:n], rho=1, rw.fit$par[n+3])
    names(rw.p0)[n+2] <- "sigma_M"
    
    beta0 <- p0[1:n]
    res0 <- Y - X %*% beta0
    PAR0.mr <- fit.par(res0, model="ar1", robust=robust, nu=nu)   
    mr.p0 <- c(beta0, PAR0.mr$rho, PAR0.mr$sigma_M)
    names(mr.p0)[n+2] <- "sigma_M"
    
    mr.pmin <- c(rep(-Inf, n), -1, 0)
    mr.pmax <- c(rep(Inf, n), 1, 2.0 * rw.p0[n+3])

    highval <- max(mr.ofun(rw.p0), mr.ofun(mr.p0)) + 1
    fit1 <- optim(rw.p0, mr.ofun, method="L-BFGS-B", lower=mr.pmin, upper=mr.pmax, hessian=TRUE, control=list(pgtol=pgtol))
    fit2 <- optim(mr.p0, mr.ofun, method="L-BFGS-B", lower=mr.pmin, upper=mr.pmax, hessian=TRUE, control=list(pgtol=pgtol))
    fit <- if(fit1$value < fit2$value) fit1 else fit2
    
    fit.R0 <- (Y - X %*% fit$par[1:n]) [1]
    mr.par <- c(fit$par[1:(n+2)], sigma_R=0, M0=0, R0=fit.R0)

    mr.se <- rep(NA_real_, nrow(fit$hessian))
    suppressWarnings(try(mr.se <- sqrt(diag(solve(fit$hessian))), silent=TRUE))
    mr.se <- c(mr.se[1:(n+2)], sigma_R=NA, M0=NA, R0=NA)

    return(list(par=mr.par, se=mr.se, nll=c(negloglik=fit$value)))
}

fit.pci.jointpenalty.both.a0 <- function (Y, X, lambda=0, p0=pci.jointpenalty.guess.a0(Y,X), 
                                       robust=FALSE, nu=5, pgtol=1e-8) {
    # On input, Y is an n x 1 column vector, and X is an n x k matrix.
    # Fits an PCI model to Y,X.  Returns a three-component list:
    #   par: The parameter estimates
    #   se:  The estimated standard error of the parameter estimates
    #   nll: The negative log likelihood of the fit

    if (is.null(dim(Y))) Y <- as.matrix(Y, ncol=1)
    if (is.null(dim(X))) X <- as.matrix(X, ncol=1)
  
    n <- ncol(X)
    highval <- NA
    ll_calc_method <- if (robust) "csst" else "css"

    fit.rw <- fit.pci.jointpenalty.rw.a0 (Y, X, lambda, p0, robust=robust, nu=nu)
    fit.mr <- fit.pci.jointpenalty.mr.a0 (Y, X, lambda, p0, robust=robust, nu=nu)
    fit.twostep <- fit.pci.twostep.a0 (Y, X, robust=robust, nu=nu)
    twostep.par <- c(fit.twostep$beta, 
        fit.twostep$rho, fit.twostep$sigma_M, fit.twostep$sigma_R)
    
    start_list <- list(p0[1:(n+3)], fit.rw$par[1:(n+3)], fit.mr$par[1:(n+3)], twostep.par)

    objective <- function (p) {
#        print(p)
        beta <- p[1:n]
        rho <- p[n+1]
        sigma_M <- p[n+2]
        sigma_R <- p[n+3]
        if (rho < -1 || rho > 1) return(highval)
        if ((sigma_M < 0) || (sigma_R < 0)) return(highval)
        if ((sigma_M == 0) && (sigma_R == 0)) return(highval)
        Z <- Y - X %*% beta
        M0 <- 0
        R0 <- Z[1]
        ll <- loglik.par (Z, rho, sigma_M, sigma_R, M0, R0, ll_calc_method, nu=nu) + lambda * sigma_R^2
#        print(c(p, ll))
        ll
    }
#    debug(objective)

    maxsig <- fit.rw$par[["sigma_R"]]
    pmin <- c(rep(-Inf, n), -1, 0, 0)
    pmax <- c(rep(Inf, n), 1, 2.0 * maxsig, 2.0 * maxsig)

    best_value <- objective(start_list[[1]])+1
    for (start in start_list) {
        highval <- objective(start) + 1
        rfit <- optim(start, objective, hessian=TRUE, method="L-BFGS-B", lower=pmin, upper=pmax,control=list(pgtol=pgtol))
        if (rfit$value < best_value) {
            bestfit <- rfit
            best_value <- rfit$value
#            cat(sprintf("r %6.2f rho %8.4f sigma_M %8.4f sigma_R %8.4f -> %8.4f\n",
#                rrho, bestfit$par[1], bestfit$par[2], bestfit$par[3], bestfit$value))
        }        
    }

    bestfit.R0 <- (Y - X %*% bestfit$par[1:n]) [1]
    bestfit.par <- c(bestfit$par[1:(n+3)], M0=0, R0=bestfit.R0)
    bestfit.se <- rep(NA_real_, nrow(bestfit$hessian))
    suppressWarnings(try(bestfit.se <- sqrt(diag(solve(bestfit$hessian))), silent=TRUE))
    bestfit.se <- c(bestfit.se[1:(n+3)], M0=NA, R0=NA)

#    names(bestfit.par)[n+6] <- "R0"
#    names(bestfit.se)[n+6] <- "R0"
    
    return(list(par=bestfit.par, se=bestfit.se, nll=c(negloglik=bestfit$value)))
    
}

fit.pci.jointpenalty.a0 <- function (Y, X, par_model=c("par", "ar1", "rw"), lambda=0, 
                                  robust=FALSE, nu=5) {
  #  On input, Y is an n x 1 column vector, and X is an n x k matrix.
  #  lambda is a penalty value that drives the optimization towards a
  #  solution where sigma_R has the lowest possible value.
  #
  #  Fits the model
  #
  #  Y[t] = beta[1] X[t,1] + beta[2] X[t,2] + ... + beta [k] X[t,k]
  #    + m[t] + r[t]
  #
  # where
  #
  #   m[t] = rho * m[t-1] + eps_M[t]
  #   r[t] = r[t-1] + eps_R[t]
  #   eps_M,t ~ N(0,sigma_M^2)
  #   eps_R,t ~ N(0,sigma_R^2)
  #   m[0] = 0
  #   r[0] = Y[0] - beta[1] X[0,1] - beta[2] X[0,2] - ... - beta[k] X[0,k]
  # 
  # Estimates the values of beta, rho, sigma_M and sigma_R using
  # the following procedure:
  #   1.  Initial estimates of beta are obtained by regressing the
  #       first differences of X[,i] on the first difference of Y.
  #   2.  Initial estimates of rho, sigma_M and sigma_R are obtained by
  #       fitting a PAR model to the residuals
  #   3.  Having obtained these initial estimates for all of the parameters,
  #       the following cost function is maximized to final parameter
  #       estimates:
  #          C[rho, sigma_M, sigma_R] = -LL[rho, sigma_M, sigma_R] + lambda * sigma_R^2
  #       In the above, LL is the log likelihood function for the steady
  #       state Kalman filter with parameters rho, sigma_M and sigma_R.
  #
  # The parameter par_model specifies which parameters of the PAR model are
  # to be optimized.  

    if (is.null(dim(Y))) Y <- as.matrix(Y, ncol=1)
    if (is.null(dim(X))) X <- as.matrix(X, ncol=1)
  
    par_model <- match.arg(par_model)
    p0 = pci.jointpenalty.guess.a0(Y,X)
    res <- switch (par_model,
        rw = fit.pci.jointpenalty.rw.a0(Y, X, lambda, p0, robust=robust, nu=nu),
        ar1 = fit.pci.jointpenalty.mr.a0(Y, X, lambda, p0, robust=robust, nu=nu),
        par = fit.pci.jointpenalty.both.a0(Y, X, lambda, p0, robust=robust, nu=nu))
    n <- ncol(X)   
    
    beta <- res$par[1:n]
    R <- Y - X %*% beta
    
    fit <- structure(list(
        data=Y,
        basis=X,
        residuals=R,
        index=1:length(Y),
        beta = beta,
        rho = res$par[n+1],
        sigma_M = res$par[n+2],
        sigma_R = res$par[n+3],
        M0 = res$par[n+4],
        R0 = res$par[n+5],
        beta.se = res$se[1:n],
        rho.se = res$se[n+1],
        sigma_M.se = res$se[n+2],
        sigma_R.se = res$se[n+3],
        M0.se = res$se[n+4],
        R0.se = res$se[n+5],
        negloglik = res$nll,
        pvmr = pvmr.par(res$par[n+1], res$par[n+2], res$par[n+3]),
        par.model=par_model,
        robust=robust,
        pci.fit="jointpenalty"), 
    class="pci.fit")
    
    names(fit$beta) <- paste("beta_", colnames(X), sep="")
    names(fit$beta.se) <- paste(paste("beta_", colnames(X), sep=""), ".se", sep="")
    
    fit
}

fit.pci.a0 <- function (Y, X, pci_opt_method=c("jp", "twostep"), par_model=c("par", "ar1", "rw"), 
   lambda=0, robust=FALSE, nu=5) {
  #  On input, Y is an n x 1 column vector, and X is an n x k matrix.
  #  lambda is a penalty value that drives the optimization towards a
  #  solution where sigma_R has the lowest possible value.
  #
  #  Fits the model
  #
  #  Y[t] = beta[1] X[t,1] + beta[2] X[t,2] + ... + beta [k] X[t,k]
  #    + m[t] + r[t]
  #
  # where
  #
  #   m[t] = rho * m[t-1] + eps_M[t]
  #   r[t] = r[t-1] + eps_R[t]
  #   eps_M,t ~ N(0,sigma_M^2)
  #   eps_R,t ~ N(0,sigma_R^2)
  #   m[0] = 0
  #   r[0] = Y[0] - beta[1] X[0,1] - beta[2] X[0,2] - ... - beta[k] X[0,k]
  #
  # Fits the model using either the two step procedure or the joint penalty
  # procedure, according to the value of the parameter 'pci_opt_method'.  Returns
  # an S3 object of class "pci.fit" representing the fit that was obtained.
  #
  # The parameter par_model specifies which parameters of the PAR model are
  # to be optimized.  
  
    pci_opt_method <- match.arg(pci_opt_method)
    par_model <- match.arg(par_model)
    
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

    Xorig <- X
    X <- as.matrix(X)
    
    A <- switch (pci_opt_method,
        twostep = fit.pci.twostep.a0(Y, X, par_model=par_model, robust=robust, nu=nu),
        jp = fit.pci.jointpenalty.a0(Y, X, par_model=par_model, lambda, robust=robust, nu=nu))

    if (is.zoo(Yorig)) {
        A$index <- index(Yorig)
    } else {
        A$index <- 1:length(Y)
    }
    
    if (!is.null(names(Yorig))) {
      A$target_name <- names(Yorig)[1]
    } else if (!is.null(colnames(Yorig))) {
      A$target_name <- colnames(Yorig)[1]
    } else {
      A$target_name <- "Y"
    }
    
    if (!is.null(colnames(X))) {
      A$factor_names <- colnames(X)
    } else if (!is.null(names(X))) {
      A$factor_names <- names(X)
    } else {
      A$factor_names <- paste("X", 1:ncol(X), sep="")
    }
    
    A
}

