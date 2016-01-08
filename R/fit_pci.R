# Functions pertaining to fitting partially cointegrated (PCI) series


fit.pci.twostep <- function (Y, X, par_model=c("par", "ar1", "rw"), robust=FALSE, nu=5) {
  #  On input, Y is an n x 1 column vector, and X is an n x k matrix.
  #  Fits the model
  #
  #  Y[t] = alpha + beta[1] X[t,1] + beta[2] X[t,2] + ... + beta [k] X[t,k]
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
  # Estimates the values of alpha, beta, rho, sigma_M and sigma_R using
  # the two step procedure:
  #   1.  Perform a linear regression of Y on X to obtain alpha and beta
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
        alpha = c(alpha=coef(L)[1,1]),
        beta = coef(L)[2:nrow(coef(L)),1],
        rho = PAR$rho,
        sigma_M = PAR$sigma_M,
        sigma_R = PAR$sigma_R,
        M0 = PAR$par[4],
        R0 = PAR$par[5],
        alpha.se = c(alpha.se=coef(L)[1,2]),
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

pci.jointpenalty.guess <- function (Y, X) {
    # On input, Y is an n x 1 column vector, and X is an n x k matrix.
    # Generates a guess for the starting point of optimization for an PCI
    # fit as follows:
    #   1.  Estimates beta by fitting the differenced X series to the
    #       differenced Y series.
    #   2.  Estimates alpha by a Y[1] - X[1,] %*% beta
    #   3.  Estimates rho, sigma_M and sigma_R by fitting a PAR series
    #       to  Y - X %*% beta - alpha
    # Returns the guess that is found

    X <- as.matrix(X)
    DY <- diff(Y)
    DX <- apply(X, 2, diff)
    beta0 <- coef(lm(DY ~ DX + 0))
    alpha0 <- as.numeric(Y[1] - X[1,] %*% beta0)
    res0 <- Y - X %*% beta0 - alpha0
    PAR0 <- fit.par(res0)
    rho0 <- PAR0$rho
    sigma_M0 <- PAR0$sigma_M
    sigma_R0 <- PAR0$sigma_R
    M00 <- 0
    p0 <- c(alpha0, beta0, rho0, sigma_M0, sigma_R0, M00, 0)
    names(p0)[1] <- "alpha"
    names(p0)[2:(ncol(X)+1)] <- paste("beta_", colnames(X), sep="")
    names(p0)[length(p0)-1] <- "M0"
    names(p0)[length(p0)] <- "R0"
    p0
}

fit.pci.jointpenalty.rw <- function (Y, X, lambda=0, p0=pci.jointpenalty.guess(Y,X), 
                                     robust=FALSE, nu=5) {
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
        alpha <- p[1]
        beta <- p[2:(n+1)]
        sigma_R <- p[n+2]
        if (sigma_R <= 0.0) return(highval)
        Z <- Y - X %*% beta - alpha
        loglik.par (Z, 0, 0, sigma_R, 0, 0, ll_calc_method, nu=nu) + lambda * sigma_R^2
    }

    alpha0 <- p0[1]
    beta0 <- p0[2:(ncol(X)+1)]
    res0 <- Y - X %*% beta0 - alpha0
    sdmax <- 2.0 * sd(diff(Y))
    rw.p0 <- c(alpha0, beta0, sdmax * 0.5)
    rw.pmin <- c(-Inf, rep(-Inf, n), 0.0)
    rw.pmax <- c(Inf, rep(Inf, n), sdmax)
    highval <- rw.ofun(rw.p0) + 1.0

    rw.val <- optim(rw.p0, rw.ofun, method="L-BFGS-B", lower=rw.pmin, upper=rw.pmax, hessian=TRUE)
    rw.par <- c(rw.val$par[1:(n+1)], rho=0, sigma_M=0, sigma_R=rw.val$par[n+2], M0=0, R0=0)
    rw.se <- rep(NA_real_, nrow(rw.val$hessian))
    suppressWarnings(try(rw.se <- sqrt(diag(solve(rw.val$hessian))), silent=TRUE))
    rw.se <- c(rw.se[1:(n+1)], rho=NA, sigma_M=NA, sigma_R=rw.se[n+2], M0=NA, R0=NA)

    return(list(par=rw.par, se=rw.se, nll=c(negloglik=rw.val$value)))
}

fit.pci.jointpenalty.mr <- function (Y, X, lambda=0, p0=pci.jointpenalty.guess(Y,X), 
                                     robust=FALSE, nu=5) {
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
        alpha <- p[1]
        beta <- p[2:(n+1)]
        rho <- p[n+2]
        sigma_M <- p[n+3]
        if (rho < -1 || rho > 1 || sigma_M <= 0.0) return(highval)
        Z <- Y - X %*% beta - alpha
        loglik.par (Z, rho, sigma_M, 0, 0, 0, ll_calc_method, nu=nu)
    }

    rw.fit <- fit.pci.jointpenalty.rw (Y, X, lambda, p0, robust=robust, nu=nu)
    rw.p0 <- c(rw.fit$par[1:(n+1)], rho=1, rw.fit$par[n+4])
    names(rw.p0)[n+3] <- "sigma_M"
    
    alpha0 <- p0[1]
    beta0 <- p0[2:(ncol(X)+1)]
    res0 <- Y - X %*% beta0 - alpha0
    PAR0.mr <- fit.par(res0, model="ar1", robust=robust, nu=nu)   
    mr.p0 <- c(alpha0, beta0, PAR0.mr$rho, PAR0.mr$sigma_M)
    names(mr.p0)[n+3] <- "sigma_M"
    
    mr.pmin <- c(-Inf, rep(-Inf, n), -1, 0)
    mr.pmax <- c(Inf, rep(Inf, n), 1, 2.0 * rw.p0[n+3])

    highval <- max(mr.ofun(rw.p0), mr.ofun(mr.p0)) + 1
    fit1 <- optim(rw.p0, mr.ofun, method="L-BFGS-B", lower=mr.pmin, upper=mr.pmax, hessian=TRUE)
    fit2 <- optim(mr.p0, mr.ofun, method="L-BFGS-B", lower=mr.pmin, upper=mr.pmax, hessian=TRUE)
    fit <- if(fit1$value < fit2$value) fit1 else fit2
    
    mr.par <- c(fit$par[1:(n+3)], sigma_R=0, M0=0, R0=0)

    mr.se <- rep(NA_real_, nrow(fit$hessian))
    suppressWarnings(try(mr.se <- sqrt(diag(solve(fit$hessian))), silent=TRUE))
    mr.se <- c(mr.se[1:(n+3)], sigma_R=NA, M0=NA, R0=NA)

    return(list(par=mr.par, se=mr.se, nll=c(negloglik=fit$value)))
}

fit.pci.jointpenalty.both <- function (Y, X, lambda=0, p0=pci.jointpenalty.guess(Y,X), 
                                       robust=FALSE, nu=5) {
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

    fit.rw <- fit.pci.jointpenalty.rw (Y, X, lambda, p0, robust=robust, nu=nu)
    fit.mr <- fit.pci.jointpenalty.mr (Y, X, lambda, p0, robust=robust, nu=nu)
    fit.twostep <- fit.pci.twostep (Y, X, robust=robust, nu=nu)
    twostep.par <- c(fit.twostep$alpha, fit.twostep$beta, 
        fit.twostep$rho, fit.twostep$sigma_M, fit.twostep$sigma_R)
    
    start_list <- list(p0[1:(n+4)], fit.rw$par[1:(n+4)], fit.mr$par[1:(n+4)], twostep.par)

    objective <- function (p) {
        alpha <- p[1]
        beta <- p[2:(n+1)]
        rho <- p[n+2]
        sigma_M <- p[n+3]
        sigma_R <- p[n+4]
        if (rho < -1 || rho > 1) return(highval)
        if ((sigma_M < 0) || (sigma_R < 0)) return(highval)
        if ((sigma_M == 0) && (sigma_R == 0)) return(highval)
        M0 <- 0
        R0 <- 0
        Z <- Y - X %*% beta - alpha
        loglik.par (Z, rho, sigma_M, sigma_R, M0, R0, ll_calc_method, nu=nu) + lambda * sigma_R^2
    }

    maxsig <- fit.rw$par[["sigma_R"]]
    pmin <- c(-Inf, rep(-Inf, n), -1, 0, 0)
    pmax <- c(Inf, rep(Inf, n), 1, 2.0 * maxsig, 2.0 * maxsig)

    best_value <- objective(start_list[[1]])+1
    for (start in start_list) {
        highval <- objective(start) + 1
        rfit <- optim(start, objective, hessian=TRUE, method="L-BFGS-B", lower=pmin, upper=pmax)
        if (rfit$value < best_value) {
            bestfit <- rfit
            best_value <- rfit$value
#            cat(sprintf("r %6.2f rho %8.4f sigma_M %8.4f sigma_R %8.4f -> %8.4f\n",
#                rrho, bestfit$par[1], bestfit$par[2], bestfit$par[3], bestfit$value))
        }        
    }

    bestfit.par <- c(bestfit$par[1:(n+4)], M0=0, R0=0)
    bestfit.se <- rep(NA_real_, nrow(bestfit$hessian))
    suppressWarnings(try(bestfit.se <- sqrt(diag(solve(bestfit$hessian))), silent=TRUE))
    bestfit.se <- c(bestfit.se[1:(n+4)], M0=NA, R0=NA)

#    names(bestfit.par)[n+6] <- "R0"
#    names(bestfit.se)[n+6] <- "R0"
    
    return(list(par=bestfit.par, se=bestfit.se, nll=c(negloglik=bestfit$value)))
    
}

fit.pci.jointpenalty <- function (Y, X, par_model=c("par", "ar1", "rw"), lambda=0, 
                                  robust=FALSE, nu=5) {
  #  On input, Y is an n x 1 column vector, and X is an n x k matrix.
  #  lambda is a penalty value that drives the optimization towards a
  #  solution where sigma_R has the lowest possible value.
  #
  #  Fits the model
  #
  #  Y[t] = alpha + beta[1] X[t,1] + beta[2] X[t,2] + ... + beta [k] X[t,k]
  #    + m[t] + r[t]
  #
  # where
  #
  #   m[t] = rho * m[t-1] + eps_M[t]
  #   r[t] = r[t-1] + eps_R[t]
  #   eps_M,t ~ N(0,sigma_M^2)
  #   eps_R,t ~ N(0,sigma_R^2)
  #   m[0] = r[0] = 0
  # 
  # Estimates the values of alpha, beta, rho, sigma_M and sigma_R using
  # the following procedure:
  #   1.  Initial estimates of beta are obtained by regressing the
  #       first differences of X[,i] on the first difference of Y.
  #   2.  Given these estimates for beta, an initial estimate for
  #       alpha is obtained as alpha = Y[1] - beta X[1,].
  #   3.  Initial estimates of rho, sigma_M and sigma_R are obtained by
  #       fitting a PAR model to the residuals
  #   4.  Having obtained these initial estimates for all of the parameters,
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
    p0 = pci.jointpenalty.guess(Y,X)
    res <- switch (par_model,
        rw = fit.pci.jointpenalty.rw(Y, X, lambda, p0, robust=robust, nu=nu),
        ar1 = fit.pci.jointpenalty.mr(Y, X, lambda, p0, robust=robust, nu=nu),
        par = fit.pci.jointpenalty.both(Y, X, lambda, p0, robust=robust, nu=nu))
    n <- ncol(X)   
    
    alpha <- res$par[1]
    beta <- res$par[2:(n+1)]
    R <- Y - alpha - X %*% beta
    
    fit <- structure(list(
        data=Y,
        basis=X,
        residuals=R,
        index=1:length(Y),
        alpha = alpha,
        beta = beta,
        rho = res$par[n+2],
        sigma_M = res$par[n+3],
        sigma_R = res$par[n+4],
        M0 = res$par[n+5],
        R0 = res$par[n+6],
        alpha.se = res$se[1],
        beta.se = res$se[2:(n+1)],
        rho.se = res$se[n+2],
        sigma_M.se = res$se[n+3],
        sigma_R.se = res$se[n+4],
        M0.se = res$se[n+5],
        R0.se = res$se[n+6],
        negloglik = res$nll,
        pvmr = pvmr.par(res$par[n+2], res$par[n+3], res$par[n+4]),
        par.model=par_model,
        robust=robust,
        pci.fit="jointpenalty"), 
    class="pci.fit")
    
    names(fit$beta) <- paste("beta_", colnames(X), sep="")
    names(fit$beta.se) <- paste(paste("beta_", colnames(X), sep=""), ".se", sep="")
    
    fit
}

fit.pci <- function (Y, X, pci_opt_method=c("jp", "twostep"), par_model=c("par", "ar1", "rw"), 
   lambda=0, robust=FALSE, nu=5) {
  #  On input, Y is an n x 1 column vector, and X is an n x k matrix.
  #  lambda is a penalty value that drives the optimization towards a
  #  solution where sigma_R has the lowest possible value.
  #
  #  Fits the model
  #
  #  Y[t] = alpha + beta[1] X[t,1] + beta[2] X[t,2] + ... + beta [k] X[t,k]
  #    + m[t] + r[t]
  #
  # where
  #
  #   m[t] = rho * m[t-1] + eps_M[t]
  #   r[t] = r[t-1] + eps_R[t]
  #   eps_M,t ~ N(0,sigma_M^2)
  #   eps_R,t ~ N(0,sigma_R^2)
  #   m[0] = r[0] = 0
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
        twostep = fit.pci.twostep(Y, X, par_model=par_model, robust=robust, nu=nu),
        jp = fit.pci.jointpenalty(Y, X, par_model=par_model, lambda, robust=robust, nu=nu))

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

print.pci.fit <- function (A) {
    # Prints an PCI fit
    cat("Fitted values for PCI model\n")
    cat("  Y[t] = alpha + X[t] %*% beta + M[t] + R[t]\n")
    cat("  M[t] = rho * M[t-1] + eps_M [t], eps_M[t] ~ N(0, sigma_M^2)\n")
    cat("  R[t] = R[t-1] + eps_R [t], eps_R[t] ~ N(0, sigma_R^2)\n\n")

    cat(sprintf("%-10s %8s %8s\n", "", "Estimate", "Std. Err"))
    p <- function(n, v, s) { cat(sprintf("%-10s %8.4f %8.4f\n", n, v, s)) }
    p("alpha", A$alpha, A$alpha.se)
    for (i in 1:length(A$beta)) {
        p(names(A$beta[i]), A$beta[i], A$beta.se[i])
    }
    p("rho", A$rho, A$rho.se)
    p("sigma_M", A$sigma_M, A$sigma_M.se)
    p("sigma_R", A$sigma_R, A$sigma_R.se)
#    p("M0", A$M0, A$M0.se)
#    p("R0", A$R0, A$R0.se)
    
    cat(sprintf("\n-LL = %.2f, R^2[MR] = %.3f\n", A$negloglik, A$pvmr))
}

statehistory.pci <- function(A, data=A$data, basis=A$basis) {
    # On input, A is an pci.fit object as produced by fit.pci.
    # Creates a data.frame containing the inferred values of
    # the states of the mean-revering and random walk components
    # of the process, based upon the model parameters that were fit.
    #
    # Returns a data.frame containing the following columns:
    #   Y:   The value of the process at this time
    #   X:   The value of the hedge at this time
    #   M:   The inferred state of the mean reverting component
    #   R:   The inferred state of the random walk component
    #   eps_M: The inferred shock to the mean reverting component
    #   eps_R: The inferred shock to the random walk component

    if (is.null(dim(basis))) basis <- as.matrix(basis, ncol=1)
    Yhat <- basis %*% A$beta + A$alpha
    Z <- data - Yhat

    n <- length(data)
    M <- numeric(n)
    R <- numeric(n)
    eps_M <- numeric(n)
    eps_R <- numeric(n)
    
#    Mprev <- A$M0
#    Mprev <- 0
#    Rprev <- A$R0
    Mprev <- 0
    Rprev <- data[1] - Yhat[1]

    K <- kalman.gain.par(rho=A$rho, sigma_M=A$sigma_M, sigma_R=A$sigma_R)

    for (i in 1:n) {
        xhat <- A$rho * Mprev + Rprev
        e <- Z[i] - xhat
        eps_M[i] <- e * K[1]
        eps_R[i] <- e * K[2]
        M[i] <- A$rho * Mprev + eps_M[i]
        R[i] <- Rprev + eps_R[i]
        Mprev <- M[i]
        Rprev <- R[i]
    }

    df <- data.frame(Y=data, Yhat=Yhat, Z=Z, M=M, R=R, eps_M=eps_M, eps_R=eps_R)
    colnames(df) <- c("Y", "Yhat", "Z", "M", "R", "eps_M", "eps_R")
    df
}

plot.pci.fit <- function (A) {
    # Plots an partially cointegrated model
    
    sh <- statehistory.pci(A)
    n <- nrow(sh)
    RW <- sh$Yhat + sh$R
    df1.1 <- data.frame(Date=A$index, Label="Actual", Value=sh$Y)
    df1.2 <- data.frame(Date=A$index, Label="Hedge", Value=sh$Yhat)
    df1.3 <- data.frame(Date=A$index, Label="Model", Value=RW)
    df1 <- rbind(df1.1, df1.2, df1.3)
#    p1 <- ggplot (df1, aes(x=Date, y=Value, colour=Label)) + geom_line () +
#        ylab("Price") + xlab("") + theme(legend.position="top") +
#    		scale_colour_discrete(name="") +
#            scale_size_manual(values=c(2,0.5,0.5)) +
#            ggtitle("Price Series") 

    labels <- c("Target", "Hedge", "Model")
    if (A$target_name != "Y") {
      labels[1] <- A$target_name
    }
    if (A$factor_names[1] != "X1") {
      factor_strings <- sapply(1:length(A$beta), 
                               function(i) sprintf("%5.2f %s",
                                                   A$beta[i], A$factor_names[i]))
      labels[2] <- paste(factor_strings, collapse=" +")
    }
    
    p1 <- ggplot (df1, aes(x=Date, y=Value, colour=Label, size=Label, linetype=Label)) + geom_line () +
        ylab("Price") + xlab("") + theme(legend.position="top") +
    		scale_colour_manual(name="",
                breaks=c("Actual", "Hedge", "Model"),
                labels=labels,
#                values=c("Black", "#0054A6", "#00AEEF")) +  # Black, Blue, Cyan
                values=c("Black", "#00A651", "#00AEEF")) +  # Black, Green, Cyan
            scale_size_manual(name="", 
                breaks=c("Actual", "Hedge", "Model"),
                labels=labels,
                values=c(1.0,1.0,0.5)) +
#                values=c(0.75,0.75,1)) +
            scale_linetype_manual(name="",
                breaks=c("Actual", "Hedge", "Model"),
                labels=labels,
                values=c("solid", "solid", "solid"))+
#                values=c("solid", "solid", "dashed"))+
           ggtitle("Price Series") 
            
    
    df2 <- data.frame(Date=A$index, Label="M[t]", Value=sh$M)
    sdR <- sd(sh$M)
    hlines <- data.frame(Value=c(2 * sdR, sdR, -sdR, -2 * sdR),
        Facet=c("two","one","one","two"))
    p2 <- ggplot(df2, aes(x=Date, y=Value)) + geom_line() +
        ggtitle("Mean Reverting Component") + ylab("Price") + xlab("") +
        geom_hline(data=hlines, aes(yintercept=Value, colour=Facet), linetype="dashed") +
         theme(legend.position="none")
    
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(9, 1)))
	print(p1, vp=viewport(layout.pos.row=1:5, layout.pos.col=1))
	print(p2, vp=viewport(layout.pos.row=6:9, layout.pos.col=1))    
    
}

as.data.frame.pci.fit <- function (A) {
    # Given an partially cointegrated model A, converts it into a one row data.frame
    # containing the following values from the model:
    #   alpha
    #   beta
    #   rho
    #   sigma_M
    #   sigma_R
    
    df1 <- data.frame(alpha = A$alpha)
    df2 <- t(as.data.frame(A$beta))
    df3 <- data.frame(rho = A$rho, sigma_M=A$sigma_M, sigma_R=A$sigma_R, M0=A$M0, R0=A$R0)
    df4 <- data.frame(alpha.se = A$alpha.se)
    df5 <- t(as.data.frame(A$beta.se))
    df6 <- data.frame(rho.se=A$rho.se, sigma_M.se=A$sigma_M.se, sigma_R.se=A$sigma_R.se, 
        M0.se=A$M0.se, R0.se=A$R0.se)
    df7 <- data.frame(negloglik=A$negloglik, pvmr=A$pvmr)
    df <- cbind(df1, df2, df3, df4, df5, df6, df7)
    df
}

