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

fit.pci.twostep <- function (Y, X, par_model=c("par", "ar1", "rw"), robust=FALSE, nu=5,
  include_alpha=FALSE) {
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
  # If include_alpha is FALSE, then the parameter alpha is omitted from
  # the model, and we take r[0] = Y[0] - beta * X[0].
  #
  # The parameter par_model specifies which parameters of the PAR model are
  # to be optimized.  

  par_model <- match.arg(par_model)
  
  if (include_alpha) {
    fit.pci.twostep.a (Y, X, par_model, robust, nu)
  } else {
    fit.pci.twostep.a0 (Y, X, par_model, robust, nu)
  }
}

pci.jointpenalty.guess <- function (Y, X, include_alpha=FALSE) {
    # On input, Y is an n x 1 column vector, and X is an n x k matrix.
    # Generates a guess for the starting point of optimization for an PCI
    # fit as follows:
    #   1.  Estimates beta by fitting the differenced X series to the
    #       differenced Y series.
    #   2.  Estimates alpha by a Y[1] - X[1,] %*% beta
    #   3.  Estimates rho, sigma_M and sigma_R by fitting a PAR series
    #       to  Y - X %*% beta - alpha
    # Returns the guess that is found.
    #
    # If include_alpha is FALSE, then the parameter alpha is omitted
    # from the model.

    if (include_alpha) {
      pci.jointpenalty.guess.a(Y, X)
    } else {
      pci.jointpenalty.guess.a0(Y, X)
    }
}

fit.pci.jointpenalty.rw <- function (Y, X, 
  lambda=0, 
  p0=pci.jointpenalty.guess(Y,X,include_alpha), 
  robust=FALSE, 
  nu=5, 
  pgtol=1e-8, 
  include_alpha=FALSE) {
    # On input, Y is an n x 1 column vector, and X is an n x k matrix.
    # Fits an PCI model to Y,X where the residual series is modeled as
    # a random walk.  Returns a three-component list:
    #   par: The parameter estimates
    #   se:  The estimated standard error of the parameter estimates
    #   nll: The negative log likelihood of the fit

    if (include_alpha) {
      fit.pci.jointpenalty.rw.a (Y, X, lambda, p0, robust, nu, pgtol)
    } else {
      fit.pci.jointpenalty.rw.a0 (Y, X, lambda, p0, robust, nu, pgtol)
    }
}

fit.pci.jointpenalty.mr <- function (Y, X, 
  lambda=0, 
  p0=pci.jointpenalty.guess(Y,X,include_alpha), 
  robust=FALSE, 
  nu=5, 
  pgtol=1e-8, 
  include_alpha=FALSE) {
    # On input, Y is an n x 1 column vector, and X is an n x k matrix.
    # Fits an PCI model to Y,X where the residual series is modeled as
    # a pure AR(1) series.  Returns a three-component list:
    #   par: The parameter estimates
    #   se:  The estimated standard error of the parameter estimates
    #   nll: The negative log likelihood of the fit

    if (include_alpha) {
      fit.pci.jointpenalty.mr.a (Y, X, lambda, p0, robust, nu, pgtol)
    } else {
      fit.pci.jointpenalty.mr.a0 (Y, X, lambda, p0, robust, nu, pgtol)
    }
}

fit.pci.jointpenalty.both <- function (Y, X, 
  lambda=0, 
  p0=pci.jointpenalty.guess(Y,X,include_alpha), 
  robust=FALSE, 
  nu=5, 
  pgtol=1e-8, 
  include_alpha=FALSE) {
    # On input, Y is an n x 1 column vector, and X is an n x k matrix.
    # Fits an PCI model to Y,X.  Returns a three-component list:
    #   par: The parameter estimates
    #   se:  The estimated standard error of the parameter estimates
    #   nll: The negative log likelihood of the fit

    if (include_alpha) {
      fit.pci.jointpenalty.both.a (Y, X, lambda, p0, robust, nu, pgtol)
    } else {
      fit.pci.jointpenalty.both.a0 (Y, X, lambda, p0, robust, nu, pgtol)
    }
}

fit.pci.jointpenalty <- function (Y, X, 
  par_model=c("par", "ar1", "rw"), 
  lambda=0, 
  robust=FALSE, 
  nu=5, 
  include_alpha=FALSE) {
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
  # to be optimized.  If par == "par", then rho, sigma_M and sigma_R are each
  # estimated.  If par == "ar1", then rho and sigma_M are estimated, while
  # sigma_R is fixed at 0.  If par == "rw", then sigma_R is estimated, while
  # rho and sigma_M are fixed at 0.
  #
  # If include_alpha is FALSE, then the parameter alpha is omitted from
  # the model, and we take r[0] = Y[0] - beta * X[0].

  par_model <- match.arg(par_model)

  if (include_alpha) {
    fit.pci.jointpenalty.a (Y, X, lambda, p0, robust, nu)
  } else {
    fit.pci.jointpenalty.a0 (Y, X, lambda, p0, robust)
  }
}

fit.pci <- function (Y, X, 
  pci_opt_method=c("jp", "twostep"), 
  par_model=c("par", "ar1", "rw"), 
  lambda=0, 
  robust=FALSE, 
  nu=5, 
  include_alpha=FALSE) {
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
  # to be optimized.  If par == "par", then rho, sigma_M and sigma_R are each
  # estimated.  If par == "ar1", then rho and sigma_M are estimated, while
  # sigma_R is fixed at 0.  If par == "rw", then sigma_R is estimated, while
  # rho and sigma_M are fixed at 0.
  #
  # If include_alpha is FALSE, then the parameter alpha is omitted from
  # the model, and we take r[0] = Y[0] - beta * X[0].
  
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
  
  if (include_alpha) {
    fit.pci.a (Y, X, pci_opt_method, par_model, lambda, robust, nu)
  } else {
    fit.pci.a0 (Y, X, pci_opt_method, par_model, lambda, robust, nu)
  }
  
  
}

print.pci.fit <- function (x, ...) {
  # Given a PCI structure A, prints it in summary form
  print.internal.pci.fit(x)
}


print.internal.pci.fit <- function (A, ...) {
    # Prints a PCI fit
    cat("Fitted values for PCI model\n")
    if ("alpha" %in% names(A)) {
      cat("  Y[t] = alpha + X[t] %*% beta + M[t] + R[t]\n")
    } else {
      cat("  Y[t] = X[t] %*% beta + M[t] + R[t]\n")
    }
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
    if ("alpha" %in% names(A)) {
      Yhat <- basis %*% A$beta + A$alpha
    } else {
      Yhat <- basis %*% A$beta
    }
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

plot.pci.fit <- function (x, ...) {
  # Given a PCI structure A, plots it.
  
  plot.internal.pci.fit(x)
}

plot.internal.pci.fit <- function (A, ...) {
    # Plots an partially cointegrated model
    
  # Initial definition for Date, Value, Label
  Date <- Value <- Label<- NULL
  
  
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

as.data.frame.pci.fit <- function (x, row.names, optional, ...) {
  as.data.frame.internal.pci.fit (x)
}



as.data.frame.internal.pci.fit <- function (A, ...) {
    # Given an partially cointegrated model A, converts it into a one row data.frame
    # containing the following values from the model:
    #   alpha
    #   beta
    #   rho
    #   sigma_M
    #   sigma_R

    if ("alpha" %in% names(A)) {    
      df1 <- data.frame(alpha = A$alpha, beta=A$beta)
    } else {
      df1 <- data.frame(beta = A$beta)
    }
    df2 <- data.frame(rho = A$rho, sigma_M=A$sigma_M, sigma_R=A$sigma_R, M0=A$M0, R0=A$R0)
    if ("alpha.se" %in% names(A)) {
      df3 <- data.frame(alpha.se = A$alpha.se, beta.se = A$beta.se)
    } else {
      df3 <- data.frame(beta.se = A$beta.se)
    }
    df4 <- data.frame(rho.se=A$rho.se, sigma_M.se=A$sigma_M.se, sigma_R.se=A$sigma_R.se, 
        M0.se=A$M0.se, R0.se=A$R0.se)
    df5 <- data.frame(negloglik=A$negloglik, pvmr=A$pvmr)
    df <- cbind(df1, df2, df3, df4, df5)
    df
}

