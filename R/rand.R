# rand.R -- functions for generating random variates

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


rpci <- function (n, alpha, beta, sigma_C, rho, sigma_M, sigma_R, 
                  include.state = FALSE, robust=FALSE, nu=5) {
    # Generates a random partially cointegrated sequence.  On input, n is the
    # length of the sequence to be generated.  beta is a vector of length k
    # representing the coefficients of the factor loadings, and sigma_C is a
    # vector of length k representing the standard deviations of the increments
    # of the factor loadings.  
    #
    # Generates a random realization of the sequence
    #    y_t = alpha + beta_1 F_{1,t} + beta_2 F_{2,t} + ... + beta_k F_{k,t} + m_t + r_t
    #    F_{i,j} = F_{i,j-1} + delta_{i,j}
    #    m_t = rho m_{t-1} + eps_{M,t}
    #    r_t = r_{t-1} + eps_{R,t}
    #    delta_{i,j} ~ N(0, sigma_C^2)
    #    eps_{M,t} ~ N(0, sigma_M^2)
    #    eps_{R,t} ~ N(0, sigma_R^2)
    #    
    # If include.state is FALSE, returns an n x (k+1) matrix whose columns
    # are y, F_1, F_2, ..., F_k.  If include.state is TRUE, returns an
    # n x (2k + 6) matrix whose columns are 
    #   y, F_1, F_2, ..., F_k, x, m, r, delta_1, delta_2, ..., delta_k,
    #   eps_M, eps_R

    if (!robust) {
        D <- do.call("cbind", lapply(1:length(beta), function(i) rnorm(n,0,sigma_C[i])))
    } else {
        D <- do.call("cbind", lapply(1:length(beta), function(i) rt(n,nu) * sigma_C[i]))
    }
    F <- apply(D, 2, cumsum)
    colnames(F) <- paste ("F", 1:ncol(F), sep="_")
    PAR <- rpar (n, rho, sigma_M, sigma_R, include.state=TRUE, robust=robust, nu=nu)
    Y <- F %*% beta + PAR$X + alpha
    DF <- cbind(Y, F)
    colnames(DF)[1] <- "Y"
    if (include.state) {
        colnames(D) <- paste("delta", 1:ncol(D), sep="_")
        DF <- cbind(DF, X=PAR$X, M=PAR$M, R=PAR$R, D, eps_M=PAR$eps_M, eps_R=PAR$eps_R)
    }
    DF
}

