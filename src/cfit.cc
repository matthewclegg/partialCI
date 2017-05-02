/* cfit.C -- high-speed fitting of partially AR(1) models */
/* Copyright 2015 Matthew Clegg */

/*
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
*/

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]



double lagged_variance_c (NumericVector X, int k, int n) {
    // Computes the variance of (1-B^k)X[(k+1)..n]
    
    if (k < 1 || k >= n-2 || n < 4 || n+k > X.size()) return(NA_REAL);
    
    double s = 0.0;
    double ssq = 0.0;
    for (int i = 0; i < n; i++) {
        double dx = X[i+k] - X[i];
        s += dx;  // This is a telescoping sum, but taking advantage of this
                  // fact is not going to improve execution time noticeably
        ssq += dx*dx;
    }
    double v = (ssq - s*s/n)/(n-1);
    return (v);
}

// [[Rcpp::export]]

double estimate_rho_par_c (NumericVector X) {
//	Computes an estimate of mean reversion for the mean-reverting
//	portion of a PAR process.  If v[k] = Var[X[t+k]-X[t]], then
//	rho is given by the variance formula:
//	   rho = - (v[1] - 2 * v[2] + v[3]) / (2 * v[1] - v[2])
    
    int n = X.size();
    if (n < 5) return(NA_REAL);
    
    double xv1 = lagged_variance_c(X, 1, n-3);
    double xv2 = lagged_variance_c(X, 2, n-3);
    double xv3 = lagged_variance_c(X, 3, n-3);
    
    double rho = -(xv1 - 2 * xv2 + xv3) / (2 * xv1 - xv2);
    if (rho < -0.99) rho = -1.0;
    if (rho > 0.99) rho = 1.0;
    
    return rho;
}

// [[Rcpp::export]]

NumericVector estimate_par_c (NumericVector X, double rho_max = 1.0) {
    /* Estimates the parameters of an partially AR(1) sequence using the 
       variances of the differences.  On input, X is an array of doubles,
       representing the time series of the data.  p is an output vector
       of length 3.  On return, the p[1] will contain rho, p[2] will
       contain sigma_M and p[3] will contain sigma_R.
    */
    
    int n = X.size();
    if (n < 5) return(NumericVector::create(NA_REAL, NA_REAL, NA_REAL));
    
    double xv1 = lagged_variance_c(X, 1, n-3);
    double xv2 = lagged_variance_c(X, 2, n-3);
    double xv3 = lagged_variance_c(X, 3, n-3);
    
    double rho = -(xv1 - 2 * xv2 + xv3) / (2 * xv1 - xv2);
    if (rho > rho_max) rho = rho_max;
    if (rho < -0.99) {
        return NumericVector::create(-1.0, sqrt(xv1), 0.0);
    } else if (rho > 0.99) {
        return NumericVector::create(0.0, 0.0, sqrt(xv1));
    }
    
    double sigma2_M = 0.5 * ((rho + 1.0)/(rho - 1.0)) * (xv2 - 2.0 * xv1);
    if (sigma2_M > 0.5 * xv2) sigma2_M = 0.5 * xv2;  // Dead code?
    if (sigma2_M < 0.0) sigma2_M = 0.0;
        
    double sigma2_R = 0.5 * (xv2 - 2.0 * sigma2_M);
    
    double sigma_M = sqrt(sigma2_M);
    double sigma_R = sqrt(sigma2_R);
    
    return NumericVector::create(rho, sigma_M, sigma_R);
}

// [[Rcpp::export]]

double pvmr_par_c(double rho, double sigma_M, double sigma_R) {
    // Returns the proportion of variance attributable to mean reversion
    // for a PAR process with parameters rho, sigma_M and sigma_R

    if ((rho < -1.0) || (rho > 1.0)) return(NA_REAL);
    if (sigma_M < 0.0) return(NA_REAL);
    if (sigma_R < 0.0) return(NA_REAL);
    if ((sigma_M == 0.0) && (sigma_R == 0.0)) return(NA_REAL);
    
    double sigma2_M = sigma_M * sigma_M;
    double sigma2_R = sigma_R * sigma_R;
    double r2 = (2 * sigma2_M) / (2 * sigma2_M + (1 + rho) * sigma2_R);
    return r2;
}

// [[Rcpp::export]]

double kalman_gain_par_mr (double rho, double sigma_M, double sigma_R) {
    // Computes the values of the steady state Kalman gain K_mr
    // of the mean-reverting portion of the state equation
    // based on the parameters rho, sigma_M and sigma_R given
    // as input.  The Kalman gain of the random walk portion of the
    // state equation is 1 - K_mr.

    if ((rho < -1.0) || (rho > 1.0)) return(NA_REAL);
    if (sigma_M < 0.0) return(NA_REAL);
    if (sigma_R < 0.0) return(NA_REAL);
    if ((sigma_M == 0.0) && (sigma_R == 0.0)) return(NA_REAL);

    if (sigma_M == 0.0) return 0.0;
    if (sigma_R == 0.0) return 1.0;

    double sigma2_M = sigma_M * sigma_M;
    double sigma2_R = sigma_R * sigma_R;
    double rad = sqrt((rho + 1.0)*(rho + 1.0) * sigma2_R + 4.0 * sigma2_M);
    
    double num = 2.0 * sigma2_M;
    double den = sigma_R*(rad + (1.0 + rho) * sigma_R) + 2.0 * sigma2_M;
    double K_mr = num / den;
    
    return K_mr;
}

// [[Rcpp::export]]

double loglik_par_c (NumericVector Y, double rho, double sigma_M, double sigma_R, double M0, double R0) {
    /* Computes the log likelihood of Y given parameters p using
       a steady state Kalman filter.  Returns the negative log
       likelihood value.
    */

    if ((rho < -1.0) || (rho > 1.0)) return(NA_REAL);
    if (sigma_M < 0.0) return(NA_REAL);
    if (sigma_R < 0.0) return(NA_REAL);
    if ((sigma_M == 0.0) && (sigma_R == 0.0)) return(NA_REAL);

    int n = Y.size();
    if (n < 1) return(NA_REAL);

    double K_mr = kalman_gain_par_mr (rho, sigma_M, sigma_R);
    double K_rw = 1.0 - K_mr;
    
    double esumsq = 0.0;
    double M = M0;
    double R = R0;
    double tvar = sigma_M*sigma_M + sigma_R*sigma_R;
    
    for (int i=0; i < n; i++) {
        double xhat = rho * M + R;
        double e = Y[i] - xhat;
        esumsq += e*e;
        M = rho * M + e * K_mr;
        R += e * K_rw;
    }
    
    double nll = (n/2.0)*log(tvar * 2.0*PI) + esumsq/(2.0*tvar);
    return nll;
}

// [[Rcpp::export]]

double loglik_par_t_c (NumericVector Y, double rho, double sigma_M, double sigma_R, 
    double M0, double R0, double nu=5.0) {
    /* Computes the log likelihood of Y given parameters p using
       a steady state Kalman filter using the assumption that the
       error terms will be t-distributed.  Returns the negative log
       likelihood value.
    */
        
    if ((rho < -1.0) || (rho > 1.0)) return(NA_REAL);
    if (sigma_M < 0.0) return(NA_REAL);
    if (sigma_R < 0.0) return(NA_REAL);
    if ((sigma_M == 0.0) && (sigma_R == 0.0)) return(NA_REAL);
    if (nu < 2) return(NA_REAL);

    int n = Y.size();
    if (n < 1) return(NA_REAL);

    double K_mr = kalman_gain_par_mr (rho, sigma_M, sigma_R);
    double K_rw = 1.0 - K_mr;
    
    double esum = 0.0;
    double M = M0;
    double R = R0;
//    double tvar = (sigma_M*sigma_M + sigma_R*sigma_R) * (nu - 2.0) / nu;
    double tvar = (sigma_M*sigma_M + sigma_R*sigma_R);
    double tsd = sqrt(tvar);
    double const_term = n * (lgamma((nu+1.0)*0.5) - 0.5*log(nu * PI) - 
        lgamma(nu*0.5) - log(tsd));
    
    for (int i=0; i < n; i++) {
        double xhat = rho * M + R;
        double e = Y[i] - xhat;
        esum += log(1 + (e*e/tvar)/nu);
        M = rho * M + e * K_mr;
        R += e * K_rw;
    }
    
    double nll = ((nu + 1.0) * 0.5) * esum - const_term;
    return nll;
}
