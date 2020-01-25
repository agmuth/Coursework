#'This script contains all the global variables and some commonly used functions for this project.
#'


#REQUIRED LIBRARIES-------------------------------------------------------------

require("PerformanceAnalytics")
require("rugarch")
require("quadprog")
require("Rfast")

#GLOBAL VARIABLES---------------------------------------------------------------

QUARTERS <- as.Date(c("2016-12-31", "2017-03-31", "2017-06-30", "2017-09-30", "2017-12-31", "2018-03-31", "2018-06-30", "2018-09-30", "2018-12-31",
                      "2019-03-31", "2019-06-30", "2019-09-30")) #dates for end of quarters

LEAD_IN <- 2 #lead-in in number of QUARTERS for each model

#FUNCTIONS-----------------------------------------------------------------------

#' This function automatically determines the form of a GARCH model for the given series according to some criterion. 
#' This is done in a two step procedure by first fitting an ARMA model to the series and the fitting a second ARMA 
#' model to the squared residuals of the second series as outlined on page 132 of "Analysis of Financial Time Series" by R. S. Tsay.
#'
#' @param series xts vector: the time series to model
#' @param max.p.mean integer: maximum value for the AR process of the mean
#' @param max.q.mean integer: maximum value for the MA process of the mean
#' @param max.p.var integer: maximum value for the AR process of the conditional variance
#' @param max.q.var integer: maximum value for the MA process of the conditional variance
#' @param var.distn distribution.model argument to ugarchspec
#' @param ic information criterion to evaluate models on must be one of "aicc", "aic", "bic"
#'
#' @return
#' @export
#'
#' @examples
getGarchSpec <- function(series, max.p.mean = 2, max.q.mean = 2, max.p.var= 2, max.q.var = 2, var.distn = "norm", ic = c("aicc", "aic", "bic")){
  
  #fit ARMA model to series
  mean_arima <- auto.arima(series, max.p = max.p.mean, max.q = max.q.mean, max.d = 0, ic = ic, trace = FALSE)
  arima_order <- mean_arima$arma[1:2]
  
  #fit second ARMA model to the squared residuals of the first ARMA model
  garch_arima <- auto.arima(residuals(mean_arima)^2, max.p = max.p.var, max.q = max.q.var, max.d = 0, ic = ic, trace = FALSE)
  garch_order <- garch_arima$arma[1:2]
  
  if(all(garch_order == 0)){
    #garch component is required so that the ugarchspec object can be used later in mulitvariate modelling. 
    #in the event that the ARMA model fit to the squared residuals is ARMA(0, 0) a ARMA(1, 0) is chosen instead.
    garch_order <- c(1, 0)
  }
  
  spec <-  ugarchspec(mean.model = list(armaOrder = arima_order), 
                      variance.model = list(garchOrder = garch_order, model = "sGARCH"), distribution.model = var.distn)
  
  return(spec)
}


#' This functions calculates the optimal portfolio weights of a given set of assets subject to the constraints for this project. 
#' In some of the sample covariance matricies for the simulated log returns tbhe crypto curriencies have simulated variances on 
#' the order of 1x10^6 or larger. These large numbers seem to break the underlying solve.QP() function we rely on to solve the 
#' quadratic programming problem. As a workaround to this problem we introduced the optional parameter "max_var" as the maximum 
#' asset variance to be tolerated. In the event that an entry on the diagonal of the covariance matrix exceeds this threshold 
#' that asset is given the minimum allowed portfolio weight.
#'
#' @param sigma matrix: covariance matrix of the portfolio assets
#' @param max_var numeric: maximum asset variance to be tolerated
#'
#' @return
#' @export
#'
#' @examples
getIndexWeights <- function(sigma, max_var=1e6){
  
  assets <- colnames(sigma)
  too_risky <- diag(sigma) > max_var #index of assets with a greater variance than to be tolerated
  sigma <- sigma[!too_risky, !too_risky]
  N <- ncol(sigma) # number of not too risky assets 
  
  #setup of QP problem
  
  #equality constaints
  
  #weights must sum to 1
  A_eq <- rep(1, N)
  b_eq <- 1 - 0.01 * sum(too_risky)
  
  #inequality constraints
  
  #weights must be greater than 0.01
  A_neq1 <- diag(N)
  b_neq1 <- rep(0.01, N)
  
  #weights must be less than 0.25
  A_neq2 <- -diag(N)
  b_neq2 <- -rep(0.25, N)
  
  #aggregate constraints
  Amat <- rbind(A_eq, A_neq1, A_neq2)
  bvec <- c(b_eq, b_neq1, b_neq2)
  dvec <- rep(0, N)
  
  qp <- solve.QP(sigma, dvec, t(Amat), bvec, meq=1) #find portfolio weights

  if(sum(too_risky) > 0){
    weights <- rep(0.01, length(too_risky))
    weights[!too_risky] <- qp$solution
  }else{
    weights <- qp$solution
  }
  
  names(weights) <- assets
  
  return(weights)
}


