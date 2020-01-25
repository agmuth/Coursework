#' Wrapper function to compute descrpiptive statistics of multiple MOnte Carlo runs.
#'
#' @param niters number of times to do meta Monte Carlo runs / number of times to call func
#' @param func function which computes the actual Monte Carlo runs
#' @param parallel logical, should opperation be run in parallel
#' @param ncores number of cores to use if operation is to be run in parallel
#' @param ... additional parameters passed to func
#'
#' @return vector containing the mean and standard deviation of the Monte Carlo trials
#' @export
#'
#' @examples
mcWrapper <- function(niters, func, parallel=FALSE, ncores=1, ...){
  
  require(doParallel)
  require(foreach)
  
  if(parallel==TRUE){                                             #register parallel backend
    cl <- makeCluster(ncores)                                     
    registerDoParallel(cl)
  }
  
  mc_dat  <- foreach(i=1:niters, .combine=data.frame) %dopar% { #break up mc task and run in parallel if requested
    mc_sample <- func(...)
    c(sum(mc_sample),  sum(mc_sample^2), length(mc_sample))
  }
  
  if(parallel==TRUE){                                             #register sequential backend
    stopCluster(cl)
    registerDoSEQ()
  }
  
  #compute mean and standard deviation of mc trials
  x_sum <- sum(mc_dat[1,])  #sumation of x
  x2_sum <- sum(mc_dat[2,]) #sumation of x^2
  big_n <- sum(mc_dat[3,]) #total sample size
  
  mc_mean <- x_sum / big_n
  mc_sd <- (big_n-1)^-1 * (x2_sum - 2*big_n^-1*x_sum^2 + big_n^-2*x_sum^2)
  
  return(c(mc_mean, mc_sd))
}
