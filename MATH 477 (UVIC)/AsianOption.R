#' Price an asian option using Monte Carlo techniques.
#'
#' @param s initial share price.
#' @param D strike time measured in days (number of days on the contract).
#' @param k strike price.
#' @param r annual nominal interest rate.
#' @param sigma volatility.
#' @param n number of simulations.
#' 
#' @return the payoffs of the option over all n runs.
#' @export
#'
#' @examples 
#' 

priceAsianOptionMonteCarlo <- function(n, s, D, k, r, sigma){
  walks <- matrix(rnorm(n*D, mean=(r - sigma^2 / 2)/252, sd=sigma/sqrt(252)), nrow=n, ncol=D)      
  walks <- t(apply(walks, 1, cumsum))                             
  walks <- s*rowMeans(exp(walks)) -k                                             
  walks <- exp(-r*D/252)*pmax(walks, 0)
  return(walks)
}



