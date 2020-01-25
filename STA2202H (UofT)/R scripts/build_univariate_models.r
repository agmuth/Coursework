#' This script identifies the univariate garch model for each series of interest and saves the 
#' resulting ugarchspec objects for later use in fitting a mulitvariate garch model to the entire
#' reutrn series.
#'

#required libraries-------------------------------------------------------------------------------------------

require("forecast")
require("rugarch")

#source helper functions--------------------------------------------------------------------------------------
source("utils.r")


#main script---------------------------------------------------------------------------------------------------

#filepaths for where the price data is stored
path <- "../data/agriculture_"
files <- c("prices", "price_diffs", "log_returns")



for(file in files){
  
  dat <- read.csv(paste0(path, file, ".csv")) #read in file
  dat$Date <- as.Date(dat$Date)
  dat <- dat[dat$Date < as.Date("2018-09-30"),] #remove most recent year for testing later
  dates <- dat$Date
  
  for(asset in colnames(dat)[2:ncol(dat)]){
    #identify garch model for each series and then save the corresponding ugarchspec 
    #object to use fit a multivariate garch model later
    
    series <- data.frame(dat[,asset]) #format series
    rownames(series) <- dates
    series <- as.xts(series)
    
    spec <- getGarchSpec(series, max.p.mean = 2, max.q.mean = 2, max.p.var = 2, max.q.var = 2, var.distn = "norm", ic="bic")
    
    saveRDS(spec, paste0("../models/specs/ugarch_", file, "_", asset))
  }
}


