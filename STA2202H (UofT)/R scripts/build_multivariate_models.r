#'This script builds and saves the DCCGARCH models using the univariate models parameters found in 
#'the script "build_univariate_models.r".
#'

#required libraries-------------------------------------------------------------------------------------------

require("forecast")
require("rugarch")
require("rmgarch")
require("xts")

#source helper functions--------------------------------------------------------------------------------------

source("utils.r")


#main script--------------------------------------------------------------------------------------------------

#filepaths for where the price data is stored
path <- "../data/agriculture_"
files <- c("prices", "price_diffs", "log_returns")


for(file in files){
  
  dat <- read.csv(paste0(path, file, ".csv")) #read in asset data
  dat$Date <- as.Date(dat$Date)
  dates <- dat$Date
  specs <- list.files("../models/specs") #find univariate model specifications
  specs <- specs[grepl(file, specs)]
  uspecs <- vector(mode = "list", length = length(specs))
  
  #read in univariate garch specifications
  for(i in 1:length(specs)){
    uspecs[[i]] <- readRDS(paste0("../models/specs/", specs[i]))
  }
  
  mspec <- dccspec(uspec = multispec(uspecs), dccOrder = c(1,1),  distribution = "mvnorm") #specify the dccgarch model
  
  #fit the dccgarch model on moving 2 quarter windows
  for(i in (LEAD_IN + 1):length(QUARTERS)){ 
    series <- dat[dat$Date <= QUARTERS[i] & dat$Date > QUARTERS[i-LEAD_IN], 2:ncol(dat)]
    rownames(series) <- dat$Date[dat$Date <= QUARTERS[i] & dat$Date > QUARTERS[i-LEAD_IN]]
    series <- as.xts(series)
    
    fit <- dccfit(mspec, data = series, fit.control = list(eval.se=FALSE), solver="solnp")
    
    #save dccgarch model under the name of last data used to fit it
    #ex sufix "2019-01-01" indicates that the model was fit on the two QUARTERS of data
    #immediately before 2019-01-01 and no data after 2019-01-01
    
    saveRDS(fit, paste0("../models/dcc fits/", file, "_", QUARTERS[i]))
  }
}
