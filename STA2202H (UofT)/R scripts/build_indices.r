#'This script takes the DCC GARCH models fit in the file "build_multivariate_models.r" and either forecasts or simulates the 
#'price of the 12 assets at the end of the next quarter. It does this for each quarter from March 31, 2017 to September 30, 2019.
#'Using the respective variance-covariance matrices for each end of quarter the asset weights of the portfolio for that quarter 
#'are then solved for via quadratic programming subject to the constraints that each asset allocation must be between 1% and 25%. 
#'The indices for the time period under consideration are then computed using these weightings for each quarter and are saved as
#'external csv files.
#'

#required libraries-------------------------------------------------------------------------------------------
require("rmgarch")
require("RQuantLib")

#source helper functions--------------------------------------------------------------------------------------
source("utils.r")


#main script--------------------------------------------------------------------------------------------------

#read in agriculture data
agriculture <- read.csv("../data/agriculture_prices.csv")
agriculture$Date <- as.Date(agriculture$Date)

#simulation parameters
Nsim <- 10000
simseed <- 1234L

#set up dataframes to hold index weights

#index based on forecased prices
weights_prices <- data.frame(matrix(0, nrow=as.numeric(QUARTERS[12] + 1 - QUARTERS[LEAD_IN + 1], unit="days"), ncol=ncol(agriculture)))
colnames(weights_prices) <- colnames(agriculture)
weights_prices$Date <- as.Date(seq(QUARTERS[LEAD_IN + 1], QUARTERS[12], by="day"))
weights_prices <- weights_prices[weights_prices$Date %in% agriculture$Date,]

#index based on forecasted price differences
weights_price_diffs <- data.frame(matrix(0, nrow=as.numeric(QUARTERS[12] + 1 - QUARTERS[LEAD_IN + 1], unit="days"), ncol=ncol(agriculture)))
colnames(weights_price_diffs) <- colnames(agriculture)
weights_price_diffs$Date <- as.Date(seq(QUARTERS[LEAD_IN + 1], QUARTERS[12], by="day"))
weights_price_diffs <- weights_price_diffs[weights_price_diffs$Date %in% agriculture$Date,]

#index based on forcasted log returns
weights_log_returns <- data.frame(matrix(0, nrow=as.numeric(QUARTERS[12] + 1 - QUARTERS[LEAD_IN + 1], unit="days"), ncol=ncol(agriculture)))
colnames(weights_log_returns) <- colnames(agriculture)
weights_log_returns$Date <- as.Date(seq(QUARTERS[LEAD_IN + 1], QUARTERS[12], by="day"))
weights_log_returns <- weights_log_returns[weights_log_returns$Date %in% agriculture$Date,]


#get weights for price based forecasts based on forecasts from DCC models

for(i in (LEAD_IN + 1):(length(QUARTERS)-1)){
  
  #get forecasted price data
  fit <- readRDS(paste0("../models/dcc fits/prices_", QUARTERS[i])) #read in model fit on previous two quarters of data
  forecast_horizon <- sum(isBusinessDay(calendar="UnitedStates/NYSE", dates=seq(QUARTERS[i], QUARTERS[i+1], by="day"))) #number of trading days until quarter's end
  sim <- dccforecast(fit, n.ahead = forecast_horizon) #forecast end of quarter price data
  sigma <- rcov(sim)[[1]][,,forecast_horizon] 
  weights <- getIndexWeights(sigma) #get portfolio weights based on covariance matrix
  
  #populate weights dataframe
  date_idx <- weights_prices$Date > QUARTERS[i] & weights_prices$Date <= QUARTERS[i+1]
  for(d in weights_prices$Date[date_idx]){
    weights_prices[weights_prices$Date == d, 2:ncol(weights_prices)] <- weights
  }
}


#get weights for price dfference based on simulations from DCC models

for(i in (LEAD_IN + 1):(length(QUARTERS)-1)){
  
  #get forecasted price data
  fit <- readRDS(paste0("../models/dcc fits/price_diffs_", QUARTERS[i])) #read in model fit on previous two quarters of data
  forecast_horizon <- sum(isBusinessDay(calendar="UnitedStates/NYSE", dates=seq(QUARTERS[i], QUARTERS[i+1], by="day"))) #number of trading days until quarter's end
  sim <- dccsim(fit, n.sim = forecast_horizon, m.sim = Nsim, startMethod = "sample", rseed = simseed) #simulate end of quarter price data
  sim_dat <- sim@msim$simX #extract raw simulation data
  
  #aggregate simulation data to get end of quarter price data
  sim_dat <- as.data.frame(matrix(unlist(lapply(sim_dat, function(x){ #calculate simulated total dollar return over the quarter
    colSums(x)
  })), nrow=Nsim, byrow=TRUE))
  colnames(sim_dat) <- colnames(agriculture[2:ncol(agriculture)])
  curr_price <- agriculture[agriculture$Date <= QUARTERS[i],]
  curr_price <- curr_price[nrow(curr_price),2:ncol(curr_price)]
  sim_dat <- data.frame(Map("+", sim_dat, curr_price), row.names=rownames(data))
  sigma <- cov(sim_dat)
  sigma <- (sigma + t(sigma)) / 2 #average entries so matrix is symmetric
  weights <- getIndexWeights(sigma) #get portfolio weights based on covariance matrix
  
  #populate weights dataframe
  date_idx <- weights_price_diffs$Date > QUARTERS[i] & weights_price_diffs$Date <= QUARTERS[i+1]
  for(d in weights_price_diffs$Date[date_idx]){
    weights_price_diffs[weights_price_diffs$Date == d, 2:ncol(weights_price_diffs)] <- weights
  }
}



#get weights for log return based on simulations from DCC models

for(i in (LEAD_IN + 1):(length(QUARTERS)-1)){
  
  #get forecasted price data
  fit <- readRDS(paste0("../models/dcc fits/log_returns_", QUARTERS[i])) #read in model fit on previous two quarters of data
  forecast_horizon <- sum(isBusinessDay(calendar="UnitedStates/NYSE", dates=seq(QUARTERS[i], QUARTERS[i+1], by="day"))) #number of trading days until quarter's end
  sim <- dccsim(fit, n.sim = forecast_horizon, m.sim = Nsim, startMethod = "sample", rseed = simseed) #simulate end of quarter price data
  sim_dat <- sim@msim$simX #extract raw simulation data
  
  #aggregate simulation data to get end of quarter price data
  sim_dat <- as.data.frame(matrix(unlist(lapply(sim_dat, function(x){ #calculate simulated total percent return over the quarter
    exp(colSums(x))
  })), nrow=Nsim, byrow=TRUE))
  colnames(sim_dat) <- colnames(agriculture[2:ncol(agriculture)])
  curr_price <- agriculture[agriculture$Date <= QUARTERS[i],]
  curr_price <- curr_price[nrow(curr_price),2:ncol(curr_price)]
  sim_dat <- data.frame(Map("*", sim_dat, curr_price), row.names=rownames(data))
  sigma <- cov(sim_dat)
  sigma <- (sigma + t(sigma)) / 2 #average entries so matrix is symmetric
  weights <- getIndexWeights(sigma) #get portfolio weights based on covariance matrix
  
  #populate weights dataframe
  date_idx <- weights_log_returns$Date > QUARTERS[i] & weights_log_returns$Date <= QUARTERS[i+1]
  for(d in weights_log_returns$Date[date_idx]){
    weights_log_returns[weights_log_returns$Date == d, 2:ncol(weights_log_returns)] <- weights
  }
}


#save index wieghts 

write.csv(weights_prices, "../data/prices_index_weights.csv")
write.csv(weights_price_diffs, "../data/price_diffs_index_weights.csv")
write.csv(weights_log_returns, "../data/log_returns_index_weights.csv")


#calculate actual indicies

initial_capital <- 100

agri_indicies <- as.data.frame(matrix(0, nrow=sum(agriculture$Date >= QUARTERS[3]), ncol=4))
colnames(agri_indicies) <- c("Date", "Index_prices", "Index_price_diffs", "Index_log_returns")
agri_indicies$Date <- agriculture$Date[sum(agriculture$Date <= QUARTERS[3]):nrow(agriculture)]
agri_indicies[1, 2:4] <- initial_capital

price_changes <- agriculture[2:nrow(agriculture), 2:ncol(agriculture)] / agriculture[1:(nrow(agriculture)-1), 2:ncol(agriculture)]
price_changes <- rbind.data.frame(rep(1, 12), price_changes)
price_changes$Date <- agriculture$Date

for(i in 2:length(agri_indicies$Date)){
  
  agri_indicies$Index_prices[i] <- agri_indicies$Index_prices[i-1] * as.matrix(price_changes[i, 1:(ncol(price_changes)-1)]) %*% t(weights_prices[i, 2:ncol(weights_prices)])
  agri_indicies$Index_price_diffs[i] <- agri_indicies$Index_price_diffs[i-1] * as.matrix(price_changes[i, 1:(ncol(price_changes)-1)]) %*% t(weights_price_diffs[i, 2:ncol(weights_price_diffs)])
  agri_indicies$Index_log_returns[i] <- agri_indicies$Index_log_returns[i-1] * as.matrix(price_changes[i, 1:(ncol(price_changes)-1)]) %*% t(weights_log_returns[i, 2:ncol(weights_log_returns)])
  
}

write.csv(agri_indicies, "../data/agriculture_indicies.csv")


# repeat for completely out of sample data

agri_indicies <- as.data.frame(matrix(0, nrow=sum(agriculture$Date >= QUARTERS[8]), ncol=4))
colnames(agri_indicies) <- c("Date", "Index_prices", "Index_price_diffs", "Index_log_returns")
agri_indicies$Date <- agriculture$Date[(sum(agriculture$Date <= QUARTERS[8]) + 1) :nrow(agriculture)]
agri_indicies[1, 2:4] <- initial_capital

price_changes <- agriculture[2:nrow(agriculture), 2:ncol(agriculture)] / agriculture[1:(nrow(agriculture)-1), 2:ncol(agriculture)]
price_changes <- rbind.data.frame(rep(1, 12), price_changes)
price_changes$Date <- agriculture$Date

for(i in 2:length(agri_indicies$Date)){
  
  agri_indicies$Index_prices[i] <- agri_indicies$Index_prices[i-1] * as.matrix(price_changes[i, 1:(ncol(price_changes)-1)]) %*% t(weights_prices[i, 2:ncol(weights_prices)])
  agri_indicies$Index_price_diffs[i] <- agri_indicies$Index_price_diffs[i-1] * as.matrix(price_changes[i, 1:(ncol(price_changes)-1)]) %*% t(weights_price_diffs[i, 2:ncol(weights_price_diffs)])
  agri_indicies$Index_log_returns[i] <- agri_indicies$Index_log_returns[i-1] * as.matrix(price_changes[i, 1:(ncol(price_changes)-1)]) %*% t(weights_log_returns[i, 2:ncol(weights_log_returns)])
  
}

write.csv(agri_indicies, "../data/agriculture_indicies_out_of_sample.csv")


