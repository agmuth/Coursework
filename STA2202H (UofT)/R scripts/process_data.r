#' This script cleans and merges the data found in "TSA 2019 Project 110419.xlsx" into the indicies outlined on 
#' sheet1. The time series of the prices of the indicies as well as the daily log returns are then save the in 
#' the"/data" folder.
#'


#required libraries-------------------------------------------------------------------------------------------

require("readxl")
require("RQuantLib")
require("zoo")
require("dplyr")

#helper functions---------------------------------------------------------------------------------------------

#The helper functions below were written specifically for the indicies defined beginning at line 147. As such descriptions are brief. 

clean_dates <- function(dat){
  #merges the time series onto a single date index
  
  dat <- merge(data.frame("Date"=seq(min(dat$Date), max(dat$Date), 1)), dat, by="Date", all.x=TRUE) #merge indicies onto complete sequence of dates in case of NA's
  dat <- dat[isBusinessDay(calendar="UnitedStates/NYSE", dates=dat$Date),] #remove weekends and US holidays
  dat <- as.data.frame(apply(dat, 2, function(x){na.locf(x, na.rm=FALSE)})) #fill any NA's with last value
  dat <- dat[complete.cases(dat),]  #remove any NA's at beginning of series
  dat[,-1] <- apply(dat[,-1], 2, as.numeric) #convert values back to numeric
  dat$Date <- as.Date(dat$Date) #convert dates to Date object
  
  return(dat)
}

convert_to_xdr <- function(dat){
  #converts all prices into XRD
  
  dat[,-c(1, 2)] <- dat[,-c(1, 2)] * dat[,c("USD")]
  
  return(dat)
}

get_log_returns <- function(dat){
  #returns "daily" log-returns for each asset in index
  
  Date <- dat$Date[2:nrow(dat)]
  log_returns <- apply(dat[,-c(1)], 2, FUN=function(x){log(x[2:length(x)] / x[1:(length(x)-1)])})
  
  return(cbind.data.frame(Date, log_returns))
}

get_returns <- function(dat){
  #returns "daily" returns for each asset in index
  
  Date <- dat$Date[2:nrow(dat)]
  returns <- apply(dat[,-c(1)], 2, FUN=function(x){x[2:length(x)] - x[1:(length(x)-1)]})
  
  return(cbind.data.frame(Date, returns))
}

#main script---------------------------------------------------------------------------------------------------

#read in asset time series

fname <- "../data/TSA 2019 Project 110419.xlsx"

#currencies
usd <- read_excel(fname, sheet=2, skip=2)[,c("Date", "Close")] #measured in XRD
eur <- read_excel(fname, sheet=3, skip=2)[,c("Date", "Close")] #measured in USD
jpy <- read_excel(fname, sheet=4, skip=2)[,c("Date", "Close")] #measured in USD
cyn <- read_excel(fname, sheet=5, skip=2)[,c("Date", "Close")] #measured in USD

usd$Date <- as.Date(usd$Date, format="%m/%d/%Y")
eur$Date <- as.Date(eur$Date, format="%m/%d/%Y")
jpy$Date <- as.Date(jpy$Date, format="%m/%d/%Y")
cyn$Date <- as.Date(cyn$Date, format="%m/%d/%Y")

colnames(usd) <- c("Date", "USD")
colnames(eur) <- c("Date", "EURUSD")
colnames(jpy) <- c("Date", "JPYUSD")
colnames(cyn) <- c("Date", "CYNUSD")

#crypto-currencies
bitcoin <- read_excel(fname, sheet=6, skip=0)[,c("Date", "Close")] 
ethereum <- read_excel(fname, sheet=7, skip=0)[,c("Date", "Close")] 
ripple <- read_excel(fname, sheet=8, skip=0)[,c("Date", "Close")] 
monero <- read_excel(fname, sheet=9, skip=0)[,c("Date", "Close")] 

bitcoin$Date <- gsub(" ", "-", bitcoin$Date)
ethereum$Date <- gsub(" ", "-", ethereum$Date)
ripple$Date <- gsub(" ", "-", ripple$Date)
monero$Date <- gsub(" ", "-", monero$Date)

bitcoin$Date <- as.Date(bitcoin$Date, format="%b-%d-%Y")
ethereum$Date <- as.Date(ethereum$Date, format="%b-%d-%Y")
ripple$Date <- as.Date(ripple$Date, format="%b-%d-%Y")
monero$Date <- as.Date(monero$Date, format="%b-%d-%Y")

colnames(bitcoin) <- c("Date", "BTC")
colnames(ethereum) <- c("Date", "ETH")
colnames(ripple) <- c("Date", "XRP")
colnames(monero) <- c("Date", "XMR")

#agri commodities
wheat <- read_excel(fname, sheet=19, skip=2)[,c("Date", "Close")] 
soy <- read_excel(fname, sheet=20, skip=2)[,c("Date", "Close")] 
cattle <- read_excel(fname, sheet=21, skip=2)[,c("Date", "Close")] 
lumber <- read_excel(fname, sheet=22, skip=1)[,c("Date", "Settle")] 

wheat$Date <- as.Date(wheat$Date, format="%m/%d/%Y")
soy$Date <- as.Date(soy$Date, format="%m/%d/%Y")
cattle$Date <- as.Date(cattle$Date, format="%m/%d/%Y")
lumber$Date <- as.Date(lumber$Date, format="%Y-%m-%d")

colnames(wheat) <- c("Date", "__W_USSD")
colnames(soy) <- c("Date", "__SYB_TD")
colnames(cattle) <- c("Date", "_ICXD")
colnames(lumber) <- c("Date", "CHRIS-CME_LB1")


#merge time series into indicies 
agriculture <- Reduce(function(...) merge(..., by="Date", all=TRUE), 
                      list(usd, eur, jpy, cyn, bitcoin, ethereum, ripple, monero, wheat, soy, cattle, lumber))


#clean and match dates and convert assets to XRD
agriculture <- agriculture %>% clean_dates(.) %>% convert_to_xdr(.)

#save cleaned price data to csv
write.csv(agriculture, "../data/agriculture_prices.csv", row.names = FALSE)

#get daily returns and save data to csv
agriculture %>% get_returns(.) %>%  write.csv(., "../data/agriculture_price_diffs.csv", row.names = FALSE)

#get daily log-returns and save data to csv
agriculture %>% get_log_returns(.) %>%  write.csv(., "../data/agriculture_log_returns.csv", row.names = FALSE)







