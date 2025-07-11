# This script computes bid ask spreads using 2 different methods:
# 1: We compute quoted spreads.
# 2: We follow Ledoit Wolf (2024)

# Clear env.
rm(list=ls())

# Libraries
library(tidyr)
library(lubridate)
library(data.table)
library(dplyr)

LW <- function(open, high, low, close){
  vol = sqrt(mean( (1/2)*log(high/low)^2 - (2*log(2) -1)*log(close/open)^2))
  exp(-4.137 + 0.777 * log(vol))
}
QS <- function(bid,ask){
  mean( (ask-bid)/( (1/2)*(ask+bid)) , na.rm=TRUE)
}

# Load daily data
data <- fread('../data/DailyData.csv')
data <- data %>% filter(EXCHCD %in% c(1,2,3)) # Keep only NYSE,AMEX,NASDAQ
md = month(data$date)
yd = year(data$date)
data$yyyymm <- paste0(yd, ifelse(nchar(md)==2,md,paste0('0',md)))
data$year <- yd
data$permno_date <- paste0(data$PERMNO,data$yyyymm)

data[OPENPRC<0, OPENPRC := NA]
data[ASKHI<0, ASKHI := NA]
data[BIDLO<0, BIDLO := NA]
data[PRC<0, PRC := NA]
data[, RET := as.numeric(RET)]

data <- data %>% filter(!is.na(PRC),!is.na(RET))

spreads = data[, .(lw_spread   = LW(OPENPRC, ASKHI, BIDLO, PRC),
                   qs_spread   = QS(BID,ASK),
                   permno = unique( PERMNO),
                   date = unique( yyyymm )), by = permno_date]


saveRDS(spreads,'EstimatedBidAskSpread.RDS')

