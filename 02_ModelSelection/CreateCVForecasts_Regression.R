rm(list=ls())
library(dplyr)
library(lubridate)
library(Rcpp)
library(data.table)
source('../utils.R')

# Running sds
recursiveVol <- function(x){
  n = length(x)
  # assumes ordered by dates.
  if( n < 3){ # require at least two months of PRIOR data to forecast time t
    out = NA
  } else {
    out <- sapply(3:n, function(w) sd(x[1:(w-1)],na.rm=TRUE))
    out <- c(out[1],out[1],out) # first sd is backcast.
  }
  out
}
# Load the data
data <- readRDS('../data/prediction_dataset_2025.RDS')
data <- data %>% select(c('permno','date','RET','ret_dec'))
data <- data[ data$date >= '1974-01-01' , ] # keep only out of sample to save memory

data$permno_date <- paste0(data$permno,data$date)
data$yyyymm <- paste0(year(data$date),
                      ifelse(nchar(month(data$date))==2,month(data$date),paste0('0',month(data$date))))

# First, XGB(R)
xgb_r_models <- list()
xgb_r_models[[1]] <- readRDS('../forecasts/XGB_RETS_D2_E0.01_N100.RDS')
xgb_r_models[[2]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D2_E0.1_N100.RDS')
xgb_r_models[[3]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D2_E0.25_N100.RDS')
xgb_r_models[[4]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D2_E0.5_N100.RDS')
xgb_r_models[[5]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D2_E0.75_N100.RDS')
xgb_r_models[[6]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D2_E1_N100.RDS')

xgb_r_models[[7]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D3_E0.01_N100.RDS')
xgb_r_models[[8]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D3_E0.1_N100.RDS')
xgb_r_models[[9]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D3_E0.25_N100.RDS')
xgb_r_models[[10]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D3_E0.5_N100.RDS')
xgb_r_models[[11]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D3_E0.75_N100.RDS')
xgb_r_models[[12]] <- readRDS('../../hbw-replication/forecasts/XGB_RETS_D3_E1_N100.RDS')


xgb_r_permno_date <- paste0(xgb_r_models[[1]]$permno,xgb_r_models[[1]]$date)
keep <- match( data$permno_date, xgb_r_permno_date)

for( i in 1:length(xgb_r_models)){
  xgb_r_models[[i]] <- xgb_r_models[[i]][ keep ,]
}

# Then, OLS
ols <- readRDS('../../hbw-replication/forecasts/OLS.RDS')
ols_permno_date <- paste0(ols$permno,ols$date)
keep <- match( data$permno_date, ols_permno_date)
ols <- ols[keep,]
data$ols <- ols$ols

## Prepare for tuning
idx <- sort(unique(data$date)) # unique date
idx.yr <- idx[seq(1,length(idx),12)] # define model update date
idx.oos <- idx[ idx >= "1987-01-01"] # out of sample months
oos.seq <- idx.oos[seq(1,length(idx.oos),12)] # find the out of sample months to update forecasts

## We first find the parameters to maximize pseudo R2. 
R2_OOS <- matrix(NA,nrow=length(oos.seq), ncol=length(xgb_r_models) )
data$xgb_r <- NA

for( t in 1:length(oos.seq) ){
  idx.start = idx.yr[t]
  idx.end = oos.seq[t]
  data.val = data[data$date >= idx.start & data$date < idx.end, ]
  for ( m in 1:length(xgb_r_models)){
    xgb.val = xgb_r_models[[m]]
    xgb.val = xgb.val[xgb.val$date >= idx.start & xgb.val$date < idx.end, ]
    R2_OOS[t, m] <- 1-sum( (data.val$RET- xgb.val$xgb)^2 )/sum(data.val$RET^2)
  }
  xgb_pred <- xgb_r_models[[which.max(R2_OOS[t,])]]
  
  if( t == 1){
    idx.tmp <- data$date < oos.seq[t+1] # Use the selected model on the validation sample, too.
  } else if (t < length(oos.seq)){
    idx.tmp <- data$date >= oos.seq[t] & data$date < oos.seq[t+1]
  } else {
    idx.tmp <- data$date >= oos.seq[t] 
  }   
  data$xgb_r[idx.tmp]  <- xgb_pred$xgb[idx.tmp]
  cat(sprintf('%s, %s \n',t,which.max(R2_OOS[t,])))
}

# Then, we estimate idiosyncratic variances.
data$xgb_eps <- data$RET-data$xgb_r
data$ols_eps <- data$RET-data$ols

setDT(data)
data <- data[order(permno, date)]
data[, `:=`(
  xgb_idio_sd = recursiveVol(xgb_eps),
  ols_idio_sd = recursiveVol(ols_eps)
), by = permno]

data[, `:=`(
  xgb_r_pg90 = pnorm((xgb_r - quantile(xgb_r, 0.9, na.rm = TRUE)) / xgb_idio_sd),
  xgb_r_pleq10 = pnorm((quantile(xgb_r, 0.1, na.rm = TRUE) - xgb_r) / xgb_idio_sd),
  xgb_r_td_top = (xgb_r >= quantile(xgb_r, 0.9, na.rm = TRUE)),
  xgb_r_td_bot = (xgb_r <= quantile(xgb_r, 0.1, na.rm = TRUE)),
  ols_pg90 = pnorm((ols - quantile(ols, 0.9, na.rm = TRUE)) / ols_idio_sd),
  ols_pleq10 = pnorm((quantile(ols, 0.1, na.rm = TRUE) - ols) / ols_idio_sd),
  ols_td_top = (ols >= quantile(ols, 0.9, na.rm = TRUE)),
  ols_td_bot = (ols <= quantile(ols, 0.1, na.rm = TRUE))
), by = date]

data <- as.data.frame(data[,.(permno, date, RET, permno_date, yyyymm,
                              ols, ols_idio_sd, ols_pg90, ols_pleq10, ols_td_top, ols_td_bot,
                              xgb_r, xgb_idio_sd, xgb_r_pg90, xgb_r_pleq10,xgb_r_td_top,xgb_r_td_bot)])

saveRDS(data,'RegressionModels.RDS')
