rm(list=ls())
library(dplyr)
library(lubridate)
library(Rcpp)
library(data.table)
source('../utils.R')

# Load the data
data <- readRDS('../data/prediction_dataset.RDS')
data <- data %>% select(c('permno','date','RET','ret_dec'))
data <- data[ data$date >= '1974-01-01' , ] # keep only out of sample to save memory

data$permno_date <- paste0(data$permno,data$date)
data$yyyymm <- paste0(year(data$date),
                      ifelse(nchar(month(data$date))==2,month(data$date),paste0('0',month(data$date))))

xgb_c_models <- list()
xgb_c_models[[1]] <- readRDS('../forecasts/XGB_D2_E0.01_N100.RDS')
xgb_c_models[[2]] <- readRDS('../forecasts/XGB_D2_E0.1_N100.RDS')
xgb_c_models[[3]] <- readRDS('../forecasts/XGB_D2_E0.25_N100.RDS')
xgb_c_models[[4]] <- readRDS('../forecasts/XGB_D2_E0.5_N100.RDS')
xgb_c_models[[5]] <- readRDS('../forecasts/XGB_D2_E0.75_N100.RDS')
xgb_c_models[[6]] <- readRDS('../forecasts/XGB_D2_E1_N100.RDS')

xgb_c_models[[7]] <- readRDS('../forecasts/XGB_D3_E0.01_N100.RDS')
xgb_c_models[[8]] <- readRDS('../forecasts/XGB_D3_E0.1_N100.RDS')
xgb_c_models[[9]] <- readRDS('../forecasts/XGB_D3_E0.25_N100.RDS')
xgb_c_models[[10]] <- readRDS('../forecasts/XGB_D3_E0.5_N100.RDS')
xgb_c_models[[11]] <- readRDS('../forecasts/XGB_D3_E0.75_N100.RDS')
xgb_c_models[[12]] <- readRDS('../forecasts/XGB_D3_E1_N100.RDS')

xgb_c_permno_date <- paste0(xgb_c_models[[1]]$permno,xgb_c_models[[1]]$date)
keep <- match( data$permno_date, xgb_c_permno_date)

for( i in 1:length(xgb_c_models)){
  xgb_c_models[[i]] <- xgb_c_models[[i]][ keep , ] # keep only out of sample to save memory
}

# Then, GLM
glm <- readRDS('../forecasts/GLM.RDS')
glm_permno_date <- paste0(glm$permno, glm$date)
keep <- match( data$permno_date, glm_permno_date)
glm <- glm[keep,]
data$glm_pg90 <- glm$p10
data$glm_pleq10 <- glm$p1

# MLC: 
probs.fc <- as.matrix(glm[, paste0('p',1:10)])
ptfs.fc <- maxCol_row(probs.fc)
data$glm_td_top <- ptfs.fc==10
data$glm_td_bot <- ptfs.fc==1

## Prepare for tuning
idx <- sort(unique(data$date)) # unique date
idx.yr <- idx[seq(1,length(idx),12)] # define model update date
idx.oos <- idx[ idx >= "1987-01-01"] # out of sample months
oos.seq <- idx.oos[seq(1,length(idx.oos),12)] # find the out of sample months to update forecasts

for( t in 1:length(oos.seq) ){
  
  idx.start = idx.yr[t]
  idx.end = oos.seq[t]
  if (t < length(oos.seq)){
    idx.tmp <- data$date >= oos.seq[t] & data$date < oos.seq[t+1]
  } else {
    idx.tmp <- data$date >= oos.seq[t] 
  }
  data.val = data[data$date >= idx.start & data$date < idx.end, ]
  # First step is to select a model.
  loss <- rep(NA,length(xgb_c_models))
  for (i in 1:length(xgb_c_models)){
    target = data.val$ret_dec == 9
    pred = xgb_c_models[[i]][xgb_c_models[[i]]$date >= idx.start & xgb_c_models[[i]]$date < idx.end ,  'p10' ]
    loss[i] = binLogLoss(target, pred)
    target = data.val$ret_dec == 0 
    pred = xgb_c_models[[i]][xgb_c_models[[i]]$date >= idx.start & xgb_c_models[[i]]$date < idx.end ,  'p1' ]
    loss[i] = loss[i] + binLogLoss(target, pred)
  }
  xgb_c_opt <- xgb_c_models[[which.min(loss)]]
  
  if( t == 1){
    idx.tmp <- data$date < oos.seq[t+1] # Use the selected model on the validation sample, too.
  } else if (t < length(oos.seq)){
    idx.tmp <- data$date >= oos.seq[t] & data$date < oos.seq[t+1]
  } else {
    idx.tmp <- data$date >= oos.seq[t] 
  }   
  
  data$xgb_c_pg90[idx.tmp]  <- xgb_c_opt$p10[idx.tmp]
  data$xgb_c_pleq10[idx.tmp]  <- xgb_c_opt$p1[idx.tmp]
  
  # Most likely class
  probs.fc <- as.matrix(xgb_c_opt[idx.tmp, paste0('p',1:10)])
  ptfs.fc <- maxCol_row(probs.fc)
  data$xgb_c_td_top[idx.tmp] <- ptfs.fc==10
  data$xgb_c_td_bot[idx.tmp] <- ptfs.fc==1
  
  cat(sprintf('%s \n',t))
}

saveRDS(data,'ClassificationModels.RDS')


