rm(list=ls())
library(dplyr)
library(lubridate)
library(xgboost)

data <- readRDS('../data/prediction_dataset.RDS')
data <- as.data.frame(data)
# predictors
not_chars <- c('RET','permno','date','ret_dec')
predictors <- colnames(data)[-which(colnames(data) %in% not_chars)]
nclass = length(unique(data$ret_dec))

# organize
idx <- sort(unique(data$date))
idx.oos <- idx[ idx >= "1974-01-01"]
oos.seq <- idx.oos[seq(1,length(idx.oos),12)]
elapsed <- rep(NA,length(oos.seq))
which.x <- which(colnames(data) %in% predictors)
gc()

ETA <- c(0.01,0.1,0.25,0.5,0.75,1)
DEPTH <- c(2, 3)

for (d in DEPTH){
  for( e in 1:length(ETA) ){
    forecasts <- data.frame(matrix(NA,nrow=nrow(data),ncol=2+nclass))
    colnames(forecasts) <- c('date','permno',paste0('p',1:nclass))
    forecasts$date <- data$date
    forecasts$permno <- data$permno
    elapsed <- rep(NA,length(oos.seq))
    
    for (t in 1:length(oos.seq)){
      start_time = Sys.time()
      
      xtrain <- as.matrix(data[data$date < oos.seq[t] , which.x ])
      ytrain <- data[data$date < oos.seq[t] , ]$ret_dec
      
      xgb <- xgboost(data=xtrain,label=ytrain,
                     max.depth=d, eta=ETA[e],
                     num_class=nclass,
                     nrounds=100,objective="multi:softprob",
                     verbose=0)
      
      if (t == length(oos.seq)){
        xtest <- as.matrix(data[data$date >= oos.seq[t] , which.x ])
        xgb.fc <- matrix(predict(xgb, xtest,reshape = FALSE), ncol=nclass, byrow=TRUE)
        forecasts[data$date >= oos.seq[t], 3:ncol(forecasts)] <- xgb.fc
        
      } else {
        
        xtest <- as.matrix(data[data$date >= oos.seq[t] & data$date < oos.seq[t+1] , which.x ])
        xgb.fc <- matrix(predict(xgb, xtest,reshape = FALSE), ncol=nclass, byrow=TRUE)
        forecasts[data$date >= oos.seq[t]  & data$date < oos.seq[t+1] , 3:ncol(forecasts)] <- xgb.fc
        
      }
      end_time = Sys.time()
      # print stuff
      elapsed[t] <- end_time-start_time
      cat(sprintf('XGB: %s out of %s done in %.2f. Estimated time to completion is %.2f minutes \n',
                  t,length(oos.seq),elapsed[t],mean(elapsed,na.rm=TRUE)*(length(oos.seq)-t)))
    }
    saveRDS(forecasts,sprintf('../forecasts/XGB_D%s_E%s_N100.RDS', d , ETA[e]))
  } 
  gc()

}

