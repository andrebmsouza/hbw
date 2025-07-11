rm(list=ls())
library(dplyr)
library(lubridate)
library(xgboost)
library(glmnet)
library(furrr)

data <- readRDS('../data/prediction_dataset.RDS')
options(future.globals.maxSize = 4000*1024^2)
plan(multisession, workers = 12)

# predictors
not_chars <- c('RET','permno','date','ret_dec')
predictors <- colnames(data)[-which(colnames(data) %in% not_chars)]
nclass = length(unique(data$ret_dec))

# organize
idx <- sort(unique(data$date))
idx.oos <- idx[ idx >= "1974-01-01"]
oos.seq <- idx.oos[seq(1,length(idx.oos),12)]
which.x <- which(colnames(data) %in% predictors)

# parallelizing
olsPredict <- function(xtrain, ytrain, data.test){
  which.x <- which(colnames(data) %in% predictors)
  xtest <- data.test[,which.x]
  df <- cbind.data.frame(xtrain, 'y'=ytrain)
  ols <- lm(y ~ .,data=df)
  ols.fc <- predict(ols, xtest)
  forecasts <- data.frame(matrix(NA,nrow=nrow(data.test),ncol=2))
  colnames(forecasts) <- c('date','permno')
  forecasts$date <- data.test$date
  forecasts$permno <- data.test$permno
  forecasts$ols <- ols.fc
  forecasts
}
glmPredict <- function(xtrain, ytrain, data.test){
  which.x <- which(colnames(data.test) %in% predictors)
  xtest <- as.matrix(data.test[,which.x])
  df <- cbind.data.frame(xtrain, 'y'=ytrain)
  glm <- glmnet(x=as.matrix(xtrain), y = ytrain, family = 'multinomial',lambda = 0)
  ols.fc <- predict(glm, xtest,type='response')
  forecasts <- data.frame(matrix(NA,nrow=nrow(data.test),ncol=2+nclass))
  colnames(forecasts) <- c('date','permno',paste0('p',1:nclass))
  forecasts$date <- data.test$date
  forecasts$permno <- data.test$permno
  forecasts[,3:ncol(forecasts)] <- ols.fc
  forecasts
}

# in parallel
Forecasts  <- future_map_dfr(1:length(oos.seq),
                             function(t) glmPredict( as.matrix(data[data$date < oos.seq[t] , which.x  ]) ,
                                                     data[data$date < oos.seq[t] , ]$ret_dec,
                                                     if (t < length(oos.seq)) data[data$date >= oos.seq[t] & data$date < oos.seq[t+1], ] else data[data$date >= oos.seq[t] , ]
                             ), .progress = TRUE)

saveRDS(Forecasts,'../forecasts/GLM.RDS')

# in parallel
Forecasts  <- future_map_dfr(1:length(oos.seq),
                             function(t) olsPredict( as.matrix(data[data$date < oos.seq[t] , which.x  ]) ,
                                                     data[data$date < oos.seq[t] , ]$RET,
                                                     if (t < length(oos.seq)) data[data$date >= oos.seq[t] & data$date < oos.seq[t+1], ] else data[data$date >= oos.seq[t] , ] 
                             ), .progress = TRUE)

saveRDS(Forecasts,'../forecasts/OLS.RDS')

