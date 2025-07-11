rm(list=ls())
library(dplyr)
library(lubridate)
library(Rcpp)
source('../utils.R')
sourceCpp('ptfFinder.cpp')

# Load the forecasts
data <- readRDS('../02_ModelSelection/ClassificationModels.RDS')

# Define grid for lambdas
lam.seq <- seq(0,1.5,0.01)
lam.w.seq <- lam.l.seq <- lam.seq
xgb_c_ptf <-  array(NA,dim = c(length(unique(data$date)),length(lam.w.seq),length(lam.l.seq)))

# Construct portfolios
for (i in 1:length(lam.w.seq)){
  for(j in 1:length(lam.l.seq)){
    xgb_c_ptf[,i,j] <- getPtf2Cpp(data$xgb_c_pg90,data$xgb_c_pleq10, data$date, data$RET,
                                  lambda_w = lam.w.seq[i], lambda_l = lam.l.seq[j])
  }
  cat(sprintf(' %s \n', i))
}

saveRDS(xgb_c_ptf,'XGBC_Portfolios.RDS')
