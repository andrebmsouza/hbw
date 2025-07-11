rm(list=ls())
library(dplyr)
library(lubridate)
library(Rcpp)
sourceCpp('ptfFinder.cpp')

# Load the forecasts
data <- readRDS('../02_ModelSelection/RegressionModels.RDS')

# Construct Probabilities
data <- data %>% group_by(date) %>% mutate(
  xgb_r_pg90 = pnorm( (xgb_r - quantile(xgb_r, 0.9,na.rm = TRUE) )/xgb_idio_sd ),
  xgb_r_pleq10 = pnorm( (quantile(xgb_r, 0.1,na.rm = TRUE) - xgb_r )/xgb_idio_sd )
)

# Define grid for lambdas
lam.seq <- seq(0,1.5,0.01)
lam.w.seq <- lam.l.seq <- lam.seq
xgb_r_ptf <-  array(NA,dim = c(length(unique(data$date)),length(lam.w.seq),length(lam.l.seq)))

# Construct portfolios
for (i in 1:length(lam.w.seq)){
  for(j in 1:length(lam.l.seq)){
    xgb_r_ptf[,i,j] <- getPtf2Cpp(data$xgb_r_pg90,data$xgb_r_pleq10,data$date, data$RET,
                                lambda_w = lam.w.seq[i], lambda_l = lam.l.seq[j])
  }
  cat(sprintf(' %s \n', i))
}

saveRDS(xgb_r_ptf,'XGBR_Portfolios.RDS')
