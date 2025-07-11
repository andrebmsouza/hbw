rm(list=ls())
library(dplyr)
library(lubridate)
library(Rcpp)
sourceCpp('ptfFinder.cpp')

# Load the forecasts
data <- readRDS('../02_ModelSelection/RegressionModels.RDS')

# Construct Probabilities
data <- data %>% group_by(date) %>% mutate(
  ols_pg90 = pnorm( (ols - quantile(ols, 0.9,na.rm = TRUE) )/ols_idio_sd ),
  ols_pleq10 = pnorm( (quantile(ols, 0.1,na.rm = TRUE) - ols )/ols_idio_sd )
)

# Define grid for lambdas
lam.seq <- seq(0,1.5,0.01)
lam.w.seq <- lam.l.seq <- lam.seq
ols_ptf <-  array(NA,dim = c(length(unique(data$date)),length(lam.w.seq),length(lam.l.seq)))

# Construct portfolios
for (i in 1:length(lam.w.seq)){
  for(j in 1:length(lam.l.seq)){
    ols_ptf[,i,j] <- getPtf2Cpp(data$ols_pg90,data$ols_pleq10,data$date, data$RET,
                                lambda_w = lam.w.seq[i], lambda_l = lam.l.seq[j])
  }
  cat(sprintf(' %s \n', i))
}

saveRDS(ols_ptf,'OLS_Portfolios.RDS')
