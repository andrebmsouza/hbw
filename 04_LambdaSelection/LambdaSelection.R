rm(list=ls())
library(dplyr)
library(lubridate)
library(Rcpp)
source('../utils.R')
sourceCpp('findOptimalPtfs.cpp')

# Load Fama-French data for risk-free rate
ff <- read.csv('../data/ff5f_mom_str_ltr.csv')
ff$rf = ff$rf/100
ff <- ff[ ,c("X",'rf')]
colnames(ff) <- c('yyyymm','rf')
ff <- ff[ff$yyyymm >= 197401 & ff$yyyymm <= 202112,]

# Lambda sequence
lam.seq <- seq(0,1.5,0.01)

# Load portfolio data.
ols_ptfs = readRDS('../03_PortfolioConstruction/OLS_Portfolios.RDS')
glm_ptfs = readRDS('../03_PortfolioConstruction/GLM_Portfolios.RDS')
xgb_r_ptfs = readRDS('../03_PortfolioConstruction/XGBR_Portfolios.RDS')
xgb_c_ptfs = readRDS('../03_PortfolioConstruction/XGBC_Portfolios.RDS')

# Select lambda for each time. We start from the first out-of-sample period.
sr.fun <- function(x,rf) sqrt(12)*(mean(x-rf))/sd(x)
start_idx = which(ff$yyyymm==198701)

lambda_df <- cbind.data.frame('yyyymm'=ff$yyyymm,
                              'ols_lam_w'= NA,'ols_lam_l' = NA,
                              'xgb_r_lam_w'= NA,'xgb_r_lam_l'= NA,
                              'glm_lam_w'= NA,'glm_lam_l' = NA,
                              'xgb_c_lam_w'= NA,'xgb_c_lam_l'= NA
)


for( t in start_idx:nrow(ff) ){
  # OLS
  ols_srs_t <- findOptimalPtfs(ols_ptfs[1:(t-1),,],ff$rf[1:(t-1)]) # We want best performing up to last period
  opt_lams = which(ols_srs_t == max(ols_srs_t,na.rm=TRUE),arr.ind = TRUE)
  if (nrow(opt_lams)>1){
    opt_lams = opt_lams[1,]
  }
  lambda_df$ols_lam_w[t] = lam.seq[opt_lams[1]]
  lambda_df$ols_lam_l[t] = lam.seq[opt_lams[2]]
  
  # GLM
  glm_srs_t <- findOptimalPtfs(glm_ptfs[1:(t-1),,],ff$rf[1:(t-1)]) # We want best performing up to last period
  opt_lams = which(glm_srs_t == max(glm_srs_t,na.rm=TRUE),arr.ind = TRUE)
  if (nrow(opt_lams)>1){
    opt_lams = opt_lams[1,]
  }
  lambda_df$glm_lam_w[t] = lam.seq[opt_lams[1]]
  lambda_df$glm_lam_l[t] = lam.seq[opt_lams[2]]
  
  # XGB(R)
  xgb_r_srs_t <- findOptimalPtfs(xgb_r_ptfs[1:(t-1),,],ff$rf[1:(t-1)]) # We want best performing up to last period
  opt_lams = which(xgb_r_srs_t == max(xgb_r_srs_t,na.rm=TRUE),arr.ind = TRUE)
  if (nrow(opt_lams)>1){
    opt_lams = opt_lams[1,]
  }
  lambda_df$xgb_r_lam_w[t] = lam.seq[opt_lams[1]]
  lambda_df$xgb_r_lam_l[t] = lam.seq[opt_lams[2]]
  
  # XGB(C)
  xgb_c_srs_t <- findOptimalPtfs(xgb_c_ptfs[1:(t-1),,],ff$rf[1:(t-1)]) # We want best performing up to last period
  opt_lams = which(xgb_c_srs_t == max(xgb_c_srs_t),arr.ind = TRUE)
  if (nrow(opt_lams)>1){
    opt_lams = opt_lams[1,]
  }
  lambda_df$xgb_c_lam_w[t] = lam.seq[opt_lams[1]]
  lambda_df$xgb_c_lam_l[t] = lam.seq[opt_lams[2]]

  cat(sprintf('%s\n',t))
}


# Combine with forecasts to create portfolios
data.reg <- readRDS('../02_ModelSelection/RegressionModels.RDS')

data.class <- readRDS('../02_ModelSelection/ClassificationModels.RDS')
class.cols <- colnames(data.class)[7:ncol(data.class)]
class.cols <- c(class.cols,'ret_dec')

if( mean(data.reg$RET==data.class$RET) ){ # check if data is aligned
  ptf.data <- cbind.data.frame(data.reg,data.class[,class.cols])
} else{
  cat("Data not alligned")
}

ptf.data <- merge(ptf.data,lambda_df)

# Final portfolios.
ptf.data <- ptf.data %>% mutate(
  # OLS 
  ols_opt_top = ols_pg90 >=  ifelse( ols_lam_w/(1+ols_lam_w) >= (ols_lam_w-ols_lam_l)/(1+ols_lam_w) + ( (1+ols_lam_l)/(1+ols_lam_w) )*ols_pleq10,
                                     ols_lam_w/(1+ols_lam_w), (ols_lam_w-ols_lam_l)/(1+ols_lam_w) + ( (1+ols_lam_l)/(1+ols_lam_w) )*ols_pleq10),
  ols_opt_bot = ols_pleq10 >=  ifelse( ols_lam_l/(1+ols_lam_l) >= (ols_lam_l-ols_lam_w)/(1+ols_lam_l) + ( (1+ols_lam_w)/(1+ols_lam_l) )*ols_pg90,
                                     ols_lam_l/(1+ols_lam_l), (ols_lam_l-ols_lam_w)/(1+ols_lam_l) + ( (1+ols_lam_w)/(1+ols_lam_l) )*ols_pg90),
  # XGB(R)
  xgb_r_opt_top = xgb_r_pg90 >=  ifelse( xgb_r_lam_w/(1+xgb_r_lam_w) >= (xgb_r_lam_w-xgb_r_lam_l)/(1+xgb_r_lam_w) + ( (1+xgb_r_lam_l)/(1+xgb_r_lam_w) )*xgb_r_pleq10,
                                     xgb_r_lam_w/(1+xgb_r_lam_w), (xgb_r_lam_w-xgb_r_lam_l)/(1+xgb_r_lam_w) + ( (1+xgb_r_lam_l)/(1+xgb_r_lam_w) )*xgb_r_pleq10),
  xgb_r_opt_bot = xgb_r_pleq10 >=  ifelse( xgb_r_lam_l/(1+xgb_r_lam_l) >= (xgb_r_lam_l-xgb_r_lam_w)/(1+xgb_r_lam_l) + ( (1+xgb_r_lam_w)/(1+xgb_r_lam_l) )*xgb_r_pg90,
                                       xgb_r_lam_l/(1+xgb_r_lam_l), (xgb_r_lam_l-xgb_r_lam_w)/(1+xgb_r_lam_l) + ( (1+xgb_r_lam_w)/(1+xgb_r_lam_l) )*xgb_r_pg90),
  
  # GLM
  glm_opt_top = glm_pg90 >=  ifelse( glm_lam_w/(1+glm_lam_w) >= (glm_lam_w-glm_lam_l)/(1+glm_lam_w) + ( (1+glm_lam_l)/(1+glm_lam_w) )*glm_pleq10,
                                     glm_lam_w/(1+glm_lam_w), (glm_lam_w-glm_lam_l)/(1+glm_lam_w) + ( (1+glm_lam_l)/(1+glm_lam_w) )*glm_pleq10),
  glm_opt_bot = glm_pleq10 >=  ifelse( glm_lam_l/(1+glm_lam_l) >= (glm_lam_l-glm_lam_w)/(1+glm_lam_l) + ( (1+glm_lam_w)/(1+glm_lam_l) )*glm_pg90,
                                       glm_lam_l/(1+glm_lam_l), (glm_lam_l-glm_lam_w)/(1+glm_lam_l) + ( (1+glm_lam_w)/(1+glm_lam_l) )*glm_pg90),
  
  # XGB(C)
  xgb_c_opt_top = xgb_c_pg90 >=  ifelse( xgb_c_lam_w/(1+xgb_c_lam_w) >= (xgb_c_lam_w-xgb_c_lam_l)/(1+xgb_c_lam_w) + ( (1+xgb_c_lam_l)/(1+xgb_c_lam_w) )*xgb_c_pleq10,
                                     xgb_c_lam_w/(1+xgb_c_lam_w), (xgb_c_lam_w-xgb_c_lam_l)/(1+xgb_c_lam_w) + ( (1+xgb_c_lam_l)/(1+xgb_c_lam_w) )*xgb_c_pleq10),
  xgb_c_opt_bot = xgb_c_pleq10 >=  ifelse( xgb_c_lam_l/(1+xgb_c_lam_l) >= (xgb_c_lam_l-xgb_c_lam_w)/(1+xgb_c_lam_l) + ( (1+xgb_c_lam_w)/(1+xgb_c_lam_l) )*xgb_c_pg90,
                                       xgb_c_lam_l/(1+xgb_c_lam_l), (xgb_c_lam_l-xgb_c_lam_w)/(1+xgb_c_lam_l) + ( (1+xgb_c_lam_w)/(1+xgb_c_lam_l) )*xgb_c_pg90),
)

saveRDS(ptf.data,'PortfolioData.RDS')
