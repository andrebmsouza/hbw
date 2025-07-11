rm(list=ls())
library(dplyr)
library(lubridate)
library(Rcpp)
library(lattice)
library(viridisLite)
library(RColorBrewer)
library(gridExtra)

source('../utils.R')
sourceCpp('findOptimalPtfs.cpp')

# Load Fama-French data for risk-free rate
ff <- read.csv('../data/ff5f_mom_str_ltr.csv')
ff$rf = ff$rf/100
ff <- ff[ ,c("X",'rf')]
colnames(ff) <- c('yyyymm','rf')
ff <- ff[ff$yyyymm >= 197401 & ff$yyyymm <= 202112,]

start.idx = sum(ff$yyyymm<=198701)
end.idx = nrow(ff)
# Lambda sequence
lam.seq <- seq(0,1.5,0.01)

# Load portfolio data.
ols_ptfs = readRDS('../03_PortfolioConstruction/OLS_Portfolios.RDS')[start.idx:end.idx,,]
glm_ptfs = readRDS('../03_PortfolioConstruction/GLM_Portfolios.RDS')[start.idx:end.idx,,]
xgb_r_ptfs = readRDS('../03_PortfolioConstruction/XGBR_Portfolios.RDS')[start.idx:end.idx,,]
xgb_c_ptfs = readRDS('../03_PortfolioConstruction/XGBC_Portfolios.RDS')[start.idx:end.idx,,]

ff <- ff[start.idx:end.idx,]
# Select lambda for each time. We start from the first out-of-sample period.
sr.fun <- function(x,rf) sqrt(12)*(mean(x-rf))/sd(x)


# OLS
ols_srs_t <- findOptimalPtfs(ols_ptfs, ff$rf) # We want best performing up to last period
opt_lams = which(ols_srs_t == max(ols_srs_t,na.rm=TRUE),arr.ind = TRUE)
if (nrow(opt_lams)>1){
  opt_lams = opt_lams[1,]
}
ols_lam_w = lam.seq[opt_lams[1]]
ols_lam_l = lam.seq[opt_lams[2]]
  
ols_srs_t[opt_lams]

# GLM
glm_srs_t <- findOptimalPtfs(glm_ptfs,ff$rf) # We want best performing up to last period
opt_lams = which(glm_srs_t == max(glm_srs_t,na.rm=TRUE),arr.ind = TRUE)
if (nrow(opt_lams)>1){
  opt_lams = opt_lams[1,]
}
glm_lam_w = lam.seq[opt_lams[1]]
glm_lam_l = lam.seq[opt_lams[2]]
  
# XGB(R)
xgb_r_srs_t <- findOptimalPtfs(xgb_r_ptfs, ff$rf) # We want best performing up to last period
opt_lams = which(xgb_r_srs_t == max(xgb_r_srs_t,na.rm=TRUE),arr.ind = TRUE)
if (nrow(opt_lams)>1){
  opt_lams = opt_lams[1,]
}
xgb_r_lam_w = lam.seq[opt_lams[1]]
xgb_r_lam_l = lam.seq[opt_lams[2]]
  
# XGB(C)
xgb_c_srs_t <- findOptimalPtfs(xgb_c_ptfs, ff$rf) # We want best performing up to last period
opt_lams = which(xgb_c_srs_t == max(xgb_c_srs_t),arr.ind = TRUE)
if (nrow(opt_lams)>1){
  opt_lams = opt_lams[1,]
}
xgb_c_lam_w = lam.seq[opt_lams[1]]
xgb_c_lam_l = lam.seq[opt_lams[2]]
  
# Plots
plots <- list()
## GLM 
colnames(glm_srs_t) <- rownames(glm_srs_t) <- lam.seq
df.glm <- as.data.frame(as.table(t(glm_srs_t)))
df.glm$Freq[!is.finite(df.glm$Freq)] <- min(df.glm$Freq[is.finite(df.glm$Freq)])
names(df.glm) <- c("X", "Y", "Value")
df.glm$X <- as.numeric(as.character(df.glm$X))
df.glm$Y <- as.numeric(as.character(df.glm$Y))
# Plot
plots[[2]] <-  levelplot(Value ~ X * Y, data=df.glm,
                         xlab="Lambda Loser", ylab="Lambda Winner",col.regions = coul,
                         main='GLM',
                         scales=list(x=list(at=seq(0, max(df.glm$X), by=0.05),
                                            rot=45),
                                     y=list(at=seq(0, max(df.glm$Y), by=0.05))),
                         panel=function(...) {
                           panel.levelplot(...)
                           panel.abline(h=glm_lam_w,lty=2)
                           panel.abline(v=glm_lam_l,lty=2)
                         })
## XGBC 
colnames(xgb_c_srs_t) <- rownames(xgb_c_srs_t) <- lam.seq
df.xgb_c <- as.data.frame(as.table(t(xgb_c_srs_t)))
names(df.xgb_c) <- c("X", "Y", "Value")
df.xgb_c$X <- as.numeric(as.character(df.xgb_c$X))
df.xgb_c$Y <- as.numeric(as.character(df.xgb_c$Y))

# Plot
plots[[1]] <- levelplot(Value ~ X * Y, data=df.xgb_c,
                        xlab="Lambda Loser", ylab="Lambda Winner",col.regions = coul,
                        main='XGB(C)',
                        scales=list(x=list(at=seq(0, max(df.xgb_c$X), by=0.05),rot=45,
                                           cex=1.5),
                                    y=list(at=seq(0, max(df.xgb_c$Y), by=0.05),
                                           cex=1.5),
                                    xlab=list(cex=1.5),
                                    ylab= list(cex=1.5)),
                        panel=function(...) {
                          panel.levelplot(...)
                          panel.abline(h=xgb_c_lam_w,lty=2)
                          panel.abline(v=xgb_c_lam_l,lty=2)
                        })

## XGBR 
colnames(xgb_r_srs_t) <- rownames(xgb_r_srs_t) <- lam.seq
df.xgb_r <- as.data.frame(as.table(t(xgb_r_srs_t)))
names(df.xgb_r) <- c("X", "Y", "Value")
df.xgb_r$X <- as.numeric(as.character(df.xgb_r$X))
df.xgb_r$Y <- as.numeric(as.character(df.xgb_r$Y))
# Plot
plots[[3]] <- levelplot(Value ~ X * Y, data=df.xgb_r,
                        xlab="Lambda Loser", ylab="Lambda Winner",col.regions = coul,
                        main='XGB(R)',
                        scales=list(x=list(at=seq(0, max(df.xgb_r$X), by=0.05),rot=45),
                                    y=list(at=seq(0, max(df.xgb_r$Y), by=0.05))),
                        panel=function(...) {
                          panel.levelplot(...)
                          panel.abline(h=1, col="red", lwd=1)
                          panel.abline(v=1, col="red", lwd=1)
                          panel.abline(h=xgb_r_lam_w,lty=2)
                          panel.abline(v=xgb_r_lam_l,lty=2)
                        })

## OLS 
colnames(ols_srs_t) <- rownames(ols_srs_t) <- lam.seq
df.ols <- as.data.frame(as.table(t(ols_srs_t)))
names(df.ols) <- c("X", "Y", "Value")
df.ols$X <- as.numeric(as.character(df.ols$X))
df.ols$Y <- as.numeric(as.character(df.ols$Y))
# Plot
plots[[4]] <- levelplot(Value ~ X * Y, data=df.ols,
                        xlab="Lambda Loser", ylab="Lambda Winner",col.regions = coul,
                        main='OLS',
                        scales=list(x=list(at=seq(0, max(df.ols$X), by=0.05),rot=45),
                                    y=list(at=seq(0, max(df.ols$Y), by=0.05))),
                        panel=function(...) {
                          panel.levelplot(...)
                          panel.abline(h=1, col="red", lwd=1)
                          panel.abline(v=1, col="red", lwd=1)
                          panel.abline(h=ols_lam_w,lty=2)
                          panel.abline(v=ols_lam_l,lty=2)
                        })




grid.arrange(grobs=plots, ncol=2)


glm_srs_t[is.na(glm_srs_t)] <- 0
xgb_c_srs_t[is.na(xgb_c_srs_t)] <- 0
xgb_r_srs_t[is.na(xgb_r_srs_t)] <- 0

write.table(unname(ols_srs_t),file = '../08_Figures/ols_lambda_heatmap.dat',row.names = FALSE,col.names = FALSE)
write.table(unname(xgb_c_srs_t),file = '../08_Figures/xgb_c_lambda_heatmap.dat',row.names = FALSE,col.names = FALSE)
write.table(unname(xgb_r_srs_t),file = '../08_Figures/xgb_r_lambda_heatmap.dat',row.names = FALSE,col.names = FALSE)
write.table(unname(glm_srs_t),file = '../08_Figures/glm_lambda_heatmap.dat',row.names = FALSE,col.names = FALSE)



coul <- viridis(20000)
rgb_vals <- t(col2rgb(coul)) / 255

# Create Gnuplot palette definition
gp_palette <- paste0("(", paste(
  sprintf("%d \"#%02X%02X%02X\"", 
          0:(length(coul)-1), 
          rgb_vals[,1]*255, 
          rgb_vals[,2]*255, 
          rgb_vals[,3]*255), 
  collapse = ", "), ")")

writeLines(paste("set palette defined", gp_palette), "../08_Figures/viridis_palette.gp")

# Export colorbox
cb = matrix(rep(seq(0,3.75,length.out=(nrow(ols_srs_t))),each=10),ncol = 10,byrow = TRUE)
write.table(unname(cb),file = '../08_Figures/colorbox.dat',row.names = FALSE,col.names = FALSE)


