rm(list=ls())
library(dplyr)
library(lubridate)
library(xtable)
library(lmtest)
library(sandwich)
source('../utils.R')

# Load ff
ff <- read.csv('../../data/ff5f_mom_str_ltr.csv')
ff$dates <- parse_date_time(ff$X,'%Y%m')
ff$rf = ff$rf/100
ff$excmkt = ff$excmkt/100
ff$smb = ff$smb/100
ff$hml = ff$hml/100
ff$rmw = ff$rmw/100
ff$cma = ff$cma/100
ff$mom = ff$mom/100
ff$str = ff$str/100
ff$ltr = ff$ltr/100
ff <- ff[ff$dates >='1987-01-01' & ff$dates < '2022-01-01',]

# Load portfolio data
data <- readRDS('../04_LambdaSelection/PortfolioData.RDS')
# Market caps
data.mcap <- readRDS('../data/prediction_dataset_2025.RDS')
mcap.pd <- paste0(data.mcap$permno,data.mcap$date)
data$mcap <- data.mcap$mcap[match(data$permno_date,mcap.pd)]

tx.costs <- readRDS('../../hbw-replication/TransactionCosts/EstimatedBidAskSpread.RDS')
colnames(tx.costs)[1] <- 'permno_yyyymm'
data$permno_yyyymm <- paste0(data$permno,data$yyyymm)
gc()

tx.costs <- tx.costs %>% ungroup() %>% select(permno_yyyymm,lw_spread,qs_spread) 
data <- merge(data, tx.costs)
data <- data %>% filter(!is.na(qs_spread),
                        !is.na(lw_spread)) # Drop rows for which we dont have all spreads.

# start from 2000
data <- data %>% filter(date>='2000-01-01',!is.na(mcap))

models = c('ols_opt','ols_td','xgb_r_opt','xgb_r_td',
           'glm_opt','glm_td','xgb_c_opt','xgb_c_td')

dates <- sort(unique(data$date))
RETS.LW <- matrix(NA,nrow=length(dates),ncol=length(models))
RETS.QS <- matrix(NA,nrow=length(dates),ncol=length(models))
RETS <- matrix(NA,nrow=length(dates),ncol=length(models))
colnames(RETS.LW) <- colnames(RETS.QS) <- colnames(RETS) <- models

for( mdl in models){
  data.tmp = data
  data.tmp$ptf_weights = 0 
  data.tmp$lead_ptf_weights = 0 
  data.tmp = data.tmp %>%
    filter(!is.na(eval(parse(text=sprintf('%s_top',mdl)))) & 
             !is.na(eval(parse(text=sprintf('%s_bot',mdl)))))  %>% group_by(date) %>%
    mutate(
      ptf_weights = ifelse(eval(parse(text=sprintf('%s_top',mdl))),
                           mcap/sum(mcap[which(eval(parse(text=sprintf('%s_top',mdl))))]),ptf_weights),
      ptf_weights = ifelse(eval(parse(text=sprintf('%s_bot',mdl))),
                           -mcap/sum(mcap[which(eval(parse(text=sprintf('%s_bot',mdl))))]),ptf_weights))
  
  data.tmp = data.tmp %>% group_by(permno) %>%
    mutate(lead_ptf_weights = lead(ptf_weights,default=0,order_by = date)) %>% ungroup()
  
  ret <- ret.lw <- ret.qs <- rep(NA,length(dates))
  
  for( t in 1:length(dates)){
    tmp = data.tmp %>% filter(date==dates[t]) 
    tmp = tmp %>% mutate(
      w0star = ptf_weights*(1 + RET),
      w1 = lead_ptf_weights,
      reb.cost.lw = (lw_spread/2)*abs(w1-w0star),
      reb.cost.qs = (qs_spread/2)*abs(w1-w0star),
      total.reb.cost.lw = sum(reb.cost.lw),
      total.reb.cost.qs = sum(reb.cost.qs),
      ptf.val.lw = sum(w0star) - total.reb.cost.lw,
      ptf.val.qs = sum(w0star) - total.reb.cost.qs,
      ptf.val = sum(w0star)
    )
    ret[t] <- unique(tmp$ptf.val)
    ret.lw[t] <-unique(tmp$ptf.val.lw)
    ret.qs[t] <- unique(tmp$ptf.val.qs)
  }
  RETS[,mdl] <- ret
  RETS.LW[,mdl] <- ret.lw
  RETS.QS[,mdl] <- ret.qs
  
  cat(sprintf('Model %s done', mdl))
}


yyyymm <- unique(data$yyyymm)
RF <- ff$rf[match(yyyymm,ff$X)]
MKT <- ff$excmkt[match(yyyymm,ff$X)] + RF
dates <- ff$dates[match(yyyymm,ff$X)]
RETS <- as.data.frame(RETS)
RETS.LW <- as.data.frame(RETS.LW)
RETS.QS <- as.data.frame(RETS.QS)
RETS[,'MKT'] <- RETS.LW[,'MKT'] <-RETS.QS[,'MKT'] <- MKT

# Table No Tx Cost
AVRETS <- colMeans(RETS)*100
SDS <- sapply(1:ncol(RETS),function(w) sd(100*RETS[,w]))
DVOL <- sapply(1:ncol(RETS),function(w) sd(100*RETS[RETS[,w]<0,w]))
SKEW <- sapply(1:ncol(RETS),function(w) moments::skewness(RETS[,w]))
SR <- sqrt(12)*100*colMeans(RETS - RF)/SDS
Sortino <- sqrt(12)*100*colMeans(RETS - RF)/DVOL
QS <- sapply(1:ncol(RETS),function(w) quantile(RETS[,w],na.rm=TRUE))*100
tb.notx <- t(cbind.data.frame(AVRETS,SDS,DVOL,SKEW,SR,Sortino,t(QS)))

# Table LedoitWolf
AVRETS <- colMeans(RETS.LW)*100
SDS <- sapply(1:ncol(RETS.LW),function(w) sd(100*RETS.LW[,w]))
DVOL <- sapply(1:ncol(RETS.LW),function(w) sd(100*RETS.LW[RETS.LW[,w]<0,w]))
SKEW <- sapply(1:ncol(RETS.LW),function(w) moments::skewness(RETS.LW[,w]))
SR <- sqrt(12)*100*colMeans(RETS.LW - RF)/SDS
Sortino <- sqrt(12)*100*colMeans(RETS.LW - RF)/DVOL
QS <- sapply(1:ncol(RETS.LW),function(w) quantile(RETS.LW[,w],na.rm=TRUE))*100
tb.wolf <- t(cbind.data.frame(AVRETS,SDS,DVOL,SKEW,SR,Sortino,t(QS)))

# Table QS
AVRETS <- colMeans(RETS.QS)*100
SDS <- sapply(1:ncol(RETS.QS),function(w) sd(100*RETS.QS[,w]))
DVOL <- sapply(1:ncol(RETS.QS),function(w) sd(100*RETS.QS[RETS.QS[,w]<0,w]))
SKEW <- sapply(1:ncol(RETS.QS),function(w) moments::skewness(RETS.QS[,w]))
SR <- sqrt(12)*100*colMeans(RETS.QS - RF)/SDS
Sortino <- sqrt(12)*100*colMeans(RETS.QS - RF)/DVOL
QS <- sapply(1:ncol(RETS.QS),function(w) quantile(RETS.QS[,w],na.rm=TRUE))*100
tb.qs <- t(cbind.data.frame(AVRETS,SDS,DVOL,SKEW,SR,Sortino,t(QS)))

### 
nsel_perf <- data %>% group_by(date) %>% 
  summarise('ols_top' = mean( ols_opt_top==1,na.rm = TRUE),
            'ols_bot' = mean( ols_opt_bot==1,na.rm = TRUE),
            'glm_top' = mean( glm_opt_top==1,na.rm = TRUE),
            'glm_bot' = mean( glm_opt_bot==1,na.rm = TRUE),
            'xgb_r_top' = mean( xgb_r_opt_top==1,na.rm = TRUE),
            'xgb_r_bot' = mean( xgb_r_opt_bot==1,na.rm = TRUE),
            'xgb_c_top' = mean( xgb_c_opt_top==1,na.rm = TRUE),
            'xgb_c_bot' = mean( xgb_c_opt_bot==1,na.rm = TRUE),
  ) %>% mutate(across(c(ols_top:xgb_c_bot), ~ifelse(is.na(.x),0,.x))) %>% summarise(across(ols_top:xgb_c_bot, ~ mean(.x)))

nsel_td <- data %>% group_by(date) %>% 
  summarise('ols_top' = mean(ols_td_top==1,na.rm = TRUE),
            'ols_bot' = mean(ols_td_bot==1,na.rm = TRUE),
            'glm_top' = mean(glm_td_top==1,na.rm = TRUE),
            'glm_bot' = mean(glm_td_bot==1,na.rm = TRUE),
            'xgb_r_top' = mean(xgb_r_td_top==1,na.rm = TRUE),
            'xgb_r_bot' = mean(xgb_r_td_bot==1,na.rm = TRUE),
            'xgb_c_top' = mean(xgb_c_td_top==1,na.rm = TRUE),
            'xgb_c_bot' = mean(xgb_c_td_bot==1,na.rm = TRUE),
  ) %>%mutate(across(c(ols_top:xgb_c_bot), ~ifelse(is.na(.x),0,.x))) %>% summarise(across(ols_top:xgb_c_bot, ~ mean(.x)))



# combine new rows
nsel.row <- cbind.data.frame('ols_opt' = nsel_perf$ols_top + nsel_perf$ols_bot,
                             'ols_td' = nsel_td$ols_top + nsel_td$ols_bot,
                             'xgb_r_opt' = nsel_perf$xgb_r_top + nsel_perf$xgb_r_bot,
                             'xgb_r_td' = nsel_td$xgb_r_bot + nsel_td$xgb_r_top,
                             'glm_opt' = nsel_perf$glm_top + nsel_perf$glm_bot,
                             'glm_td' = nsel_td$glm_top + nsel_td$glm_bot,
                             'xgb_c_opt' = nsel_perf$xgb_c_top + nsel_perf$xgb_c_bot,
                             'xgb_c_td' = nsel_td$xgb_c_top + nsel_td$xgb_c_bot
)



# Update Tables:
mkt <- tb.qs[,ncol(tb.qs)]
tb.qs <- rbind.data.frame(tb.qs[,-ncol(tb.qs)],'NSEL'= 100*nsel.row)
tb.wolf <- rbind.data.frame(tb.wolf[,-ncol(tb.wolf)],'NSEL'= 100*nsel.row)
# QS
tb <- tb.qs
tb <- tb[,c('ols_td','ols_opt','glm_td','glm_opt',
            'xgb_r_td','xgb_r_opt','xgb_c_td','xgb_c_opt')]
rownames(tb) <- c('Avg. Exc. Returns', 'Vol.', 'Downside Vol.', 'Skew.', 'Ann. Sharpe Ratio',
                  'Ann. Sortino Ratio', 'Min.','25\\%','Median','75\\%','Max.',
                  '\\% of Selected Stocks')

dig = matrix(2,ncol=ncol(tb)+1,nrow=nrow(tb))
dig[nrow(dig):(nrow(dig)-1),] <- 2
t1     <- xtable(tb, digits = dig)
lines  <- print(t1, sanitize.text.function = identity,booktabs=TRUE,NA.string = '$-$')
lines  <- strsplit(lines,'\n')[[1]]
lines[7] <- "  & \\multicolumn{2}{c}{LIN-R}  & \\multicolumn{2}{c}{LIN-C}  & \\multicolumn{2}{c}{ML-R} & \\multicolumn{2}{c}{ML-C} \\\\"  
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}  ")
lines[7] <- paste(lines[7], " & BM & Opt. & BM & Opt. &  BM & Opt. & BM & Opt. \\\\ ")
lines[8] <- " \\cmidrule(lr){2-2}\\cmidrule(lr){3-3} \\cmidrule(lr){4-4}  \\cmidrule(lr){5-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7} \\cmidrule(lr){8-8} \\cmidrule(lr){9-9} "
lines[5] <- "\\begin{tabular}{ccccccccc}"
lines <- lines[grep('begin{tabular}',lines,fixed = TRUE) : grep('end{tabular}',lines,fixed = TRUE)]
lines[10] <- paste0(lines[10],'[0.75em]')
lines[15] <- paste0(lines[15],'[0.75em]')
fileConn <- '../tables/Table_Performance_VW_TXCosts_QS.tex'
write(lines,fileConn)
# Wolf
tb <- tb.wolf
tb <- tb[,c('ols_td','ols_opt','glm_td','glm_opt',
            'xgb_r_td','xgb_r_opt','xgb_c_td','xgb_c_opt')]
rownames(tb) <- c('Avg. Exc. Returns', 'Vol.', 'Downside Vol.', 'Skew.', 'Ann. Sharpe Ratio',
                  'Ann. Sortino Ratio', 'Min.','25\\%','Median','75\\%','Max.',
                  '\\% of Selected Stocks')

dig = matrix(2,ncol=ncol(tb)+1,nrow=nrow(tb))
dig[nrow(dig):(nrow(dig)-1),] <- 2
t1     <- xtable(tb, digits = dig)
lines  <- print(t1, sanitize.text.function = identity,booktabs=TRUE,NA.string = '$-$')
lines  <- strsplit(lines,'\n')[[1]]
lines[7] <- "  & \\multicolumn{2}{c}{LIN-R}  & \\multicolumn{2}{c}{LIN-C}  & \\multicolumn{2}{c}{ML-R} & \\multicolumn{2}{c}{ML-C} \\\\"  
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}  ")
lines[7] <- paste(lines[7], " & BM & Opt. & BM & Opt. &  BM & Opt. & BM & Opt. \\\\ ")
lines[8] <- " \\cmidrule(lr){2-2}\\cmidrule(lr){3-3} \\cmidrule(lr){4-4}  \\cmidrule(lr){5-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7} \\cmidrule(lr){8-8} \\cmidrule(lr){9-9} "
lines[5] <- "\\begin{tabular}{ccccccccc}"
lines <- lines[grep('begin{tabular}',lines,fixed = TRUE) : grep('end{tabular}',lines,fixed = TRUE)]
lines[10] <- paste0(lines[10],'[0.75em]')
lines[15] <- paste0(lines[15],'[0.75em]')
fileConn <- '../tables/Table_Performance_VW_TXCosts_LW.tex'
write(lines,fileConn)


RETS.QS <- cbind.data.frame(dates,RETS.QS)
saveRDS(RETS.QS,file = 'Returns_VW_TxCosts_QS.RDS')
RETS.LW <- cbind.data.frame(dates,RETS.LW)
saveRDS(RETS.LW,file = 'Returns_VW_TxCosts_LW.RDS')
RETS <- cbind.data.frame(dates,RETS)
saveRDS(RETS,file = 'Returns_VW_TxCosts_NoTx.RDS')

