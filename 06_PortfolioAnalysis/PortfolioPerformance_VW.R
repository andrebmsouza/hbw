rm(list=ls())
library(dplyr)
library(lubridate)
library(xtable)
library(lmtest)
library(sandwich)
source('../utils.R')

# Load ff
ff <- read.csv('../data/ff5f_mom_str_ltr.csv')
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

# Load portfolio data.
data <- readRDS('../04_LambdaSelection/PortfolioData.RDS')
# Market caps
data.mcap <- readRDS('../data/prediction_dataset.RDS')
mcap.pd <- paste0(data.mcap$permno,data.mcap$date)
data$mcap <- data.mcap$mcap[match(data$permno_date,mcap.pd)]

# Construct final portfolios.
mutate_vars_long = colnames(data)[c( grep("td_top", colnames(data)) ,
                                     grep("opt_top", colnames(data)))]
mutate_vars_short = colnames(data)[c( grep("td_bot", colnames(data)) ,
                                      grep("opt_bot", colnames(data)))]

data <- data %>% filter(date>='1987-01-01')
ptfs_long <- data %>% group_by(date) %>% 
  summarise_at(mutate_vars_long,
               list(~ weighted.mean(RET[.x],w=mcap[.x],na.rm=TRUE)))
ptfs_short <- data %>% group_by(date) %>% 
  summarise_at(mutate_vars_short,
               list(~ mean(RET[.x], w=mcap[.x],na.rm=TRUE)))


ptfs_long$rf  <- ff$rf
ptfs_short$rf <- ff$rf
mutate_vars <- c(mutate_vars_long, mutate_vars_short)

# Equal weights
EXCRETS_LONG <- as.matrix(ptfs_long[,mutate_vars_long]-ptfs_long$rf)
EXCRETS_SHORT <- as.matrix(ptfs_short[,mutate_vars_short]-ptfs_short$rf)
EXCRETS_LONG[is.na(EXCRETS_LONG)] <- 0
EXCRETS_SHORT[is.na(EXCRETS_SHORT)] <- 0
EXCRETS_HML <- (EXCRETS_LONG-EXCRETS_SHORT)

RF <- ptfs_long$rf
# Table HML
RETS <- colMeans(EXCRETS_HML)*100
SDS <- sapply(1:ncol(EXCRETS_HML),function(w) sd(100*EXCRETS_HML[,w]))
DVOL <- sapply(1:ncol(EXCRETS_HML),function(w) sd(100*EXCRETS_HML[EXCRETS_HML[,w]<0,w]))
SKEW <- sapply(1:ncol(EXCRETS_HML),function(w) moments::skewness(EXCRETS_HML[,w]))
SR <- sqrt(12)*100*colMeans(EXCRETS_HML - RF)/SDS
Sortino <- sqrt(12)*100*colMeans(EXCRETS_HML - RF)/DVOL
QS <- sapply(1:ncol(EXCRETS_HML),function(w) quantile(EXCRETS_HML[,w],na.rm=TRUE))*100
tb.hml <- t(cbind.data.frame(RETS,SDS,DVOL,SKEW,SR,Sortino,t(QS)))
colnames(tb.hml) <- gsub('_top','',colnames(tb.hml))

# Table Long
RETS <- colMeans(EXCRETS_LONG)*100
SDS <- sapply(1:ncol(EXCRETS_LONG),function(w) sd(100*EXCRETS_LONG[,w]))
DVOL <- sapply(1:ncol(EXCRETS_LONG),function(w) sd(100*EXCRETS_LONG[EXCRETS_LONG[,w]<0,w]))
SKEW <- sapply(1:ncol(EXCRETS_LONG),function(w) moments::skewness(EXCRETS_LONG[,w]))
SR <- sqrt(12)*100*colMeans(EXCRETS_LONG - RF)/SDS
Sortino <- sqrt(12)*100*colMeans(EXCRETS_LONG - RF)/DVOL
QS <- sapply(1:ncol(EXCRETS_LONG),function(w) quantile(EXCRETS_LONG[,w],na.rm=TRUE))*100
tb.long <- t(cbind.data.frame(RETS,SDS,DVOL,SKEW,SR,Sortino,t(QS)))
tb.long

# Table Short
RETS <- colMeans(EXCRETS_SHORT)*100
SDS <- sapply(1:ncol(EXCRETS_SHORT),function(w) sd(100*EXCRETS_SHORT[,w]))
DVOL <- sapply(1:ncol(EXCRETS_SHORT),function(w) sd(100*EXCRETS_SHORT[EXCRETS_SHORT[,w]<0,w]))
SKEW <- sapply(1:ncol(EXCRETS_SHORT),function(w) moments::skewness(EXCRETS_SHORT[,w]))
SR <- sqrt(12)*100*colMeans(EXCRETS_SHORT - RF)/SDS
Sortino <- sqrt(12)*100*colMeans(EXCRETS_SHORT - RF)/DVOL
QS <- sapply(1:ncol(EXCRETS_SHORT),function(w) quantile(EXCRETS_SHORT[,w],na.rm=TRUE))*100
tb.short <- t(cbind.data.frame(RETS,SDS,DVOL,SKEW,SR,Sortino,t(QS)))
tb.short

nsel_perf <- data %>% filter(date>='1987-01-01') %>% group_by(date) %>% 
  summarise(
    'ols_top' = mean( ols_opt_top==1,na.rm = TRUE),
    'ols_bot' = mean( ols_opt_bot==1,na.rm = TRUE),
    'glm_top' = mean( glm_opt_top==1,na.rm = TRUE),
    'glm_bot' = mean( glm_opt_bot==1,na.rm = TRUE),
    'xgb_r_top' = mean( xgb_r_opt_top==1,na.rm = TRUE),
    'xgb_r_bot' = mean( xgb_r_opt_bot==1,na.rm = TRUE),
    'xgb_c_top' = mean( xgb_c_opt_top==1,na.rm = TRUE),
    'xgb_c_bot' = mean( xgb_c_opt_bot==1,na.rm = TRUE),
  ) %>% mutate(across(c(ols_top:xgb_c_bot), ~ifelse(is.na(.x),0,.x))) %>% summarise(across(ols_top:xgb_c_bot, ~ mean(.x)))

nsel_td <- data %>% filter(date>='1987-01-01') %>% group_by(date) %>% 
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
tb.long <- rbind(tb.long,
                 'NSEL' = 100*c(nsel_td$ols_top, nsel_td$xgb_r_top, nsel_td$glm_top, nsel_td$xgb_c_top,
                                nsel_perf$ols_top, nsel_perf$xgb_r_top, nsel_perf$glm_top, nsel_perf$xgb_c_top)
                 )

tb.short <- rbind(tb.short,
                  'NSEL' = 100*c(nsel_td$ols_bot, nsel_td$xgb_r_bot, nsel_td$glm_bot, nsel_td$xgb_c_bot,
                                 nsel_perf$ols_bot, nsel_perf$xgb_r_bot, nsel_perf$glm_bot, nsel_perf$xgb_c_bot)
                 )

tb.hml <- rbind(tb.hml,
                'NSEL' = tb.short['NSEL',] + tb.long['NSEL',])


# Final table
td.tb <- cbind.data.frame( 'LIN-R_LONG' = tb.long[,'ols_td_top'],
                           'LIN-R_SHORT' = tb.short[,'ols_td_bot'],
                           'LIN-R_HML' = tb.hml[,'ols_td'],
                           'LIN-C_LONG' = tb.long[,'glm_td_top'],
                           'LIN-C_SHORT' = tb.short[,'glm_td_bot'],
                           'LIN-C_HML' = tb.hml[,'glm_td'],
                           'XGBR_LONG' = tb.long[,'xgb_r_td_top'],
                           'XGBR_SHORT' = tb.short[,'xgb_r_td_bot'],
                           'XGBR_HML' = tb.hml[,'xgb_r_td'],
                           'XGBC_LONG' = tb.long[,'xgb_c_td_top'],
                           'XGBC_SHORT' = tb.short[,'xgb_c_td_bot'],
                           'XGBC_HML' = tb.hml[,'xgb_c_td']
)
opt.tb <- cbind.data.frame('LIN-R_LONG' = tb.long[,'ols_opt_top'],
                           'LIN-R_SHORT' = tb.short[,'ols_opt_bot'],
                           'LIN-R_HML' = tb.hml[,'ols_opt'],
                           'LIN-C_LONG' = tb.long[,'glm_opt_top'],
                           'LIN-C_SHORT' = tb.short[,'glm_opt_bot'],
                           'LIN-C_HML' = tb.hml[,'glm_opt'],
                           'XGBR_LONG' = tb.long[,'xgb_r_opt_top'],
                           'XGBR_SHORT' = tb.short[,'xgb_r_opt_bot'],
                           'XGBR_HML' = tb.hml[,'xgb_r_opt'],
                           'XGBC_LONG' = tb.long[,'xgb_c_opt_top'],
                           'XGBC_SHORT' = tb.short[,'xgb_c_opt_bot'],
                           'XGBC_HML' = tb.hml[,'xgb_c_opt']
)

colnames(EXCRETS_HML) <- gsub('_top','_hml',colnames(EXCRETS_HML))
RETS <- cbind.data.frame('Date'=ptfs_long$date,EXCRETS_HML,EXCRETS_LONG,EXCRETS_SHORT)
saveRDS(RETS,file = 'Returns_VW.RDS')

# Export Tables
## HML
tb <- tb.hml
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
fileConn <- '../tables/Table_Performance_VW.tex'
write(lines,fileConn)


## Long 
tb <- tb.long
tb <- tb[,c('ols_td_top','ols_opt_top','glm_td_top','glm_opt_top',
            'xgb_r_td_top','xgb_r_opt_top','xgb_c_td_top','xgb_c_opt_top')]
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
fileConn <- '../tables/Table_Performance_VW_Long.tex'
write(lines,fileConn)

## Short 
tb <- tb.short
tb <- tb[,c('ols_td_bot','ols_opt_bot','glm_td_bot','glm_opt_bot',
            'xgb_r_td_bot','xgb_r_opt_bot','xgb_c_td_bot','xgb_c_opt_bot')]
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
fileConn <- '../tables/Table_Performance_VW_Short.tex'
write(lines,fileConn)



