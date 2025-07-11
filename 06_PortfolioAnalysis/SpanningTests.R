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

## No transaction costs
EWRETS <- readRDS('../05_PortfolioAnalysis/Returns_EW.RDS')
VWRETS <- readRDS('../05_PortfolioAnalysis/Returns_VW.RDS')
colnames(EWRETS)[1] <- colnames(VWRETS)[1] <-  'dates'

EWRETS$X <- paste0(year(EWRETS$dates),ifelse(nchar(month(EWRETS$dates))==2,
                        month(EWRETS$dates),paste0('0',month(EWRETS$dates))))
VWRETS$X <- paste0(year(VWRETS$dates),ifelse(nchar(month(VWRETS$dates))==2,
                                                  month(VWRETS$dates),paste0('0',month(VWRETS$dates))))
EWRETS <- merge(EWRETS,ff,by='X')
VWRETS <- merge(VWRETS,ff,by='X')

## Regressions
factors <- colnames(EWRETS)[colnames(EWRETS) %in% c('excmkt','smb','hml','rmw','cma','mom','str','ltr')]
models <- c('ols_td_hml','ols_opt_hml','glm_td_hml','glm_opt_hml',
            'xgb_r_td_hml','xgb_r_opt_hml','xgb_c_td_hml','xgb_c_opt_hml')
rows <- c('Alpha','excmkt','smb','hml','rmw','cma','mom','str','ltr')
coef.tbl <- star.tbl <- t.tbl <- data.frame(matrix(NA, nrow=length(rows), ncol=length(models)))
colnames(coef.tbl) <- colnames(star.tbl) <- colnames(t.tbl) <- models
rownames(coef.tbl) <- rownames(star.tbl) <- rownames(t.tbl) <- rows

for( model in models ){
  reg.data <- cbind.data.frame( y = EWRETS[[model]], Alpha = 1, EWRETS[,c(factors)] )
  mdl = lm(y ~ 0 + ., data=reg.data)
  nw    <- NeweyWest(mdl)
  betas <- coef(mdl)
  betas[1] <- betas[1]*100
  betas <- round(betas,2)
  test  <- coeftest(mdl,nw)
  stars <- sapply(1:nrow(test), function(w) star(test[w,4]))
  coef.tbl[names(betas),model]   <- sprintf('$%.2f^%s$',betas,stars)
  t.tbl[rownames(test),model]    <- sprintf( '$(%.2f)$', test[,3])
  star.tbl[rownames(test),model] <- sapply(1:nrow(test), function(w) star(test[w,4]))
  # Stats
  coef.tbl['T',model]        <- round( length(mdl$residuals),0)
  coef.tbl['Adj. R2',model]  <- format( round( summary(mdl)$adj.r.squared, 2), nsmall=2)
}


## Create table ##
t1  <-  xtable(coef.tbl)
tstats <- print( xtable(t.tbl), include.rownames = FALSE, sanitize.text.function = identity, booktab=TRUE)
lines  <- print(t1, sanitize.text.function = identity,booktab=TRUE)
lines <- strsplit(lines,'\n')[[1]]
tstats <- strsplit(tstats,'\n')[[1]]
i=0
for( j in 9:17){
  i=i+1 
  lines[j] <- paste( gsub(sprintf('%s',rownames(coef.tbl)[i]), sprintf('\\\\multirow{2}{*}{%s}',rownames(coef.tbl)[i]),lines[j]),
                     '&', tstats[j],'[1em]')
}
lines[17] <- paste(lines[17],'\\midrule')

lines[7] <- "  & \\multicolumn{2}{c}{LIN-R}  & \\multicolumn{2}{c}{LIN-C}  & \\multicolumn{2}{c}{ML-R} & \\multicolumn{2}{c}{ML-C} \\\\"  
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}  ")
lines[7] <- paste(lines[7], " & BM & Opt. & BM & Opt. &  BM & Opt. & BM & Opt. \\\\ ")

lines[20] <- '\\midrule'

lines <- lines[5:which(lines=='\\end{tabular}')]
lines[1]  <- "\\begin{tabular}{ccccccccc}"
lines[2]  <- '\\midrule'
lines[15] <- gsub('Adj. R2','R$^2$',lines[15])

lines[6] <- gsub('excmkt','MKT',lines[6])
lines[7] <- gsub('smb','SMB',lines[7])
lines[8] <- gsub('hml','HML',lines[8])
lines[9] <- gsub('rmw','RMW',lines[9])
lines[10] <- gsub('cma','CMA',lines[10])
lines[11] <- gsub('mom','MOM',lines[11])
lines[12] <- gsub('str','STR',lines[12])
lines[13] <- gsub('ltr','LTR',lines[13])

fileConn <- '../tables/Spanning_Regressions_EW.tex'
write(lines,fileConn)


#### VALUE WEIGHTED ###################################

factors <- colnames(VWRETS)[colnames(VWRETS) %in% c('excmkt','smb','hml','rmw','cma','mom','str','ltr')]
models <- c('ols_td_hml','ols_opt_hml','glm_td_hml','glm_opt_hml',
            'xgb_r_td_hml','xgb_r_opt_hml','xgb_c_td_hml','xgb_c_opt_hml')
rows <- c('Alpha','excmkt','smb','hml','rmw','cma','mom','str','ltr')
coef.tbl <- star.tbl <- t.tbl <- data.frame(matrix(NA, nrow=length(rows), ncol=length(models)))
colnames(coef.tbl) <- colnames(star.tbl) <- colnames(t.tbl) <- models
rownames(coef.tbl) <- rownames(star.tbl) <- rownames(t.tbl) <- rows

for( model in models ){
  reg.data <- cbind.data.frame( y = VWRETS[[model]], Alpha = 1, VWRETS[,c(factors)] )
  mdl = lm(y ~ 0 + ., data=reg.data)
  nw    <- NeweyWest(mdl)
  betas <- coef(mdl)
  betas[1] <- betas[1]*100
  betas <- round(betas,2)
  test  <- coeftest(mdl,nw)
  stars <- sapply(1:nrow(test), function(w) star(test[w,4]))
  coef.tbl[names(betas),model]   <- sprintf('$%.2f^%s$',betas,stars)
  t.tbl[rownames(test),model]    <- sprintf( '$(%.2f)$', test[,3])
  star.tbl[rownames(test),model] <- sapply(1:nrow(test), function(w) star(test[w,4]))
  # Stats
  coef.tbl['T',model]        <- round( length(mdl$residuals),0)
  coef.tbl['Adj. R2',model]  <- format( round( summary(mdl)$adj.r.squared, 2), nsmall=2)
}


## Create table ##
t1  <-  xtable(coef.tbl)
tstats <- print( xtable(t.tbl), include.rownames = FALSE, sanitize.text.function = identity, booktab=TRUE)
lines  <- print(t1, sanitize.text.function = identity,booktab=TRUE)
lines <- strsplit(lines,'\n')[[1]]
tstats <- strsplit(tstats,'\n')[[1]]
i=0
for( j in 9:17){
  i=i+1 
  lines[j] <- paste( gsub(sprintf('%s',rownames(coef.tbl)[i]), sprintf('\\\\multirow{2}{*}{%s}',rownames(coef.tbl)[i]),lines[j]),
                     '&', tstats[j],'[1em]')
}
lines[17] <- paste(lines[17],'\\midrule')

lines[7] <- "  & \\multicolumn{2}{c}{LIN-R}  & \\multicolumn{2}{c}{LIN-C}  & \\multicolumn{2}{c}{ML-R} & \\multicolumn{2}{c}{ML-C} \\\\"  
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}  ")
lines[7] <- paste(lines[7], " & BM & Opt. & BM & Opt. &  BM & Opt. & BM & Opt. \\\\ ")

lines[20] <- '\\midrule'

lines <- lines[5:which(lines=='\\end{tabular}')]
lines[1]  <- "\\begin{tabular}{ccccccccc}"
lines[2]  <- '\\midrule'
lines[15] <- gsub('Adj. R2','R$^2$',lines[15])

lines[6] <- gsub('excmkt','MKT',lines[6])
lines[7] <- gsub('smb','SMB',lines[7])
lines[8] <- gsub('hml','HML',lines[8])
lines[9] <- gsub('rmw','RMW',lines[9])
lines[10] <- gsub('cma','CMA',lines[10])
lines[11] <- gsub('mom','MOM',lines[11])
lines[12] <- gsub('str','STR',lines[12])
lines[13] <- gsub('ltr','LTR',lines[13])

fileConn <- '../tables/Spanning_Regressions_VW.tex'
write(lines,fileConn)


############### TRANSACTION COSTS #############################################
VWRETS <- readRDS('../05_PortfolioAnalysis/Returns_VW_TxCosts_QS.RDS')
VWRETS$X <- paste0(year(VWRETS$dates),ifelse(nchar(month(VWRETS$dates))==2,
                                             month(VWRETS$dates),paste0('0',month(VWRETS$dates))))
VWRETS <- merge(VWRETS,ff,by='X')

## Regressions
factors <- colnames(VWRETS)[colnames(VWRETS) %in% c('excmkt','smb','hml','rmw','cma','mom','str','ltr')]
models <- c('ols_td','ols_opt','glm_td','glm_opt',
            'xgb_r_td','xgb_r_opt','xgb_c_td','xgb_c_opt')
rows <- c('Alpha','excmkt','smb','hml','rmw','cma','mom','str','ltr')
coef.tbl <- star.tbl <- t.tbl <- data.frame(matrix(NA, nrow=length(rows), ncol=length(models)))
colnames(coef.tbl) <- colnames(star.tbl) <- colnames(t.tbl) <- models
rownames(coef.tbl) <- rownames(star.tbl) <- rownames(t.tbl) <- rows

for( model in models ){
  reg.data <- cbind.data.frame( y = VWRETS[[model]], Alpha = 1, VWRETS[,c(factors)] )
  mdl = lm(y ~ 0 + ., data=reg.data)
  nw    <- NeweyWest(mdl)
  betas <- coef(mdl)
  betas[1] <- betas[1]*100
  betas <- round(betas,2)
  test  <- coeftest(mdl,nw)
  stars <- sapply(1:nrow(test), function(w) star(test[w,4]))
  coef.tbl[names(betas),model]   <- sprintf('$%.2f^%s$',betas,stars)
  t.tbl[rownames(test),model]    <- sprintf( '$(%.2f)$', test[,3])
  star.tbl[rownames(test),model] <- sapply(1:nrow(test), function(w) star(test[w,4]))
  # Stats
  coef.tbl['T',model]        <- round( length(mdl$residuals),0)
  coef.tbl['Adj. R2',model]  <- format( round( summary(mdl)$adj.r.squared, 2), nsmall=2)
}


## Create table ##
t1  <-  xtable(coef.tbl)
tstats <- print( xtable(t.tbl), include.rownames = FALSE, sanitize.text.function = identity, booktab=TRUE)
lines  <- print(t1, sanitize.text.function = identity,booktab=TRUE)
lines <- strsplit(lines,'\n')[[1]]
tstats <- strsplit(tstats,'\n')[[1]]
i=0
for( j in 9:17){
  i=i+1 
  lines[j] <- paste( gsub(sprintf('%s',rownames(coef.tbl)[i]), sprintf('\\\\multirow{2}{*}{%s}',rownames(coef.tbl)[i]),lines[j]),
                     '&', tstats[j],'[1em]')
}
lines[17] <- paste(lines[17],'\\midrule')

lines[7] <- "  & \\multicolumn{2}{c}{LIN-R}  & \\multicolumn{2}{c}{LIN-C}  & \\multicolumn{2}{c}{ML-R} & \\multicolumn{2}{c}{ML-C} \\\\"  
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}  ")
lines[7] <- paste(lines[7], " & BM & Opt. & BM & Opt. &  BM & Opt. & BM & Opt. \\\\ ")

lines[20] <- '\\midrule'

lines <- lines[5:which(lines=='\\end{tabular}')]
lines[1]  <- "\\begin{tabular}{ccccccccc}"
lines[2]  <- '\\midrule'
lines[15] <- gsub('Adj. R2','R$^2$',lines[15])

lines[6] <- gsub('excmkt','MKT',lines[6])
lines[7] <- gsub('smb','SMB',lines[7])
lines[8] <- gsub('hml','HML',lines[8])
lines[9] <- gsub('rmw','RMW',lines[9])
lines[10] <- gsub('cma','CMA',lines[10])
lines[11] <- gsub('mom','MOM',lines[11])
lines[12] <- gsub('str','STR',lines[12])
lines[13] <- gsub('ltr','LTR',lines[13])

fileConn <- '../tables/Spanning_Regressions_VW_TXCosts.tex'
write(lines,fileConn)


########### TRANSACTION COSTS: EW ##################
VWRETS <- readRDS('../05_PortfolioAnalysis/Returns_EW_TxCosts_QS.RDS')
VWRETS$X <- paste0(year(VWRETS$dates),ifelse(nchar(month(VWRETS$dates))==2,
                                             month(VWRETS$dates),paste0('0',month(VWRETS$dates))))
VWRETS <- merge(VWRETS,ff,by='X')

## Regressions
factors <- colnames(VWRETS)[colnames(VWRETS) %in% c('excmkt','smb','hml','rmw','cma','mom','str','ltr')]
models <- c('ols_td','ols_opt','glm_td','glm_opt',
            'xgb_r_td','xgb_r_opt','xgb_c_td','xgb_c_opt')
rows <- c('Alpha','excmkt','smb','hml','rmw','cma','mom','str','ltr')
coef.tbl <- star.tbl <- t.tbl <- data.frame(matrix(NA, nrow=length(rows), ncol=length(models)))
colnames(coef.tbl) <- colnames(star.tbl) <- colnames(t.tbl) <- models
rownames(coef.tbl) <- rownames(star.tbl) <- rownames(t.tbl) <- rows

for( model in models ){
  reg.data <- cbind.data.frame( y = VWRETS[[model]], Alpha = 1, VWRETS[,c(factors)] )
  mdl = lm(y ~ 0 + ., data=reg.data)
  nw    <- NeweyWest(mdl)
  betas <- coef(mdl)
  betas[1] <- betas[1]*100
  betas <- round(betas,2)
  test  <- coeftest(mdl,nw)
  stars <- sapply(1:nrow(test), function(w) star(test[w,4]))
  coef.tbl[names(betas),model]   <- sprintf('$%.2f^%s$',betas,stars)
  t.tbl[rownames(test),model]    <- sprintf( '$(%.2f)$', test[,3])
  star.tbl[rownames(test),model] <- sapply(1:nrow(test), function(w) star(test[w,4]))
  # Stats
  coef.tbl['T',model]        <- round( length(mdl$residuals),0)
  coef.tbl['Adj. R2',model]  <- format( round( summary(mdl)$adj.r.squared, 2), nsmall=2)
}


## Create table ##
t1  <-  xtable(coef.tbl)
tstats <- print( xtable(t.tbl), include.rownames = FALSE, sanitize.text.function = identity, booktab=TRUE)
lines  <- print(t1, sanitize.text.function = identity,booktab=TRUE)
lines <- strsplit(lines,'\n')[[1]]
tstats <- strsplit(tstats,'\n')[[1]]
i=0
for( j in 9:17){
  i=i+1 
  lines[j] <- paste( gsub(sprintf('%s',rownames(coef.tbl)[i]), sprintf('\\\\multirow{2}{*}{%s}',rownames(coef.tbl)[i]),lines[j]),
                     '&', tstats[j],'[1em]')
}
lines[17] <- paste(lines[17],'\\midrule')

lines[7] <- "  & \\multicolumn{2}{c}{LIN-R}  & \\multicolumn{2}{c}{LIN-C}  & \\multicolumn{2}{c}{ML-R} & \\multicolumn{2}{c}{ML-C} \\\\"  
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}  ")
lines[7] <- paste(lines[7], " & BM & Opt. & BM & Opt. &  BM & Opt. & BM & Opt. \\\\ ")

lines[20] <- '\\midrule'

lines <- lines[5:which(lines=='\\end{tabular}')]
lines[1]  <- "\\begin{tabular}{ccccccccc}"
lines[2]  <- '\\midrule'
lines[15] <- gsub('Adj. R2','R$^2$',lines[15])

lines[6] <- gsub('excmkt','MKT',lines[6])
lines[7] <- gsub('smb','SMB',lines[7])
lines[8] <- gsub('hml','HML',lines[8])
lines[9] <- gsub('rmw','RMW',lines[9])
lines[10] <- gsub('cma','CMA',lines[10])
lines[11] <- gsub('mom','MOM',lines[11])
lines[12] <- gsub('str','STR',lines[12])
lines[13] <- gsub('ltr','LTR',lines[13])

fileConn <- '../tables/Spanning_Regressions_EW_TXCosts.tex'
write(lines,fileConn)
