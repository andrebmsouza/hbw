rm(list=ls())

EWRETS <- readRDS('../06_PortfolioAnalysis/Returns_EW.RDS')
VWRETS <- readRDS('../06_PortfolioAnalysis/Returns_VW.RDS')
colnames(EWRETS)[1] <- colnames(VWRETS)[1] <-  'dates'
EWRETS.QS <- readRDS('../06_PortfolioAnalysis/Returns_EW_TxCosts_QS.RDS')
EWRETS.LW <- readRDS('../06_PortfolioAnalysis/Returns_EW_TxCosts_LW.RDS')
EWRETS.NTX <- readRDS('../06_PortfolioAnalysis/Returns_EW_TxCosts_NoTx.RDS')
VWRETS.QS <- readRDS('../06_PortfolioAnalysis/Returns_VW_TxCosts_QS.RDS')
VWRETS.LW <- readRDS('../06_PortfolioAnalysis/Returns_VW_TxCosts_LW.RDS')
VWRETS.NTX <- readRDS('../06_PortfolioAnalysis/Returns_VW_TxCosts_NoTx.RDS')

# OLS 
tmp.opt = log(cumprod(1+EWRETS$ols_opt_hml))
tmp.td  = log(cumprod(1+EWRETS$ols_td_hml))
tmp.opt.vw = log(cumprod(1+VWRETS$ols_opt_hml))
tmp.td.vw  = log(cumprod(1+VWRETS$ols_td_hml))
df.plot <- cbind.data.frame('dates'=EWRETS$dates, 'ols_opt' = tmp.opt, 'ols_td' = tmp.td,
                            'ols_opt_vw' = tmp.opt.vw, 'ols_td_vw'= tmp.td.vw)
write.csv(df.plot,'CumulativeReturns_OLS.csv',row.names = FALSE)

# XGBC
tmp.opt = log(cumprod(1+EWRETS$xgb_c_opt_hml))
tmp.td  = log(cumprod(1+EWRETS$xgb_c_td_hml))
tmp.opt.vw = log(cumprod(1+VWRETS$xgb_c_opt_hml))
tmp.td.vw  = log(cumprod(1+VWRETS$xgb_c_td_hml))
df.plot <- cbind.data.frame('dates'=EWRETS$dates, 'xgbc_opt' = tmp.opt, 'xgc_td' = tmp.td,
                            'xgbc_opt_vw' = tmp.opt.vw, 'xgbc_td_vw'= tmp.td.vw)
write.csv(df.plot,'CumulativeReturns.csv',row.names = FALSE)


# XGBC-QS
tmp.opt = log(cumprod(1+EWRETS.QS$xgb_c_opt))
tmp.td  = log(cumprod(1+EWRETS.QS$xgb_c_td))
tmp.opt.vw = log(cumprod(1+VWRETS.QS$xgb_c_opt))
tmp.td.vw  = log(cumprod(1+VWRETS.QS$xgb_c_td))
mkt.tmp  = log(cumprod(1+EWRETS.QS$MKT))

df.plot <- cbind.data.frame('dates'=EWRETS.QS$dates, 'xgbc_opt' = tmp.opt, 'xgc_td' = tmp.td,
                            'xgbc_opt_vw' = tmp.opt.vw, 'xgbc_td_vw'= tmp.td.vw)
write.csv(df.plot,'CumulativeReturns_TxCosts.csv',row.names = FALSE)


