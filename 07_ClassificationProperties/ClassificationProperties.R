rm(list=ls())
library(dplyr)
library(lubridate)
library(xtable)
library(pROC)

# Load portfolio data.
data <-  readRDS('../04_LambdaSelection/PortfolioData.RDS')
data$top_ret = data$ret_dec==9
data$bot_ret = data$ret_dec==0
data <- data %>% filter(date>='1987-01-01')

roc_ols_top   <- roc(data$top_ret,data$ols_pg90)
roc_xgb_r_top <- roc(data$top_ret,data$xgb_r_pg90)
roc_glm_top   <- roc(data$top_ret,data$glm_pg90)
roc_xgb_c_top <- roc(data$top_ret,data$xgb_c_pg90)

auc_ols_top <- auc(roc_ols_top)
auc_glm_top <- auc(roc_glm_top)
auc_xgb_r_top <- auc(roc_xgb_r_top)
auc_xgb_c_top <- auc(roc_xgb_c_top)

ols_td_top_tpr <- sum(data$top_ret*data$ols_td_top)/sum(data$top_ret*data$ols_td_top + data$top_ret*(data$ols_td_top==0))
ols_td_top_fpr <- sum((data$top_ret==0)*data$ols_td_top)/sum((data$top_ret==0)*data$ols_td_top + (data$top_ret==0)*(data$ols_td_top==0))

xgb_r_td_top_tpr <- sum(data$top_ret*data$xgb_r_td_top)/sum(data$top_ret*data$xgb_r_td_top + data$top_ret*(data$xgb_r_td_top==0))
xgb_r_td_top_fpr <- sum((data$top_ret==0)*data$xgb_r_td_top)/sum((data$top_ret==0)*data$xgb_r_td_top + (data$top_ret==0)*(data$xgb_r_td_top==0))

idx.xgbc = which.min( abs( (1-roc_xgb_c_top$specificities)-xgb_r_td_top_fpr))


idx.glm = which.min( abs( (1-roc_glm_top$specificities)-xgb_r_td_top_fpr))
roc_glm_top$sensitivities[idx.glm]

cbind.data.frame('xgbr' = rbind(xgb_r_td_top_tpr,xgb_r_td_top_fpr),
                 'ols' = rbind(ols_td_top_tpr,ols_td_top_fpr),
                 'xgbc' = rbind(roc_xgb_c_top$sensitivities[idx.xgbc],1-roc_xgb_c_top$specificities[idx.xgbc]),
                 'logi' = rbind(roc_glm_top$sensitivities[idx.glm],1-roc_glm_top$specificities[idx.glm])
                 )


npoints = 10000
sel.idx.xgbc = round(seq(1,length(roc_xgb_c_top$specificities),length.out=npoints))
sel.idx.xgbr = round(seq(1,length(roc_xgb_r_top$specificities),length.out=npoints))
sel.idx.ols = round(seq(1,length(roc_ols_top$specificities),length.out=npoints))
sel.idx.glm = round(seq(1,length(roc_glm_top$specificities),length.out=npoints))

pdf('../08_Figures/ROC_TOP.pdf',height = 8,width = 10)
plot(1-roc_xgb_c_top$specificities[sel.idx.xgbc], roc_xgb_c_top$sensitivities[sel.idx.xgbc],xlim=c(0,1),t='l',col='#00a0b0',lwd=2,lty=1,xlab='False Positive Rate',
     ylab='True Positive Rate',ax=F)
axis(side=1,at=seq(0,1,0.1),labels = seq(0,1,0.1))
axis(side=2,at=seq(0,1,0.1),labels = seq(0,1,0.1))
box()
lines(1-roc_glm_top$specificities[sel.idx.glm], roc_glm_top$sensitivities[sel.idx.glm],col='#cc3340',lwd=2,lty=1)
lines(1-roc_xgb_r_top$specificities[sel.idx.xgbr], roc_xgb_r_top$sensitivities[sel.idx.xgbr],col='#00a0b0',lwd=2,lty=3)
lines(1-roc_ols_top$specificities[sel.idx.ols], roc_ols_top$sensitivities[sel.idx.ols],col='#cc3340',lwd=2,lty=3)
abline(a=0,b=1,lty='dotted')
abline(v=seq(0,1,0.1),h=seq(0,1,0.1),lty=2,col=rgb(0,0,0,1/6))
points(ols_td_top_fpr,ols_td_top_tpr,pch=20,cex=1.5,col='#cc3340')
points(xgb_r_td_top_fpr,xgb_r_td_top_tpr,pch=20,cex=1.5,col='#00a0b0')
grid()
legend('bottomright',c(
  sprintf('XGBC, AUC: %.2f ', auc_xgb_c_top),
  sprintf('Logistic, AUC: %.2f ', auc_glm_top),
  sprintf('XGBR, AUC: %.2f ', auc_xgb_r_top),
  sprintf('Linear, AUC: %.2f ', auc_ols_top),
  'Random Classifier','Top Decile'),col = c('#00a0b0','#cc3340','#00a0b0','#cc3340','black','black'),
  lty=c(1,1,3,3,2,NA),pch=c(NA,NA,NA,NA,NA,20),lwd=3,ncol=3,bg='white')
dev.off()

roc_ols_bot   <- roc(data$bot_ret,data$ols_pleq10)
roc_xgb_r_bot <- roc(data$bot_ret,data$xgb_r_pleq10)
roc_glm_bot   <- roc(data$bot_ret,data$glm_pleq10)
roc_xgb_c_bot <- roc(data$bot_ret,data$xgb_c_pleq10)

auc_ols_bot <- auc(roc_ols_bot)
auc_glm_bot <- auc(roc_glm_bot)
auc_xgb_r_bot <- auc(roc_xgb_r_bot)
auc_xgb_c_bot <- auc(roc_xgb_c_bot)


ols_td_bot_tpr <- sum(data$bot_ret*data$ols_td_bot)/sum(data$bot_ret*data$ols_td_bot + data$bot_ret*(data$ols_td_bot==0))
ols_td_bot_fpr <- sum((data$bot_ret==0)*data$ols_td_bot)/sum((data$bot_ret==0)*data$ols_td_bot + (data$bot_ret==0)*(data$ols_td_bot==0))

xgb_r_td_bot_tpr <- sum(data$bot_ret*data$xgb_r_td_bot)/sum(data$bot_ret*data$xgb_r_td_bot + data$bot_ret*(data$xgb_r_td_bot==0))
xgb_r_td_bot_fpr <- sum((data$bot_ret==0)*data$xgb_r_td_bot)/sum((data$bot_ret==0)*data$xgb_r_td_bot + (data$bot_ret==0)*(data$xgb_r_td_bot==0))

idx.xgbc = which.min( abs( (1-roc_xgb_c_bot$specificities)-xgb_r_td_bot_fpr))
idx.glm = which.min( abs( (1-roc_glm_bot$specificities)-xgb_r_td_bot_fpr))

cbind.data.frame('xgbr' = rbind(xgb_r_td_bot_tpr,xgb_r_td_bot_fpr),
                 'ols' = rbind(ols_td_bot_tpr,ols_td_bot_fpr),
                 'xgbc' = rbind(roc_xgb_c_bot$sensitivities[idx.xgbc],1-roc_xgb_c_bot$specificities[idx.xgbc]),
                 'logi' = rbind(roc_glm_bot$sensitivities[idx.glm],1-roc_glm_bot$specificities[idx.glm])
)


npoints = 10000
sel.idx.xgbc = round(seq(1,length(roc_xgb_c_top$specificities),length.out=npoints))
sel.idx.xgbr = round(seq(1,length(roc_xgb_r_top$specificities),length.out=npoints))
sel.idx.ols = round(seq(1,length(roc_ols_top$specificities),length.out=npoints))
sel.idx.glm = round(seq(1,length(roc_glm_top$specificities),length.out=npoints))



pdf('../08_Figures/ROC_BOT.pdf',height = 8,width = 10)
plot(1-roc_xgb_c_bot$specificities[sel.idx.xgbc], roc_xgb_c_bot$sensitivities[sel.idx.xgbc],xlim=c(0,1),t='l',col='#00a0b0',lwd=2,lty=1,xlab='False Positive Rate',
     ylab='True Positive Rate',ax=F)
axis(side=1,at=seq(0,1,0.1),labels = seq(0,1,0.1))
axis(side=2,at=seq(0,1,0.1),labels = seq(0,1,0.1))
box()
lines(1-roc_glm_bot$specificities[sel.idx.glm], roc_glm_bot$sensitivities[sel.idx.glm],col='#cc3340',lwd=2,lty=1)
lines(1-roc_xgb_r_bot$specificities[sel.idx.xgbr], roc_xgb_r_bot$sensitivities[sel.idx.xgbr],col='#00a0b0',lwd=2,lty=3)
lines(1-roc_ols_bot$specificities[sel.idx.ols], roc_ols_bot$sensitivities[sel.idx.ols],col='#cc3340',lwd=2,lty=3)
abline(a=0,b=1,lty='dotted')
abline(v=seq(0,1,0.1),h=seq(0,1,0.1),lty=2,col=rgb(0,0,0,1/6))
points(ols_td_bot_fpr,ols_td_bot_tpr,pch=20,cex=1.5,col='#cc3340')
points(xgb_r_td_bot_fpr,xgb_r_td_bot_tpr,pch=20,cex=1.5,col='#00a0b0')

grid()
legend('bottomright',c(
  sprintf('XGBC, AUC: %.2f', auc_xgb_c_bot),
  sprintf('Logistic, AUC: %.2f', auc_glm_bot),
  sprintf('XGBR, AUC: %.2f', auc_xgb_r_bot),
  sprintf('Linear, AUC: %.2f', auc_ols_bot),
  'Random Classifier','Top Decile'),col = c('#00a0b0','#cc3340','#00a0b0','#cc3340','black','black'),
  lty=c(1,1,3,3,2,NA),pch=c(NA,NA,NA,NA,NA,20),lwd=3,ncol=3,bg='white')
dev.off()



## Confusion matrix
data$ret_class = data$top_ret - data$bot_ret
data$xgb_c_opt = data$xgb_c_opt_top - data$xgb_c_opt_bot
data$xgb_c_td = data$xgb_c_td_top - data$xgb_c_td_bot
data$xgb_r_opt = data$xgb_r_opt_top - data$xgb_r_opt_bot
data$xgb_r_td = data$xgb_r_td_top - data$xgb_r_td_bot
data$ols_opt = data$ols_opt_top - data$ols_opt_bot
data$ols_td = data$ols_td_top - data$ols_td_bot
data$glm_opt = data$glm_opt_top - data$glm_opt_bot
data$glm_td = data$glm_td_top - data$glm_td_bot

xgb_c_opt = table(data$ret_class,data$xgb_c_opt)
xgb_c_td = table(data$ret_class,data$xgb_c_td)
xgb_r_opt = table(data$ret_class,data$xgb_r_opt)
xgb_r_td = table(data$ret_class,data$xgb_r_td)
ols_opt = table(data$ret_class,data$ols_opt)
ols_td = table(data$ret_class,data$ols_td)
glm_opt = table(data$ret_class,data$glm_opt)
glm_td = table(data$ret_class,data$glm_td)


stdize = function(x){
  tmp = sapply(1:ncol(x), function(w) x[,w]/sum(x[,w]))
  colnames(tmp) <- colnames(x)
  100*tmp
}

xgbc.opt.cm.table = stdize(xgb_c_opt)
xgbc.td.cm.table = stdize(xgb_c_td)
xgbr.opt.cm.table = stdize(xgb_r_opt)
xgbr.td.cm.table = stdize(xgb_r_td)
ols.opt.cm.table = stdize(ols_opt)
ols.td.cm.table = stdize(ols_td)
glm.opt.cm.table = stdize(glm_opt)
glm.td.cm.table = stdize(glm_td)

## Output table
out <- t(cbind( rbind(ols.td.cm.table, ols.opt.cm.table), rbind(glm.td.cm.table,glm.opt.cm.table),
                rbind(xgbr.td.cm.table, xgbr.opt.cm.table), rbind( xgbc.td.cm.table , xgbc.opt.cm.table ) ))
rownames(out) <- rep( c('Loser','Neutral','Winner'), 4)
t1     <- xtable(out, digits = 2)
lines  <- print(t1, sanitize.text.function = identity,booktabs=TRUE,NA.string = '$-$')
lines  <- strsplit(lines,'\n')[[1]]
lines[7] <- " & & \\multicolumn{3}{c}{Benchmark}  & \\multicolumn{3}{c}{Optimal} \\\\"  
lines[7] <- paste(lines[7]," \\cmidrule(lr){3-5} \\cmidrule(lr){6-8} ")
lines[7] <- paste(lines[7], " Portfolio & Predicted Class & Loser  & Neutral & Winner & Loser & Neutral & Winner \\\\ ")
lines[8] <- "\\cmidrule(lr){1-1} \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4} \\cmidrule(lr){5-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7} \\cmidrule(lr){8-8} "
lines[9:20] <- paste("&",lines[9:20]) 
lines[9] <- paste('\\multirow{3}{*}{LIN-R}',lines[9])
lines[11] <- paste(lines[11],'[1em]')
lines[12] <- paste('\\multirow{3}{*}{LIN-C}',lines[12])
lines[14] <- paste(lines[14],'[1em]')
lines[15] <- paste('\\multirow{3}{*}{ML-R}',lines[15])
lines[17] <- paste(lines[17],'[1em]')
lines[18] <- paste('\\multirow{3}{*}{ML-C}',lines[18])
lines[20] <- paste(lines[20],'[1em]')

lines <- gsub('Winner.[0-9]','Winner',lines)
lines <- gsub('Neutral.[0-9]','Neutral',lines)
lines <- gsub('Loser.[0-9]','Loser',lines)

lines[5] <- "\\begin{tabular}{cccccccc}"
lines <- lines[grep('begin{tabular}',lines,fixed = TRUE) : grep('end{tabular}',lines,fixed = TRUE)]
fileConn <- sprintf('../tables/%s.tex', 'ConfusionMatrix')
write(lines,fileConn)

## Output table
out <- t(cbind( rbind(glm.td.cm.table,glm.opt.cm.table), rbind( xgbc.td.cm.table , xgbc.opt.cm.table ) ))
rownames(out) <- rep( c('Losers','Neutral','Winners'), 2)
t1     <- xtable(out, digits = 2)
lines  <- print(t1, sanitize.text.function = identity,booktabs=TRUE,NA.string = '$-$')
lines  <- strsplit(lines,'\n')[[1]]
lines[7] <- " & & \\multicolumn{3}{c}{Decile}  & \\multicolumn{3}{c}{Optimal Selection} \\\\"  
lines[7] <- paste(lines[7]," \\cmidrule(lr){3-5} \\cmidrule(lr){6-8} ")
lines[7] <- paste(lines[7], " Model & Realized Returns & Bottom  & Middle & Top & Bottom & Middle & Top \\\\ ")
lines[8] <- "\\cmidrule(lr){1-1} \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4} \\cmidrule(lr){5-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7} \\cmidrule(lr){8-8} "
lines[9:14] <- paste("&",lines[9:14]) 
lines[9] <- paste('\\multirow{3}{*}{GLM}',lines[9])
lines[11] <- paste(lines[11],'[1em]')
lines[12] <- paste('\\multirow{3}{*}{XGB(C)}',lines[12])

lines <- gsub('Winners.1','Winners',lines)
lines <- gsub('Neutral.1','Neutral',lines)
lines <- gsub('Losers.1','Losers',lines)

lines[5] <- "\\begin{tabular}{cccccccc}"
lines <- lines[grep('begin{tabular}',lines,fixed = TRUE) : grep('end{tabular}',lines,fixed = TRUE)]
fileConn <- sprintf('../../../tables/%s.tex', 'ConfusionMatrix_Classification')
write(lines,fileConn)

## Output table
out <- t(cbind( rbind(ols.td.cm.table, ols.opt.cm.table), rbind( xgbr.td.cm.table , xgbr.opt.cm.table ) ))
rownames(out) <- rep( c('Losers','Neutral','Winners'), 2)
t1     <- xtable(out, digits = 2)
lines  <- print(t1, sanitize.text.function = identity,booktabs=TRUE,NA.string = '$-$')
lines  <- strsplit(lines,'\n')[[1]]
lines[7] <- " & & \\multicolumn{3}{c}{Decile}  & \\multicolumn{3}{c}{Optimal Selection} \\\\"  
lines[7] <- paste(lines[7]," \\cmidrule(lr){3-5} \\cmidrule(lr){6-8} ")
lines[7] <- paste(lines[7], " Model & Realized Returns & Bottom  & Middle & Top & Bottom & Middle & Top \\\\ ")
lines[8] <- "\\cmidrule(lr){1-1} \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4} \\cmidrule(lr){5-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7} \\cmidrule(lr){8-8} "
lines[9:14] <- paste("&",lines[9:14]) 
lines[9] <- paste('\\multirow{3}{*}{OLS}',lines[9])
lines[11] <- paste(lines[11],'[1em]')
lines[12] <- paste('\\multirow{3}{*}{XGB(R)}',lines[12])

lines <- gsub('Winners.1','Winners',lines)
lines <- gsub('Neutral.1','Neutral',lines)
lines <- gsub('Losers.1','Losers',lines)

lines[5] <- "\\begin{tabular}{cccccccc}"
lines <- lines[grep('begin{tabular}',lines,fixed = TRUE) : grep('end{tabular}',lines,fixed = TRUE)]
fileConn <- sprintf('../../../tables/%s.tex', 'ConfusionMatrix_Regression')
write(lines,fileConn)




