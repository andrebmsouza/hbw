rm(list=ls())
library(dplyr)
library(lubridate)
library(xtable)
source('../utils.R')

# Load portfolio data.
ptf.data <- readRDS('../04_LambdaSelection/PortfolioData.RDS')

# Characteristics
data.chars <- readRDS('../data/prediction_dataset.RDS')
chars <- colnames(data.chars)
chars <- chars[-c(1,2,3)]
data.chars <- merge(ptf.data, data.chars, by = c('permno','date'))

# Construct portfolio returns.

mutate_vars_long = colnames(ptf.data)[c( grep("td_top", colnames(ptf.data)) ,
                                         grep("opt_top", colnames(ptf.data)))
                                         ]
mutate_vars_short = colnames(ptf.data)[c( grep("td_bot", colnames(ptf.data)) ,
                                          grep("opt_bot", colnames(ptf.data)))]


chars <- chars[-which(chars=='ret_dec')]
chars <- chars[-which(chars=='mcap')]
char.medians.long <- char.medians.short <-  cbind.data.frame(matrix(NA,nrow=length(chars),ncol=length(mutate_vars_long)))
colnames(char.medians.long) <- mutate_vars_long
colnames(char.medians.short) <-  mutate_vars_short
rownames(char.medians.long) <- rownames(char.medians.short) <- chars
for( char in chars ){
  char.medians.long[char,] <- data.chars %>% filter(date>='1987-01-01') %>% ungroup() %>%
    summarise_at(mutate_vars_long, list(~mean(eval(parse(text=char))[.x],na.rm=TRUE)))
  char.medians.short[char,] <- data.chars %>% filter(date>='1987-01-01') %>% ungroup() %>%
    summarise_at(mutate_vars_short, list(~mean(eval(parse(text=char))[.x],na.rm=TRUE)))
  cat('.')
}


char.gap <- (char.medians.long-char.medians.short)
colnames(char.gap) <- gsub('top','diff',colnames(char.gap))

mdl.order <- c('ols_td_top','ols_opt_top','glm_td_top','glm_opt_top',
               'xgb_r_td_top','xgb_r_opt_top','xgb_c_td_top','xgb_c_opt_top')
mdl.order.short <- gsub("_top","_bot",mdl.order)
mdl.order.gap <- gsub("_top","_diff",mdl.order)
nchars = 40
idx <- order(abs(char.gap$xgb_c_opt_diff),decreasing = TRUE)
tb <- cbind( char.medians.long[idx[1:nchars], mdl.order],
             char.medians.short[idx[1:nchars], mdl.order.short],
             char.gap[idx[1:nchars],mdl.order.gap])

tb <- 100*tb[,c('xgb_r_td_top','xgb_r_td_bot','xgb_r_td_diff',
                'xgb_r_opt_top','xgb_r_opt_bot','xgb_r_opt_diff',
                'xgb_c_td_top','xgb_c_td_bot', 'xgb_c_td_diff',
                'xgb_c_opt_top','xgb_c_opt_bot','xgb_c_opt_diff')]
tb

rownames(tb) <- sanitize(rownames(tb))

tb

dig = matrix(2,ncol=ncol(tb)+1,nrow=nrow(tb))
dig[nrow(dig):(nrow(dig)-1),] <- 2
t1     <- xtable(tb, digits = dig)
lines  <- print(t1, sanitize.text.function = identity,booktabs=TRUE,NA.string = '$-$')
lines  <- strsplit(lines,'\n')[[1]]
lines[7] <- " & \\multicolumn{6}{c}{ML-R}  & \\multicolumn{6}{c}{ML-C}\\\\ \\cmidrule(lr){2-7} \\cmidrule(lr){8-13}"  
lines[7] <- paste(lines[7]," & \\multicolumn{3}{c}{BM}  & \\multicolumn{3}{c}{Optimal} & \\multicolumn{3}{c}{BM}  & \\multicolumn{3}{c}{Optimal}  \\\\"  )
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10} \\cmidrule(lr){11-13} ")
lines[7] <-  paste(lines[7], " & Long & Short & Diff. & Long & Short & Diff. & Long & Short & Diff. & Long & Short & Diff. \\\\ ")
lines[8] <- " \\cmidrule(lr){2-2}\\cmidrule(lr){3-3} \\cmidrule(lr){4-4}  \\cmidrule(lr){5-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7} \\cmidrule(lr){8-8} \\cmidrule(lr){9-9} \\cmidrule(lr){10-10}\\cmidrule(lr){11-11}\\cmidrule(lr){12-12}\\cmidrule(lr){13-13} "
lines[5] <- "\\begin{tabular}{ccccccccccccc}"
lines <- lines[grep('begin{tabular}',lines,fixed = TRUE) : grep('end{tabular}',lines,fixed = TRUE)]
fileConn <- '../tables/Table_Characteristics_ML.tex'
write(lines,fileConn)

# Full ML table
nchars = nrow(char.gap)
idx <- order(abs(char.gap$xgb_c_opt_diff),decreasing = TRUE)
tb <- cbind( char.medians.long[idx[40:nchars], mdl.order],
             char.medians.short[idx[40:nchars], mdl.order.short],
             char.gap[idx[40:nchars],mdl.order.gap])

tb <- 100*tb[,c('xgb_r_td_top','xgb_r_td_bot','xgb_r_td_diff',
                'xgb_r_opt_top','xgb_r_opt_bot','xgb_r_opt_diff',
                'xgb_c_td_top','xgb_c_td_bot', 'xgb_c_td_diff',
                'xgb_c_opt_top','xgb_c_opt_bot','xgb_c_opt_diff')]
tb

rownames(tb) <- sanitize(rownames(tb))

tb

dig = matrix(2,ncol=ncol(tb)+1,nrow=nrow(tb))
dig[nrow(dig):(nrow(dig)-1),] <- 2
t1     <- xtable(tb, digits = dig)
lines  <- print(t1, sanitize.text.function = identity,booktabs=TRUE,NA.string = '$-$')
lines  <- strsplit(lines,'\n')[[1]]
lines[7] <- " & \\multicolumn{6}{c}{ML-R}  & \\multicolumn{6}{c}{ML-C}\\\\ \\cmidrule(lr){2-7} \\cmidrule(lr){8-13}"  
lines[7] <- paste(lines[7]," & \\multicolumn{3}{c}{BM}  & \\multicolumn{3}{c}{Optimal} & \\multicolumn{3}{c}{BM}  & \\multicolumn{3}{c}{Optimal}  \\\\"  )
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10} \\cmidrule(lr){11-13} ")
lines[7] <-  paste(lines[7], " & Long & Short & Diff. & Long & Short & Diff. & Long & Short & Diff. & Long & Short & Diff. \\\\ ")
lines[8] <- " \\cmidrule(lr){2-2}\\cmidrule(lr){3-3} \\cmidrule(lr){4-4}  \\cmidrule(lr){5-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7} \\cmidrule(lr){8-8} \\cmidrule(lr){9-9} \\cmidrule(lr){10-10}\\cmidrule(lr){11-11}\\cmidrule(lr){12-12}\\cmidrule(lr){13-13} "
lines[5] <- "\\begin{tabular}{ccccccccccccc}"
lines <- lines[grep('begin{tabular}',lines,fixed = TRUE) : grep('end{tabular}',lines,fixed = TRUE)]
fileConn <- '../tables/Table_Characteristics_ML_Full.tex'
write(lines,fileConn)

# Linear Models
idx <- order(abs(char.gap$glm_opt_diff),decreasing = TRUE)
tb <- cbind( char.medians.long[idx[1:nchars], mdl.order],char.medians.short[idx[1:nchars], mdl.order.short], char.gap[idx[1:nchars],mdl.order.gap])
tb <- 100*tb[,c('ols_td_top','ols_td_bot','ols_td_diff',
                'ols_opt_top','ols_opt_bot','ols_opt_diff',
                'glm_td_top','glm_td_bot','glm_td_diff',
                'glm_opt_top','glm_opt_bot','glm_opt_diff')]
tb

rownames(tb) <- sanitize(rownames(tb))

tb

dig = matrix(2,ncol=ncol(tb)+1,nrow=nrow(tb))
dig[nrow(dig):(nrow(dig)-1),] <- 2
t1     <- xtable(tb, digits = dig)
lines  <- print(t1, sanitize.text.function = identity,booktabs=TRUE,NA.string = '$-$')
lines  <- strsplit(lines,'\n')[[1]]
lines[7] <- " & \\multicolumn{6}{c}{LIN-R}  & \\multicolumn{6}{c}{LIN-C}\\\\ \\cmidrule(lr){2-7} \\cmidrule(lr){8-13}"  
lines[7] <- paste(lines[7]," & \\multicolumn{3}{c}{BM}  & \\multicolumn{3}{c}{Optimal} & \\multicolumn{3}{c}{BM}  & \\multicolumn{3}{c}{Optimal}  \\\\"  )
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10} \\cmidrule(lr){11-13} ")
lines[7] <-  paste(lines[7], " & Long & Short & Diff. & Long & Short & Diff. & Long & Short & Diff. & Long & Short & Diff. \\\\ ")
lines[8] <- " \\cmidrule(lr){2-2}\\cmidrule(lr){3-3} \\cmidrule(lr){4-4}  \\cmidrule(lr){5-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7} \\cmidrule(lr){8-8} \\cmidrule(lr){9-9} \\cmidrule(lr){10-10}\\cmidrule(lr){11-11}\\cmidrule(lr){12-12}\\cmidrule(lr){13-13} "
lines[5] <- "\\begin{tabular}{ccccccccccccc}"
lines <- lines[grep('begin{tabular}',lines,fixed = TRUE) : grep('end{tabular}',lines,fixed = TRUE)]
fileConn <- '../tables/Table_Characteristics_LIN.tex'
write(lines,fileConn)

# Full Linear Models
idx <- order(abs(char.gap$glm_opt_diff),decreasing = TRUE)
tb <- cbind( char.medians.long[idx[41:nchars], mdl.order],
             char.medians.short[idx[41:nchars], mdl.order.short], 
             char.gap[idx[41:nchars],mdl.order.gap])
tb <- 100*tb[,c('ols_td_top','ols_td_bot','ols_td_diff',
                'ols_opt_top','ols_opt_bot','ols_opt_diff',
                'glm_td_top','glm_td_bot','glm_td_diff',
                'glm_opt_top','glm_opt_bot','glm_opt_diff')]
tb

rownames(tb) <- sanitize(rownames(tb))

tb

dig = matrix(2,ncol=ncol(tb)+1,nrow=nrow(tb))
dig[nrow(dig):(nrow(dig)-1),] <- 2
t1     <- xtable(tb, digits = dig)
lines  <- print(t1, sanitize.text.function = identity,booktabs=TRUE,NA.string = '$-$')
lines  <- strsplit(lines,'\n')[[1]]
lines[7] <- " & \\multicolumn{6}{c}{LIN-R}  & \\multicolumn{6}{c}{LIN-C}\\\\ \\cmidrule(lr){2-7} \\cmidrule(lr){8-13}"  
lines[7] <- paste(lines[7]," & \\multicolumn{3}{c}{BM}  & \\multicolumn{3}{c}{Optimal} & \\multicolumn{3}{c}{BM}  & \\multicolumn{3}{c}{Optimal}  \\\\"  )
lines[7] <- paste(lines[7]," \\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10} \\cmidrule(lr){11-13} ")
lines[7] <-  paste(lines[7], " & Long & Short & Diff. & Long & Short & Diff. & Long & Short & Diff. & Long & Short & Diff. \\\\ ")
lines[8] <- " \\cmidrule(lr){2-2}\\cmidrule(lr){3-3} \\cmidrule(lr){4-4}  \\cmidrule(lr){5-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7} \\cmidrule(lr){8-8} \\cmidrule(lr){9-9} \\cmidrule(lr){10-10}\\cmidrule(lr){11-11}\\cmidrule(lr){12-12}\\cmidrule(lr){13-13} "
lines[5] <- "\\begin{tabular}{ccccccccccccc}"
lines <- lines[grep('begin{tabular}',lines,fixed = TRUE) : grep('end{tabular}',lines,fixed = TRUE)]
fileConn <- '../tables/Table_Characteristics_LIN_Full.tex'
write(lines,fileConn)



