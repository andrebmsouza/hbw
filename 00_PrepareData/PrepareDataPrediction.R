rm(list=ls())
library(dplyr)
library(foreign)
library(data.table)
library(lubridate)

# Load CRSP data
crsp <- fread('../data/CRSP_Monthly.csv')
crsp[, `:=` (RET = as.numeric(RET), DLRET = as.numeric(DLRET))]
# Filter share codes, exchanges, drop missing returns
crsp <- crsp[!is.na(RET) & SHRCD <= 11 & EXCHCD >= 1 & EXCHCD <= 3]
# Replace delisting returns
crsp[!is.na(DLRET), RET := DLRET]
# Select a few variables to keep
crsp <- crsp[, .(PERMNO, date, EXCHCD, RET)]
crsp <- unique(crsp)

# Characteristic data
chars <- fread('../data/DachengXiu/datashare.csv')
chars[,DATE:=as.character(chars$DATE)]
# data starts from 1957-01-31
crsp <- crsp[date >= as.Date("1957-01-01")]
crsp[,DATE := format(crsp$date,'%Y%m%d')]
colnames(crsp)[1] <- 'permno'

data <- merge(crsp, chars,by = c('permno','DATE'))
rm(crsp,chars)
gc()

# Check the timing
check <- data[ data$permno==data$permno[1], c('permno','date','RET','mom1m')]
rm(check)
gc()


# Drop unecessary columns
data <- data[, !c('sic2','DATE','EXCHCD'), with=FALSE]

# Transform characteristics
not_chars <- c('RET','permno','date','mcap')
data$mcap <- data$mvel1
chars <- setdiff(colnames(data), not_chars)

# Function to rank characteristics
getRank <- function(x){
  out <- rank(x, na.last='keep')/sum(!is.na(x))
  if (all(is.na(out))){
    out <- rep(0.5,length(x))
  } else {
    out[is.na(x)] <- median(out,na.rm=TRUE)
  }
  out
}

# Convert data into ranks
ranked_data <- data[, (chars) := lapply(.SD, getRank), by=date, .SDcols=chars]

# check that there are no NA's in characteristics
any(is.na(ranked_data[, ..chars]))

# Construct target
ranked_data[, ret_dec := findInterval(RET, quantile(RET, seq(0.1,0.9,0.1))) , by=date]

# Save all data
saveRDS(ranked_data,'../data/prediction_dataset.RDS')


