# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory
#setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

#read the datafiles
ff                  = read.csv("FF3factors.CSV", sep = "," )
trading005          = read.csv("nettradingret(005).csv", sep = ",")
trading050          = read.csv("nettradingret(050).csv", sep = ",")
trading095          = read.csv("nettradingret(095).csv", sep = ",")

#calculate the excess returns and combine the data
ff                  = ff[(nrow(ff)-252):nrow(ff), ]
rownames(ff)        = ff[,1]
ff[,1]              = ff[,2] + ff[,5]
ff                  = ff/100+1
trading005          = trading005[ ,-1]+1
trading050          = trading050[ ,-1]+1
trading095          = trading095[ ,-1]+1
cumtrading005       = matrix(0,253,4)
for (i in 1:4){
  cumtrading005[,i] = cumprod(trading005[,i])
}
cumtrading050       = matrix(0,253,4)
for (i in 1:4){
  cumtrading050[,i] = cumprod(trading050[,i])
}
cumtrading095       = matrix(0,253,4)
for (i in 1:4){
  cumtrading095[,i] = cumprod(trading095[,i])
}
fac                 = matrix(0,253,2)
fac[,1]             = cumprod(ff[,1])
fac[,2]             = cumprod(ff[,5])
returnsdata         = cbind(cumtrading005,cumtrading050,cumtrading095,fac)
for (i in 1:nrow(returnsdata)){
  returnsdata[i,]   = returnsdata[i,]^(12/i)-1
}
exreturns           = cbind(returnsdata[,1:13]-returnsdata[,14])
rownames(exreturns) = rownames(ff)
annexret            = matrix(0,253,13)

#calculate means and t-stat
summret             = matrix(0, 2, 13)
summret[1,]         = colMeans(exreturns)
summret[2,]         = colMeans(exreturns)/colSds(exreturns)

write.table(format(summret, digits = 4), file = "summret.txt", sep = "&")