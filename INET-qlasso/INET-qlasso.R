# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory
setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata")

# install and load packages
libraries = c("fGarch", "quantreg")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
source("lqrl1_test.r")
source("quantilelasso.r")

#read datafile
D                = read.csv("49_Industry_Portfolios_Daily.CSV", header = TRUE, sep = ",", dec = "." )
rownames(D)      = D[ ,1]
DD               = D[ ,-1]
DDD              = log(1+DD/100)
stdresd          = DDD
# Garch fitting of the data
for (i in 1:ncol(DDD)){
  fit            = garchFit(~garch(1, 1), data = DDD[,i], trace = F)
  stdresd[,i]    = fit@residuals / fit@sigma.t
}

# quantile level
tau              = 0.05
# moving window size
ws               = nrow(stdresd)
step             = 126
num              = as.integer((nrow(stdresd)-ws)/step)+1

# start the linear quantile lasso estimation for each moving window
for (i in 1:num) {
  print(i)
  #yw  = y[((i-1)*step+1):((i-1)*step + ws)]
  xx1            = stdresd[((i-1)*step+1):((i-1)*step + ws), ]
  
  # lambda calculated from linear quantile lasso
  lambda_l       = matrix(0, ncol(xx1), 1)
  # coefficients betas calculated from linear quantile lasso
  beta_l         = matrix(0, ncol(xx1), ncol(xx1))
  for (k in 1:ncol(xx1)) {
    cat("Industry:", k)
    # log return of firm k
    yw           = as.matrix(xx1[-1, k])
    # log returns of firms except firm k
    xxw          = as.matrix(xx1[-nrow(xx1),])  
    
    # start the quantile lasso estimation for each firm in one specific moving window
    fit          = linear(yw, xxw, tau, i, k)
    lambda_l[k]  = fit$lambda.in
    beta_l[k, ]  = fit$beta_in
  }
  write.csv(beta_l, file = paste("beta_L_uppertail", i, ".csv",  sep = ""))
  write.csv(lambda_l, file = paste("lambda_L_uppertail", i, ".csv", sep = ""))
} 

