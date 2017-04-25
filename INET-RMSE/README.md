
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **INET-RMSE** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : INET-RMSE

Published in : Industry Interdependency Dynamics in a Network Context

Description : 'Calculate the root mean square error (RMSE) of the one-month forward return
prediction under different tail levels and compare the results'

Keywords : quantile regression, interdependency, network, tail, RMSE, predictability

Author : Ya Qian

Submitted : Ya Qian

Datafile : 49IPM1970.CSV

Output : 'RMSE of the one-month forward return prediction and plot of the comparison under the
three market situations'

```

![Picture1](INET-RMSE.png)


### R Code:
```r
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory
#setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

# install and load packages
libraries = c("fGarch", "quantreg")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
source("lqrl1_test.r")
source("quantilelasso.r")

#read datafile
D                = read.csv("49IPM1970.CSV", header = TRUE, sep = ",", dec = "." )
rownames(D)      = D[ ,1]
DD               = D[ ,-1]/100

# quantile level
tau              = 0.95

# save predictive values into matrix
pre_est          = matrix(0, 253, 49)



# moving window estimation
for (i in 1:253){
  print(i)
  xx1            = DD[1:(311+i), ]
  
  # coefficients betas calculated from linear quantile lasso
  beta_l         = matrix(0, ncol(xx1), (ncol(xx1)+1))
  for (k in 1:ncol(xx1)) {
    cat("Industry:", k)
    # return of industry k
    yw           = as.matrix(xx1[-1, k])
    # returns of all industries
    xxw          = as.matrix(xx1[-nrow(xx1),])  
    
    # start the quantile lasso estimation for each firm in one specific moving window
    fit          = linear(yw, xxw, tau, i, k)
    beta_l[k, ]  = fit$beta_in
  }
  ret            = cbind(c(1, xx1[nrow(xx1),]))
  ret            = matrix(unlist(ret), ncol = 1)
  pre_est[i,]    = beta_l%*%ret
}                          
  pre_err        = DD[313:nrow(DD), ] - pre_est
  RMSE           = sqrt(colMeans(pre_err^2))
  
  write.csv(pre_est, file = paste("pre_est_uppertail", ".csv",  sep = ""))
  write.table(RMSE, file = paste("RMSE_uppertail", ".txt",  sep = ""))
```
