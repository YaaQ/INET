
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **INET-qqplot** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : INET-qqplot

Published in : Industry Interdependency Dynamics in a Network Context

Description : 'Plot the quantile-quantile plot of the monthly return of 49 industry portfolios in
USA'

Keywords : quantile, normality, plot, qq-plot, kurtosis

Author : Ya Qian

Submitted : Ya Qian

Datafile : 49IPM1970.CSV

Output : Quantile-quantile plot of the 49 industry portfolio monthly return

```

![Picture1](qq(1-10).png)

![Picture2](qq(11-20).png)

![Picture3](qq(21-30).png)

![Picture4](qq(31-40).png)

![Picture5](qq(41-49).png)


### R Code:
```r
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory
#setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

# install and load packages
libraries = c("psych", "stats")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

D                 = read.csv("49IPM1970.CSV", header = TRUE, sep = ",", dec = ".")
D[D == -99.99]    = NA

rownames(D)       = D[ ,1]
DD                = D[ ,-1]/100

par(mfrow = c(2,5), no.readonly = FALSE)
for (i in 1:10){
  qqnorm(DD[,i], xlab = "Normal Quantiles", 
         ylab = paste("Sample Quantiles of ", colnames(DD)[i], sep = " "))
  qqline(DD[,i], col = 2)
}
write.table(format(stat,digits = 4), file = "summary statistics", sep = "&")
```
