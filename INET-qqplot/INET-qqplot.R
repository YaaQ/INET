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