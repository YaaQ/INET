# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("graphics")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# set the working directory
#setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

Data1           = read.table("RMSE_median.txt", header = TRUE, sep = " ", dec = ".")
colnames(Data1) = "median"
Data2           = read.table("RMSE_lowertail.txt", header = TRUE, sep = " ", dec = ".")
colnames(Data2) = "lowertail"
Data3           = read.table("RMSE_uppertail.txt", header = TRUE, sep = " ", dec = ".")
colnames(Data3) = "uppertail"
Data            = cbind(Data1,Data2, Data3)
Datanew         = cbind(Data2, Data1, Data3)
matplot(1:49, Data[,1:3], type = "l", lwd = 3, col = 1:3, axes = FALSE, xlab = "different industries",
        ylab = "RMSE of Industry Return Predictions")
title(main = "RMSE of median, lower tail and upper tail predictions")
axis(side = 2, labels = TRUE)
mtext(rownames(Data1), side = 1, line=0.3, at = seq(1,49,1),  las = 2, cex = 0.8)
write.table(format(Datanew,digits = 4), file = "rmsesumm.txt", sep = "&")
