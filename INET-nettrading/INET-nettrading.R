# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory
#setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

# install and load packages
libraries = c("fGarch", "quantreg", "dplyr", "igraph", "qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
source("lqr1.r")
source("quantilelasso.r")

#read datafile
D                    = read.csv("49IPM1970.CSV", header = TRUE, sep = ",", dec = "." )
rownames(D)          = D[ ,1]
DD                   = D[ ,-1]/100
industrynames        = read.csv("industrynames.csv", header = TRUE, sep = ",")
industrynames        = industrynames[,-1]

# quantile level
tau                  = 0.50

# save predictive values into matrix
returns              = matrix(0, 253, 4)



# moving window estimation
for (i in 1:253){
  print(i)
  xx1                = DD[1:(311+i), ]
  
  # coefficients betas calculated from linear quantile lasso
  beta_l             = matrix(0, ncol(xx1), ncol(xx1))
  for (k in 1:ncol(xx1)) {
    cat("Industry:", k)
    # return of industry k
    yw               = as.matrix(xx1[-1, k])
    # returns of all industries
    xxw              = as.matrix(xx1[-nrow(xx1),])  
    
    # start the quantile lasso estimation for each firm in one specific moving window
    fit              = linear(yw, xxw, tau, i, k)
    beta_l[k, ]      = fit$beta_in
  }
  Adjac              = as.matrix(beta_l)
  Adjac              = t(Adjac)
  rownames(Adjac)    = industrynames
  colnames(Adjac)    = industrynames
  
  Graph1             = graph_from_adjacency_matrix(Adjac, mode = "directed", 
                                                   weighted = TRUE, diag = FALSE, add.colnames = TRUE)
  ec                 = centr_eigen(Graph1, directed = TRUE, scale = TRUE, 
                                   options = arpack_defaults, normalized = TRUE)
  eigenvec           = as.matrix(ec$vector)
  rownames(eigenvec) = rownames(Adjac)
  centrnodeshigh     = rownames(eigenvec)[order(eigenvec, decreasing = TRUE)][1:5]
  centrnodeshigh1    = rownames(eigenvec)[order(eigenvec, decreasing = TRUE)][1:10]
  centrnodeslow      = rownames(eigenvec)[order(eigenvec, decreasing = FALSE)][1:5]
  centrnodeslow1     = rownames(eigenvec)[order(eigenvec, decreasing = FALSE)][1:10]
  returns[i,1]       = (sum(DD[(312+i), centrnodeshigh])-sum(DD[(312+i), centrnodeslow]))/10
  returns[i,2]       = (sum(DD[(312+i), centrnodeshigh])+sum(DD[(312+i), centrnodeslow]))/10
  returns[i,3]       = sum(DD[(312+i), centrnodeshigh1])/10
  returns[i,4]       = sum(DD[(312+i), centrnodeslow1])/10
}                          
write.csv(returns, file = paste("nettradingret(050)", ".csv",  sep = ""))

