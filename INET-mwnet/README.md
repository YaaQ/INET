
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **INET-mwnet** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : INET-mwnet

Published in : Industry Interdependency Dynamics in a Network Context

Description : 'Plot the interdependency networks of the return data of 49 USA industries under
different stress situations (median, lowertail and uppertail) for different time periods'

Keywords : 'quantile regression, interdependency, network, tail, stress situation, movie window
analysis'

Author : Ya Qian

Submitted : Ya Qian

Datafile : 'industryname.csv, beta_L_median(1-15).csv, beta_L_lowertail(1-15).csv,
beta_L_uppertail(1-15).csv'

Output : 'Plots of interdependency networks of the 49 industry returns under different stress
situations (median, lowertail and uppertail) for various time periods'

```


### R Code:
```r
#Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))

# set the working directory
#setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

# install and load packages
libraries = c("dplyr", "igraph", "qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

date               = c("197001-197212", "197301-197512", "197601-197812", "197901-198112",
                       "198201-198412", "198501-198712", "198801-199012", "199101-199312",
                       "199401-199612", "199701-199912", "200001-200312", "200401-200612",
                       "200701-200912", "201001-201212", "201301-201512")
for (s in 1:15){
#read datafiles
industrynames      = read.csv("industrynames.csv", header = TRUE, sep = ",")
industrynames      = industrynames[,-1]
datapath1          = paste("dynamiclassocoeff/beta_L_median", s, ".csv", sep ="")
data0              = read.csv(datapath1, header = TRUE, sep = ",", dec = ".")
data0              = data0[,-1]

n                  = nrow(data0)
num                = ncol(data0)

Adjac              = as.matrix(data0)
Adjac              = t(Adjac)
rownames(Adjac)    = industrynames
colnames(Adjac)    = industrynames

#plot network with centrality
Graph1             = graph_from_adjacency_matrix(Adjac, mode = "directed", 
                     weighted = TRUE, diag = FALSE, add.colnames = TRUE)
ec                 = centr_eigen(Graph1, directed = TRUE, scale = TRUE, 
                     options = arpack_defaults, normalized = TRUE)
eigenvec           = as.matrix(ec$vector)
rownames(eigenvec) = rownames(Adjac)
eigenscores        = eigenvec[order(eigenvec, decreasing = TRUE)[1:10]]
centrnodes         = rownames(eigenvec)[order(eigenvec, decreasing = TRUE)][1:10]
eigen              = matrix(0,10,2)
eigen[,1]          = centrnodes
eigen[,2]          = eigenscores
lay                = layout.circle(Graph1)
V(Graph1)$color    = "lightblue"

plot(Graph1, main  = paste("Sub-sample industry network(tau = 0.50)", date[s], sep = "_"), edge.arrow.size=0.1,
     vertex.size = ec$vector*15+1, vertex.frame.color="#ffffff", vertex.label=colnames(Adjac), 
     vertex.label.cex = 0.7, edge.width = Adjac*3+1, vertex.label.color="black", layout = lay)

write.table(eigen, file = paste("top10centr(0.50)", s, ".txt" ))
}





```
