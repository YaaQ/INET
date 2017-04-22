#Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))

# set the working directory
setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

# install and load packages
libraries = c("dplyr", "igraph", "qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#read datafiles
industrynames      = read.csv("industrynames.csv", header = TRUE, sep = ",")
industrynames      = industrynames[,-1]
data0              = read.csv("beta_L_lowertail.csv", header = TRUE, sep = ",", dec = ".")
data0              = data0[,-1]

n                  = nrow(data0)
num                = ncol(data0)

Adjac              = as.matrix(data0)
Adjac              = abs(Adjac)
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

plot(Graph1, main = "Whole sample industry network_ tau = 0.05 (197001-201701)", edge.arrow.size=0.1,
     vertex.size = ec$vector*15+1, vertex.frame.color="#ffffff", vertex.label=colnames(Adjac), 
     vertex.label.cex = 0.7, edge.width = Adjac*3+1, vertex.label.color="black", layout = lay)

write.table(eigen, file = "top10centr(0.01).txt")





