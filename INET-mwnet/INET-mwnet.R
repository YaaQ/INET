#Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))

# set the working directory
setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata")

# install and load packages
libraries = c("dplyr", "igraph", "qgraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

date                 = c("200501-200512", "200507-200606", "200601-200612", "200607-200706", 
                           "200701-200712", "200707-200806", "200801-200812", "200807-200906", 
                           "200901-200912", "200907-201006", "201001-201012", "201007-201106", 
                           "201101-201112", "201107-201206", "201201-201212", "201207-201306", 
                           "201301-201312", "201307-201406", "201401-201512", "201407-201506", 
                           "201501-201512", "201507-201606")
for (s in 1:22){
  #read datafiles
  industrynames      = read.csv("industrynames.csv", header = TRUE, sep = ",")
  industrynames      = industrynames[,-1]
  datapath           = paste("dynamiclassocoeff/beta_L_upperertail", s, ".csv", sep ="")
  data0              = read.csv(datapath, header = TRUE, sep = ",", dec = ".")
  data0 = data0[,-1]
  
  n                  = nrow(data0)
  num                = ncol(data0)
  
  Adjac              = as.matrix(data0)
  Adjac              = abs(Adjac)
  Adjac              = t(Adjac)
  rownames(Adjac)    = industrynames
  colnames(Adjac)    = industrynames

#plot interdependency networks and find out top 10 leading industries 
  Graph1             = graph_from_adjacency_matrix(Adjac, mode = "directed", weighted = TRUE, 
                                                   diag = FALSE, add.colnames = TRUE)
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
  
  pdf(file = paste("output/mwnet_uppertail", s ,".pdf", sep =""))
  plot(Graph1, main = paste("Industry network - tau = 0.95", date[s], sep = "_"), edge.arrow.size=0.1,
       vertex.size = ec$vector*15+1, vertex.frame.color="#ffffff", vertex.label=colnames(Adjac), 
       vertex.label.cex = 0.7, edge.width = Adjac*3+1, vertex.label.color="black", layout = lay)
  dev.off()
  write.table(eigen, file = paste("top10centr(0.95)", s, ".txt", sep = ""))
}

