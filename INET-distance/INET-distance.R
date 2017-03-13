#Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))

# set the working directory
setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata")

distance = matrix(0,21,4)

for (s in 1:21){
  #read datafiles
  industrynames    = read.csv("industrynames.csv", header = TRUE, sep = ",")
  industrynames    = industrynames[,-1]
  datapath1        = paste("dynamiclassocoeff/beta_L_lowertail", s, ".csv", sep ="")
  data1            = read.csv(datapath1, header = TRUE, sep = ",", dec = ".")
  data1            = data1[,-1]
  Adjac1           = as.matrix(data1)
  Adjac1           = abs(Adjac1)
  Adjac1 = t(Adjac1)
  rownames(Adjac1) = industrynames
  colnames(Adjac1) = industrynames
  
  datapath2        = paste("dynamiclassocoeff/beta_L_lowertail", (s+1), ".csv", sep ="")
  data2            = read.csv(datapath2, header = TRUE, sep = ",", dec = ".")
  data2            = data2[,-1]
  Adjac2           = as.matrix(data2)
  Adjac2           = abs(Adjac2)
  Adjac2 = t(Adjac2)
  rownames(Adjac2) = industrynames
  colnames(Adjac2) = industrynames

  #calculate the distances
  d1               = sum(abs(Adjac1-Adjac2))
  d2               = sqrt(sum(Adjac1-Adjac2)^2)
  d3               = max(abs(Adjac1-Adjac2))
  distance[s,1]    = d1
  distance[s,2]    = d2
  distance[s,3]    = d3
  distance[s,4]    = max(Adjac1,Adjac2)
}
#plot the l_infinity distance v.s. maximum value of neighboring adjacency matrices
matplot(1:21, distance[,3:4], type = "l", lwd = 3, col = 1:2, 
        xlab = "pairs of neighboring time windows", ylab = "distances")
#save distance tables
write.csv(distance, file = "matricesdistance_lowertail.csv")