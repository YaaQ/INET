
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **INET-distance** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : INET-distance

Published in : Industry Interdependency Dynamics in a Network Context

Description : 'calculate the distance between matrices and plot the l_infinity distance v.s.
maximum value of the matrices'

Keywords : interdependency, network, distance, structure change, stress situations

Author : Ya Qian

Submitted : Ya Qian

Datafile : 'industryname.csv, beta_L_median(1-15).csv, beta_L_lowertail(1-15).csv,
beta_L_uppertail(1-15).csv'

Output : 'Plot of distance between the l_infinity distance v.s. maximum value of the negiboring
matrices'

```

![Picture1](INET-distance(005).png)

![Picture2](INET-distance(050).png)

![Picture3](INET-distance(095).png)


### R Code:
```r
#Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))

# set the working directory
#setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

distance           = matrix(0,14,4)

for (s in 1:14){
  #read datafiles
  industrynames    = read.csv("industrynames.csv", header = TRUE, sep = ",")
  industrynames    = industrynames[,-1]
  datapath1        = paste("dynamiclassocoeff/beta_L_median", s, ".csv", sep ="")
  data1            = read.csv(datapath1, header = TRUE, sep = ",", dec = ".")
  data1            = data1[,-1]
  Adjac1           = as.matrix(data1)
  Adjac1           = abs(Adjac1)
  Adjac1 = t(Adjac1)
  rownames(Adjac1) = industrynames
  colnames(Adjac1) = industrynames
  
  datapath2        = paste("dynamiclassocoeff/beta_L_median", (s+1), ".csv", sep ="")
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
matplot(1:14, distance[,3:4], type = "l", lwd = 3, col = 1:2,  
        xlab = "pairs of neighboring time windows", ylab = "distances")
#save distance tables
write.table(format(distance,digits =4), file = "matricesdistance(050).txt", sep = "&")
```
