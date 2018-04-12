##############################################################################################
#####################################  SOIL PROFILES   #######################################
##############################################################################################
#April 10, 2018
setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")

soilp<- read.csv("soil profile all.csv")

#ol and all moss
test1<- data.frame(soilp$site, soilp$organic, soilp$totalmoss)

#moss colors and all moss
test2<- data.frame(soilp$site, soilp$organic, soilp$greenmoss, soilp$brownmoss)
