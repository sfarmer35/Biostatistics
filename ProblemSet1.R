#######################################################################################
############################# PROBLEM SET 1 ###########################################
#######################################################################################

setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")

library(plyr)

#6b frequency distribution
  #textbook 2.4

#make the data frame 
snake<- c(3, 1, 3, 0, 2, 2, 3, 3, 2, 0, 3, 2, 1, 3, 2)
snake1<- data.frame(snake)

hist(snake1$snake, main = " ", xlab= "Escape Response", ylab= "Number of Observations", 
     breaks = 12, right = F, xaxt= "n", xaxs= "i", yaxs= "i" )
axis(1, seq(0,3))
  #how to add spaces between the histogram bars and to get rid of 0.5 interval?

#7 
site<- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
bullfrog<- c(34, 65, 23, 34, 18, 20, 15, 70, 15, 18, 34)
dat.bullfrog<- data.frame(site, bullfrog)
dat.bullfrog$rank<- rank(dat.bullfrog$bullfrog)
