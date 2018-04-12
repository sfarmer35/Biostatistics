
#######################################################################################
#############################      MIDTERM    #########################################
#######################################################################################

#March 5, 2018

setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
library(plyr)
library(pwr)
#for power function
library(BSDA)
#for sign test
library(e1071)
#for skewness and kurtosis 

#parameters
  #binom
    #p,f
  #poission
    #mu
  #norm
    #mu, sd
  #t
    #df
  #f
    #df num, df denom

#1
pbinom(14, size=20, p=0.8)
14/20

#2
fish.n<- c(0:6)
fish.freq<- c(101,82, 33, 14, 3, 1, 1)
fish<- data.frame(fish.n, fish.freq)
fish.x<-weighted.mean(fish.n, fish.freq)
dpois(4,fish.x)*100

#weighted mean check 
(0*101+1*82+2*33+3*14+4*3+ 5*1+6*1)/235

1-ppois(1, fish.x)

#3
ne<- c(17.27, 23.14, 14.17, 19.36, 15.43, 15.84, 18.09, 18.43)
east<- c(20.39, 20.31, 22.81, 20.25, 20.51, 23.96, 20.48, 22.50)
shapiro.test(ne)
shapiro.test(east)
shapiro.test(log(east))
  #east not normal

var.test(ne, east)
t.test(ne, east, var.equal=TRUE)

mean(ne)+qt(0.95,7)* sd(ne)/sqrt(length(ne))
mean(ne)-qt(0.95,7)* sd(ne)/sqrt(length(ne))

wilcox.test(ne,east)

#4
1-pbinom(1, 100, 0.25)
0.5*0.09

#5
qnorm(0.25, 100, 15)
pnorm(110,100,15)- pnorm(90,100,15)
1-pnorm(131.5, 100, 15)

#6
bw<- c(2466, 3941, 2807, 3118, 2098, 3175, 3515, 3317, 3742, 3062, 3033, 2353, 2013, 3515, 
       3260, 2892, 1616, 4423)
length(bw)
mean(bw)
hist(bw, main= "")
shapiro.test(bw)
t.test(bw, mu= 3400)
t.test(bw, mu=3400, alternative= "less")
#check
(mean(bw)-3400)/ (sd(bw)/sqrt(length(bw)))
pwr.t.test(n=18, sig.level= 0.05, d=200/sd(bw), type= "one.sample", alternative= "less")

SIGN.test(bw,md=3400)
SIGN.test(bw,md=3400, alternative= "less")
