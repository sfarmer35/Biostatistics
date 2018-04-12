#Damis script


#April 8, 2018
setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
library(plyr)
library(pwr)
#for power function
library(BSDA)
#for sign test
library(e1071)
#for skewness and kurtosis 
library(car)
#for leveneTest (equal var in anova world)
library(FSA)

slope<- read.csv("Dam_slope.csv")
aspect<- read.csv("Dam_aspect.csv")
elev<- read.csv("Dam_Elevation.csv")

View(aspect)

leveneTest(ndvi~Veg*Aspect, data=aspect)
aspect.a<- aov(ndvi~Veg+Aspect, data=aspect)
aspect.v<- aov(ndvi~Aspect+Veg, data=aspect)
anova(aspect.a)
anova(aspect.v)
