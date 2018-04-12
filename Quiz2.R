
#######################################################################################
#############################     Lab Quiz 2  #########################################
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
library(car)
#for leveneTest (equal var in anova world)

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
eyes<- c(-1.78, -0.86, -1.35, -1.48, -1.52, -2.04, -2.83)
knees<- c(0.73, 0.31, -0.03, -0.29,-0.56 , -0.96,-1.21  )
control<- c(0.53, 0.36, 0.20, 0.37, -0.60, -0.68, -1.07)
trt<- c(eyes, knees, control)
group<- c(rep("eyes",7), rep("knees",7), rep("cont",7))
clock<- data.frame(group,trt)

leveneTest(trt~group, data=clock)
clock.aov<- aov(trt~group, data=clock)
shapiro.test(clock.aov$residuals)
table<-anova(clock.aov)
table[1,2]+table[2,2]
View(table)
es<-c(9.3, 7.2, 7.9, 8.7, 11.0)
x<-mean(es)
se<- sd(es)/sqrt(length(es))
x+se*qt(0.975,4)
x-se*qt(0.975,4)

ts<- c(11.1, 12.4, 12.2, 11.6, 11.5)
rs<- c(2.2, 2.9, 3.1, 3.0, 3.2)
shrew<-c(es,ts,rs)
species<-c(rep("es",5),rep("ts",5),rep("rs",5))
shrews<-data.frame(species,shrew)
leveneTest(shrew~species,data=shrews)
anova(aov(shrew~species,data=shrews))

#3
1-pf(1.75, 1, 97)
