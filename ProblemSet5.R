#######################################################################################
############################# PROBLEM SET 5 ###########################################
#######################################################################################

#March 25, 2018
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

#1 Textbook 11.2 (modified by McCay)

pond<- c(0.518, 0.523, 0.495, 0.502, 0.525, 0.490)
di<- c(0.318, 0.342, 0.301, 0.390, 0.327, 0.320)
ss<- c(0.393, 0.415, 0.351, 0.390, 0.385, 0.397)
k<- c(pond, di, ss)
trt<- c(rep("p", 6), rep("di",6), rep("ss", 6))
clam<- data.frame(trt,k)

clam.a<-aov(k~trt, data=clam)
hist(clam.a$residuals)
shapiro.test(clam.a$residuals)
  #normal
leveneTest(k~trt, data=clam)
  #equal var
anova(clam.a)
  #reject H0
#total sum of squares
c.a<- anova(clam.a)
c.a[1,2]+c.a[2,2]

#2 Textbook 11.11
c<- c(535, 370, 420, 315, 485, 230, 370, 320, 335, 475)
ne<- c(15, 155, 110, 90, 35, 100, 30, 40, 75, 105)
e<- c(70, 45, 95, 95, 70, 315, 140, 260, 230, 400)
treatment<- c(c, ne, e)
group<- c(rep("c", 10), rep("ne",10), rep("e",10))
chick<-data.frame(group,treatment)

leveneTest(treatment~group, data=chick)
chick.a<- aov(treatment~group, data=chick)
shapiro.test(chick.a$residuals)

c.aov<-anova(chick.a)
View(c.aov)
c.aov[1,2]+c.aov[2,2]

#3 model 2 anova

pH3<- c(11.1,10.0,13.3,10.5,11.3)
pH5<- c(12.0, 15.3,15.1,15.0,13.2)
pH7<- c(11.2, 9.1, 9.6, 10.0, 9.8)
pH9<- c(5.6,7.2,6.4,5.9,6.3)
pH<- c(pH3, pH5,	pH7,	pH9)
group<- c(rep("three", 5),rep("five", 5), rep("seven", 5),rep("nine", 5))
enz<-data.frame(group,pH)

enz.aov<- aov(pH~group, data=enz)
anova(enz.aov)
enz.anova<-anova(enz.aov)
enz.anova[1,2]+enz.anova[2,2]

#fraction of variability explained by treatment 
ms.t<- enz.anova[1,3]
ms.e<-enz.anova[2,3]

sig.a<- (ms.t-ms.e)/5
#answer
sig.a/(sig.a+ms.e)

#assumptions
shapiro.test(enz.aov$residuals)
leveneTest(pH~group, data=enz)

#4
gen1<- c(11,14,21,27,28,30,32,36,38,49,61,71)
gen3<- c(20, 35, 36,41, 46,  47, 52, 53,58,67)
gen6<- c(31,45, 45, 47, 48,62,64, 69, 84, 86)
plot<- c(gen1,gen3,gen6)
gen.n<- c(rep("gen1", length(gen1)), rep("gen3", length(gen3)), rep("gen6", length(gen6)))
grass<- data.frame(gen.n, plot)

leveneTest(plot~gen.n, data=grass)
grass.aov<- aov(plot~gen.n, data=grass)
shapiro.test(grass.aov$residuals)

grass.anova<-anova(grass.aov)
grass.anova[1,2]+grass.anova[2,2]
View(grass.anova)

g.mst<- grass.anova[1,3]
g.mse<- grass.anova[2,3]
n0<- (length(plot)- ((length(gen1)^2)+length(gen3)^2+length(gen6)^2)/length(plot))/(3-1)
sig.a<-(g.mst-g.mse)/n0
sig.a/(sig.a+g.mse)

#5 use the kruskal-wallis
cont<- c(87,76,65,81,75)
drugA<- c(63,70,87,92,70)
drugB<- c(45,60,43,56,60)
decay<- c(cont,drugA,drugB)
group<-c(rep("cont",5), rep("drugA",5), rep("drugB",5))
tooth<- data.frame(group,decay)

kruskal.test(decay~group, data=tooth)
