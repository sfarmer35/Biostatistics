#######################################################################################
############################# PROBLEM SET 6 ###########################################
#######################################################################################

#April 1, 2018
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

#1
dbinom(6, size=30, p= 0.065)
1-pbinom(5, size=30, p= 0.065)

#2 two sample t-test
fvc.f<- (0.73)^2/(0.63)^2
2*(1-pf(fvc.f, 65, 68))
  #assume equal variance
psd<- sqrt(((69-1)*(0.63)^2+(66-1)*(0.78)^2)/(66+69-2))
fvc.t<-(4.27-5.19-0)/sqrt(psd^2/66+psd^2/69)
2*pt(fvc.t,66+69-2)

#3 text 11.4
drugA<- c(118, 120, 125, 119, 121)
drugB<- c(105, 110,98, 106, 105)
untrt<- c(130,135, 132, 128, 130)
bp<-c(drugA, drugB, untrt)
grp<- c(rep("a",5), rep("b",5), rep("ut",5))
python<- data.frame(grp, bp)
python.aov<-aov(bp~grp, data=python)
shapiro.test(python.aov$residuals)
leveneTest(bp~grp, data=python)
anova(python.aov)
TukeyHSD(python.aov)

#4 text 11.8
high<- c(2.9, 1.8, 2.1, 1.5, 0.9, 2.8)
mod<- c(3.7, 4.6, 4.2, 2.8, 3.2, 4.5)
low<- c(5.2, 6.2, 5.7,4.6, 5.1, 4.9)
none<-c(6.8, 7.9, 7.1, 6.5, 6.3, 7.6)
stress<- c(high, mod,low, none)
level<- c(rep("h",6), rep("m",6), rep("l", 6), rep("n",6))
rugrat<- data.frame(level, stress)

r.aov<-aov(stress~level, data=rugrat)
shapiro.test(r.aov$residuals)
leveneTest(stress~level, data=rugrat)
kruskal.test(stress~level, data=rugrat)
library(FSA)
dunnTest(rugrat$stress, rugrat$level, method="holm")

#5 11.10
chemA<-c(115, 103, 98, 121, 130, 107, 106, 120,100,125)
chemB<-c(120, 125, 122, 100, 90, 128, 121, 115, 130, 120)
cont<- c(82, 97, 105, 90, 102, 98, 105, 89, 100, 90)
rl<- c(chemA, chemB,cont)
groups<-c(rep("A",10), rep("B",10),  rep("cont",10))
pea<- data.frame(groups, rl)

leveneTest(rl~groups, data=pea)
pea.a<- aov(rl~groups, data=pea)
shapiro.test(pea.a$residuals)

anova(pea.a)
  #no contrast because need to know if A and B differ from control
TukeyHSD(pea.a)
          
          
#6 11.12
sA<- c(439, 568, 134, 897, 229, 329)
sB<- c(102, 115, 98, 126, 115, 120)
sC<- c(107, 99, 102, 105, 89, 110)
test<- c(sA,sB,sC)
grp<- c(rep("a",6), rep("b",6), rep("c",6))
rooster<-data.frame(grp,test)

rooster.aov<-aov(test~grp, data=rooster)
shapiro.test(rooster.aov$residuals)
leveneTest(test~grp, data=rooster)

kruskal.test(test~grp, data=rooster)
dunnTest(rooster$test, rooster$grp, method= "holm")

#7
zero<- c(4.4, 3.3, 5.0, 5.3, 4.1, 5.0, 4.6)
two<- c(4.9, 5.3, 6.4, 5.3, 6.8, 6.0, 5.2)
four<- c(5.2, 6.9, 7.2, 7.4, 9.6, 6.8, 6.6)
six<- c(5.7, 7.0, 7.7, 7.0, 11.5, 8.3, 7.4)
eight<- c(5.7, 8.8, 9.3, 8.3, 12.0, 8.1, 7.1)
chloride<- c(zero, two, four, six, eight)
exp<- c(rep("z",7), rep("two",7), rep("four", 7), rep("six",7), rep("eight",7))

hemo<- data.frame(exp,chloride)

leveneTest(chloride~exp, data=hemo)
hemo.a<- aov(chloride~exp, data=hemo)
shapiro.test(hemo.a$residuals)

hemo$log<- log(hemo$chloride)
hemo.a<-aov(log~exp, data=hemo)
shapiro.test(hemo.a$residuals)
leveneTest(log~exp, data= hemo)

log.a<-anova(hemo.a)
variability<- (log.a[1,3]-log.a[2,3])/ 7
frac.v<- variability/(variability+log.a[2,3])
frac.v*100

#8
moz<- c(12.56, 15.16, 11.91, 13.42, 10.91)
beet<- c(9.24, 11.29, 15.08, 11.20, 13.10)
cont<- c(9.63, 14.37, 9.26, 13.79, 11.84)
time<- c(moz, beet, cont)
music<- c(rep("moz",5), rep("beet",5), rep("cont",5))
hog<- data.frame(music,time)

leveneTest(time~music, data=hog)
hog.aov<-aov(time~music, data=hog)
shapiro.test(hog.aov$residuals)

anova(hog.aov)
