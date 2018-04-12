#######################################################################################
############################# PROBLEM SET 7 ###########################################
#######################################################################################

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
#

#1  12.2
diet<- c(65, 60, 55, 53, 64, 75, 69, 50, 54, 69, 80, 79, 70, 80, 85)
dtype<- c(rep("A", 5), rep("B", 5),rep("C", 5))
loc<- c(rep(seq(1:5),3))
loc<-as.factor(loc)
toad<- data.frame(loc,dtype,diet)

leveneTest(diet~dtype, data=toad)
  #normal?
toad.a<-aov(diet~loc+dtype, data=toad)
shapiro.test(toad.a$residuals)

Anova(toad.a)
TukeyHSD(toad.a)

#2
setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
mollusc<- read.csv("mollusc.csv")
View(mollusc)
#one factor, model II ANOVA, unequal sample sizes
  #treatment-> Sp_number   variable-> Impulse
mollusc$Sp_number<- as.factor(mollusc$Sp_number)
leveneTest(Impulse~Sp_number, data= mollusc)
  #reject equal variances
moll.a<- aov(Impulse~Sp_number, data= mollusc)
shapiro.test(moll.a$residuals)
  #fail to reject normal  
#says to use ANOVA anyways
anova(moll.a)
moll.anova<-anova(moll.a)

count(mollusc$Sp_number==1)
count(mollusc$Sp_number==2)
count(mollusc$Sp_number==3)
count(mollusc$Sp_number==4)
count(mollusc$Sp_number==5)
attach(mollusc)
n.sum<- (54^2+19^2+10^2+ 8^2 + 34^2)
n0<- (length(Impulse)-n.sum/length(Impulse))/4
detach(mollusc)
sig.a<- (moll.anova[1,3]-moll.anova[2,3])/n0
sig.a/(sig.a+moll.anova[2,3])

#3 text 12.4
growth<- c(105, 98, 125, 100, 130, 80, 156, 145, 170, 150, 185, 135, 187, 167, 201, 180, 210, 162)
nitro<- c(rep("none", 6), rep("ten",6), rep("hund", 6))
block<- c(rep(seq(1:6),3))
yield<- data.frame(block,nitro,growth)

leveneTest(growth~nitro, data=yield)
yield.a<- aov(growth~block+nitro, data=yield)
shapiro.test(yield.a$residuals)
anova(yield.a)

contrasts(yield$nitro)<-cbind(c(-1, 2, -1), c(1,0,-1))
contrasts(yield$nitro)
yield.a2<- aov(growth~block+nitro, data=yield)
summary(yield.a2, split=list(nitro=list("0 vs 10/100"=1, "10 vs 100"=2)))

#4
dbinom(2, 2, 0.5)

#5
offsp<- c(18, 20, 15, 27, 30, 20, 28, 30, 17, 29, 35, 30, 32, 28, 35,
          25, 30, 19, 30, 25, 28, 29, 32, 38, 29, 35, 39, 30, 40, 38,
          28, 36, 29, 30, 37, 33, 39, 42, 47, 38, 51, 42, 48, 39, 55)
feeds<- c(rep("one", 15), rep("two", 15), rep("three", 15))
temp<- c(rep(c(rep("70", 5), rep("75",5), rep("80",5)),3))
gup<- data.frame(temp,feeds, offsp)
gup$temp<- as.factor(gup$temp)

leveneTest(offsp~temp*feeds, data= gup)
gup.a<- aov(offsp~temp*feeds, data= gup)
shapiro.test(gup.a$residuals)
gup.aa<-anova(gup.a)
#both random factors so need to calc out proper F and p 
temp.f<- gup.aa[1,4]/gup.aa[3,4]
1-pf(temp.f,2,4)
feed.f<-gup.aa[2,4]/gup.aa[3,4]
1-pf(feed.f, 2, 4)

#6
chick<- read.csv("chickens.csv")
View(chick)
#fixed factors: Base, Methionine #response: Drumstick
count(chick$Base==1)
count(chick$Methionine==1)
  #balanced
chick$Base<- as.factor(chick$Base)
chick$Methionine<- as.factor(chick$Methionine)

leveneTest(Drumstick~Base*Methionine, data= chick)
  #the variances are not equal
chick.a<- aov(Drumstick~Base*Methionine, data= chick)
shapiro.test(chick.a$residuals)

#two way anova kruskal-wallis
chick$r<- rank(chick$Drumstick)
chick.npa<- aov(r~Base*Methionine, data=chick)
anova(chick.npa)
aov<-anova(chick.npa)
ms.tot<- sum(aov[1,2], aov[2,2], aov[3,2], aov[4,2])/sum(1,1,1,236)
Hbase<- aov[1,2]/ms.tot
Hmeth<- aov[2,2]/ms.tot
Hint<- aov[3,2]/ms.tot

1-pchisq(Hbase,1)
1-pchisq(Hmeth,1)
1-pchisq(Hint,1)

tapply(chick$r, chick$Base, mean)
tapply(chick$r, chick$Methionine, mean)
interaction.plot(chick$Base, chick$Methionine, chick$r, type= "b",
                 ylab= "Mean Rank", xlab = "Base", lty= c(1,2), lwd = 2, pch= c(0, 19), legend = F,
                 ylim = c(40,160))
legend("bottomleft", c("M", "no M"), bty= "n", lty= c(1,2), lwd = 2, pch= c(0,19), title = "", )

dunnTest(chick$Drumstick, chick$Methionine, method="holm")
  #error...

#7 12.14 

oxi<-c( 102, 115,98, 97, 85, 63, 127, 150, 168, 237, 219, 201, 193, 175, 160, 230, 249, 250,
        117,95, 128, 105, 60, 91, 135, 145, 170 )
virus<- c(rep("none", 9), rep("TMV",9), rep("TRSV",9))
strain<- c(rep(c(rep("A",3), rep("B",3),rep ("C",3)),3))
tobacco<- data.frame(virus,strain,oxi)
leveneTest(oxi~virus*strain, data=tobacco)
  #equal var
tobacco.a<- aov(oxi~virus*strain, data=tobacco)
shapiro.test(tobacco.a$residuals)
  #fail to reject h0...close call tho
anova(tobacco.a)
TukeyHSD(tobacco.a)

interaction.plot(tobacco$strain,tobacco$virus, tobacco$oxi, xlab= "Strain", ylab= "Oxidase Activity",
                 type= "b", lwd=2, pch= c(0,19,2), legend= F, ylim= c(25,250))
legend("bottomright", c("None","TMV", "TRSV"),bty="n",lty=c(1,3),
       lwd=2,pch=c(0,19,2), title="",inset = .0)
