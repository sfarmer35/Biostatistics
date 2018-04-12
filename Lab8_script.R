##########################################################################################
########################   MCCAY INTRO SCRIPT LAB 02.27.2018   ###########################
##########################################################################################
########################       ANOVA for factorial designs      ##########################
##########################################################################################

#April 2, 2018

# Running ANOVA for Factorial Designs
# Make sure that all factors are considered "factors" in R
ToothGrowth
summary(ToothGrowth)
  #see that it thinks length is continuous quant (gives quantiles) and supp is a factor (level: #of obs)
  #dose is coming up as a continuos variable
ToothGrowth$dose = as.factor(ToothGrowth$dose)
  #make sure R knows explanatory variable is a factor 
my.anova = aov(len~supp*dose, data = ToothGrowth)
  #reesponse~explanatoryvariable1*explanatoryvarible2
  #explanatory variable order doesn't matter
anova(my.anova)
# Technical assumptions --> check in same way
hist(my.anova$residuals, freq = FALSE, main = "", xlab = "Residuals (mm)")
curve(dnorm(x,mean(my.anova$residuals),sd(my.anova$residuals)),-10,10, 
      add=TRUE)
shapiro.test(my.anova$residuals)
leveneTest(len~supp*dose, data = ToothGrowth)

# Visualizing results
tapply(ToothGrowth$len,ToothGrowth$dose,mean)
  #pull out means
tapply(ToothGrowth$len,ToothGrowth$supp,mean)

interaction.plot(ToothGrowth$supp,ToothGrowth$dose,ToothGrowth$len,
                 trace.label = "Dose")
  #give three factors: x (factor 1), line (factor 2),y (response)
#creating a better intereaction plot
interaction.plot(ToothGrowth$supp,ToothGrowth$dose,ToothGrowth$len,
                 type="b", ylab = "Tooth Growth (mm)", xlab = "Supplement",
                 lty=c(1,2,3), lwd = 2, pch = c(0,19,2), legend = F,
                 ylim = c(0,30))
legend("bottomleft", c("0.5 mg/day","1.0 mg/day","2.0 mg/day"),bty="n",lty=c(1,2,3),
       lwd=2,pch=c(0,19,2), title="",inset = .02)

interaction.plot(ToothGrowth$dose,ToothGrowth$supp,ToothGrowth$len,
                 type="b", ylab = "Tooth Growth (mm)", xlab = "Dose",
                 lty=c(1,3), lwd = 2, pch = c(0,19), legend = F,
                 ylim = c(0,30))
legend("bottomright", c("Orange Juice","Ascorbic Acid"),bty="n",lty=c(1,3),
       lwd=2,pch=c(0,19), title="",inset = .02)

# Model statements
anova(aov(len~supp*dose, data = ToothGrowth))
anova(aov(len~supp+dose+supp*dose, data = ToothGrowth))
anova(aov(len~supp+dose, data = ToothGrowth))
  #includes main effect but not interaction, cannot just do interaction
  #important for excluding interaction when doing randomized complete block design

# Randomized Complete Block
watershed = c(5,6,8,10,5,6,8,10)
treatment = c(rep("CON",4),rep("LIM",4))
LossPct = c(0.3855000, 0.4456000, 0.4698000, 0.4767000, 
            0.2865000, 0.3931111, 0.3691250, 0.4223750)
decomp = data.frame(watershed,treatment,LossPct)
decomp$loss=asin(sqrt(decomp$LossPct))
decomp
summary(decomp)
decomp$watershed = as.factor(decomp$watershed)
decomp$treatment = as.factor(decomp$treatment)
summary(decomp)
my.anova=aov(loss~watershed+treatment, data=decomp)
  #+ sign means NO interaction
  #watershed first because blocking factor 
  #remember no appropriate to report significance of blocking factor
anova(my.anova)
my.anova=aov(loss~treatment, data=decomp)
anova(my.anova)
  #this is the beaty of blocking! it makes for a more powerful test

boxplot(loss~treatment, data=decomp, names=c("Control","Limed"), ylab = "Transformed Mass Loss")

# Unbalanced designs and types of sums of squares
mass<-c(560,500,350,520,540,620,
        600,560,450,340,440,
        530,580,520,460,340,640,520,560,
        410,540,340,580,470,550,
        480,400,600,450,420,550,
        550,420,370,600,440,560)
sex<-c(rep("M",11),rep("F",8),rep("M",12),rep("F",6))
trt<-c(rep("CON",19),rep("THY",18))
chicks<-data.frame(mass,sex,trt)
head(chicks)

# These are Type I (Sequential) SS
anova(lm(mass~sex*trt,data=chicks))
  #lm is equivalent functionto aov
anova(lm(mass~trt*sex,data=chicks))
  #showing that order matters

# To calculate Type II or Type III SS, Must Run This
library(car)
options(contrasts = c(unordered="contr.sum", ordered="contr.poly"))
  #must run this line which changes some of the contrast options

# Then use "Anova" function in package car
Anova(lm(mass~sex*trt,data=chicks),type="II")
Anova(lm(mass~sex*trt,data=chicks),type="III")

# Non-parametric 2-way ANOVA
  #kruskal test (long way) for multiple factors 
food<-c(709,679,699,657,594,677,592,538,476,508,505,539)
sex<-c(rep("M",3),rep("F",3),rep("M",3),rep("F",3))
fat<-c(rep("FR",6),rep("RN",6))
rat<-data.frame(sex,fat,food)
rat
rat$food.r<-rank(rat$food)
np.aov<-aov(food.r~sex*fat,data=rat)
anova(np.aov)
MSTOT = sum(8.333,108,5.333,21.333)/sum(1,1,1,8)
  #adding all ss / all df 
Hsex = 8.333/MSTOT
Hfat = 108/MSTOT
Hint = 5.333/MSTOT
  
Hsex
  #test stat
1-pchisq(Hsex,1)
  #p-value
Hfat
1-pchisq(Hfat,1)
Hint
1-pchisq(Hint,1)

#summary: ranking data, running anova, using MS to calculat H and comparing to chi sq

#PRACTICE PROBLEMS

#1 textbook 12.5
lake<- c(rep(c("1","2","3","4"),3))
lake<-as.factor(lake)
surf<- c(425, 500, 100, 325)
one<- c(130, 215, 30, 100)
three<- c(56, 115, 10,28)
chloro<-c(surf,one,three)
depth<- c(rep("surf", 4), rep("one",4), rep("three", 4))
algae<- data.frame(chloro, depth, lake)
leveneTest(chloro~depth, data=algae)
  #equal variance 
  #only look at factor interested in 

algae.a<-aov(chloro~lake+depth, data=algae)
shapiro.test(algae.a$residuals)
  #normal
anova(algae.a)

TukeyHSD(algae.a)

#12.9 

low<-c(5,8,13, 9, 15, 11, 12, 19, 15, 20, 11, 18, 37, 42, 50, 35, 40, 36)
moderate<-c(115,122, 119, 130,114, 129, 112, 115, 121, 117, 118, 120, 157, 160, 173, 182, 168,170)
high<- c(253,249,260, 257, 280, 263, 219, 222, 218, 220, 223, 225, 289, 273, 280, 291, 205, 296 )
plasma<-c(low,moderate,high)
length(low)
crowd<- c(rep("low", 18), rep("med",18), rep("hi",18))
crowd<-as.factor(crowd)
sex<- c(rep(c(rep("males", 6), rep("ngf",6), rep("gf",6)),3))
sex<-as.factor(sex)
corti<- data.frame(crowd,sex,plasma)

leveneTest(plasma~crowd*sex, data=corti)
  #equal variane
corti.a<-aov(plasma~crowd*sex, data=corti)
shapiro.test(corti.a$residuals)
  #not norrmal....continue anyway

corti.tab<-anova(corti.a)
  #significant interaction
  #random interaction signif
#cannot trust the F value for the fixed factor in the fixed vs. random scenario
f.sex<-corti.tab[2,3]/corti.tab[3,3]
1-pf(f.sex, 2,4)
  #sex is signif
#to check the random factor F/p
f.crowd<-corti.tab[1,3]/corti.tab[4,3]
1-pf(f.crowd,2,45)

#percet of var explained?
  #MScrowding-MSE/ n*a
  #get this equation by solving for the additional variance componenet in equations in H15 page 8
anova(corti.a)
sig.a<- (corti.tab[1,3]-corti.tab[4,3])/(6*3)
sig.a/(sig.a+corti.tab[4,3])

#3
bp<- c(108, 110, 90, 80, 100, 120, 125, 130, 120, 130, 145, 150, 130, 155, 140,
       110, 105, 100, 90, 102, 110, 105, 115, 100, 120, 130, 125, 135, 130, 120)
sex<- c(rep("male",15), rep("female",15))
age<- c(rep(c(rep("ad",5), rep("mat",5), rep("old",5)),2))
hamster<- data.frame(sex,age,bp)

ham.a<-aov(bp~sex*age, data=hamster)
anova(ham.a)

interaction.plot(hamster$sex, hamster$age, hamster$bp,
                 type="b", ylab = "Plasma", xlab = "Supplement",
                 lty=c(1,2,3), lwd = 2, pch = c(0,19,2), legend = F,
                 ylim = c(75,150))
legend("bottomleft", c("adolescent", "mature", "old"),bty="n",lty=c(1,2,3),
       lwd=2,pch=c(0,19,2), title="",inset = .02)

interaction.plot(hamster$age, hamster$sex, hamster$bp,
                 type="b", ylab = "Plasma", xlab = "Supplement",
                 lty=c(1,2,3), lwd = 2, pch = c(0,19,2), legend = F,
                 ylim = c(75,150))
legend("bottomleft", c("male", "female"),bty="n",lty=c(1,2,3),
       lwd=2,pch=c(0,19,2), title="",inset = .02)

#4
setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
heart<- read.csv("Heart.csv")
head(heart)
heart$Diet<-as.factor(heart$Diet)
heart$Training<- as.factor(heart$Training)

leveneTest(Triglycerides~Diet*Training, data=heart)
  #equal var
heart.a<- aov(Triglycerides~Diet*Training, data=heart)
shapiro.test(heart.a$residuals)
  #not normal
  #would not use anova

#but we are using anova anyway
anova(heart.a)
