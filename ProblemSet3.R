#######################################################################################
############################# PROBLEM SET 3 ###########################################
#######################################################################################

#February 25, 2018

setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
library(plyr)
library(pwr)

#checking confidence intervals against tectbook example 9.3
se.check<- 1.76/sqrt(20)
21.0+se.check*qt(0.975, 19)
qt(0.975,19)

#1
x.bp<- 162
n.bp<- 25
sd.bp<- 12
se.bp<- sd.bp/sqrt(n.bp)
qt(0.975, 24)
qt(0.025, 24)

x.bp+se.bp*qt(0.975, 24)
x.bp-se.bp*qt(0.975, 24)

#2 
se.d<- 1.2/sqrt(25)
x.d<- 4.7

x.d+se.d*qt(0.975, 24)
x.d-se.d*qt(0.975, 24)

#3 90 and 95% confidence interval

#95%
n.act<-15
x.act<- 35.4
sd.act<-4.27
se.act<- sd.act/sqrt(n.act)
x.act+(se.act*qt(0.975, 14))
x.act-(se.act*qt(0.975, 14))
#90%
x.act+(se.act*qt(0.95, 14))
x.act-(se.act*qt(0.95, 14))

#4 
snakes<- c(6.5, 6.2, 4.9, 5.1, 4.3, 5.8, 5.0, 5.4, 4.6, 5.4, 4.7, 4.8, 6.0, 5.2, 3.4, 3.8,
           4.7, 4.8, 3.9, 6.1)
x.s<-mean(snakes)
sd.s<- sd(snakes)
n.s<-length(snakes)
se.s<-sd.s/sqrt(n.s)
#95
x.s + se.s*qt(0.975,19)
x.s - se.s*qt(0.975,19)
#99
#1-alpha/2
1-0.01/2
x.s + se.s*qt(0.995,19)
x.s - se.s*qt(0.995,19)

#5
#Mean coffee label 250. Are the labels accurate?
#one sample t test 
u.caff<-250
x.caff<- 267
n.caff<- 15
sd.caff<- 12.1
#alpha = 0.05
t.caff<- (x.caff-u.caff)/(sd.caff/sqrt(n.caff))
2*(1-pt(t.caff, 14))
#power?
pwr.t.test(n=15,sig.level=0.05, d= (10/sd.caff), type="one.sample")

#6 
merc<- c(1.030, 1.113, 1.052, 1.129, 1.057, 0.884, 1.262, 1.231, 1.039, 1.159,
         0.869, 1.024, 1.112, 1.231, 1.105)
#accepted level <= 1.0 ppm, one sided 
u.merc<- 1.0
x.merc<- mean(merc)
sd.merc<-sd(merc)
n.merc<- length(merc)
se.merc<- sd.merc/sqrt(n.merc)
#alpha = 0.05
t.merc<- (x.merc-u.merc)/se.merc
1-pt(t.merc, 14)
#easier way from lab 4
t.test(merc, mu= 1.0, alternative = "greater")
#power
pwr.t.test(n=15, sig.level= 0.05, d= ((x.merc-u.merc)/sd.merc), type= "one.sample")

#7
water<- c(346, 496, 352, 378, 315, 420, 485, 446, 479, 422, 494, 289, 436, 516, 615, 491, 
          360, 385, 500, 558, 381, 303, 434, 562, 496)
hist(water, main= "", ylim= c(0,.0075), xlab= "Water Hardness", ylab= "Frequency", breaks = 10, 
     freq= FALSE)
curve(dnorm(x, mean(water), sd(water)), 200, 700, add=TRUE)
shapiro.test(water)

x.w<- mean(water)
u.w<- 425
sd.w<- sd(water)
n.w<-length(water)
se.w<- sd.w/sqrt(n.w)
t.test(water, mu= 425, alternative= "less")

#check
t.w<- (x.w-u.w)/se.w
pt(t.w, 24)

#8 paired t test
ind<- c(seq(1:8))
rest<- c(99.0, 97.8, 98.6, 98.7, 98.7, 98.2, 98.7, 98.6)
exercise<- c(99.4, 98.1, 98.6, 98.7, 98.7, 98.2, 98.8, 99.2)
temp<- data.frame(rest,exercise, row.names= ind)
temp$diff<- temp$rest-temp$exercise
#one sample, two-sided
t.temp<- (mean(temp$diff)-0)/(sd(temp$diff)/sqrt(length(temp$diff)))
2*pt(t.temp, 7)
#simpler:
t.test(temp$diff, mu=0)
#power
pwr.t.test(n=8, sig.level = 0.05, d= 1/sd(temp$diff), type= "one.sample")

#9
number<- seq(1:15)
pretreat<- c(1.05, 1.01, 0.78, 0.98, 0.81, 0.95, 1.00, 0.83, 0.78, 1.05, 1.04, 1.03, 0.95, 1.46,
             0.78)
posttreat<-c(3.48, 5.02, 5.37, 5.45, 5.37, 3.92, 6.54, 3.42, 3.72, 3.25, 3.66, 3.12, 4.22, 2.53, 
             4.39)
chicken<- data.frame(pretreat,posttreat, row.names= number)
chicken$diff<- chicken$pretreat- chicken$posttreat
attach(chicken)
#H0: thickness difference >= 0 
t.chick<- (mean(diff)-0)/(sd(diff)/sqrt(length(diff)))
pt(t.chick, 14)
#check
t.test(diff, mu=0, alternative= "less")
detach(chicken)

#10
View(PlantGrowth)
# group:ctrl1, trt1, trt2 
# weight
attach(PlantGrowth)
x.p<-tapply(weight,group, FUN= "mean")
sd.p<-tapply(weight,group, FUN= "sd")
n.p<-tapply(weight,group, FUN= "length")
detach(PlantGrowth)
se.p<- sd.p/sqrt(n.p)
ub.p<- x.p + se.p*(qt(0.975, (n.p-1)))
lb.p<- x.p - se.p*(qt(0.975, (n.p-1)))

plant<- data.frame(x.p, sd.p, n.p,lb.p, ub.p)
attach(plant)
plant.plot<- barplot(x.p, xlab= "Treatment", ylab= "Weight (g)", ylim= c(0,6))
abline(h=0)
segments(plant.plot, ub.p, plant.plot, lb.p, lwd=2)
segments(plant.plot- 0.15, ub.p, plant.plot+0.15, ub.p, lwd=2)
segments(plant.plot- 0.15, lb.p, plant.plot+0.15, lb.p, lwd=2)
detach(plant)
#11
x.wbc<- 4767
sd.wbc<- 3204
n.wbc<-15
u.wbc<- 7250

t.wbc<- (x.wbc-u.wbc)/(sd.wbc/sqrt(n.wbc))
pt(t.wbc, 14)

