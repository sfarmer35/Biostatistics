#######################################################################################
############################# PROBLEM SET 4 ###########################################
#######################################################################################

#March 5, 2018

setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
library(plyr)
library(pwr)
  #for power function
library(BSDA)
  #for sign test

#testing f distribution calcs from notes
2*(1-pf(2^2/1.79^2,9,9 ))
#ftr equal variances

#1 (book prob 10.2)

#testing for equal variance, H0: var a= var b
bass.f<- 96.4^2/40^2
2*(1-pf(bass.f, 124, 97))
  #reject H0 --> cannot assume equal variance
#pooled standard error function
my.pse<- function(sda,sdb,na,nb){
  sqrt((sda^2/na)+(sdb^2/nb))
}

bass.t<- (272.8-164.8)/ (my.pse(96.4, 40.0, 125, 97))
#df function when not using full data set
my.df.2<- function(vara,na, varb, nb) {
    (((vara/na)+(varb/nb))^2 / ((((vara/na)^2)/(na-1))+(((varb/nb)^2)/(nb-1))))
  }
bass.df<-my.df.2(96.4^2, 125, 40^2, 97)
2*(1-pt(bass.t,bass.df))

#2   textbook 10.4

#put bigger sd first, so women are first
rxn.f<-45.988^2/ 32.643^2
2*(1-pf(rxn.f, 67, 57))
  #reject H0
rxn.t<- (181.31-170.21)/my.pse(45.988, 32.643, 68, 58)
rxn.df<-my.df.2(45.988^2, 68, 32.643^2, 58)
2*(1-pt(rxn.t, rxn.df))
#confidence interval male-female
  #textbook says use n-1 of lower sample size
  #could also use calculated out df?
#ub
(170.21-181.31)+ qt(0.975,rxn.df)*my.pse(45.988, 32.643, 68, 58)
#lb
(170.21-181.31)- qt(0.975,rxn.df)*my.pse(45.988, 32.643, 68, 58)  

#3   textbook 10.6
us<- c(10, 25, 8, 11, 19, 7, 5, 30)
ds<- c(30, 32, 28, 35, 29, 32, 32, 38, 31)

shapiro.test(us)
shapiro.test(ds)
var.test(us,ds)
  #var not equal
  #one-sided
t.test(us, ds, var.equal= FALSE, alternative= "less")

#4  textbook 10.14
treated<- c(35, 45, 36, 11, 41, 29, 38)
hist(treated, breaks = 10)
untreated<- c(10, 18, 8, 29, 17, 8, 11)
hist(untreated)
shapiro.test(treated)
shapiro.test(untreated)
var.test(treated,untreated)

t.test(treated, untreated, var.equal = TRUE)

#5   10.16 paired t test
ID<- seq(1:15)
pre<- c(20, 18, 19, 18, 17, 14, 17, 10, 13, 16, 20, 17, 16, 19, 8)
post<- c(17, 14, 16, 19, 14, 18, 8, 10, 12, 15, 8, 6, 17, 5, 3)
turkey<- data.frame(ID,pre, post)
turkey$diff<- turkey$pre - turkey$post
attach(turkey)
shapiro.test(diff)
t.test(diff)
detach(turkey)

#6   10.18

inf<- c(25.6, 27.8, 29.3, 26.9, 26.0, 25.9)
health<- c(20.8, 22.9, 26.0, 23.2, 25.1, 23.7, 25.6, 23.2)
shapiro.test(inf)
shapiro.test(health)
var.test(inf,health)
t.test(inf, health, var.equal = TRUE)

###Did wrong problem in the book for # 3 and # 6 ####

#3 actual textbook 10.6
ut<- c(16, 17, 12,  18, 11, 18, 12, 15, 16, 14, 18, 12)
ex<- c(10, 8, 10, 12, 13, 14, 6, 5, 7, 5, 10, 11, 9, 8)
shapiro.test(ut)
shapiro.test(ex)
  #both normal but ut questionable
var.test(ut,ex)
  #equal variances
t.test(ex, ut, var.equal=TRUE, alternative = "less")
  #reject null

#6 actual textbook problem 10.18
phos<- c(0.45, 0.58, 0.51, 0.52, 0.49, 0.49, 0.54, 0.52, 0.52, 0.48)
shapiro.test(phos)
  #can assume normal
t.test(phos, mu = 0.5, alternative = "greater")
