##########################################################################################
########################   MCCAY INTRO SCRIPT LAB 02.20.2018   ###########################
##########################################################################################
######################## inferences and sampling distributions  ##########################
##########################################################################################

# Inferences about a single mean
#
# All of the techniques we are talking about here assume a normal distribution
# for the sample mean - that the sampling distribution is normal. Based on just a 
# single sample, we will never know this for sure, but we say what we can based on the
# sample

# Establish the simulated full distribution
pop = rnorm(10000,275,2)
  #uses randome #s
hist(pop)
  #know that whole population is normal 

# Now take a single sample from the population
M = 50
one.sample = sample(pop, size=M)
  #sample function pulls a sample of a set size (here size is 50 or M)
hist(one.sample, freq = FALSE)
# Is this distributed normally?
#could it have come from a sample normally distributed? it could by chance look really bad

#two tools to look @ if the sample is normal

  #1- look at the histogram
    #overlay a normal curve with same x and sd
curve(dnorm(x,mean(one.sample), sd(one.sample)), 271, 279, add=TRUE)
  #add=TRUE lays it on top of hist

  #2- Shapiro-Wilk Normality test
shapiro.test(one.sample)
  #tests null hypothesis that the data may have come from a population that is normally distributed
  #look @ p--value, if greater than 0.05 fail to reject H0
  #output is the W test statistic (Wilks)
# Descriptives for the one sample
mean(one.sample)
sd(one.sample)

# Now take 10000 samples and construct the sampling distribution
S = 10000
Xbar = rep(0,S)
for(i in 1:S) Xbar[i] = mean(sample(pop, size=M, replace=TRUE))
  #S is the # of times you are going to replicate it 
hist(Xbar, breaks=20)
hist(pop)
mean(Xbar)
sd(Xbar)

# How do we estimate the sd of Xbar (aka the standard error of Xbar) from 
# the sd for x? 
# standard error!
my.se = function(x) sd(x)/sqrt(length(x))
my.se(one.sample)

# Constructing the confidence bounds around the sample mean can be done the
# hard way or the easy way

# The hard way
alpha = 0.05
my.ci.lb = function(x) mean(x)-(qt(1-alpha/2,length(x)-1)*sd(x)/sqrt(length(x)))
my.ci.ub = function(x) mean(x)+(qt(1-alpha/2,length(x)-1)*sd(x)/sqrt(length(x)))
my.ci.lb(one.sample)
my.ci.ub(one.sample)

# The easy way
t.test(one.sample)
t.test(one.sample, conf.level = 0.90)
t.test(one.sample)$conf

#95% chance that that confidence interval includes mieu

# Making bar plots with error bars
  #duplicating table in handout 8
tnames=c("AS","AY","AL","BA","CH")
n=c(172,2746,579,985,1292)
means=c(8.4,8.3,13.1,10.2,11.5)
sds=c(2.2,1.3,1.9,2.2,1.6)
strees = data.frame(n, means, sds, tnames)
strees

# Fist plotting standard deviations
my.plot = barplot(strees$means, xlab = "Tree Species Code", 
                  names=strees$tnames, ylab = "Height (m)", ylim=c(0,15))
abline(h=0)
#verticals
segments(my.plot, means - sds, my.plot, means + sds, lwd=2)
#horizontals
segments(my.plot - 0.15, means - sds, my.plot + 0.15, means - sds, lwd=2)
segments(my.plot - 0.15, means + sds, my.plot + 0.15, means + sds, lwd=2)
  #lwd= line width
  #four items in command- (x,y) (x,y) and draws a line between the points 
  #my.plot means middle points of bars 


# Then plotting standard errors
strees$ses = strees$sds/sqrt(strees$n)
attach(strees)
my.plot = barplot(strees$means, xlab = "Tree Species Code", 
                  names=strees$tnames, ylab = "Height (m)", ylim=c(0,15))
abline(h=0)
segments(my.plot, means - ses, my.plot, means + ses, lwd=2)
segments(my.plot - 0.15, means - ses, my.plot + 0.15, means - ses, lwd=2)
segments(my.plot - 0.15, means + ses, my.plot + 0.15, means + ses, lwd=2)

# Finally 95% confidence intervals
alpha = 0.05
strees$CI = strees$ses*qt(1-alpha/2,strees$n-1)
attach(strees)
my.plot = barplot(strees$means, xlab = "Tree Species Code", 
                  names=strees$tnames, ylab = "Height (m)", ylim=c(0,15))
abline(h=0)
segments(my.plot, means - CI, my.plot, means + CI, lwd=2)
segments(my.plot - 0.15, means - CI, my.plot + 0.15, means - CI, lwd=2)
segments(my.plot - 0.15, means + CI, my.plot + 0.15, means + CI, lwd=2)

# Testing hypotheses about a single population
# Define the hypothesis
# Define the test statistic
# Define alpha
# Calculate test statistic
# Make a decision
# Estimate Power (sometimes)


hg = c(23.2, 24.8, 30.2, 38.4, 30.6, 35.0, 59.4, 37.0, 30.2, 24.6, 34.8,
       39.8, 44.3, 28.8, 25.4, 34.3, 41.9, 26.0, 25.7, 39.8, 26.6, 27.9,
       24.5, 26.8, 29.8, 29.1, 23.8, 30.0, 25.4, 29.9, 24.8, 34.2, 26.6, 
       38.0, 31.8, 15.2, 32.8, 29.4, 38.6, 26.9, 26.2, 42.1, 30.8, 23.6,
       15.7, 24.4, 17.0, 43.5, 32.7, 36.1, 19.3, 32.7, 26.0, 14.6, 25.8,
       42.5, 25.7, 20.0, 13.1, 10.5, 24.3, 11.1, 16.6, 16.1)
length(hg)
hist(hg, xlab="Mercury Concentration (ppb)", ylim=c(0,25), main="", 
     xlim=c(0,60),axes=FALSE)
#axes = false, allows you to put in your own x axis 
axis(1, pos=0)
axis(2, pos=0)
abline(v=mean(hg), lwd=2, col="red")
abline(v=25, lwd=2, col="green")
text(44, 23, "< Sample Mean = 28.7 ppb")
text(19.5, 23, "25 ppb >")

# Testing Ho: mu = 25
t.test(hg)$conf
t.test(hg, mu=25)
#if want to do one-sided, add (..., alternative = "greater") and would only reject in high direction


# Power = 1-B
# Depends on: n, alpha, variability, and effect size
# Effect size is the trickiest one
# Variability and effect size combined into distance measure
d = 3.7/sd(hg)
  #3.7 is difference between H0 and sample x
library(pwr)
pwr.t.test(n=64,sig.level=0.05, d=d, type="one.sample")
#low power indicates that is is not very suprising that we did not reject H0
#to increase power, increase sample size

#PROBLEMS

#BACK OF HANDOUT 8
#1 Lincoln-Peterson
#N=(n2+1)*(m+1)/(r+1)-1
#se=sqrt(((n2+1)(m+1)(n2-r)(m-r))/ ((r+1)^2)(r+2))
n.est<-(54+1)*(45+1)/(24+1)-1
=100.2
se.est<-sqrt(((54+1)*(45+1)*(54-24)*(45-24))/((24+1)^2*(24+2)))
=9.9

confidence interval
#N +- 2* SE
#100.2 +- 2*9.9
#We are 95% confident 80-120 includes the true abundance of this population

#2
setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
anage<- read.csv("anage_data.csv")
#a
hist(anage$MLS_YRS, breaks=80)
#not normally distributed, must transform 
anage$ls.ln<- log(anage$MLS_YRS)
hist(anage$ls.ln, freq=FALSE
     ,main= "", xlab= "length", ylab= "frequency")
curve(dnorm(x, mean(anage$ls.ln), sd(anage$ls.ln)), 0, 5, add=TRUE)
#the x is just there, not referring to a varibale or anything

#b
attach(anage)
x<-tapply(ls.ln, Class, mean)
sd<-tapply(ls.ln, Class, sd)
n<-tapply(ls.ln, Class, length)
detach(anage)
sum<- data.frame(x,sd,n)
#amphibia lowest, reptilia highest max age
sum$se<-sd/sqrt(n)
#magnitude of confidence error 
#t = qt(% confidence, df)
#just want the value up and down, here I calculated the mean +- these values
sum$ub<-sum$x+qt(0.975, sum$n-1)*sum$se 
sum$lb<-sum$x-qt(0.975, sum$n-1)*sum$se 
#so instead
sum$ub<-qt(0.975, sum$n-1)*sum$se
sum$lb<-qt(0.025, sum$n-1)*sum$se

#c
ls.plot<- barplot(sum$x, xlab= "Class", names = sum$row.names,
                  ylab= "Max Lifespan", ylim= c(0,3))
abline(h=0)
segments(ls.plot, sum$x + sum$lb, ls.plot, sum$x + sum$ub, lwd=2 )
segments(ls.plot - 0.15, sum$x+sum$lb, ls.plot +0.15, sum$x+sum$lb, lwd=2)
segments(ls.plot - 0.15, sum$x+sum$ub, ls.plot +0.15, sum$x+sum$ub, lwd=2)

#d
#backtransform
bt.sum<-exp(sum)
bt.sum$n<-sum$n
#accidentally exp the n values too so had to fix

bs.plot<- barplot(bt.sum$x, xlab= "Class", names = bt.sum$row.names,
                  ylab= "Max Lifespan", ylim= c(0,20))
abline(h=0)
#now that I used the exp funciton, my lb were wrong because raised e to a -, yiedling wrong #

segments(bs.plot, bt.sum$x - bt.sum$ub, bs.plot, bt.sum$x + bt.sum$ub, lwd=2 )
segments(bs.plot - 0.15, bt.sum$x-bt.sum$ub, bs.plot +0.15, bt.sum$x-bt.sum$ub, lwd=2)
segments(bs.plot - 0.15, bt.sum$x+bt.sum$ub, bs.plot +0.15, bt.sum$x+bt.sum$ub, lwd=2)
#####THIS IS WRONG BECAUSE it has to be changed due to backtransform... go see him. 


#BACK OF HANDOUT 9
#2 two-sided one-sample t test
n.2<- 100
x.2<- 195.52
sd.2<- 40
u.2<-190
t.2<- (x.2-u.2)/(sd.2/sqrt(n.2))

2*(1-pt(t.2, 99))
#fail to reject H0
pwr.t.test(n=100,sig.level=0.05, d=(x.2-u.2)/sd.2, type="one.sample")

#3
vitc<- c(988.1410, 972.2629, 1022.6358, 1013.4543, 985.5875, 988.7789, 982.2324, 
1023.5058, 1002.3445, 1005.9458)
t.test(vitc, mu= 1000)
pwr.t.test(n=length(vitc), sig.level=0.05, d=5/sd(vitc), type= "one.sample")

#4
t.4<- (10.022-10)/ (0.120/sqrt(100))
2*(1-pt(t.4,99))

#Chapter 9 questions 9.7 9.9 9.11

#9.7
t.cig<- (4.2-3.5)/(1.4/sqrt(10))
2*(1-pt(t.cig, 9))

#9.9
enz<- c(48, 50, 56, 50, 38, 43, 42,49, 49, 52, 51, 44, 44, 38, 32, 42, 45, 47, 46, 56)
t.test(enz, mu=50, alternative= "less")

#9.11
bird<- c(8.1, 8.0, 8.0, 7.7, 8.1, 7.9, 7.6, 7.6, 7.9, 8.1, 7.8, 8.2, 8.1, 8.3, 8.0, 
         8.0, 7.8, 7.9, 7.6, 8.1)
t.test(bird, mu=8, alternative= "less")
