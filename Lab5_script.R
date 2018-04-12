##########################################################################################
########################   MCCAY INTRO SCRIPT LAB 02.27.2018   ###########################
##########################################################################################
########################           two sample t-test            ##########################
##########################################################################################

# Script for Lab 5

#PAIRED T-TEST
# The paired t-test is simply a one-sample t-test on the differences within 
# each sample unit. Example from text, in Table 9.2

patient = c(1:20)
old = c(23.5, 22.3, 20.2, 22.5, 21.3, 
        23.2, 23.7, 22.7, 20.2, 20.9,
        22.3, 23.1, 20.3, 25.1, 24.8,
        22.0, 21.3, 23.2, 21.3, 22.0)
new = c(19.5, 20.1, 19.1, 19.2, 21.2,
        19.6, 19.8, 20.2, 17.7, 18.6,
        20.8, 21.1, 18.5, 16.1, 17.3, 
        22.4, 21.5, 18.6, 20.5, 20.4)
diff = old-new
wound = data.frame(patient, old, new, diff)
# This is a one-sided test, testing for improvement to healing time
# So, Ho: diff<=0
# Two equivelent ways to do this.
t.test(diff, alternative="greater") # remember assumes mu=0
t.test(old, new, paired=TRUE, alternative="greater")
  #this second way is more confusing because which one is greater?
  #order mattters- here old is greater than new
#in R always specify the directionality of the alternative

# Two-sample ("independent samples") t-test
# Problem 10.5. Liver dehydrogenase activity upstream (us) and 
# downstream (ds) of a brewery.
us = c(10,25,8,11,19,7,5,30)
ds = c(30,32,28,35,29,32,32,38,31)

# No need to worry about this part - just a function to make semi-transparent
# colors for charts. From Mark Gardener, dataanalytics.uk.org

t_col <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  invisible(t.col)
}


# Testing assumptions - normality

mycol <- t_col("red", perc = 50)
hist(us, xlim=c(0,40), col=mycol, breaks = 10, freq=FALSE, main="", 
     xlab="Liver Dehydrogenase Activity (mmol/min/mg", ylim=c(0,.25))
mycol <- t_col("blue", perc = 50)
hist(ds, add=TRUE, col=mycol, freq=FALSE)
curve(dnorm(x, mean(us), sd(us)), 0,40, add=TRUE, col="red", lwd=2)
curve(dnorm(x, mean(ds), sd(ds)), 0,40, add=TRUE, col="blue", lwd=2)
text(11.5,.09,"Upstream")
text(30,.24,"Downstream")
shapiro.test(us)
shapiro.test(ds)
# Conclude that it is OK ... I suppose
# Testing assumptions - equal variances
  #hmmm they are looking pretty spread out but can we test?

var.test(us,ds)

#checking math
2*(1-pf(sd(us)^2/sd(ds)^2, length(us)-1 , length(ds)-1))

# Conclude not equal variance

mean(us)
sd(us)
length(us)
mean(ds)
sd(ds)
length(ds)
  #could do the t test without cheating by plug and chug into equations

t.test(us, ds, var.equal = FALSE, alternative = "less") 
# two sample because give it to vectors to work with
# var.equal tells whether you want to assume equal variances
# BE CAREFUL!!! LESS MEANS THE FIRST vector IS LESS THAN THE SECOND vector.

# In case you need to do a two-sample test the hard way, here are some
# scripts for the harder equations

# Determining degrees of freedom for test without equal variance assumption
my.df = function(a,b) {
  ((var(a)/length(a))+(var(b)/length(b)))^2 / ((((var(a)/length(a))^2)/(length(a)-1))+((var(b)/length(b))^2)/(length(b)-1))
}
my.df(us,ds)


# Calculating the pooled standard deviation
my.sp = function(a,b) {
  sqrt( 
    ( ( (length(a)-1)*var(a) ) + ((length(b)-1)*var(b)) )  / (length(a)+length(b)-2) 
  )
}
sd(us)
sd(ds)
my.sp(us,ds)
  #expect pooled standard deviatino to be roughly in the middle 
  #weighted by n of each sample

# Power of two-sample test
# Same as for one-sample, except n = lowest n, and sd = pooled standard deviation

pwr.t.test(n=length(us), sig.level = 0.05, type="two.sample", d = 10/my.sp(us,ds))
  #use smaller of the two sample size with the power function, so in this case it is upstream
  #d= difference want to detect / pooled standard deviation

# Sign Test - nonparametric alternative to one-sample t-test / paired t-test
  #if normality FAILS

svl = c(6.5, 4.3, 4.6, 6.0, 4.7, 
        6.2, 5.8, 5.4, 5.2, 4.8,
        4.9, 5.0, 4.7, 3.4, 3.9,
        5.1, 5.4, 4.8, 3.8, 6.1)
# Test Ho: Median is 4 with sign test
sort(svl)
length(svl[which(svl<4)])
length(svl[which(svl>4)])
# There are 3 observations less than 4 and 17 above 4
barplot(dbinom(c(0:20),20,.5),names.arg = c(0:20), 
        xlab="Successes out of 20 Trials")
abline(h=0)
# Now we pick lower number and calculate probability
2*pbinom(3,20,.5)
  #= probability of # this extreme
  #multiply by 2 because two sided
  #reject null hypothesis
# Or use BSDA Package, SIGN.test function
library(BSDA)
SIGN.test(svl,md=4)
  #md = hypothetical median

# Mann-Whitney U Test, nonparamteric alternative to two-sample test
wilcox.test(us, ds, alternative = "less")
# Conclusion is phrased in terms of median or "location of central tendency"
#error message is fine, its just telling us our ranks have ties

#PROBLEMS 10.1, 10.7, 10.11, 10.13, 10.15, 10.17

#10.1
#pooled standard error function
my.pse<- function(sda,sdb,na,nb){
  sqrt((sda^2/na)+(sdb^2/nb))
}
pse.cran<-my.pse(0.25, 0.27,25,27)
t.cran<-(1.31-1.27) / pse.cran
df.cran<- (((0.25^2/25)+(0.27^2)/27)^2)/((((0.25^2)/25)^2/(25-1))+((0.27^2/27)^2/(27-1)))
(1-pt(t.cran,df.cran))*2


#10.7
gluc<- c(6.3, 5.7, 6.8,6.1, 5.2)
suc<- c(5.8, 6.2,  6.0, 5.1, 5.8)
shapiro.test(gluc)
shapiro.test(suc)
  #both normal
var.test(gluc,suc)
  #use equal variances
t.test(gluc, suc, var.equal= TRUE)
#fail to reject

#10.11
hairy<- c(10, 10, 8, 9, 7, 9, 9, 5, 9, 8)
nothairy<- c(7,5,6,4,8,5, 6,6,1,3)
wilcox.test(hairy,nothairy, alternative= "greater")
  #the format describes the first value, so hairy is greater than not

#10.13
control<-c(1631, 50102, 1369, 41188, 387, 498, 259, 329, 4330, 5002, 658, 300)
treat<- c(87700, 69553, 76215, 366, 40104, 38661, 141153, 154805, 123075, 627, 126175,
          11223, 300)
shapiro.test(control)
shapiro.test(treat)
  #not normal 
wilcox.test(control, treat)
#they are significantly different, reject H0 

#10.15
spray<- c(3,0,1,5,2,1,5,3,6,0,2,6,5)
unspray<- c(2,5,6,3,3,4,8,2,1,8)
wilcox.test(spray,unspray)
#fail to reject

#10.17
infect<- c(25.6,27.8, 29.3, 26.9, 26.0, 25.9)
health<- c(20.8, 22.9, 26.0, 23.2, 25.1, 23.7, 25.6, 23.2)
shapiro.test(infect)
shapiro.test(health)
var.test(infect,health)
#normal and equal variance
t.test(infect, health, var.equal=TRUE)
#there is a difference, reject null hypothesis of same
#gives you confidence ite
#long way for confidence interval
