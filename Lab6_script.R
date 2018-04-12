##########################################################################################
########################   MCCAY INTRO SCRIPT LAB 03.20.2018   ###########################
##########################################################################################
######################## single factor ANOVA and kruskal-wallis ##########################
##########################################################################################

druga = c(17.1,19.4,17.1)
drugb = c(18.1,20.0,19.1)
contr = c(11.2,11.6,11.3)
# Create a combined vector for all jump distances
Distances = c(druga,drugb,contr)
# Create a vector of identifiers for the groups
Factors = c(rep("A",3), rep("B",3), rep("C",3))
# Create a data frame including these data
Frogs = data.frame(Factors, Distances) 
Frogs
# Visualizing the data
boxplot(Distances ~ Factors, data = Frogs, ylab="Distances (ft)",
        xlab="Group")
# Conducting the ANOVA
my.anova=aov(Distances~Factors, data = Frogs)
anova(my.anova)
my.anova=lm(Distances~Factors, data = Frogs)
  #this function doesn't output, have to request something specific about results
  #if reporting ANOVA table, remember the total df and total ss
  #within= residuals
anova(my.anova)
  #no differences in these models for one factor ANOVA
  #however later on the "lm" one must be used 
1-pf(57,2,6)

# That was a fixed-factor (Model I) example. Now for a random-factor (Model II)
# example
setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
dung <- read.csv("dung.csv")
head(dung)
op <- par(mar = c(12,4,4,2) + 0.1)
    #changes the margins to give self more room at bottom
    #rotates x axis labels 

boxplot(BLL ~ Species, data = dung, ylab="Back leg length (mm)",
        xlab="", las=3)
op <- par(mar = c(4,4,4,2) + 0.1)
my.anova=aov(BLL~Species, data = dung)
anova(my.anova)
### Testing assumptions of ANOVA ###

# Normality
my.anova=aov(Distances~Factors, data = Frogs)
hist(Distances, breaks=20)
#wouldn't expect this to be normal because we expect lots of variation in our groups
hist(my.anova$residuals, freq=FALSE, 
     xlab = "Residual of Differences", main="")
#so instead to check normality, run ANOVA which creates vector of residuals, check those residuals 
curve(dnorm(x,mean(my.anova$residuals), sd(my.anova$residuals)), -1,
      2, add=T)
shapiro.test(my.anova$residuals)
qqnorm(my.anova$residuals)
  #qq plot --> bivariate plot of theoretical and sample quantiles
  #essentially compares probability of vallue to expected value under the normal
    #he hasn't emphasized this because it rarely changes the outcome from decision of curve or shapiro

# Equal variances

#this is shows what levene test is doing
Frogs$Means = c(rep(mean(Frogs$Distances[Frogs$Factors=="A"]), 3),
                rep(mean(Frogs$Distances[Frogs$Factors=="B"]), 3),
                rep(mean(Frogs$Distances[Frogs$Factors=="C"]), 3)
)
Frogs$Z = abs(Frogs$Distances-Frogs$Means)
Frogs
anova(aov(Z~Factors, data = Frogs))

#download car package for levene test
library(car)
leveneTest(Distances~Factors, data=Frogs, center="mean")
  #uses F statistic, accept H0 that the variances are equal
  #others have proposed that the median is better for developing residual values from
  #when  using this text in action, use the default (no center = "mean")

# Now for nonparametric "ANOVA"
dogsmarts <- read.csv("dogsmarts.csv")
View(dogsmarts)
dogsmarts$rTrainability = rank(dogsmarts$Trainability)
  #trainability not really a quantatative variable
  #step one is to rank the variable
View(dogsmarts)
my.rankoav = aov(rTrainability ~ Size, data=dogsmarts)
  #ran anova on ranks
  #doing kruskal-wallis need to calc H statistic
a = anova(my.rankoav)
  #saved anova table
  #a r considers a matrix, then referring to different cells within the matrix
a
H = a[1,2]/((a[1,2]+a[2,2])/(a[1,1]+a[2,1])-1)
H
  #this is our test statistic, new probability distibution chi squared
# Distributed as Chi-squared with df = among df
#let's look at threee diff chi square distributions with various degrees of freedom
curve(dchisq(x,3),0,10, xlab="Value of the Statistic", ylab="Probability")
curve(dchisq(x,4),0,10, col= "red", add=T)
curve(dchisq(x,5),0,10, col="blue", add=T)
1-pchisq(4.694,2)
#or could just use the kruskal-wallis test function

# Built in KW command
kruskal.test(Trainability ~ Size, data=dogsmarts)

#if it recognizes as a charachter and not a factor use
as.factor(dogsmarts$Size)

#PROBLEMS

#Handout 13 

#1
#a this is a model one because we care about the individual
#effects of each of the three food types

#b/c
mw<- c(1.61,1.45, 1.65, 1.08, 1.42, 1.26, 1.70, 1.33, 1.55,
       0.61)
ss<- c(1.09, 0.16, 0.83, 0.70, 0.82, 0.55, 0.36, 
       0.62, 0.88, 0.31)
pl<- c(1.76, 1.13, 1.87, 2.0, 1.80, 1.59, 1.97, 1.28, 2.26,
       1.33)
weight<- c(mw,ss,pl)
length(mw)
diet<- c(rep("mw",10), rep("ss",10), rep("pl",10))
bird<-data.frame(diet, weight)
#assumption checking: 1) yes SRS 2) yes continuous quantatative
                    #3)normal?
#to check normalcy must calc anova to obtain residuals
bird.aov<- aov(weight~diet, data= bird)
View(bird.aov$residuals)
  #the residuals are automatically tagged on to the anova info
hist(bird.aov$residuals)
shapiro.test(bird.aov$residuals)
  #accept that the data are normal
# assumption 4) equal variances 
leveneTest(weight~diet, data=bird)
#safe to assume equal variances
anova(bird.aov)
#we reject the null hypothesis that the mean weight gain is equal across the three diets

#2
#a Model 1
pea<- read.csv("PeaLength.csv")
#b ANOVA and assumption
# assumption 1) yes SRS, 2) yes continuous 3)normalcy?
pea.anova<- aov(Length ~ TRT, data= pea)
shapiro.test(pea.anova$residuals)
  #3) the data is normal
  #4) equal variances?
leveneTest(Length~TRT, data=pea)
  #we cannot assume the variances are equal 
  #so in fact we could NOT use ANOVA and must use the kruskall wallis
  # cannot do -->  anova(pea.anova)
kruskal.test(Length ~ TRT, data=pea)
#reject H0 that they are equal 

#3
#model 2 <- want to know what the relative contribution to morphology is from species or within spec
  #they don't care about individual species much 
#assumptions 1 and 2 are good
#normalcy?
dung.anova<- aov(Biomass~Species, data= dung)
shapiro.test(dung.anova$residuals)
  #cannot assume normal
kruskal.test(Biomass~Species, data=dung)
#one or more of the species means are diff

#TEXTBOOK PROBLEMS

#11.1 assume normal and equal variance
cont<-c(1.47, 1.62, 1.06, 0.89, 1.67)
tmv<- c(2.44, 2.31, 1.98, 2.76, 2.39)
trsv<- c(2.87, 3.05, 2.36, 3.21, 3)
enz<- c(cont, tmv, trsv)
trt<- c(rep("cont", 5), rep("tmv", 5), rep("trsv", 5))
tobacco<- data.frame(trt,enz)
t.anova<- aov(enz~trt, data=tobacco)
anova(t.anova)
  #reject the null hypothesis that virus activity is the same between groups, one 
  #or more of the means are diff

#11.3 assume normal and equal variance
a<-c(10, 12,9, 11, 10)
b<- c(20,21, 19, 23,18)
c<- c(15, 18, 13, 14, 12)
d<- c(5,3, 6, 6, 4)
cellulase<- c(a,b,c,d)
variant<- c(rep("a", 5), rep("b", 5), rep("c", 5), rep("d", 5))
enzyme<- data.frame(variant, cellulase)
enzy.anova<- aov(cellulase~variant, data=enzyme)
anova(enzy.anova)

#11.5 do kruskal wallis
acon<- c(5, 10, 20, 8,11)
bcon<- c(20, 15, 20, 25, 27)
ccon<- c(60, 50, 70, 25, 90)
agg<- c(acon,bcon,ccon)
condition<- c(rep("a", 5), rep("b", 5), rep("c", 5))
gullycat<-data.frame(condition, agg)

kruskal.test(agg~condition, data= gullycat)

#11.7
z<- c(81,72,68,87)
fifty<- c(88, 89, 92, 107, 101, 91)
sat<-c(111, 109, 133)
benzene<- c(z,fifty, sat)
group<- c(rep("zero",4), rep("fifty", 6), rep("sat", 3))
eggs<- data.frame(group,benzene)

kruskal.test(benzene~group, data=eggs)
