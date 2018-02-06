
####MCCAY INTRO SCRIPT LAB 02.05.2018####

setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
library(plyr)

# Script for Lab 2, Week of 2-5-2018
# Descriptive Statistics

# Empirical Distributions

# It is easiest to use the barplot command to make histograms for 
# categorical variables

#this is how the histogram in handout 5 was made 
counts = c(156,38,40)
labs = c("Red","Pink","White")
barplot(counts,
        main="",
        xlab = "Flower Color",
        ylab="Number of Flowers",
        names.arg=labs)
  #names.arg= labs could also be names. arg = "Red", "Pink", "White"
  #default doesn't have a x axis line so add it with abline(h=0) <- h represents horizontal
abline(h=0)


# The histogram command works with quantitative data

  #subset iris data set to look only at species setosa
setosa<- subset(iris, Species == "setosa")

hist(setosa$Petal.Length, 
     main="", 
     xlab="Length of Petals (cm)",
     ylab="Number of Observations")

hist.info = hist(setosa$Petal.Length, 
                 main="", 
                 xlab="Length of Petals (cm)",
                 ylab="Number of Observations")

# This saves the histogram data

# Let's take a look at the data in this list

hist.info
  #saved graph info into this function

# Let's vary the number of categories in our histogram

hist.info = hist(setosa$Petal.Length, 
                 breaks = 5,
                 main="", 
                 xlab="Length of Petals (cm)",
                 ylab="Number of Observations",
                 )
#sabs question: How do I get the x and y axis to intersect? What about the way the
  #axis ends pre the last value?


# Measures of central tendency
mean(setosa$Petal.Length)
median(setosa$Petal.Length)
  #recomended way of getting the quartile!
quantile(setosa$Petal.Length)
  #the interquartile range is 0.75(3rd q - 1st q)

# What is the sample size?
length(setosa$Petal.Length)

# Want the 10-quantiles (deciles)?
quantile(setosa$Petal.Length, probs=seq(0,1,0.1))
  #go from 0 to 1 quantiles breaking it up by 0.1 (or 10 quantiles)

# Mode
mode(setosa$Petal.Length) #??
#no mode function in R!

# Weighted average
class.year = c(2018, 2019, 2020, 2021)
students = c(550,600,650,700)
mean.height = c(180.1, 179.9, 183.8, 182.1)
anycollege = data.frame(students, mean.height, row.names = class.year)
anycollege
attach(anycollege)
  #if attach data frame, it can look and see if there is a variable with that datafarm
  #so I don't need to do anycollege$mean.height
  #convenient way to shorten up your commands but can confuse...
  #masks the variable within the code in preference for the one in the dataframe
weighted.mean(mean.height, students)
detach(anycollege)

# Measures of variability

# The R range function stinks
x = c(1.3, 4.2, 8.3, 6.3, 6.2, 6.9)
range(x)
  #just shoots out largest and smallest values

# So, let's write our own function
my.range = function(x) max(range(x))-min(range(x))
my.range(x)

# Plant biomass example from class
biomass = c(175.29,185.03,165.34,144.74,197.74,122.98,158.58)
biomass.devs = biomass-mean(biomass)
biomass.absdevs = abs(biomass.devs)
biomass.sqdevs = (biomass.devs)^2
plant = data.frame(biomass, biomass.devs, biomass.absdevs, biomass.sqdevs)
plant

# This is the sum of squares
sum(plant$biomass.sqdevs)

# This is the "niave" average squared deviation (i.e., the population variance)
sum(plant$biomass.sqdevs)/length(plant$biomass.sqdevs)
# Remember that this is a biased estimator of the population variance when used
# as a sample variance. We want to divide by n-1, the degrees of freedom, not n.
sum(plant$biomass.sqdevs)/(length(plant$biomass.sqdevs)-1)
# or we just tell R to give us the variance - it assumes sample variance formula
var(plant$biomass)
help(var)

# Recall that the standard deviation is more intuitive than the variance because
# it is not obsurdly large or in squared units
sqrt(var(plant$biomass))
sd(plant$biomass)

# ERROR IN THE HANDOUT - NOT STDEV!!

# The coefficient of variation is a simple function of the standard deviation
# and mean. Write your own function to calculate CV.

# We talked about skew in a general sense in lecture. R can calculate 
# quantitative measures of skew.
### install the package
library(e1071)

skew = skewness(plant$biomass) # Need the e1071 library for this command
skew

x <- rnorm(100) #making vector of random normal numbers, generates 100 random #s with mean of 0 
#and standard dev of 1
hist(x)
skewness(x)
  #doing this multiple times to give a sense of how the skewness changes

for(i in 1:10000) {x[i]=skewness(rnorm(100))}
  #do it 10,000 times! now we have 10,000 sets of 100 numbers and have it calculate the skewness
  #now look at histrogram of the skewness of these 10,000 sets (distribution of skewness)
hist(x, xlab="Skewness of 1000 Simulated Normal Distributions", main = "")
abline(v=skewness(plant$biomass), col="red", lwd=3)
  # this red line is indicating our data skewness

# Generally, would not say that a distribution is very skewed unless this
# metric is < -1 or > 1.

kurt = kurtosis(plant$biomass) # Need the e1071 library for this command
kurt

x <- rnorm(100)
kurtosis(x)

for(i in 1:10000) {x[i]=kurtosis(rnorm(100))}
hist(x, xlab="Kurtosis of 1000 Simulated Normal Distributions", main = "")
abline(v=kurtosis(plant$biomass), col="red", lwd=3)
  #is it meso or platy or lepto curtic 
  #if below -2 or above positive 2 than would say in the platy curtic or leptocurtic 

# Generally, would not say that a distribution is lepto or platykurtic unless this
# metric is < -2 or > 2.

# Applying a funtion to different groups in a stacked dataset using tapply
tapply.example <-
  data.frame(age = rnorm(100, mean = 60, sd = 12),
             treatment = gl(2, 50, labels = c("Drug X", "Control")))
head(tapply.example)
summary(tapply.example) # treatment is a factor <- must be a factor to use t apply (knoww it is
 #because not represented continually)
attach(tapply.example)
tapply(age, treatment, mean)
  #basically the same as aggregate but quicker
tapply(age, treatment, my.range)
  #my.range is the function that we made up

#### PROBLEMS @ THE END OF HANDOUT 5 ####

#1 
  #a describe the masses using a histogram and descriptive stats
mass<- c( 5.28, 5.23,5.15, 7.30, 6.29, 4.86, 6.05, 6.85, 4.75, 6.35, 6.15, 5.50, 6.30, 6.10,
            4.05, 6.00, 5.50, 4.80, 5.00, 5.15, 6.78,6.13, 6.50, 5.10, 7.10, 6.42, 7.42, 5.05,
            5.48, 5.25,  4.68, 6.93, 6.13, 5.06, 4.87)
hist(mass, xlab= "Mass (g)", ylab= "Frequency")
#central tendancy
mean(mass)
  # 5.75
median(mass)
  #5.5
sd(mass)
  #.848
quantile(mass)
#dispersion
  #range function
my.range = function(x) max(range(x))-min(range(x))
my.range(mass)
  #3.37
#skew (remember requires the e1071 package)
skewness(mass)
  #0.195
#kurtosis
kurtosis(mass)
  #maybe a little platycurtic?

 #b No outliers because the histogram looks pretty normal

#2 import sally dataset
sally<- read.csv("sallys.csv")
head(sally)
#a sort by snout:vent length (SVL)
sally.s<- sally[order(sally$SVL),]
  #JSF 86 has the largest snout

#b summarize the data by sex
  #could have used tapply

sally.m<- subset(sally, Gender == "M")
sally.f<- subset(sally, Gender == "F")
length(sally.m$SVL)
length(sally.f$SVL)
mean(sally.m$SVL)
mean(sally.f$SVL)
sd(sally.m$SVL)
sd(sally.f$SVL)
var(sally.m$SVL)
var(sally.f$SVL)
skewness(sally.m$SVL)
skewness(sally.f$SVL)
kurtosis(sally.m$SVL)
kurtosis(sally.f$SVL)
#c
hist(sally.m$Mass)
hist(sally.f$Mass)

#d
#two different ways of making box plots
  # calling from the subsetted data frams
boxplot(sally.m$SVL, sally.f$SVL, xlab= "Gender", ylab= "SVL", names= c("Male", "Female") )
  # calling from the single data frame but subsetting by gender
boxplot(sally$SVL [sally$Gender== "M"], sally$SVL [sally$Gender == "F"],xlab= "Gender"
        ,ylab= "SVL", names= c("Male", "Female"))

#e I do not think that there is sexual dimorphism

#3 Read in the blood pressure data
bp<- read.csv("bp.csv")
#a males are code 1 and females are code 2, summarize for each the sample size, median, and IQR
  #my way.... 
length(bp$Sex [bp$Sex==1])
length(bp$Sex [bp$Sex==2])
  # or subset
bp.m<- subset(bp, Sex ==1)
bp.f<- subset(bp, Sex ==2)
  #using subset might be easier because then don't need to use additional commands for summary
mean(bp.m$SBPRA)
mean(bp.f$SBPRA)
median(bp.m$SBPRA)
median(bp.f$SBPRA)
  #or use tapply
  #definitely easiest because it can do all the functions for both subsets at same time  
tapply(bp$SBPRA, bp$Sex, length)
tapply(bp$SBPRA, bp$Sex, median)
tapply(bp$SBPRA, bp$Sex, IQR)

#b histograms of SBPLN by gender
hist(bp.m$SBPLN)
hist(bp.f$SBPLN)
 #comment on the central tendency (mean/median), dispersion(sd or range?), skew, and kurtosis

#c boxplot comparing arm and ankle blood pressure
boxplot(bp$SBPRA, bp$SBPLN, xlab= "Limb", ylab= "Blood Pressure", names= c("Arm", "Leg"))

#### Chapter 4 Problems ####

#4.1
four<- c(2,5,3,7,8,3,9,3,10,4,7,4,6,11,9,9,11,5,7,3,8,9,2,1,3,8,3,8,9,3)
mean(four)
median(four)
IQR(four)
var(four)
sd(four)
#coefficient of variation is sd/mean
sd(four)/mean(four)

#4.3
culmen<- c(48.1,56.8, 48.0, 53.4,57.8,56.3,62.4, 50.8, 60.2, 57.1, 55.2, 50.1, 59.8,
           61.1, 48.8, 55.8, 51.8, 51.0, 56.0, 61.8, 59.9, 56.8, 59.2, 52.3, 59.3, 56.5, 56.2,
           55.6, 57.7, 52.5, 47.8, 61.5, 55.8, 57.5, 56.8, 47.0, 50.4, 58.0, 61.2, 56.5, 59.3, 
           59.2)
mean(culmen)
median(culmen)
IQR(culmen)
var(culmen)
sd(culmen)
#coefficient of variation is sd/mean
sd(culmen)/mean(culmen)

#4.5
size<- c(1,2,3, 4, 5, 6, 7,8, 9, 10, 11, 12, 13, 14, 15, 16)
freq<- c(5,10,32,61,81,95, 80, 88, 72, 67, 61, 41,24, 1,1, 2)
brood<- data.frame(size, freq)
weighted.mean(size, freq)
