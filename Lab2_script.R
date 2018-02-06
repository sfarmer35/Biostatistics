
####MCCAY INTRO SCRIPT LAB 02.05.2018####

# Script for Lab 2, Week of 2-5-2018
# Descriptive Statistics

# Empirical Distributions

# It is easiest to use the barplot command to make histograms for 
# categorical variables

counts = c(156,38,40)
labs = c("Red","Pink","White")
barplot(counts,
        main="",
        xlab = "Flower Color",
        ylab="Number of Flowers",
        names.arg=labs)
abline(h=0)

# The histogram command works with quantitative data

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

# Let's vary the number of categories in our histogram

hist.info = hist(setosa$Petal.Length, 
                 breaks = 5,
                 main="", 
                 xlab="Length of Petals (cm)",
                 ylab="Number of Observations")

# Measures of central tendency
mean(setosa$Petal.Length)
median(setosa$Petal.Length)
quantile(setosa$Petal.Length)

# What is the sample size?
length(setosa$Petal.Length)

# Want the 10-quantiles (deciles)?
quantile(setosa$Petal.Length, probs=seq(0,1,0.1))

# Mode
mode(setosa$Petal.Length) #??

# Weighted average
class.year = c(2018, 2019, 2020, 2021)
students = c(550,600,650,700)
mean.height = c(180.1, 179.9, 183.8, 182.1)
anycollege = data.frame(students, mean.height, row.names = class.year)
anycollege
attach(anycollege)
weighted.mean(mean.height, students)
detach(anycollege)

# Measures of variability

# The R range function stinks
x = c(1.3, 4.2, 8.3, 6.3, 6.2, 6.9)
range(x)

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

library(e1071)

skew = skewness(plant$biomass) # Need the e1071 library for this command
skew

x <- rnorm(100)
skewness(x)

for(i in 1:10000) {x[i]=skewness(rnorm(100))}
hist(x, xlab="Skewness of 1000 Simulated Normal Distributions", main = "")
abline(v=skewness(plant$biomass), col="red", lwd=3)

# Generally, would not say that a distribution is very skewed unless this
# metric is < -1 or > 1.

kurt = kurtosis(plant$biomass) # Need the e1071 library for this command
kurt

x <- rnorm(100)
kurtosis(x)

for(i in 1:10000) {x[i]=kurtosis(rnorm(100))}
hist(x, xlab="Kurtosis of 1000 Simulated Normal Distributions", main = "")
abline(v=kurtosis(plant$biomass), col="red", lwd=3)

# Generally, would not say that a distribution is lepto or platykurtic unless this
# metric is < -2 or > 2.

# Applying a funtion to different groups in a stacked dataset using tapply
tapply.example <-
  data.frame(age = rnorm(100, mean = 60, sd = 12),
             treatment = gl(2, 50, labels = c("Drug X", "Control")))
head(tapply.example)
summary(tapply.example) # treatment is a factor
attach(tapply.example)
tapply(age, treatment, mean)
tapply(age, treatment, my.range)