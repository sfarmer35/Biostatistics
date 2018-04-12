#######################################################################################
############################# PROBLEM SET 2 ###########################################
#######################################################################################

#February 19 2018

setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")

library(plyr)

#1 a
pH<- c(5.37, 4.89, 6.99, 4.33, 4.61, 4.96, 4.96, 4.8, 5.13, 5.77, 6.12, 5.95, 5.75, 5.83, 
       5.9, 5.05, 5.12, 6.1, 6.35, 5.4, 6.15, 5.91, 7.43, 6.35, 5.72, 4.74, 5.3, 5.5, 5.18,
       6.27, 5.49, 5.57, 7.03, 7.21, 5.93, 5.36, 3.76, 5.68, 4.6, 4.53, 6.14, 5.54, 5, 5.29,
       5.08, 6.12, 4.39, 5.85, 4.74, 6.13, 5.29, 5.41, 6.12, 3.57, 5.27, 5.87, 5.86, 5.83, 4.25,
       4.43, 5.47, 4.33, 6.17, 4.8, 5.75, 5.02, 5.05, 6.39, 4.54, 5.67, 4.64, 5.29, 6.7, 5.8,
       5.79, 5.03, 5.13, 5.12, 6.19, 5.36, 5.51, 4.94, 5.14, 4.79, 5.75)

hist(pH, main="", xlab= "pH", ylab= "Number of Observations", breaks= 10,
     #right = F,
     xaxt= "n", xaxs= "i" )
axis(1, seq(2,8))

#1 b
trees<- c("DF", "DF", "DF", "DF", "DF", "DF", "DF", "DF", "DF", "OF", "OF", "PL", "PL",
          "DF", "DF", "DF", "DF", "OF", "OF", "PL", "PL", "YD", "YD", "DF", "DF", "PL",
          "DF", "DF", "DF", "OF", "PL","PL", "YD", "YD", "DF", "DF", "DF","DF", "PL","PL",
          "PL", "DF", "DF","DF", "DF", "YD", "DF", "PL", "PL", "DF", "DF", "DF", "YD", "DF",
          "DF", "YD", "DF", "PL", "DF", "DF", "DF", "DF", "PL", "PL", "PL","DF", "DF",
          "OF", "DF", "PL", "DF", "DF", "DF", "YD", "DF", "DF", "DF", "DF","DF", "PL",
          "PL", "DF", "DF", "DF", "DF")
  #could have done it by counting each and using c(rep("Df", 20), rep("OF", 30)...)
count(trees)
treesc<-c(52,6,19,8)
barplot(treesc, main= "", xlab= "Ecosystem Type", ylab= "Number of Observations", 
        names.arg= c("DF", "OF", "PL", "YD" ))
abline(h=0)

#2

moisture<- c(4.85, 3.78, 3.24, 3.26, 3.30, 3.31, 3.28, 3.23, 5.07, 4.61, 3.80, 
            3.41, 3.80, 4.02, 3.43, 3.81, 3.56, 4.26, 3.46, 3.23, 4.83, 3.73,
            4.45, 4.92, 3.71, 3.70, 3.59, 4.85, 3.19, 3.69, 3.57, 3.06, 5.70,
            4.37, 3.86, 4.12, 4.17, 3.83, 4.21, 5.34, 3.84, 3.41, 2.87, 3.23,
            3.84, 4.69, 4.07, 3.38, 3.54, 3.96, 5.04, 3.26, 4.19, 3.68, 7.81,
            7.10, 5.08, 3.39, 2.48, 4.40, 4.63, 3.48, 5.80, 3.47, 4.53, 4.35, 
            5.68, 5.95, 3.02, 2.65, 3.42, 4.30, 4.49, 5.39, 3.80, 3.37, 3.77, 
            3.43, 3.64, 3.51, 4.18, 3.21, 3.29, 2.96, 3.36)
mean(moisture)
median(moisture)
sd(moisture)
IQR(moisture)
quantile(moisture)
  
#3 arctic birder response

def<- c(rep(1,4), rep(2,9), rep(3,4), rep(4,6), rep(5,1))
hist(def)
mean(def)
median(def)
sd(def)
#4
bac<-c(5.8,6.2,6.0,5.1,5.8,6.3,5.7,6.8,6.1,5.2)
bac10<- bac*10^6
mean(bac10)
median(bac10)
range(bac10)
#sum of squares= (x-xbar)^2
sum((bac10-mean(bac10))^2)
sd(bac10)
  
#5.  Imagine that you roll a fair die 10 times, what is the probability of obtaining
  #at least 2 sixes? 
1/6
x<-2:10
sum(dbinom(x, size= 10, p= (1/6)))
#better
1-pbinom(1, size=10, p=1/6)

#6
(1313-13)/1313
#assume binomial ??? student survives or doesn't
1-pbinom(12, 130, 0.99)
pbinom(12,130, 0.99)

#7
#textbook 5.19
dbinom(3, 6, 0.5)
#textbook 5.24
boys<- dbinom(0:6, 6, 0.5)
outcome<-c(0,1,2,3,4,5,6)
bprob<-data.frame(outcome,boys)
colnames(bprob)<- c("# of boys", "probability")
barplot(boys, main="", xlab= "Number of Boys", ylab= "Probability", names.arg=(c(0,1,2,3,4,5,6)))
abline(h=0)

#8 Handout 6 #1

Decay<-c(50,140)
Fresh<- c(25,10)
gender<-c("andro","gyno")
morph<-data.frame(Decay,Fresh, row.names= gender)

#a
75/sum(morph)

#b
200/sum(morph)

#c
dbinom(3,3, 1/3)

#d fresh and andro? fresh and gyno?


#9
snake.n<- c(14,16,7,28,23,186)
snake.x<- c(86.2, 57.5, 52.6, 60.8, 59.1, 44.6)
weighted.mean(snake.x, snake.n)
