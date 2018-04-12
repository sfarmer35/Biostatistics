##########################################################################################
########################   MCCAY INTRO SCRIPT LAB 02.13.2018   ###########################
##########################################################################################
###################### Probabilities- binomial, poission, normal #########################
##########################################################################################

# Calculating Probabilities

# Binomial probability 
# Just a graph to look at while we calculate binomial probabilities
x<-c(0:3)
barplot(dbinom(c(0:3), size = 3, p = 0.20), names.arg = x,
        xlab = "X",
        ylab = "Probability",
        main="Binomial, p=0.2, k=3",
        ylim=c(0,.6))
abline(h=0)

dbinom(0,3,.2)  # d* stands for probability density
#density function at 0, size=3, prob = 0.2

# last two values are parameters, k and p
dbinom(1,3,.2)  # probability at a particular value

pbinom(1,3,.2)  # p* stands for cumulative probability
# up to the specified value 
#and including the values before it 

# Probability of 2 or more out of three?
1-pbinom(1,3,.2) 

qbinom(.6,3,.2) # q* stands for the value (quantile) at the 
# specified cumulative probability
  #give it a proability and walks through distibution until hits that value, spits out 
  #which value that corresponds with a probability 

rbinom(1,3,.2)  # r* stands for a random number drawn from that
# probability distribution
#when we normally think of random numbers, colloquially we are considering the chance of
#picking each individual number is equal
#however, here it randomly draws a value from the distribution with set probability

# Poisson probability
# Another graph to look at
barplot(dpois(c(0:10),2),names.arg=c(0:10),ylim=c(0,0.30),
        xlab = "X",
        ylab = "Probability",
        main="Poisson, mu=2"
)
abline(h=0)

dpois(0,2) # Probability of zero
# last value is the single parameter, lambda/mu (the mean)
ppois(4,2) # Probability of 0, 1, 2, 3, or 4
ppois(10,2) # Probability of up to and including 10
  #distribution infinitely right
  #at somepoint, R will round for you... but poission never hits 1

1-ppois(3,2) # Probability of 4 or more

# Normal probabilities
# A graph to look at


curve(dnorm(x,100,2),90,110,xlab="X", ylab="Probability Density",
      main="Normal (mean=100, sd=2)")

#d functionn is not useful for continuous data 
  #integration of probability over some unit span

dnorm(98,100,2) # d* indicates density at a specific value
# this is of essentially no value in continuous dns
# last two values are parameters, mu and sd
# if they are not provided, 0 and 1 are assumed

pnorm(100,100,2) # p* indicates probability up to a value and including value
# end points do not matter

qnorm(.975,100,2) # q* is quantile "cutoff" function
  #will want to define cut off points that allow us to know what is above a certain probability 
  #for example, from 103.9 onward is only a part of 2.5% of probability

#whats the probability of having a value between 100 and 105 
pnorm(105, 100, 2)- pnorm(100, 100, 2)

###PROBLEMS FOR LAB WEEK 3 ######

#1
x<-c(0:1)
barplot(dbinom(c(0:1), size = 2, p = 0.20), names.arg = x,
        xlab = "Life or Death",
        ylab = "Probability",
        main="Binomial, p=0.2, k=20",
        ylim=c(0,1))
abline(h=0)
  #a
pbinom(3, 20, 0.2)
  #b
pbinom(6,20,0.2)-pbinom(2,20,0.2)

#2
ans<- 1- ppois(1, 0.929)
  #probability of being bombed more than once
ans*550
  #number out of 550 that would be bombed more than once

#3
#gives the probability of hearing zero calls in an hour
zc<-ppois(0, 2.2)
zc*8

#4
1-pbinom(120, 150, 0.2)

#5
  #infected -> yes or no?
  #binomial
#chance of 0 affected
  dbinom(0,5, 0.4)
#chance of 1
  dbinom(1,5, 0.4)
#chance of 2
  dbinom(2,5, 0.4)
#chance of 3
  dbinom(3,5,0.4)
#chance of 4
  dbinom(4, 5, 0.4)
#chance of 5
  dbinom(5,5, 0.4)

#^ to make this much faster
  dbinom(0:5,5, 0.4)

#picking 3 or more?
  1-pbinom(2,5, 0.4)

#8.2
#a
pnorm(0.23, 0, 1)
#b
pnorm(-1.67, 0,1)
#c
pnorm(1, 0, 1)- pnorm(-0.5, 0, 1)
#d
pnorm(2.5, 0, 1)- pnorm(-2.5,0,1)

#8.4 bluegill mean=152.09 sd= 19.62
#a 
pnorm(140, 152.09, 19.62)
#c
pnorm(160, 152.09, 19.62)-pnorm(148, 152.09, 19.62)

#8.5 birthweight x=3576 g, sd= 462 g
#a
1-pnorm(3600, 3576, 462)
#b
qnorm(0.95, 3576, 462)
  # this solves for the upper 5% 
  #often it means a lower and upper cutoff (95 percent surrounding the mean)
  #instead would do the range of these two values: 
qnorm(0.975, 3576, 462)
qnorm(0.025, 3576, 462)
