
####MCCAY INTRO SCRIPT LAB 01.30.2018####

setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")

# Script file to accompany Handouts 3 and 4
# Lab Week 1

# 3.1 INSTALLING AND FIRST STEPS

# Most recent version of R is 3.4.3
# Most recent version of RStudio is 1.1.419

# Orientation to RStudio environment

# Use script files!

# Getting help

help(mean)
help(Mean) # Capitalization counts! so this capitalized mean doesn't work
help(average)
??average

# Cursors

# 3.2 R AS A CALCULATOR

3+4
3*4
3/4
3-4
3^4

# Defining variables
# = and <- are synonymous

answer=3+4
answer<-3+4
answer
answer=10+4
answer

# Beware of over-writing
#have it tell you how may values you have going in your environment by typing object ()

objects()

# Running multiple lines

shrews <- c(3, 5, 6, 2, 3)
mean(shrews)
var(shrews)

# 3.3 FUNCTIONS AND PACKAGES

# Many functions are built in to the "Base Package"
mean(c(3,5,6,2,3,4,2,1))

# Installing new packages
# Don't forget to add them to your library (a seperate step)
#he does this not using code


install.packages("plyr")
library("plyr", lib.loc="~/R/win-library/3.4")

#see, this is one of the packages you get when you download plyr:
help(ddply)



# Writingd your own functions
ftc = function(x) (x-32)*(5/9)
ftc(100)
#feed the function lots of numbers
tempsf = c(10,15,20,25,30,35)
ftc(tempsf)

# 3.4 R AS A PROGRAMMING LANGUAGE

print("Hello World!")

#not going to do too much with this
#good to learn to make loops, get actually coding
inp = as.integer(readline(prompt="Enter a number for factorial: "))
factorial = 1
for(i in 1:inp){factorial=factorial*i}
print(paste("The factorial of ", inp, "is", factorial))

# Your scripts are programs
# Comment generously
# Make notes about necessary packages
# Keep track of input and output files

# 3.5 R IS A GRAPHICS PACKAGE

View(trees)
summary(trees)
  #when it looks like this (ie min, median, max etc) we know R sees it as numeric
plot(trees$Girth,trees$Height)

# Be sure to review guidelines for good figures on page 7 of handout 3

plot(trees$Girth,trees$Height,
     xlab="Girth (in)",ylab="Height (ft)",
     cex=1.2,cex.lab=1.2)

# DATA MANAGEMENT

# 4.1 DATA TYPES IN R


# Character
# Numeric
# Integer
# Logical
  #these  are different class types, use the class() to see how R is reading something

x = 3.01
x
class(x)

#change the type of variable with "as.____"
x = as.integer(x)
x
class(x)
  #when converting to charachter, this function truncates
  #for example, 3.9 will be truncated to 3 not 4

x = as.character(x)
x
class(x)
  #they become names, there is no ordering

x = as.logical(x)
x
class(x)
  #doesn't interpret

# 4.2 DATA OBJECTS

# Atomic vectors are the workhorses

length = c(55.2, 53.1, 66.9)
sexes = c("M", "F", "F")

# Often bound together in a dataframe (i.e., a dataset)

snake = data.frame(length,sexes)
snake
ids = c(9304,8934,6739)
snake = data.frame(length,sexes, row.names = ids)

# Refering to vectors within a dataframe can be confusing
#need to refer to vector by dataframe$vectorname

snake$length

# Matricies will be useful for certain analyses

Z = matrix(c(4,9,7,2,5,3), nrow=2)
    #populates matrix based on number of rows or columns you give it
    #since it is 2d, only need to specify row or column length, not both
    #it populates the matrix down and not over
Z
#refereing to individual point
Z[1,1]
#referring to entire row
Z[1,]
  #so this is referring to column 1? [,1]

# Lists are very flexible bundles of vectors of any type and any length

Y = list(length,Z)
Y

# 4.3 BRINGING DATA INTO R

#direct entry
weights<- c(
  5.4, 3.2, 6.2, 4.3, 
  5.6, 7.2, 8.3, 9.3 
)

# Importing data from Excel - bring in dogstats.csv
  #can use click command to import
  #can also use the code way

dogstats$Height

# Importing data from a file on the internet

bitedat =
  read.table("http://datadryad.org/bitstream/handle/10255/dryad.53749/BiteForceMdasMdau.txt?sequence=1", 
             header=TRUE, check.names= FALSE)
 #^ this was causing issues due to some illegal charachters so added "check.names= FALSE
head(bitedat)
# read.table reads space or tab-delimited files; read.csv reads comma delimited files

#DATA MANAGEMENT

# 4.4 SORTING A DATAFRAME
  #brackets specify matrix address
  #here says "sort by bitedat, ___" and the blank indicates that all other variables follow
newbite = bitedat[order(bitedat$weight),]
bitedat[1,]
order(bitedat$weight)

bitedat = bitedat[order(bitedat$weight),]
bitedat
bitedat = bitedat[order(-bitedat$weight),]
bitedat

# 4.5 SUMMARIZING DATA BY A FACTOR

sumdat = aggregate(max ~ Species, data = bitedat, FUN = mean)
  #squiggle is "function of"
  #max has a function of the species
  #the "data= bitedat" is so that we don't have to write bitedat$Species, bitedat$data
sumdat
# The squiggle means "as a function of"

# 4.6 CREATING SUBSETS

mdbite = subset(bitedat, Species == 1)
  #== is the logical "=" not just the way that <- has turned into = 
mdbite

# 4.7 MISSING VALUES
# NA

dogstats$Height[2] <- NA
dogstats$Height
mean(dogstats$Height)
mean(dogstats$Height, na.rm=TRUE)
  #mean doesn't work unless you remove the NA using na.rm
# 4.8 DESIGNATING FACTORS

grp <- c("M", "F", "M", "M", "M", "F", "F", "M")
grp
grp = as.factor(grp)
  #factor has relatively small # of levels to it, needs to be distinguished as factor for stats

# 4.9 TRANSFORMING VALUES IN A VARIABLE

dogstats$Weight
dogstats$lnWeight = log(dogstats$Weight)
dogstats$lnWeight

# A FEW EXTRA THINGS

# Creating a regular series of numbers
S=c(1:50)
S

# Sampling numbers from a vector
S = sample(S) # reorder
S
S = sample(S,10) # random sample
S

#What sort does
sort(S) # just works on vectors
S

#stacking
males = c(1.72, 1.69, 1.70, 1.66)
females = c(1.55, 1.62, 1.58, 1.53)
my.dat = data.frame(males, females) #must be a data frame to stack
my.dat
#groups are in seperate columns -> can join them using stacked function
stacked.dat = stack(my.dat, )
                          #(my.dat, ___) indicates bring everything along
stacked.dat
#gives bogus names when stacks, so rename:
names(stacked.dat) = c("height","gender")

###Handout 3 Problems###

#1 a
 (4/(27+2))+28
#1 b
 1/(sin(sqrt(0.782)))
 asin(sqrt(0.782))
#1 c
 log(2.3)+25

#2 make a area funciton
area<- function(x) (pi*x^2)
diameter<-c(1.2, 2.3, 3.4, 4.5, 5.6, 6.7, 7.8, 8.9, 9.0, 10.1)
radius<- 0.5*diameter
area(radius)

#3 Install datasets
library(datasets)
DNase
  #this data set is already in there since I have this package installed
  #it doesn;t show up in data because it is not a dat frame I made,however I can still use it

plot(DNase$conc, DNase$density, xlab= "Concentration", ylab= "Density" )
#caption:
  #not really sure what this data set is saying...
  #DNase density change with increaseing ALISA concentration

###HANDOUT 4###

#1
  #a I entered all the data into excel
soils<- read.csv("Lab1_soils.csv")

#check what type of data it is:
class(soils$Site)
#if needed to convert it to charachter:
soils$Site<- as.character(soils$Site)
soils$Site

soils$Use<- as.factor(soils$Use)
soils$Use

  #b sorting
sortsoil<- soils[order(soils$pH),]

  #c
#way that I have been aggregating:
mse<- aggregate(sortsoil$Se, by= list(sortsoil$Use), FUN= mean)
colnames(mse)<- c("Use", "Mean Se")
#shorter way using squiggle: this way is faster because it keeps titles!
mse<- aggregate(Se ~ Use, data= sortsoil, FUN=mean)
  #aggregate selenium by landuse in the data set, use mean function

  #d subset
pasture<- subset(soils, Use == "p")
  
#2 built in dataset Iris

#a
iris
  #^viewing dataset
class(iris$Sepal.Length)
class(iris$Sepal.Width)
class(iris$Petal.Length)
class(iris$Petal.Width)
class(iris$Species)

#b sort the dataset by sepal.length
iris.s<- iris[order(iris$Sepal.Length),]

#c summarize each flower attribute by species
summary(iris$Sepal.Length)
#summary function is too general so must use the sumdat function
SL<- aggregate(Sepal.Length ~ Species, data= iris, FUN= mean)
SW<- aggregate(Sepal.Width ~ Species, data=iris, FUN= mean)
PL<- aggregate(Petal.Length ~ Species, data= iris, FUN= mean)
PW<- aggregate(Petal.Width ~ Species, data=iris, FUN=mean)

#d subset for only species Iris setosa

setosa<- subset(iris, Species == "setosa")

#3 compare & contrast the following: matrices, vectors, dataframes, lists
