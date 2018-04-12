########################   MCCAY INTRO SCRIPT LAB 04.10.18     ###########################
##########################################################################################
########################          linear regressions         #############################
##########################################################################################

setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
# Correlation Script
crab <- read.csv("crab.csv")
View(crab)
# Scatter plot and basic correlation analysis
dev.off()
plot(crab$Left,crab$Right, xlab="Left Claw Length (cm)", ylab="Right Claw Length (cm)",
     xlim = c(0,14), ylim=c(0,14))
cor(crab$Left,crab$Right)
cor.test(crab$Left,crab$Right)
  #this one is more useful because give t results and confidence interval
# Testing assumptions
shapiro.test(crab$Left)
shapiro.test(crab$Right)

# Load MVN (it's a doozy)
library(MVN)
mvn(data=crab, mvnTest = "royston",
    univariatePlot = "histogram",
    multivariatePlot = "persp")
par(mfrow=c(1,1))
mvn(data=crab, mvnTest = "royston",
    univariatePlot = "histogram",
    multivariatePlot = "contour")
#extension of shapiro wilks test for more than one populat
#simultaneously testing normality and linearity
#p greater than 0.05 we have multivariate normality

# New dataset - bats!
bats <- read.csv("bats.csv")
View(bats)
batnoname = data.frame(bats$BMR, bats$MASS)
mvn(data=batnoname, mvnTest = "royston",
    univariatePlot = "histogram",
    multivariatePlot = "contour")
cor.test(rank(bats$BMR), rank(bats$MASS))
  #gives t stat
# Another way to do it, but I don't really like it...
# This uses the 'AS89' approximation method
cor.test(bats$BMR, bats$MASS, method = "spearman")
  #gives S stat... R_s is same and same conclusion

###TEXTBOOK PROBS###
#MVA, mv outlier, pcapp
library(MVN)
library(pcaPP)
library(mvoutlier)
#13.3
old<- c(25,30,20, 35, 40, 25, 33, 50, 65, 60)
new1<- c(27,28, 19, 36, 38, 25, 32, 52, 67, 58)
both<- data.frame(old,new1)
shapiro.test(old)
  #normal
shapiro.test(new1)
  #normal
plot(crab$Left,crab$Right, xlab="Left Claw Length (cm)", ylab="Right Claw Length (cm)",
     xlim = c(0,14), ylim=c(0,14))
cor.test(old,new1)
  #correlation
royston.test(both)

##13.5
bw<- c(53.81, 56.26, 59.86, 59.96, 61.75, 55.28, 56.57, 49.91, 54.25)
sw<- c(18.9, 20.4, 15.9, 19.9, 17.4, 24.0, 21.3, 16.2, 19.3)
bbw<- c(50.8, 51.4, 28.4, 66.6, 35.5, 38.8, 50.3, 33.2, 39.3)
SB<- data.frame(bw,sw)
BB<- data.frame(bw,bbw)
SBB<- data.frame(sw,bbw)
royston.test(SB)
