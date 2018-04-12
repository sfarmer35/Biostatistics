########################   MCCAY INTRO SCRIPT LAB 02.27.2018   ###########################
##########################################################################################
########################     follow up techniques to ANOVA      ##########################
##########################################################################################

# Create vectors of jump distances for each of the three groups
druga = c(17.1,19.4,17.1)
drugb = c(18.1,20.0,19.1)
contr = c(11.2,11.6,11.3)

# Create a combined vector for all jump distances 
Distances = c(druga,drugb,contr)

# Create a vector of identifiers for the groups
Factors = c(rep("A",3), rep("B",3), rep("C",3))

# Create a data frame including these data
Frogs = data.frame(Factors, Distances)
boxplot(Distances ~ Factors, data = Frogs, ylab="Distances (ft)", xlab="Group")

my.anova = aov(Distances~Factors, data=Frogs)
anova(my.anova)
1-pf(57,2,6)

### MODEL 1 ###

### Contrasts ###
  #1, compare two drugs to control
      #set up coefficients (1,1,-2)
  #2, compare a vs. b
      #set up coefficitents (1,-1,0)
contrasts(Frogs$Factors)<-cbind(c(1,1,-2),c(1,-1,0))
contrasts(Frogs$Factors)
  #always look at it! sometimes R has things in diff orders
my.anova2 = aov(Distances~Factors, data=Frogs)
summary(my.anova2, split=list(Factors=list("AB v C" = 1,"A v B" = 2)))
  #to see results of ANOVA need this summary command with # of contrasts laid out 
  #this line adds rows to F table

# Post-hoc means comparisons
  #want to compare drug A to control and then drug B to control 
# Pairwise t-tests
     #response, explanatory, methods of adujustment
pairwise.t.test(Frogs$Distances, Frogs$Factors, p.adj="none")
pairwise.t.test(Frogs$Distances, Frogs$Factors, p.adj="bonferroni")
pairwise.t.test(Frogs$Distances, Frogs$Factors, p.adj="holm")
  #spits out p values for each comparison
t.test(druga,drugb,var.equal = TRUE) # DON'T DO IT THIS WAY!
  #wrong because doesn't use the right sp, right df, or p adjustments

# Tukey HSD
TukeyHSD(my.anova)
  #no dif between A and B but diff between each and control
1-ptukey(1.2/(sqrt(0.903/3)),3,6)

# That was a fixed-factor (Model I) example. Now for a random-factor 
# (Model II) example
setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioStatistics\\Lab")
dung <- read.csv("dung.csv")
head(dung)
my.anova=aov(BLL~Species, data = dung)
anova(my.anova)
n_o <- function(x) (sum(x)-(sum(x^2)/sum(x)))/(length(x)-1)
ns = tapply(dung$BLL, dung$Species, length)
View(ns)
n_o(ns)
  #explaining fraction of variability  explained by treatment
  #if unbalanced us n0 (function in line 64!)
#calculate the varianec by mst-mse/n0
#calculate percent of full variation siga/(siga+sig) when sig=mse

# Non-parametric Multiple-group Comparisons 
 dogsmarts<- read.csv("dogsmarts.csv")
head(dogsmarts)
kruskal.test(Trainability ~ Size, data=dogsmarts)

# Next section: Doing Dunn't Test "the hard way" for illustration
  #calculates Q from each test
dogsmarts$rTrainability = rank(dogsmarts$Trainability)
tapply(dogsmarts$rTrainability,dogsmarts$Size, mean)
tapply(dogsmarts$rTrainability,dogsmarts$Size, length)
anova(aov(rTrainability~Size, data=dogsmarts))
MSTOT = (6864+184698)/(2+129)
Q_SM = (75.97-59.11)/(sqrt(MSTOT*(1/46+1/46)))
Q_SM
(1-pnorm(Q_SM))*2
Q_SL = (75.97-64.11)/(sqrt(MSTOT*(1/46+1/40)))
Q_SL
(1-pnorm(Q_SL))*2
Q_ML = (59.11-64.11)/(sqrt(MSTOT*(1/46+1/40)))
Q_ML
(1-pnorm(Q_ML))*2

# Now for the "easy way"
#for followup to kruskal
library(FSA)
  #installed FSA package with dunntest
dunnTest(dogsmarts$Trainability,dogsmarts$Size, method="bonferroni")
dunnTest(dogsmarts$Trainability,dogsmarts$Size, method="sidak")
dunnTest(dogsmarts$Trainability,dogsmarts$Size, method="holm")
  #in general just use holm adjustment because more conservative

### Handout 14 problems ###

#1
ulim<- c(2.5, 3.1, 2.3, 1.9, 2.4)
diet90<- c(2.7, 3.1, 2.9, 3.7, 3.5)
diet80<- c(3.1, 2.9, 3.8, 3.9, 4.0)
ls<-c(ulim,diet90,diet80)
food<-c(rep("ulim",5), rep("diet90",5), rep("diet80",5))
rat<-data.frame(food,ls)

leveneTest(ls~food, data=rat)
  #can assume equal var
rat.a<-aov(ls~food, data=rat)
shapiro.test(rat.a$residuals)
  #can assume normal
anova(rat.a)
  #group means significantly differ

#contrasts
  #unlimited to 90 and 80
    #2, -1, -1
  #two restricted
    #0, 1, -1
contrasts(rat$food)<- cbind(c(-1,-1,2), c(1, -1,0))
contrasts(rat$food)
rat.a2<- aov(ls~food, data=rat)
summary(rat.a2, split=list(food=list("diet vs unlim"=1, "80 vs 90"=2)))
  #the diets are diff than unlimited
  #the diets aren't different from one another

#means comparisions
pairwise.t.test(rat$ls, rat$food,p.adj="holm")
  #same result as contrasts

#tukey
TukeyHSD(rat.a)
  #same result

#2
one<- c(9,7,5,5,3)
two<- c(2,6,7,11,5)
three<-c(3,5,9,10,6)
four<- c(4,10,9,8,10)
five<- c(8,10,12,13, 11)
glucose<- c(one, two, three, four,five )
drops<- c(rep("one",5), rep("two",5), rep("three",5), rep("four",5),rep("five",5))
mouse<- data.frame(drops,glucose)

mouse.a<-anova(aov(glucose~drops, data=mouse))
  #significant!
sig.a<-(mouse.a[1,3]-mouse.a[2,3])/length(one)
sig.a/(sig.a+mouse.a[2,3])

#3
feb<- c(4.7, 4.9, 5.0, 4.8, 4.47)
may<- c(4.6, 4.4, 4.3, 4.4, 4.1, 4.2)
aug<- c(4.8, 4.7, 4.6, 4.4, 4.1, 4.2)
nov<- c(4.9, 5.2, 5.4, 5.1, 5.6)
weight<- c(feb, may, aug, nov)
month<- c(rep("feb", 5), rep("may",6), rep("aug",6), rep("nov",5))
food<- data.frame(month, weight)
food.a<- aov(weight~month, data=food)
shapiro.test(food.a$residuals)
  #normal
leveneTest(weight~month, data=food)
  #equalvar
#wants to do nonparametric anyways
kruskal.test(weight~month, data=food)
  #significant
#had to add the dunn.test package in seperately (supposed to be included in FSA)
dunnTest(food$weight, food$month, method="holm")
  #may-nov and aug-nov are different

#11.13 of textbook
norm<-c(156, 282, 197, 297, 116, 127, 119, 29, 253, 122, 349, 110, 143, 64
        , 26, 86, 122, 455, 655)
all<- c(391, 46, 469, 86, 174, 133, 13, 499, 168, 62, 127, 276, 176, 146, 108, 
        276, 50, 73, 44)
all.in<- c(82, 100, 98, 150, 243, 68, 228, 131, 73, 18, 20, 100, 72, 133, 465,
           40, 46, 34, 14)
antibod<- c(norm, all, all.in)
trt<- c(rep("norm", length(norm)), rep("all", length(norm)), rep("all.in", length(norm)))
diab<- data.frame(trt,antibod)

diab.a<- aov(antibod~trt, data=diab)
shapiro.test(diab.a$residuals)
  #not normal
library(car)
leveneTest(antibod~trt,data=diab)
kruskal.test(antibod~trt,data=diab)
  #not signif
#log transform
diab$log<-log(diab$antibod)
log.a<- aov(log~trt,data=diab)
shapiro.test(log.a$residuals)
  #log fixed the issues!
anova(log.a)
