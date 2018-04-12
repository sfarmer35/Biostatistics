#Lab quiz 1
library(plyr)

Trees$species<- c("ts", "ts", "ts", "ts", "ts", "es", "es", "es", "es", "es", "rs", "rs", "rs", rs, rs)
Trees<- c(11.1, 12.4, 12.2, 11.6, 11.5, 9.3, 7.2, 7.9, 8.7, 11.0, 2.2, 
          2.9, 3.1, 3.0, 3.2)

mean(Trees)
sd(Trees)
median(Trees)
my.range = function(x) max(range(x))-min(range(x))
my.range(Trees)

#se
sd(Trees)/sqrt(length(Trees))

trees1<- data.frame(Trees)
Trees$ln<- log(trees)

treeln<- log(Trees)
median(treeln)

treex<- (Trees - mean(Trees))/ sd(Trees)
sd(treex)

ts<- c( 11.1, 12.4, 12.2, 11.6, 11.5)
es<- c(9.3, 7.2, 7.9, 8.7, 11.0)
rs<- c(2.2, 2.9, 3.1, 3.0, 3.2)
shrew<- data.frame(ts, es, rs)
mean(ts)
mean(es)
mean(rs)
sd(ts)
sd(es)
sd(rs)


#####needed to add:
shrew2<- stack(shrew,)
#then could have used tapply
tapply(shrew2$values, shrew2$ind, mean)
tapply(shrew2$values, shrew2$ind, sd)


ToothGrowth

ToothGrowth$len
tg.order<- ToothGrowth[order(ToothGrowth$len),]
count