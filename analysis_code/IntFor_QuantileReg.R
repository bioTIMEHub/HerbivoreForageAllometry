# Quantile regression between foraging area and mean interforay distance
# M Dornelas 26.06.2016

## Clear anything old
rm(list=ls(all=TRUE)) 

## Import data ##
Lizard<-read.table(file.choose(),header=T)

require(quantreg)
#loading quantile regression libary

areadistmed<- rq(Area ~ IntFor, tau=0.5, data=Lizard)
#quantile regression of Area as Function of Interforay distance, for median
areadisttop<- rq(Area ~ IntFor, tau=0.05, data=Lizard)
#quantile regression of Area as Function of Interforay distance, for bottom 0.05 centile
areadistbot<- rq(Area ~ IntFor, tau=0.95, data=Lizard)
#quantile regression of Area as Function of Interforay distance, for top 0.05 centile

# Generate Fig 4 comparing IntFor with Area
setEPS()
postscript("Fig1Emmy.eps")

plot(Lizard$IntFor, Lizard$Area, xlab="Mean inter-foray distance (m)", ylab=expression(Foraging~area~"("~m^2~")"), pch=16)
#adding model lines
abline(areadistmed)
abline(areadisttop)
abline(areadistbot)
dev.off()

# Making Figure 2

Speciesordered<-ordered(Lizard$Species, levels=c("scopas","striatus", "nigricauda", "unicornis", "frenatus", "sordidus", "rivulatus", "doliatus", "vulpinis"))
# ordering species acording to Families
msize<- glm(formula=Area~Size, family=Gamma(link="log"),data=Lizard)


library(beanplot)
setEPS()
postscript("Fig2Emmy.eps")
par(mfrow =c(3,2))
# setting up figure with 3x2 plots
plot(Lizard$Size, log(Lizard$Area), xlab="size", ylab="log(foraging area)", pch=16)
abline(msize)
beanplot(log(Lizard$Area)~Lizard$SS,bty="n",las=1,xaxt="n")
beanplot(log(Lizard$Area)~Lizard$Func,bty="n",las=1,xaxt="n")
beanplot(log(Lizard$Area)~Speciesordered,bty="n",las=1)
beanplot(log(Lizard$Area)~Lizard$Diet,bty="n",las=1,xaxt="n")
dev.off()
