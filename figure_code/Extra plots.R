# Plot of mean-interforay distance vs Area
# This gets replaced by the quantile regression plot
# Written: 30/4/2015

## Clear anything old
rm(list=ls(all=TRUE)) 

## Import data ##
Lizard<-read.table(file.choose(),header=T)

Lizard$SS<-factor(Lizard$SS)
Lizard$Func<-factor(Lizard$Func)
Lizard$Species<-factor(Lizard$Species)
Lizard$Diet<-factor(Lizard$Diet)

Heron<-read.table(file.choose(),header=T)

Heron$Func<-factor(Heron$Func)
Heron$Species<-factor(Heron$Species)
Heron$Diet<-factor(Heron$Diet)

#Setting up plotting format
par(mfrow=c(1,2))
par(mar=c(1.5,1.5,1.5,1.5))
par(oma=c(5,5,0,0))

plot(Area~IntFor, data=Lizard,
     las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03)
mtext("Lizard Island",side=3, line=-1,adj=0.05,cex=0.75, family="")
plot(Area~IntFor, data=Heron,
     las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03)
mtext("Heron Island",side=3, line=-1,adj=0.05,cex=0.75, family="")

mylab<-expression(Foraging~area~"("~m^2~")")

title(xlab="Mean inter-foray distance (m)",ylab=mylab, outer=T,
      cex.lab=1, family="")

#...................................................................

Correlation_Lizard<- lm(Area~IntFor, data=Lizard)
anova(Correlation_Lizard)

Correlation_Heron<- lm(Area~IntFor, data=Heron)
anova(Correlation_Heron)

#plotting Intfor-Area, Lizard
plot(IntFor~Area, data=Lizard, main="Lizard Island")
plot(Area~IntFor, data=Lizard, main="Lizard Island")

#plotting Intfor-Area, Heron
plot(IntFor~Area, data=Heron, main="Heron Island")
plot(Area~IntFor, data=Heron, main="Heron Island")
