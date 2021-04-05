##New plots using predict and model CIs
##For Area
##Written 22/5/2015

## Clear anything old
rm(list=ls(all=TRUE)) 

## Import data ##
Lizard<-read.table(file.choose(),header=T)

Lizard$SS<-factor(Lizard$SS)
Lizard$Func<-factor(Lizard$Func)
Lizard$Species<-factor(Lizard$Species)
Lizard$Diet<-factor(Lizard$Diet)


#Remember to split per species (different script)
#Making Size*Species models with unique names for each response variable
IntFormodel<-glm(formula=IntFor~Size*Species,family=Gamma(link="log"), data=Lizard)
Areamodel<-glm(formula=Area~Size*Species,family=Gamma(link="log"), data=Lizard)
VarIntmodel<-glm(formula=VarInt~Size*Species,family=Gamma(link="log"), data=Lizard)

#Establishing CIs
critval <- 1.96 ## approx 95% CI

#Finding CIs
dol<-predict.glm(Areamodel,newdata=doliatus1, type=c("response"),se.fit=TRUE)
dolupr <- dol$fit + (critval * dol$se.fit)
dollwr <- dol$fit - (critval * dol$se.fit)

fren<-predict.glm(Areamodel,newdata=frenatus1, type=c("response"),se.fit=TRUE)
frenupr <- fren$fit + (critval * fren$se.fit)
frenlwr <- fren$fit - (critval * fren$se.fit)

nig<-predict.glm(Areamodel,newdata=nigricauda1, type=c("response"),se.fit=TRUE)
nigupr <- nig$fit + (critval * nig$se.fit)
niglwr <- nig$fit - (critval * nig$se.fit)

riv<-predict.glm(Areamodel,newdata=rivulatus1, type=c("response"),se.fit=TRUE)
rivupr <- riv$fit + (critval * riv$se.fit)
rivlwr <- riv$fit - (critval * riv$se.fit)

sco<-predict.glm(Areamodel,newdata=scopas1, type=c("response"),se.fit=TRUE)
scoupr <- sco$fit + (critval * sco$se.fit)
scolwr <- sco$fit - (critval * sco$se.fit)

spi<-predict.glm(Areamodel,newdata=spilurus1, type=c("response"),se.fit=TRUE)
spiupr <- spi$fit + (critval * spi$se.fit)
spilwr <- spi$fit - (critval * spi$se.fit)

stri<-predict.glm(Areamodel,newdata=striatus1, type=c("response"),se.fit=TRUE)
striupr <- stri$fit + (critval * stri$se.fit)
strilwr <- stri$fit - (critval * stri$se.fit)

uni<-predict.glm(Areamodel,newdata=unicornis1, type=c("response"),se.fit=TRUE)
uniupr <- uni$fit + (critval * uni$se.fit)
unilwr <- uni$fit - (critval * uni$se.fit)

vul<-predict.glm(Areamodel,newdata=vulpinus1, type=c("response"),se.fit=TRUE)
vulupr <- vul$fit + (critval * vul$se.fit)
vullwr <- vul$fit - (critval * vul$se.fit)

#*******************************************************************************
# Set up plot format
par(mfrow=c(3,3))
par(mar=c(1.5,1.5,1.5,1.5))
par(oma=c(5,5,0,0))

#Graphs

#No1 frenatus
plot(Area~Size, data=frenatus,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,800),log="y")
points(fren$fit~frenatus1$Size,type="l")
points(frenupr~frenatus1$Size,type="l",lty=2)
points(frenlwr~frenatus1$Size,type="l",lty=2)
mtext("Scarus frenatus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No2 nigricauda
plot(Area~Size, data=nigricauda,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,800),log="y")
points(nig$fit~nigricauda1$Size,type="l")
points(nigupr~nigricauda1$Size,type="l",lty=2)
points(niglwr~nigricauda1$Size,type="l",lty=2)
mtext("Acanthurus nigricauda",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No3 doliatus
plot(Area~Size, data=doliatus,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,800),log="y")
points(dol$fit~doliatus1$Size,type="l")
points(dolupr~doliatus1$Size,type="l",lty=2)
points(dollwr~doliatus1$Size,type="l",lty=2)
mtext("Siganus doliatus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No4 rivulatus
plot(Area~Size, data=rivulatus,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,800),log="y")
points(riv$fit~rivulatus1$Size,type="l")
points(rivupr~rivulatus1$Size,type="l",lty=2)
points(rivlwr~rivulatus1$Size,type="l",lty=2)
mtext("Scarus rivulatus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No5 scopas
plot(Area~Size, data=scopas,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,800),log="y")
points(sco$fit~scopas1$Size,type="l")
points(scoupr~scopas1$Size,type="l",lty=2)
points(scolwr~scopas1$Size,type="l",lty=2)
mtext("Zebrasoma scopas",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No6 vulpinus
plot(Area~Size, data=vulpinis,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,800),log="y")
points(vul$fit~vulpinus1$Size,type="l")
points(vulupr~vulpinus1$Size,type="l",lty=2)
points(vullwr~vulpinus1$Size,type="l",lty=2)
mtext("Siganus vulpinus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No7 spilurus
plot(Area~Size, data=spilurus,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,800),log="y")
points(spi$fit~spilurus1$Size,type="l")
points(spiupr~spilurus1$Size,type="l",lty=2)
points(spilwr~spilurus1$Size,type="l",lty=2)
mtext("Chlorurus spilurus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No8 striatus
plot(Area~Size, data=striatus,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,800),log="y")
points(stri$fit~striatus1$Size,type="l")
points(striupr~striatus1$Size,type="l",lty=2)
points(strilwr~striatus1$Size,type="l",lty=2)
mtext("Ctenochaetus striatus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No9 unicornis
plot(Area~Size, data=unicornis,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,60), ylim=c(1,800),log="y")
points(uni$fit~unicornis1$Size,type="l")
points(uniupr~unicornis1$Size,type="l",lty=2)
points(unilwr~unicornis1$Size,type="l",lty=2)
mtext("Naso unicornis",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)


mylab<-expression(Foraging~area~"("~m^2~")")

title(xlab="Total length (cm)",ylab=mylab, outer=T,
      cex.lab=1.5, family="")

