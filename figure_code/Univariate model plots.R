## New plots for univariate models against Area (Lizard)
#Written 18/01/2016
#Updated 29/01/2016
#Updated 5/2/2016

## Clear anything old
rm(list=ls(all=TRUE)) 

#Clear graphics parameters
dev.off()

## Import data ##
Lizard<-read.table(file.choose(),header=T)

Lizard$SS<-factor(Lizard$SS)
Lizard$Func<-factor(Lizard$Func)
Lizard$Species<-factor(Lizard$Species)
Lizard$Diet<-factor(Lizard$Diet)

# Set up plot format
par(mfrow=c(2,3))
par(mar=c(1.5,1.5,1.5,1.5))
par(oma=c(5,5,0,0))
par(las=1,bty="l")

mylab5<-expression(paste(
  "Foraging Area"~
    "(m"^"2"~
    ")"
))

#********************************************************************************
plot(Area~Size, data=Lizard,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="Total length (cm)",ylab=mylab5,tck=0.03,log="y", ylim=c(1,800))
mtext("A",side=3, line=-1,adj=1,cex=0.7)


plot(Area~SS, data=Lizard,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03,log="y", ylim=c(1,800))
mtext("B",side=3, line=-1,adj=1,cex=0.7)

plot(Area~Func, data=Lizard,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03,log="y", ylim=c(1,800))
mtext("C",side=3, line=-1,adj=1,cex=0.7)


plot(Area~Species, data=Lizard,las=2, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",log="y", ylim=c(1,800), tck=0.03, names=c("S. doliatus","S. frenatus",
                                                               "A. nigricauda", "S. rivulatus",
                                                               "Z. scopas", "Ch. spilurus","Ct. striatus",
                                                               "N. unicornis", "S. vulpinus"))
mtext("D",side=3, line=-1,adj=1,cex=0.7)

plot(Area~Diet, data=Lizard,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="",ylab="",tck=0.03,log="y", ylim=c(1,800))
mtext("E",side=3, line=-1,adj=1,cex=0.7)

plot.new()
mtext("A: E(Y)=exp(B0+B1*Total Length) + E", cex=0.55,line=-1.5, side=3 )
mtext("B: E(Y)=exp(B0+B1*Social Structure) + E", cex=0.55,line=-3.5, side=3 )
mtext("C: E(Y)=exp(B0+B1*Functional group) + E", cex=0.55,line=-5.5, side=3 )
mtext("D: E(Y)=exp(B0+B1*Species) + E", cex=0.55,line=-7.5, side=3 )
mtext("E: E(Y)=exp(B0+B1*Diet) + E", cex=0.55,line=-9.5, side=3 )

title(xlab="",ylab=mylab5, outer=T,
      cex.lab=1, family="sans")

##Stripchart in case we want
#Written 5/2/2016

plot(Area~Size, data=Lizard,las=1, type="p",bty="l",pch=16, cex =0.9,
     xlab="Total length (cm)",ylab=mylab5,tck=0.03,log="y", ylim=c(1,800))
mtext("A",side=3, line=-1,adj=1,cex=0.7)


stripchart(Area~SS, data=Lizard,vertical=T,pch=16, cex =0.9,tck=0.03,log="y")
mtext("B",side=3, line=-1,adj=1,cex=0.7)

stripchart(Area~Func, data=Lizard,pch=16, cex =0.9,vertical=T,
           tck=0.03,log="y")
mtext("C",side=3, line=-1,adj=1,cex=0.7)