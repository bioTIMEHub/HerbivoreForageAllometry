##New plots using predict and model CIs
##For Area
##Written 22/5/2015

## Clear anything old
rm(list=ls(all=TRUE)) 

require(tidyverse)

# read in the model fit result from our analysis
area.model <- readRDS('analysis_outputs/AreaModel.rds')
# make sure we load data
forage.data<-read.table('../original/src/R-data_Lizard_ellipse_Area.txt',header=T) # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)
# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SS, Diet, Func), .fns = as.factor))


#Establishing CIs
critval <- 1.96 ## approx 95% CI

# Create new data for predictions -----------------------------------------

sp.data <- as.list(rep('', length(unique(forage.data$Species))))
names(sp.data) <- unique(forage.data$Species)

for (i in 1:length(unique(forage.data$Species))) {
   sp.data[[i]] <- data.frame(
      Species = rep(unique(forage.data$Species)[i], 100), 
      Size = runif(100, min = forage.data %>% filter(Species == unique(Species)[i]) %>% select(Size) %>% as_vector() %>% min(),
                   max = forage.data %>% filter(Species == unique(Species)[i]) %>% select(Size) %>% as_vector() %>% max())
   )
   sp.data[[i]] <- sp.data[[i]] %>% arrange(Size)
}

#Finding CIs
dol<-predict.glm(area.model,newdata=sp.data[[1]], type=c("response"),se.fit=TRUE)
dolupr <- dol$fit + (critval * dol$se.fit)
dollwr <- dol$fit - (critval * dol$se.fit)

fren<-predict.glm(area.model,newdata=sp.data[[2]], type=c("response"),se.fit=TRUE)
frenupr <- fren$fit + (critval * fren$se.fit)
frenlwr <- fren$fit - (critval * fren$se.fit)

nig<-predict.glm(area.model,newdata=sp.data[[3]], type=c("response"),se.fit=TRUE)
nigupr <- nig$fit + (critval * nig$se.fit)
niglwr <- nig$fit - (critval * nig$se.fit)

riv<-predict.glm(area.model,newdata=sp.data[[4]], type=c("response"),se.fit=TRUE)
rivupr <- riv$fit + (critval * riv$se.fit)
rivlwr <- riv$fit - (critval * riv$se.fit)

sco<-predict.glm(area.model,newdata=sp.data[[5]], type=c("response"),se.fit=TRUE)
scoupr <- sco$fit + (critval * sco$se.fit)
scolwr <- sco$fit - (critval * sco$se.fit)

spi<-predict.glm(area.model,newdata=sp.data[[6]], type=c("response"),se.fit=TRUE)
spiupr <- spi$fit + (critval * spi$se.fit)
spilwr <- spi$fit - (critval * spi$se.fit)

stri<-predict.glm(area.model,newdata=sp.data[[7]], type=c("response"),se.fit=TRUE)
striupr <- stri$fit + (critval * stri$se.fit)
strilwr <- stri$fit - (critval * stri$se.fit)

uni<-predict.glm(area.model,newdata=sp.data[[8]], type=c("response"),se.fit=TRUE)
uniupr <- uni$fit + (critval * uni$se.fit)
unilwr <- uni$fit - (critval * uni$se.fit)

vul<-predict.glm(area.model,newdata=sp.data[[9]], type=c("response"),se.fit=TRUE)
vulupr <- vul$fit + (critval * vul$se.fit)
vullwr <- vul$fit - (critval * vul$se.fit)


# Visualise predictions with uncertainty ----------------------------------

# Set up plot format
par(mfrow=c(3,3))
par(mar=c(1.5,1.5,1.5,1.5))
par(oma=c(5,5,0,0))

#Graphs

#No1 frenatus
plot(Area ~ Size, data=forage.data %>% filter(Species == 'frenatus'),las=1, type="p",bty="l",pch=16, cex=0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,1000),log="y")
points(fren$fit ~ sp.data[[2]]$Size,type="l")
points(frenupr ~ sp.data[[2]]$Size,type="l",lty=2)
points(frenlwr ~ sp.data[[2]]$Size,type="l",lty=2)
mtext("Sc. frenatus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No2 nigricauda
plot(Area ~ Size, data=forage.data %>% filter(Species == 'nigricauda'),las=1, type="p",bty="l",pch=16, cex=0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,1000),log="y")
points(nig$fit ~ sp.data[[3]]$Size,type="l")
points(nigupr ~ sp.data[[3]]$Size,type="l",lty=2)
points(niglwr ~ sp.data[[3]]$Size,type="l",lty=2)
mtext("A. nigricauda",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No3 doliatus
plot(Area ~ Size, data=forage.data %>% filter(Species == 'doliatus'),las=1, type="p",bty="l",pch=16, cex=0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,1000),log="y")
points(dol$fit ~ sp.data[[1]]$Size,type="l")
points(dolupr ~ sp.data[[1]]$Size,type="l",lty=2)
points(dollwr ~ sp.data[[1]]$Size,type="l",lty=2)
mtext("S. doliatus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No4 rivulatus
plot(Area ~ Size, data=forage.data %>% filter(Species == 'rivulatus'),las=1, type="p",bty="l",pch=16, cex=0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,1000),log="y")
points(riv$fit ~ sp.data[[4]]$Size,type="l")
points(rivupr ~ sp.data[[4]]$Size,type="l",lty=2)
points(rivlwr ~ sp.data[[4]]$Size,type="l",lty=2)
mtext("Sc. rivulatus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No5 scopas
plot(Area ~ Size, data=forage.data %>% filter(Species == 'scopas'),las=1, type="p",bty="l",pch=16, cex=0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,1000),log="y")
points(sco$fit ~ sp.data[[5]]$Size,type="l")
points(scoupr ~ sp.data[[5]]$Size,type="l",lty=2)
points(scolwr ~ sp.data[[5]]$Size,type="l",lty=2)
mtext("Z. scopas",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No6 vulpinus
plot(Area ~ Size, data=forage.data %>% filter(Species == 'vulpinis'),las=1, type="p",bty="l",pch=16, cex=0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,1000),log="y")
points(vul$fit ~ sp.data[[9]]$Size,type="l")
points(vulupr ~ sp.data[[9]]$Size,type="l",lty=2)
points(vullwr ~ sp.data[[9]]$Size,type="l",lty=2)
mtext("S. vulpinus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No7 spilurus
# might be called by older synonym sordidus
plot(Area ~ Size, data=forage.data %>% filter(Species == 'sordidus'),las=1, type="p",bty="l",pch=16, cex=0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,1000),log="y")
points(spi$fit ~ sp.data[[6]]$Size,type="l")
points(spiupr ~ sp.data[[6]]$Size,type="l",lty=2)
points(spilwr ~ sp.data[[6]]$Size,type="l",lty=2)
mtext("Ch. spilurus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No8 striatus
plot(Area ~ Size, data=forage.data %>% filter(Species == 'striatus'),las=1, type="p",bty="l",pch=16, cex=0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,35), ylim=c(1,1000),log="y")
points(stri$fit ~ sp.data[[7]]$Size,type="l")
points(striupr ~ sp.data[[7]]$Size,type="l",lty=2)
points(strilwr ~ sp.data[[7]]$Size,type="l",lty=2)
mtext("C. striatus",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)

#No9 unicornis
plot(Area ~ Size, data=forage.data %>% filter(Species == 'unicornis'),las=1, type="p",bty="l",pch=16, cex=0.9,
     xlab="",ylab="",tck=0.03, xlim=c(5,60), ylim=c(1,1000),log="y")
points(uni$fit ~ sp.data[[8]]$Size,type="l")
points(uniupr ~ sp.data[[8]]$Size,type="l",lty=2)
points(unilwr ~ sp.data[[8]]$Size,type="l",lty=2)
mtext("N. unicornis",side=3, line=-1,adj=0.05,cex=0.7, family="",font=3)


mylab<-expression(Foraging~area~"("~m^2~")")

title(xlab="Total length (cm)",ylab=mylab, outer=T,
      cex.lab=1.5, family="")

