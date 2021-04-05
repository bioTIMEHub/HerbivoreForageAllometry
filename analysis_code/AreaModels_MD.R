# Univariate models for each response variable
# Re-running the model selection with GLM
# For response variable: Area
# Written/updated 5/2/15
# updated by MD on 12 July 2016 to generate Fig 2 after North shore writing trip
# updated by CC on 4 Feb 2021 to generate model selection summary table
# CC changes model selection criterion to AICc

## Clear anything old
rm(list=ls(all=TRUE))
require(dplyr)
require(performance)

## Import data ##
Lizard<-read.table(file.choose(),header=T)
Lizard<- Lizard[order(Lizard$Size),]

## For Lizard (and Heron) data, there are some abbreviations
# Species= Species
# SS = Social Status (single=1, pair=2, single species chool=3, mixed-species school =4)
# Diet = Diet (EAM=1, macroalgae=2, Detritus=3, Corticated algae=4, Cyanobacteria=5)
# Func = Functional group (excavator=1, scraper=2, grazer/detritivore=3, browser=4)
# Size = Total length (cm)
# IntFor = Mean Inter-foray distance (m)
# Area = Foraging Area
# Tort = Tortuosity ratio (sum of inter-foray/start-finish)
# VarInt = Variance of inter-foray distances

Lizard$SS<-factor(Lizard$SS)
Lizard$Func<-factor(Lizard$Func)
Lizard$Species<-factor(Lizard$Species)
Lizard$Diet<-factor(Lizard$Diet)

#********************************************************************************

SizeSSFunc<-glm(formula=Area~Size+SS+Func, family=Gamma(link="log"), data=Lizard)
summary(SizeSSFunc) 

SizeSSDiet<-glm(formula=Area~Size+SS+Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeSSDiet)

SizeSSSpecies<-glm(formula=Area~Size+SS+Species,family=Gamma(link="log"), data=Lizard)
summary(SizeSSSpecies)

SizeSSFuncint<-glm(formula=Area~Size*SS*Func,family=Gamma(link="log"), data=Lizard)
summary(SizeSSFuncint) #NAs present

SizeSSDietint<-glm(formula=Area~Size*SS*Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeSSDietint) #NAs present

SizeSSSpeciesint<-glm(formula=Area~Size*SS*Species,family=Gamma(link="log"), data=Lizard)
summary(SizeSSSpeciesint) #NAs present

SizeSSFuncDiet<-glm(formula=Area~Size+SS+Func+Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeSSFuncDiet) # One NA

SizeSS<-glm(formula=Area~Size+SS,family=Gamma(link="log"), data=Lizard)
summary(SizeSS)

SizeSpecies<-glm(formula=Area~Size+Species,family=Gamma(link="log"), data=Lizard)
summary(SizeSpecies)

SizeDiet<-glm(formula=Area~Size+Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeDiet)

SizeFunc<-glm(formula=Area~Size+Func,family=Gamma(link="log"), data=Lizard)
summary(SizeFunc)

SizeSSint<-glm(formula=Area~Size*SS,family=Gamma(link="log"), data=Lizard)
summary(SizeSSint)

SizeSpeciesint<-glm(formula=Area~Size*Species,family=Gamma(link="log"), data=Lizard)
summary(SizeSpeciesint)

SizeDietint<-glm(formula=Area~Size*Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeDietint)

SizeFuncint<-glm(formula=Area~Size*Func,family=Gamma(link="log"), data=Lizard)
summary(SizeFuncint)

SSSpecies<-glm(formula=Area~Species+SS,family=Gamma(link="log"), data=Lizard)
summary(SSSpecies)

SSSpeciesint<-glm(formula=Area~Species*SS,family=Gamma(link="log"), data=Lizard)
summary(SSSpeciesint) #Too many NAs

SSDiet<-glm(formula=Area~Diet+SS,family=Gamma(link="log"), data=Lizard)
summary(SSDiet)

SSDietint<-glm(formula=Area~Diet*SS,family=Gamma(link="log"), data=Lizard)
summary(SSDietint) # Too many NAs

SSFunc<-glm(formula=Area~Func+SS,family=Gamma(link="log"), data=Lizard)
summary(SSFunc) 

SSFuncint<-glm(formula=Area~Func*SS, family=Gamma(link="log"),data=Lizard)
summary(SSFuncint) # NAs present

SSm<-glm(formula=Area~SS,family=Gamma(link="log"), data=Lizard)
summary(SSm)

Speciesm<-glm(formula=Area~Species,family=Gamma(link="log"), data=Lizard)
summary(Speciesm)

Funcm<-glm(formula=Area~Func,family=Gamma(link="log"), data=Lizard)
summary(Funcm)

Dietm<-glm(formula=Area~Diet, family=Gamma(link="log"),data=Lizard)
summary(Dietm)

Sizem<-glm(formula=Area~Size, family=Gamma(link="log"),data=Lizard)
summary(Sizem)
#**************************************************************************
#19 models without NAs

model_list <- c('SSDiet', 'SSSpecies', 'SizeDiet', 'SizeDietint','SizeFunc',
                'SizeFuncint', 'SizeSS', 'SizeSSDiet', 'SizeSSSpecies', 'SizeSSint',
                'SizeSpecies', 'SizeSpeciesint','SSm', 'Speciesm', 'Funcm', 'Dietm', 'SSFunc',
                'SizeSSFunc', 'Sizem')

area_model_comparison <- tibble(model=model_list)
area_model_comparison$AICc <- sapply(mget(model_list), performance_aicc)
area_model_comparison$AICc <- area_model_comparison$AICc %>% round(., 3)
area_model_comparison <- area_model_comparison %>% arrange(AICc)

# also by residual deviance
area_model_comparison$ResDev <- sapply(mget(model_list), deviance)
area_model_comparison$ResDev <- sapply(area_model_comparison$ResDev, round, 3)

# calculate dAICc
area_model_comparison$dAIC <- rep('', nrow(area_model_comparison))
for (i in 2:nrow(area_model_comparison)) { # calculate AIC difference after ranking
  area_model_comparison$dAIC[i] <- with(area_model_comparison, AICc[1]-AICc[i])
}

area_model_comparison$dAIC <- area_model_comparison$dAIC %>% as.numeric() %>% round(., 3)

write.csv(area_model_comparison, 'area_selectiontable.csv')

#Establishing CIs
critval <- 1.96 ## approx 95% CI

#Finding CIs
sizemodepred<-predict.glm(Sizem, type=c("response"),se.fit=TRUE)
sizeupr <- sizemodepred$fit + (critval * sizemodepred$se.fit)
sizewr <- sizemodepred$fit - (critval * sizemodepred$se.fit)

Speciesordered<-ordered(Lizard$Species, levels=c("scopas","striatus", "nigricauda", "unicornis", "frenatus", "sordidus", "rivulatus", "doliatus", "vulpinis"))
# ordering species acording to Families

# Figure 2 Univariate model bean plots + size
library(beanplot)
setEPS()
postscript("Fig2Emmy.eps")
par(mfrow =c(3,2))
# setting up figure with 3x2 plots
plot(Lizard2$Size, Lizard2$Area, log="y", xlab="size", ylab="foraging area", pch=16)
points(sizemodepred$fit~Lizard$Size,type="l")
points(sizeupr~Lizard$Size,type="l",lty=2)
points(sizewr~Lizard$Size,type="l",lty=2)
beanplot(Lizard$Area~Lizard$SS,bty="n",las=1,xaxt="n")
beanplot(Lizard$Area~Lizard$Func,bty="n",las=1,xaxt="n")
beanplot(Lizard$Area~Speciesordered,bty="n",las=1)
beanplot(Lizard$Area~Lizard$Diet,bty="n",las=1,xaxt="n")
dev.off()
