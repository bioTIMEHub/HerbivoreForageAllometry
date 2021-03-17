#Re-running uni-variate glm:s for Heron data to see if same model is picked
#Since predictive power wasn't high. 
#Looning at VarInt
#5/3/2015

## Clear anything old
rm(list=ls(all=TRUE)) 

## Import data ##
Heron<-read.table(file.choose(),header=T)

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

Heron$Func<-factor(Heron$Func)
Heron$Species<-factor(Heron$Species)
Heron$Diet<-factor(Heron$Diet)

#********************************************************************************

HSizeSpecies<-glm(formula=VarInt~Size+Species,family=Gamma(link="log"), data=Heron)
summary(HSizeSpecies)

HSizeDiet<-glm(formula=VarInt~Size+Diet,family=Gamma(link="log"), data=Heron)
summary(HSizeDiet)

HSizeFunc<-glm(formula=VarInt~Size+Func,family=Gamma(link="log"), data=Heron)
summary(HSizeFunc)


HSizeSpeciesint<-glm(formula=VarInt~Size*Species,family=Gamma(link="log"), data=Heron)
summary(HSizeSpeciesint)

HSizeDietint<-glm(formula=VarInt~Size*Diet,family=Gamma(link="log"), data=Heron)
summary(HSizeDietint) # Model does not run

HSizeFuncint<-glm(formula=VarInt~Size*Func,family=Gamma(link="log"), data=Heron)
summary(HSizeFuncint)

HSpeciesm<-glm(formula=VarInt~Species,family=Gamma(link="log"), data=Heron)
summary(HSpeciesm)

HFuncm<-glm(formula=VarInt~Func,family=Gamma(link="log"), data=Heron)
summary(HFuncm)

HDietm<-glm(formula=VarInt~Diet, family=Gamma(link="log"),data=Heron)
summary(HDietm)

HSizem<-glm(formula=VarInt~Diet, family=Gamma(link="log"),data=Heron)
summary(HSizem)
#**************************************************************************
AICall<-AIC(HSizeDiet,HSizeFunc,HSizeFuncint,
            HSizeSpecies, HSizeSpeciesint, HSpeciesm, HFuncm, HDietm, HSizem)

