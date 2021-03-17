# Univariate models for each response variable
# Re-running the model selection with GLM
# For response variable: VarInt
#Written/updated 5/2/15

## Clear anything old
rm(list=ls(all=TRUE)) 

## Import data ##
Lizard<-read.table(file.choose(),header=T)

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

SizeSSFunc<-glm(formula=VarInt~Size+SS+Func, family=Gamma(link="log"), data=Lizard)
summary(SizeSSFunc) 

SizeSSDiet<-glm(formula=VarInt~Size+SS+Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeSSDiet) 

SizeSSSpecies<-glm(formula=VarInt~Size+SS+Species,family=Gamma(link="log"), data=Lizard)
summary(SizeSSSpecies) 

SizeSSFuncint<-glm(formula=VarInt~Size*SS*Func,family=Gamma(link="log"), data=Lizard)
summary(SizeSSFuncint) #Too many NAs, will not use

SizeSSDietint<-glm(formula=VarInt~Size*SS*Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeSSDietint) #Too many NAs, will not use

SizeSSSpeciesint<-glm(formula=VarInt~Size*SS*Species,family=Gamma(link="log"), data=Lizard)
summary(SizeSSSpeciesint) #Too many NAs

SizeSSFuncDiet<-glm(formula=VarInt~Size+SS+Func+Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeSSFuncDiet) #NAs present

SizeSS<-glm(formula=VarInt~Size+SS,family=Gamma(link="log"), data=Lizard)
summary(SizeSS) 

SizeSpecies<-glm(formula=VarInt~Size+Species,family=Gamma(link="log"), data=Lizard)
summary(SizeSpecies)

SizeDiet<-glm(formula=VarInt~Size+Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeDiet) 

SizeFunc<-glm(formula=VarInt~Size+Func,family=Gamma(link="log"), data=Lizard)
summary(SizeFunc) 

SizeSSint<-glm(formula=VarInt~Size*SS,family=Gamma(link="log"), data=Lizard)
summary(SizeSSint) 

SizeSpeciesint<-glm(formula=VarInt~Size*Species,family=Gamma(link="log"), data=Lizard)
summary(SizeSpeciesint)

SizeDietint<-glm(formula=VarInt~Size*Diet,family=Gamma(link="log"), data=Lizard)
summary(SizeDietint)

SizeFuncint<-glm(formula=VarInt~Size*Func,family=Gamma(link="log"), data=Lizard)
summary(SizeFuncint) 

SSSpecies<-glm(formula=VarInt~Species+SS,family=Gamma(link="log"), data=Lizard)
summary(SSSpecies)

SSSpeciesint<-glm(formula=VarInt~Species*SS,family=Gamma(link="log"), data=Lizard)
summary(SSSpeciesint) ##Too many NAs

SSDiet<-glm(formula=VarInt~Diet+SS,family=Gamma(link="log"), data=Lizard)
summary(SSDiet) 

SSDietint<-glm(formula=VarInt~Diet*SS,family=Gamma(link="log"), data=Lizard)
summary(SSDietint) # Too many NAs

SSFunc<-glm(formula=VarInt~Func+SS,family=Gamma(link="log"), data=Lizard)
summary(SSFunc) 

SSFuncint<-glm(formula=VarInt~Func*SS, family=Gamma(link="log"),data=Lizard)
summary(SSFuncint) # NAs present

SSm<-glm(formula=VarInt~SS,family=Gamma(link="log"), data=Lizard)
summary(SSm)

Speciesm<-glm(formula=VarInt~Species,family=Gamma(link="log"), data=Lizard)
summary(Speciesm)

Funcm<-glm(formula=VarInt~Func,family=Gamma(link="log"), data=Lizard)
summary(Funcm)

Dietm<-glm(formula=VarInt~Diet, family=Gamma(link="log"),data=Lizard)
summary(Dietm)

Sizem<-glm(formula=VarInt~Size, family=Gamma(link="log"),data=Lizard)
summary(Sizem)
#**************************************************************************
# 19 Models without NAs
AICall<-AIC( SizeDietint,SizeSpecies, SizeSpeciesint,SSm, Speciesm, Funcm, Dietm,
            SSDiet,SSFunc,SSSpecies,SizeDiet,SizeFunc, SizeSSSpecies,
            SizeFuncint, SizeSS, SizeSSDiet, SizeSSFunc, SizeSSint, Sizem)
