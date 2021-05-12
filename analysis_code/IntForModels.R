

# Mean-interforay distance models for every predictor structure
# For response variable: IntFor
# Updated by CC 12/05/2021

# Some abbreviations/metadata to be aware of
# Species= Species
# SS = Social Status (single=1, pair=2, single species chool=3, mixed-species school =4)
# Diet = Diet (EAM=1, macroalgae=2, Detritus=3, Corticated algae=4, Cyanobacteria=5)
# Func = Functional group (excavator=1, scraper=2, grazer/detritivore=3, browser=4)
# Size = Total length (cm)
# IntFor = Mean Inter-foray distance (m)
# Area = Foraging Area
# Tort = Tortuosity ratio (sum of inter-foray/start-finish)
# VarInt = Variance of inter-foray distances
# Start_finish = straight line displacement between start and finish points


## Clear anything old
rm(list=ls(all=TRUE)) 
require(dplyr) # for data wrangling
require(DHARMa) # model checks
require(MuMIn)

## Import data ##
forage.data<-read.table('../original/src/R-data_Lizard_ellipse_Area.txt',header=T) # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)

# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SS, Diet, Func), .fns = as.factor))
str(forage.data)

#********************************************************************************

SizeSSFunc<-glm(formula=IntFor~Size+SS+Func, family=Gamma(link="log"), data=forage.data)
summary(SizeSSFunc)

SizeSSDiet<-glm(formula=IntFor~Size+SS+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSSDiet)

SizeSSSpecies<-glm(formula=IntFor~Size+SS+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSSSpecies)

SizeSSFuncint<-glm(formula=IntFor~Size*SS*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeSSFuncint) #Too many NAs, will not use

SizeSSDietint<-glm(formula=IntFor~Size*SS*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSSDietint) #Too many NAs, will not use

SizeSSSpeciesint<-glm(formula=IntFor~Size*SS*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSSSpeciesint) #Too many NAs, will not use

SizeSSFuncDiet<-glm(formula=IntFor~Size+SS+Func+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSSFuncDiet) # One NA

SizeSS<-glm(formula=IntFor~Size+SS,family=Gamma(link="log"), data=forage.data)
summary(SizeSS)

SizeSpecies<-glm(formula=IntFor~Size+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpecies)

SizeDiet<-glm(formula=IntFor~Size+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDiet)

SizeFunc<-glm(formula=IntFor~Size+Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFunc)

SizeSSint<-glm(formula=IntFor~Size*SS,family=Gamma(link="log"), data=forage.data)
summary(SizeSSint)

SizeSpeciesint<-glm(formula=IntFor~Size*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpeciesint)

SizeDietint<-glm(formula=IntFor~Size*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDietint)

SizeFuncint<-glm(formula=IntFor~Size*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFuncint)

SSSpecies<-glm(formula=IntFor~Species+SS,family=Gamma(link="log"), data=forage.data)
summary(SSSpecies)

SSSpeciesint<-glm(formula=IntFor~Species*SS,family=Gamma(link="log"), data=forage.data)
summary(SSSpeciesint) #Too many NAs

SSDiet<-glm(formula=IntFor~Diet+SS,family=Gamma(link="log"), data=forage.data)
summary(SSDiet)

SSDietint<-glm(formula=IntFor~Diet*SS,family=Gamma(link="log"), data=forage.data)
summary(SSDietint) # Too many NAs

SSFunc<-glm(formula=IntFor~Func+SS,family=Gamma(link="log"), data=forage.data)
summary(SSFunc)

SSFuncint<-glm(formula=IntFor~Func*SS, family=Gamma(link="log"),data=forage.data)
summary(SSFuncint) # NAs present

SSm<-glm(formula=IntFor~SS,family=Gamma(link="log"), data=forage.data)
summary(SSm)

Speciesm<-glm(formula=IntFor~Species,family=Gamma(link="log"), data=forage.data)
summary(Speciesm)

Funcm<-glm(formula=IntFor~Func,family=Gamma(link="log"), data=forage.data)
summary(Funcm)

Dietm<-glm(formula=IntFor~Diet, family=Gamma(link="log"),data=forage.data)
summary(Dietm)

Sizem<-glm(formula=IntFor~Size, family=Gamma(link="log"),data=forage.data)
summary(Sizem)
#**************************************************************************
#19 models without NAs

# Model comparison + selection --------------------------------------------

model_list <- c('SizeSSFunc',
                'SizeSSDiet', 
                'SizeSSSpecies', 
                'SizeSS', 'SizeSpecies', 
                'SizeDiet', 'SizeFunc',
                'SizeSSint', 'SizeSpeciesint',
                'SizeDietint','SizeFuncint',
                'SSSpecies','SSDiet',
                'SSFunc','SSm', 
                'Speciesm','Funcm',
                'Dietm','Sizem')

intfor_compare <- AICc(SizeSSFunc,
                      SizeSSDiet, 
                      SizeSSSpecies, 
                      SizeSS, SizeSpecies, 
                      SizeDiet, SizeFunc,
                      SizeSSint, SizeSpeciesint,
                      SizeDietint,SizeFuncint,
                      SSSpecies,SSDiet,
                      SSFunc,SSm, 
                      Speciesm,Funcm,
                      Dietm,Sizem)
intfor_compare$AICc <- intfor_compare$AICc %>% round(., 3)

# also by residual deviance
intfor_compare$ResDev <- sapply(mget(model_list), deviance)
intfor_compare$ResDev <- sapply(intfor_compare$ResDev, round, 3)
intfor_compare <- intfor_compare %>% arrange(AICc)

# calculate dAICc
intfor_compare$dAIC <- rep('', nrow(intfor_compare))
for (i in 2:nrow(intfor_compare)) { # calculate AIC difference after ranking
  intfor_compare$dAIC[i] <- with(intfor_compare, AICc[i]-AICc[1])
}

intfor_compare$dAIC <- intfor_compare$dAIC %>% as.numeric() %>% round(., 3)

write.csv(intfor_compare, 'analysis_outputs/intfor_selectiontable.csv') # model comparison output
write.csv(summary(SizeSpecies)$coefficients, 'analysis_outputs/intfor_sizespecies.csv') # model parameter estimates output


