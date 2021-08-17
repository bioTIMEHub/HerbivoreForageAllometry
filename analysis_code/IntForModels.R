

# Mean-interforay distance models for every predictor structure
# For response variable: IntFor
# Updated by CC 12/05/2021

# Some abbreviations/metadata to be aware of
# Species= Species
# SG = Social Status (single=1, pair=2, single species chool=3, mixed-species school =4)
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
forage.data<-read.table('./src/SpForagingMetrics.csv',header=T, sep=',') # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)

# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SG, Diet, Func), .fns = as.factor))
str(forage.data)

#********************************************************************************

SizeSGFunc<-glm(formula=IntFor~Size+SG+Func, family=Gamma(link="log"), data=forage.data)
summary(SizeSGFunc)

SizeSGDiet<-glm(formula=IntFor~Size+SG+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSGDiet)

SizeSGSpecies<-glm(formula=IntFor~Size+SG+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSGSpecies)

SizeSGFuncint<-glm(formula=IntFor~Size*SG*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeSGFuncint) #Too many NAs, will not use

SizeSGDietint<-glm(formula=IntFor~Size*SG*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSGDietint) #Too many NAs, will not use

SizeSGSpeciesint<-glm(formula=IntFor~Size*SG*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSGSpeciesint) #Too many NAs, will not use

SizeSGFuncDiet<-glm(formula=IntFor~Size+SG+Func+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSGFuncDiet) # One NA

SizeSG<-glm(formula=IntFor~Size+SG,family=Gamma(link="log"), data=forage.data)
summary(SizeSG)

SizeSpecies<-glm(formula=IntFor~Size+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpecies)

SizeDiet<-glm(formula=IntFor~Size+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDiet)

SizeFunc<-glm(formula=IntFor~Size+Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFunc)

SizeSGint<-glm(formula=IntFor~Size*SG,family=Gamma(link="log"), data=forage.data)
summary(SizeSGint)

SizeSpeciesint<-glm(formula=IntFor~Size*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpeciesint)

SizeDietint<-glm(formula=IntFor~Size*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDietint)

SizeFuncint<-glm(formula=IntFor~Size*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFuncint)

SGSpecies<-glm(formula=IntFor~Species+SG,family=Gamma(link="log"), data=forage.data)
summary(SGSpecies)

SGSpeciesint<-glm(formula=IntFor~Species*SG,family=Gamma(link="log"), data=forage.data)
summary(SGSpeciesint) #Too many NAs

SGDiet<-glm(formula=IntFor~Diet+SG,family=Gamma(link="log"), data=forage.data)
summary(SGDiet)

SGDietint<-glm(formula=IntFor~Diet*SG,family=Gamma(link="log"), data=forage.data)
summary(SGDietint) # Too many NAs

SGFunc<-glm(formula=IntFor~Func+SG,family=Gamma(link="log"), data=forage.data)
summary(SGFunc)

SGFuncint<-glm(formula=IntFor~Func*SG, family=Gamma(link="log"),data=forage.data)
summary(SGFuncint) # NAs present

SGm<-glm(formula=IntFor~SG,family=Gamma(link="log"), data=forage.data)
summary(SGm)

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

model_list <- c('SizeSGFunc',
                'SizeSGDiet', 
                'SizeSGSpecies', 
                'SizeSG', 'SizeSpecies', 
                'SizeDiet', 'SizeFunc',
                'SizeSGint', 'SizeSpeciesint',
                'SizeDietint','SizeFuncint',
                'SGSpecies','SGDiet',
                'SGFunc','SGm', 
                'Speciesm','Funcm',
                'Dietm','Sizem')

intfor_compare <- AICc(SizeSGFunc,
                      SizeSGDiet, 
                      SizeSGSpecies, 
                      SizeSG, SizeSpecies, 
                      SizeDiet, SizeFunc,
                      SizeSGint, SizeSpeciesint,
                      SizeDietint,SizeFuncint,
                      SGSpecies,SGDiet,
                      SGFunc,SGm, 
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


