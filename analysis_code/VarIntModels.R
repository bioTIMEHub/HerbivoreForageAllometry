# Univariate models for each response variable
# Re-running the model selection with GLM
# For response variable: VarInt
#Written/updated 5/2/15

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
forage.data<-read.table('./src/SpForagingMetrics.csv', header=T, sep=',') # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)

# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SG, Diet, Func), .fns = as.factor))
str(forage.data)

#********************************************************************************

SizeSGFunc<-glm(formula=VarInt~Size+SG+Func, family=Gamma(link="log"), data=forage.data)
summary(SizeSGFunc) 

SizeSGDiet<-glm(formula=VarInt~Size+SG+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSGDiet) 

SizeSGSpecies<-glm(formula=VarInt~Size+SG+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSGSpecies) 

SizeSGFuncint<-glm(formula=VarInt~Size*SG*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeSGFuncint) #Too many NAs, will not use

SizeSGDietint<-glm(formula=VarInt~Size*SG*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSGDietint) #Too many NAs, will not use

SizeSGSpeciesint<-glm(formula=VarInt~Size*SG*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSGSpeciesint) #Too many NAs

SizeSGFuncDiet<-glm(formula=VarInt~Size+SG+Func+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSGFuncDiet) #NAs present

SizeSG<-glm(formula=VarInt~Size+SG,family=Gamma(link="log"), data=forage.data)
summary(SizeSG) 

SizeSpecies<-glm(formula=VarInt~Size+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpecies)

SizeDiet<-glm(formula=VarInt~Size+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDiet) 

SizeFunc<-glm(formula=VarInt~Size+Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFunc) 

SizeSGint<-glm(formula=VarInt~Size*SG,family=Gamma(link="log"), data=forage.data)
summary(SizeSGint) 

SizeSpeciesint<-glm(formula=VarInt~Size*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpeciesint)

SizeDietint<-glm(formula=VarInt~Size*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDietint)

SizeFuncint<-glm(formula=VarInt~Size*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFuncint) 

SGSpecies<-glm(formula=VarInt~Species+SG,family=Gamma(link="log"), data=forage.data)
summary(SGSpecies)

SGSpeciesint<-glm(formula=VarInt~Species*SG,family=Gamma(link="log"), data=forage.data)
summary(SGSpeciesint) ##Too many NAs

SGDiet<-glm(formula=VarInt~Diet+SG,family=Gamma(link="log"), data=forage.data)
summary(SGDiet) 

SGDietint<-glm(formula=VarInt~Diet*SG,family=Gamma(link="log"), data=forage.data)
summary(SGDietint) # Too many NAs

SGFunc<-glm(formula=VarInt~Func+SG,family=Gamma(link="log"), data=forage.data)
summary(SGFunc) 

SGFuncint<-glm(formula=VarInt~Func*SG, family=Gamma(link="log"),data=forage.data)
summary(SGFuncint) # NAs present

SGm<-glm(formula=VarInt~SG,family=Gamma(link="log"), data=forage.data)
summary(SGm)

Speciesm<-glm(formula=VarInt~Species,family=Gamma(link="log"), data=forage.data)
summary(Speciesm)

Funcm<-glm(formula=VarInt~Func,family=Gamma(link="log"), data=forage.data)
summary(Funcm)

Dietm<-glm(formula=VarInt~Diet, family=Gamma(link="log"),data=forage.data)
summary(Dietm)

Sizem<-glm(formula=VarInt~Size, family=Gamma(link="log"),data=forage.data)
summary(Sizem)
#**************************************************************************
# 19 Models without NAs

# Model comparison + selection --------------------------------------------

model_list <- ls(all.names=T, sorted=T)
model_list <- str_remove_all(model_list,
                              'SGFuncint|SGDietint|SGSpeciesint|SizeSGFuncDiet|SizeSGSpeciesint|SizeSGSpeciesint|SizeSGDietint|SizeSGFuncint|forage.data')
model_list <- model_list[-which(model_list == '')]

varint_compare <- AICc(Dietm,Funcm,
                       SizeDiet,SizeDietint,
                       SizeFunc,SizeFuncint,
                       Sizem,SizeSpecies,SizeSpeciesint,
                       SizeSG,SizeSGDiet,SizeSGFunc,
                       SizeSGint,SizeSGSpecies,
                       Speciesm,SGDiet,
                       SGFunc,SGm,SGSpecies)
varint_compare$AICc <- varint_compare$AICc %>% round(., 3)

# also by residual deviance
varint_compare$ResDev <- sapply(mget(model_list), deviance)
varint_compare$ResDev <- sapply(varint_compare$ResDev, round, 3)
varint_compare <- varint_compare %>% arrange(AICc)

# calculate dAICc
varint_compare$dAIC <- rep('', nrow(varint_compare))
for (i in 2:nrow(varint_compare)) { # calculate AIC difference after ranking
  varint_compare$dAIC[i] <- with(varint_compare, AICc[i]-AICc[1])
}

varint_compare$dAIC <- varint_compare$dAIC %>% as.numeric() %>% round(., 3)

write.csv(varint_compare, 'analysis_outputs/varint_selectiontable.csv') # model comparison output
write.csv(summary(SizeSpecies)$coefficients, 'analysis_outputs/varint_sizespecies.csv') # model parameter estimates output



