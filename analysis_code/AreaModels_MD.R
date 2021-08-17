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
require(MuMIn)

# make sure we load data
forage.data<-read.table('./src/SpForagingMetrics.csv',header=T, sep=',') # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)
# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SG, Diet, Func), .fns = as.factor))

#********************************************************************************

SizeSGFunc<-glm(formula=Area~Size+SG+Func, family=Gamma(link="log"), data=forage.data)
summary(SizeSGFunc) 

SizeSGDiet<-glm(formula=Area~Size+SG+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSGDiet)

SizeSGSpecies<-glm(formula=Area~Size+SG+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSGSpecies)

SizeSGFuncint<-glm(formula=Area~Size*SG*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeSGFuncint) #NAs present

SizeSGDietint<-glm(formula=Area~Size*SG*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSGDietint) #NAs present

SizeSGSpeciesint<-glm(formula=Area~Size*SG*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSGSpeciesint) #NAs present

SizeSGFuncDiet<-glm(formula=Area~Size+SG+Func+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSGFuncDiet) # One NA

SizeSG<-glm(formula=Area~Size+SG,family=Gamma(link="log"), data=forage.data)
summary(SizeSG)

SizeSpecies<-glm(formula=Area~Size+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpecies)

SizeDiet<-glm(formula=Area~Size+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDiet)

SizeFunc<-glm(formula=Area~Size+Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFunc)

SizeSGint<-glm(formula=Area~Size*SG,family=Gamma(link="log"), data=forage.data)
summary(SizeSGint)

SizeSpeciesint<-glm(formula=Area~Size*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpeciesint)

SizeDietint<-glm(formula=Area~Size*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDietint)

SizeFuncint<-glm(formula=Area~Size*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFuncint)

SGSpecies<-glm(formula=Area~Species+SG,family=Gamma(link="log"), data=forage.data)
summary(SGSpecies)

SGSpeciesint<-glm(formula=Area~Species*SG,family=Gamma(link="log"), data=forage.data)
summary(SGSpeciesint) #Too many NAs

SGDiet<-glm(formula=Area~Diet+SG,family=Gamma(link="log"), data=forage.data)
summary(SGDiet)

SGDietint<-glm(formula=Area~Diet*SG,family=Gamma(link="log"), data=forage.data)
summary(SGDietint) # Too many NAs

SGFunc<-glm(formula=Area~Func+SG,family=Gamma(link="log"), data=forage.data)
summary(SGFunc) 

SGFuncint<-glm(formula=Area~Func*SG, family=Gamma(link="log"),data=forage.data)
summary(SGFuncint) # NAs present

SGm<-glm(formula=Area~SG,family=Gamma(link="log"), data=forage.data)
summary(SGm)

Speciesm<-glm(formula=Area~Species,family=Gamma(link="log"), data=forage.data)
summary(Speciesm)

Funcm<-glm(formula=Area~Func,family=Gamma(link="log"), data=forage.data)
summary(Funcm)

Dietm<-glm(formula=Area~Diet, family=Gamma(link="log"),data=forage.data)
summary(Dietm)

Sizem<-glm(formula=Area~Size, family=Gamma(link="log"),data=forage.data)
summary(Sizem)
#**************************************************************************
#19 models without NAs

model_list <- c('SGDiet', 'SGSpecies', 'SizeDiet', 'SizeDietint','SizeFunc',
                'SizeFuncint', 'SizeSG', 'SizeSGDiet', 'SizeSGSpecies', 'SizeSGint',
                'SizeSpecies', 'SizeSpeciesint','SGm', 'Speciesm', 'Funcm', 'Dietm', 'SGFunc',
                'SizeSGFunc', 'Sizem') # list of candidates without NAs for comparison

area_model_comparison <- AICc(SGDiet, SGSpecies, SizeDiet, SizeDietint,SizeFunc,
                              SizeFuncint, SizeSG, SizeSGDiet, SizeSGSpecies, SizeSGint,
                              SizeSpecies, SizeSpeciesint,SGm, Speciesm, Funcm, Dietm, SGFunc,
                              SizeSGFunc, Sizem)
area_model_comparison$AICc <- area_model_comparison$AICc %>% round(., 3)

# also by residual deviance
area_model_comparison$ResDev <- sapply(mget(model_list), deviance)
area_model_comparison$ResDev <- sapply(area_model_comparison$ResDev, round, 3)
area_model_comparison <- area_model_comparison %>% arrange(AICc)

# calculate dAICc
area_model_comparison$dAIC <- rep('', nrow(area_model_comparison))
for (i in 2:nrow(area_model_comparison)) { # calculate AIC difference after ranking
  area_model_comparison$dAIC[i] <- with(area_model_comparison, AICc[i]-AICc[1])
}

area_model_comparison$dAIC <- area_model_comparison$dAIC %>% as.numeric() %>% round(., 3)
write.csv(area_model_comparison, 'analysis_outputs/area_selectiontable.csv') # model comparison table output
write.csv(summary(Speciesm)$coefficients, 'analysis_outputs/area-species.csv') # model parameter estimates output
saveRDS(SizeSpecies, file = 'analysis_outputs/AreaModel.rds') # save selected model
save(list=model_list, file='analysis_outputs/areamodels_all.RData') # save this as an RData object for loading elsewhere
