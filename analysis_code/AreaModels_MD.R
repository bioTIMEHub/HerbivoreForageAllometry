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

# make sure we load data
forage.data<-read.table('../original/src/R-data_Lizard_ellipse_Area.txt',header=T) # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)
# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SS, Diet, Func), .fns = as.factor))

#********************************************************************************

SizeSSFunc<-glm(formula=Area~Size+SS+Func, family=Gamma(link="log"), data=forage.data)
summary(SizeSSFunc) 

SizeSSDiet<-glm(formula=Area~Size+SS+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSSDiet)

SizeSSSpecies<-glm(formula=Area~Size+SS+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSSSpecies)

SizeSSFuncint<-glm(formula=Area~Size*SS*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeSSFuncint) #NAs present

SizeSSDietint<-glm(formula=Area~Size*SS*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSSDietint) #NAs present

SizeSSSpeciesint<-glm(formula=Area~Size*SS*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSSSpeciesint) #NAs present

SizeSSFuncDiet<-glm(formula=Area~Size+SS+Func+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSSFuncDiet) # One NA

SizeSS<-glm(formula=Area~Size+SS,family=Gamma(link="log"), data=forage.data)
summary(SizeSS)

SizeSpecies<-glm(formula=Area~Size+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpecies)

SizeDiet<-glm(formula=Area~Size+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDiet)

SizeFunc<-glm(formula=Area~Size+Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFunc)

SizeSSint<-glm(formula=Area~Size*SS,family=Gamma(link="log"), data=forage.data)
summary(SizeSSint)

SizeSpeciesint<-glm(formula=Area~Size*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpeciesint)

SizeDietint<-glm(formula=Area~Size*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDietint)

SizeFuncint<-glm(formula=Area~Size*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFuncint)

SSSpecies<-glm(formula=Area~Species+SS,family=Gamma(link="log"), data=forage.data)
summary(SSSpecies)

SSSpeciesint<-glm(formula=Area~Species*SS,family=Gamma(link="log"), data=forage.data)
summary(SSSpeciesint) #Too many NAs

SSDiet<-glm(formula=Area~Diet+SS,family=Gamma(link="log"), data=forage.data)
summary(SSDiet)

SSDietint<-glm(formula=Area~Diet*SS,family=Gamma(link="log"), data=forage.data)
summary(SSDietint) # Too many NAs

SSFunc<-glm(formula=Area~Func+SS,family=Gamma(link="log"), data=forage.data)
summary(SSFunc) 

SSFuncint<-glm(formula=Area~Func*SS, family=Gamma(link="log"),data=forage.data)
summary(SSFuncint) # NAs present

SSm<-glm(formula=Area~SS,family=Gamma(link="log"), data=forage.data)
summary(SSm)

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

model_list <- c('SSDiet', 'SSSpecies', 'SizeDiet', 'SizeDietint','SizeFunc',
                'SizeFuncint', 'SizeSS', 'SizeSSDiet', 'SizeSSSpecies', 'SizeSSint',
                'SizeSpecies', 'SizeSpeciesint','SSm', 'Speciesm', 'Funcm', 'Dietm', 'SSFunc',
                'SizeSSFunc', 'Sizem') # list of candidates without NAs for comparison

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
  area_model_comparison$dAIC[i] <- with(area_model_comparison, AICc[i]-AICc[1])
}

area_model_comparison$dAIC <- area_model_comparison$dAIC %>% as.numeric() %>% round(., 3)
write.csv(area_model_comparison, 'area_selectiontable.csv')
save(list=model_list, file='areamodels_all.RData') # save this as an RData object for loading elsewhere
