# Univariate models for each response variable
# Re-running the model selection with GLM
# For response variable: VarInt
#Written/updated 5/2/15

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

SizeSSFunc<-glm(formula=VarInt~Size+SS+Func, family=Gamma(link="log"), data=forage.data)
summary(SizeSSFunc) 

SizeSSDiet<-glm(formula=VarInt~Size+SS+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSSDiet) 

SizeSSSpecies<-glm(formula=VarInt~Size+SS+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSSSpecies) 

SizeSSFuncint<-glm(formula=VarInt~Size*SS*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeSSFuncint) #Too many NAs, will not use

SizeSSDietint<-glm(formula=VarInt~Size*SS*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSSDietint) #Too many NAs, will not use

SizeSSSpeciesint<-glm(formula=VarInt~Size*SS*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSSSpeciesint) #Too many NAs

SizeSSFuncDiet<-glm(formula=VarInt~Size+SS+Func+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeSSFuncDiet) #NAs present

SizeSS<-glm(formula=VarInt~Size+SS,family=Gamma(link="log"), data=forage.data)
summary(SizeSS) 

SizeSpecies<-glm(formula=VarInt~Size+Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpecies)

SizeDiet<-glm(formula=VarInt~Size+Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDiet) 

SizeFunc<-glm(formula=VarInt~Size+Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFunc) 

SizeSSint<-glm(formula=VarInt~Size*SS,family=Gamma(link="log"), data=forage.data)
summary(SizeSSint) 

SizeSpeciesint<-glm(formula=VarInt~Size*Species,family=Gamma(link="log"), data=forage.data)
summary(SizeSpeciesint)

SizeDietint<-glm(formula=VarInt~Size*Diet,family=Gamma(link="log"), data=forage.data)
summary(SizeDietint)

SizeFuncint<-glm(formula=VarInt~Size*Func,family=Gamma(link="log"), data=forage.data)
summary(SizeFuncint) 

SSSpecies<-glm(formula=VarInt~Species+SS,family=Gamma(link="log"), data=forage.data)
summary(SSSpecies)

SSSpeciesint<-glm(formula=VarInt~Species*SS,family=Gamma(link="log"), data=forage.data)
summary(SSSpeciesint) ##Too many NAs

SSDiet<-glm(formula=VarInt~Diet+SS,family=Gamma(link="log"), data=forage.data)
summary(SSDiet) 

SSDietint<-glm(formula=VarInt~Diet*SS,family=Gamma(link="log"), data=forage.data)
summary(SSDietint) # Too many NAs

SSFunc<-glm(formula=VarInt~Func+SS,family=Gamma(link="log"), data=forage.data)
summary(SSFunc) 

SSFuncint<-glm(formula=VarInt~Func*SS, family=Gamma(link="log"),data=forage.data)
summary(SSFuncint) # NAs present

SSm<-glm(formula=VarInt~SS,family=Gamma(link="log"), data=forage.data)
summary(SSm)

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
                              'SSFuncint|SSDietint|SSSpeciesint|SizeSSFuncDiet|SizeSSSpeciesint|SizeSSSpeciesint|SizeSSDietint|SizeSSFuncint|forage.data')
model_list <- model_list[-which(model_list == '')]

varint_compare <- AICc(Dietm,Funcm,
                       SizeDiet,SizeDietint,
                       SizeFunc,SizeFuncint,
                       Sizem,SizeSpecies,SizeSpeciesint,
                       SizeSS,SizeSSDiet,SizeSSFunc,
                       SizeSSint,SizeSSSpecies,
                       Speciesm,SSDiet,
                       SSFunc,SSm,SSSpecies)
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



