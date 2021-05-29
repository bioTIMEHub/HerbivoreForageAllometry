

# Candidate models for tortuosity
# For response variable: Tortuosity
# Written/updated 02/04/2021
# Adapted by CC from area models script written by MD


# Environment set up ------------------------------------------------------

## Clear anything old
rm(list=ls(all=TRUE))
require(dplyr) # for data wrangling
require(DHARMa) # model checks
require(MuMIn)

## Import data ##
forage.data<-read.table('./src/SpForagingMetrics.csv',header=T) # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)

# Some abbreviations/metadata to be aware of
# Species= Species
# SG = Social grouping during feeding (single=1, pair=2, single species chool=3, mixed-species school =4)
# Diet = Diet (EAM=1, macroalgae=2, Detritus=3, Corticated algae=4, Cyanobacteria=5)
# Func = Functional group (excavator=1, scraper=2, grazer/detritivore=3, browser=4)
# Size = Total length (cm)
# IntFor = Mean Inter-foray distance (m)
# Area = Foraging Area
# Tort = Tortuosity ratio (sum of inter-foray/start-finish)
# VarInt = Variance of inter-foray distances
# Start_finish = straight line displacement between start and finish points

# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SG, Diet, Func), .fns = as.factor))
str(forage.data)


# Model fitting -----------------------------------------------------------

# fit all possible candidates

SizeSGFunc<-glm(formula=Tort~Size+SG+Func, family=Gamma(link="log"), data=forage.data)

SizeSGDiet<-glm(formula=Tort~Size+SG+Diet,family=Gamma(link="log"), data=forage.data)

SizeSGSpecies<-glm(formula=Tort~Size+SG+Species,family=Gamma(link="log"), data=forage.data)

SizeSGFuncint<-glm(formula=Tort~Size*SG*Func,family=Gamma(link="log"), data=forage.data)

SizeSGDietint<-glm(formula=Tort~Size*SG*Diet,family=Gamma(link="log"), data=forage.data)

SizeSGSpeciesint<-glm(formula=Tort~Size*SG*Species,family=Gamma(link="log"), data=forage.data)

SizeSGFuncDiet<-glm(formula=Tort~Size+SG+Func+Diet,family=Gamma(link="log"), data=forage.data)

SizeSG<-glm(formula=Tort~Size+SG,family=Gamma(link="log"), data=forage.data)

SizeSpecies<-glm(formula=Tort~Size+Species,family=Gamma(link="log"), data=forage.data)

SizeDiet<-glm(formula=Tort~Size+Diet,family=Gamma(link="log"), data=forage.data)

SizeFunc<-glm(formula=Tort~Size+Func,family=Gamma(link="log"), data=forage.data)

SizeSGint<-glm(formula=Tort~Size*SG,family=Gamma(link="log"), data=forage.data)

SizeSpeciesint<-glm(formula=Tort~Size*Species, family=Gamma(link='log'), data=forage.data)

SizeDietint<-glm(formula=Tort~Size*Diet,family=Gamma(link="log"), data=forage.data)

SizeFuncint<-glm(formula=Tort~Size*Func,family=Gamma(link="log"), data=forage.data)

SGSpecies<-glm(formula=Tort~Species+SG,family=Gamma(link="log"), data=forage.data)

SGSpeciesint<-glm(formula=Tort~Species*SG,family=Gamma(link="log"), data=forage.data)

SGDiet<-glm(formula=Tort~Diet+SG,family=Gamma(link="log"), data=forage.data)

SGDietint<-glm(formula=Tort~Diet*SG,family=Gamma(link="log"), data=forage.data)

SGFunc<-glm(formula=Tort~Func+SG,family=Gamma(link="log"), data=forage.data)

SGFuncint<-glm(formula=Tort~Func*SG, family=Gamma(link="log"),data=forage.data)

SGm<-glm(formula=Tort~SG,family=Gamma(link="log"), data=forage.data)

Speciesm<-glm(formula=Tort~Species,family=Gamma(link="log"), data=forage.data)

Funcm<-glm(formula=Tort~Func,family=Gamma(link="log"), data=forage.data)

Dietm<-glm(formula=Tort~Diet, family=Gamma(link="log"),data=forage.data)

Sizem<-glm(formula=Tort~Size, family=Gamma(link="log"),data=forage.data)

summary(SizeSGDiet)
summary(SizeSGSpecies)
summary(SizeSGFuncint) # NAs
summary(SizeSGDietint) # NAs
summary(SizeSGSpeciesint) # NAs
summary(SizeSGFuncDiet) # NAs
summary(SizeSG)
summary(SizeSpecies) 
summary(SizeDiet)
summary(SizeFunc)
summary(SizeSGint) # did not converge
summary(SizeSpeciesint)
summary(SizeDietint)
summary(SizeFuncint)
summary(SGSpecies)
summary(SGSpeciesint) # NAs
summary(SGDiet)
summary(SGDietint) # NAs
summary(SGFunc) 
summary(SGFuncint) # NAs
summary(SGm)
summary(Speciesm)
summary(Funcm)
summary(Dietm)
summary(SizeSGFunc) 
summary(Sizem)
#7 models without NAs


# Model comparison + selection --------------------------------------------

model_list <- c('SizeSGDiet',
                'SizeSGSpecies',
                'SizeSG',
                'SizeSpecies', 
                'SizeDiet',
                'SizeFunc',
                'SizeSpeciesint',
                'SizeDietint',
                'SizeFuncint',
                'SGSpecies',
                'SGDiet',
                'SGFunc', 
                'SGm',
                'Speciesm',
                'Funcm',
                'Dietm',
                'SizeSGFunc', 
                'Sizem')

tort_model_comparison <- AICc(SizeSGDiet,
                              SizeSGSpecies,
                              SizeSG,
                              SizeSpecies, 
                              SizeDiet,
                              SizeFunc,
                              SizeSpeciesint,
                              SizeDietint,
                              SizeFuncint,
                              SGSpecies,
                              SGDiet,
                              SGFunc, 
                              SGm,
                              Speciesm,
                              Funcm,
                              Dietm,
                              SizeSGFunc, 
                              Sizem)
tort_model_comparison$AICc <- tort_model_comparison$AICc %>% round(., 3)

# also by residual deviance
tort_model_comparison$ResDev <- sapply(mget(model_list), deviance)
tort_model_comparison$ResDev <- sapply(tort_model_comparison$ResDev, round, 3)
tort_model_comparison <- tort_model_comparison %>% arrange(AICc)

# calculate dAICc
tort_model_comparison$dAIC <- rep('', nrow(tort_model_comparison))
for (i in 2:nrow(tort_model_comparison)) { # calculate AIC difference after ranking
  tort_model_comparison$dAIC[i] <- with(tort_model_comparison, AICc[i]-AICc[1])
}

tort_model_comparison$dAIC <- tort_model_comparison$dAIC %>% as.numeric() %>% round(., 3)

write.csv(tort_model_comparison, 'analysis_outputs/tort_selectiontable.csv') # model comparison output
write.csv(summary(SizeSpecies)$coefficients, 'analysis_outputs/tort-sizespecies.csv') # model parameter estimates output
write.csv(summary(SizeDiet)$coefficients, 'analysis_outputs/tort-sizediet.csv') # diet to discuss in case
saveRDS(SizeSpecies, 'analysis_outputs/TortModel.rds') # save the selected model object
save(list=model_list, file='analysis_outputs/tortmodels_all.RData') # save this as an RData object for loading elsewhere

# Performance/residual checks ---------------------------------------------

check_model <- simulateResiduals(fittedModel = SizeSpecies, n = 500)
plot(check_model)
# select the second model based on residual deviance as the tiebreaker after dAICc

plot(get(tort_model_comparison$model[2]))

