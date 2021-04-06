

# Candidate models for tortuosity
# For response variable: Tortuosity
# Written/updated 02/04/2021
# Adapted by CC from area models script written by MD


# Environment set up ------------------------------------------------------

## Clear anything old
rm(list=ls(all=TRUE))
require(dplyr) # for data wrangling
require(DHARMa) # model checks

## Import data ##
forage.data<-read.table('../original/src/R-data_Lizard_ellipse_Area.txt',header=T) # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)

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

# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SS, Diet, Func), .fns = as.factor))
str(forage.data)


# Model fitting -----------------------------------------------------------

# fit all possible candidates

SizeSSFunc<-glm(formula=Tort~Size+SS+Func, family=Gamma(link="log"), data=forage.data)

SizeSSDiet<-glm(formula=Tort~Size+SS+Diet,family=Gamma(link="log"), data=forage.data)

SizeSSSpecies<-glm(formula=Tort~Size+SS+Species,family=Gamma(link="log"), data=forage.data)

SizeSSFuncint<-glm(formula=Tort~Size*SS*Func,family=Gamma(link="log"), data=forage.data)

SizeSSDietint<-glm(formula=Tort~Size*SS*Diet,family=Gamma(link="log"), data=forage.data)

SizeSSSpeciesint<-glm(formula=Tort~Size*SS*Species,family=Gamma(link="log"), data=forage.data)

SizeSSFuncDiet<-glm(formula=Tort~Size+SS+Func+Diet,family=Gamma(link="log"), data=forage.data)

SizeSS<-glm(formula=Tort~Size+SS,family=Gamma(link="log"), data=forage.data)

SizeSpecies<-glm(formula=Tort~Size+Species,family=Gamma(link="log"), data=forage.data)

SizeDiet<-glm(formula=Tort~Size+Diet,family=Gamma(link="log"), data=forage.data)

SizeFunc<-glm(formula=Tort~Size+Func,family=Gamma(link="log"), data=forage.data)

SizeSSint<-glm(formula=Tort~Size*SS,family=Gamma(link="log"), data=forage.data)

SizeSpeciesint<-glm(formula=Tort~Size*Species, family=Gamma(link='log'), data=forage.data)

SizeDietint<-glm(formula=Tort~Size*Diet,family=Gamma(link="log"), data=forage.data)

SizeFuncint<-glm(formula=Tort~Size*Func,family=Gamma(link="log"), data=forage.data)

SSSpecies<-glm(formula=Tort~Species+SS,family=Gamma(link="log"), data=forage.data)

SSSpeciesint<-glm(formula=Tort~Species*SS,family=Gamma(link="log"), data=forage.data)

SSDiet<-glm(formula=Tort~Diet+SS,family=Gamma(link="log"), data=forage.data)

SSDietint<-glm(formula=Tort~Diet*SS,family=Gamma(link="log"), data=forage.data)

SSFunc<-glm(formula=Tort~Func+SS,family=Gamma(link="log"), data=forage.data)

SSFuncint<-glm(formula=Tort~Func*SS, family=Gamma(link="log"),data=forage.data)

SSm<-glm(formula=Tort~SS,family=Gamma(link="log"), data=forage.data)

Speciesm<-glm(formula=Tort~Species,family=Gamma(link="log"), data=forage.data)

Funcm<-glm(formula=Tort~Func,family=Gamma(link="log"), data=forage.data)

Dietm<-glm(formula=Tort~Diet, family=Gamma(link="log"),data=forage.data)

Sizem<-glm(formula=Tort~Size, family=Gamma(link="log"),data=forage.data)

summary(SizeSSDiet)
summary(SizeSSSpecies)
summary(SizeSSFuncint) # NAs
summary(SizeSSDietint) # NAs
summary(SizeSSSpeciesint) # NAs
summary(SizeSSFuncDiet) # NAs
summary(SizeSS)
summary(SizeSpecies) 
summary(SizeDiet)
summary(SizeFunc)
summary(SizeSSint) # did not converge
summary(SizeSpeciesint)
summary(SizeDietint)
summary(SizeFuncint)
summary(SSSpecies)
summary(SSSpeciesint) # NAs
summary(SSDiet)
summary(SSDietint) # NAs
summary(SSFunc) 
summary(SSFuncint) # NAs
summary(SSm)
summary(Speciesm)
summary(Funcm)
summary(Dietm)
summary(SizeSSFunc) 
summary(Sizem)
#7 models without NAs


# Model comparison + selection --------------------------------------------

model_list <- c('SizeSSDiet',
                'SizeSSSpecies',
                'SizeSS',
                'SizeSpecies', 
                'SizeDiet',
                'SizeFunc',
                'SizeSpeciesint',
                'SizeDietint',
                'SizeFuncint',
                'SSSpecies',
                'SSDiet',
                'SSFunc', 
                'SSm',
                'Speciesm',
                'Funcm',
                'Dietm',
                'SizeSSFunc', 
                'Sizem')

tort_model_comparison <- tibble(model=model_list)
tort_model_comparison$AICc <- sapply(mget(model_list), performance_aicc)
tort_model_comparison$AICc <- tort_model_comparison$AICc %>% round(., 3)
tort_model_comparison <- tort_model_comparison %>% arrange(AICc)

# also by residual deviance
tort_model_comparison$ResDev <- sapply(mget(model_list), deviance)
tort_model_comparison$ResDev <- sapply(tort_model_comparison$ResDev, round, 3)

# calculate dAICc
tort_model_comparison$dAIC <- rep('', nrow(tort_model_comparison))
for (i in 2:nrow(tort_model_comparison)) { # calculate AIC difference after ranking
  tort_model_comparison$dAIC[i] <- with(tort_model_comparison, AICc[i]-AICc[1])
}

tort_model_comparison$dAIC <- tort_model_comparison$dAIC %>% as.numeric() %>% round(., 3)

write.csv(tort_model_comparison, 'tort_selectiontable.csv')
saveRDS(SizeSpecies, '../original/src/TortModel.rds') # save the selected model object
save(list=model_list, file='tortmodels_all.RData') # save this as an RData object for loading elsewhere

# Performance/residual checks ---------------------------------------------

check_model <- simulateResiduals(fittedModel = SizeSpecies, n = 500)
plot(check_model)
# select the second model based on residual deviance as the tiebreaker after dAICc

plot(get(tort_model_comparison$model[2]))

