

# Sensitivity analyses
# For response variable: foraging area, tortuosity, and inter-foray distance
# Written/updated 12/05/2021
# Author: CC

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

# Environment set up ------------------------------------------------------

## Clear anything old
rm(list=ls(all=TRUE))
require(dplyr) # for data wrangling

## Import data ##
forage.data<-read.table('../original/src/R-data_Lizard_ellipse_Area.txt',header=T) # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)

# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SS, Diet, Func), .fns = as.factor))
str(forage.data)

# fit the selected models (resulting from model selection)

tort.model <- glm(formula = Tort ~ Size + Species, family=Gamma(link="log"), data=forage.data)
area.model <- glm(formula = Area ~ Species, family=Gamma(link="log"), data=forage.data)
intfor.model <- glm(formula= IntFor ~ Size * Species, family=Gamma(link="log"), data=forage.data)

# fit the models without the outlier record(s) in the independent and response variables

tort.s <- glm(formula = Tort ~ Size + Species, family=Gamma(link="log"),
              data = forage.data %>% filter(!Size == max(Size), !Tort == max(Tort)))
area.s <- glm(formula = Area ~ Species, family=Gamma(link="log"), 
              data = forage.data %>% filter(!Area == max(Area)))
intfor.s <- glm(formula= IntFor ~ Size * Species, family=Gamma(link="log"), 
                data = forage.data %>% filter(!Size == max(Size)))

# Tortuosity
# ∆ deviance
deviance(tort.model) - deviance(tort.s)
# ∆ effect estimates
delta.tort <- (summary(tort.model)$coefficients[,1:2] - summary(tort.s)$coefficients[,1:2]) %>% as_tibble()

# Area
# ∆ deviance
deviance(area.model) - deviance(area.s)
# ∆ effect estimates
delta.area <- (summary(area.model)$coefficients[,1:2] - summary(area.s)$coefficients[,1:2]) %>% as_tibble()


# Inter-foray (mean)
# ∆ deviance
deviance(intfor.model) - deviance(intfor.s)
# ∆ effect estimates
delta.intfor <- (summary(intfor.model)$coefficients[,1:2] - summary(intfor.s)$coefficients[,1:2]) %>% as_tibble()
