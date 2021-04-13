
# Univariate plots for area and tortuosity
# Authors: EW, MD, CC
# Updated by CC in Apr 2021 for better plotting legibility

rm(list=ls(all=TRUE))
require(tidyverse)
require(patchwork)
load('areamodels_all.RData')
forage.data<-read.table('../original/src/R-data_Lizard_ellipse_Area.txt',header=T) # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)

# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SS, Diet, Func), .fns = as.factor))

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

# Calculate confidence intervals ------------------------------------------

#Establishing CI
critval <- 1.96 ## approx 95% CI

# Calculate CI for each univariate area model
area.pred <- list(rep('', 5)) # make an empty list for inputting predictions for each univar area model
area.ci <- list(rep('', 5))
univars <- c('Sizem', 'Speciesm', 'SSm', 'Dietm', 'Funcm') # vector of univariate models to loop
for (i in 1:5) { # calculate predictions and CIs for each univariate model
  area.pred[[i]]<-predict.glm(get(univars[i]), type=c("response"),se.fit=TRUE)
  area.ci[[i]] <- data.frame(upper = area.pred[[i]]$fit + (critval * area.pred[[i]]$se.fit), 
                        lower = area.pred[[i]]$fit - (critval * area.pred[[i]]$se.fit))
}
model_list <- c('SSDiet', 'SSSpecies', 'SizeDiet', 'SizeDietint','SizeFunc',
                'SizeFuncint', 'SizeSS', 'SizeSSDiet', 'SizeSSSpecies', 'SizeSSint',
                'SizeSpecies', 'SizeSpeciesint','SSm', 'Speciesm', 'Funcm', 'Dietm', 'SSFunc',
                'SizeSSFunc', 'Sizem') # list of candidates without NAs for comparison
rm(list=model_list)

# again for tortuosity
load('tortmodels_all.RData') # load tortuosity model data
tort.pred <- list(rep('', 5)) # make an empty list for inputting predictions for each univar area model
tort.ci <- list(rep('', 5))
for (i in 1:5) { # calculate predictions and CIs for each univariate model
  tort.pred[[i]]<-predict.glm(get(univars[i]), type=c("response"), se.fit=TRUE)
  tort.ci[[i]] <- data.frame(upper = tort.pred[[i]]$fit + (critval * tort.pred[[i]]$se.fit), 
                             lower = tort.pred[[i]]$fit - (critval * tort.pred[[i]]$se.fit))
}

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

rm(list=model_list)


# Area plot panels --------------------------------------------------------------

# order species by family
forage.data$Species <- factor(forage.data$Species, levels=c("nigricauda", "striatus","unicornis", "scopas", "sordidus", "frenatus", "rivulatus", "doliatus", "vulpinis"))
sp_names <- c("A. nigricauda", "C. striatus", "N. unicornis", "Z. scopas","Ch. spilurus", "Sc. frenatus", "Sc. rivulatus", "S. doliatus", "S. vulpinus")
diet_names <- c('EAM', 'macro- algae', 'detritus', 'corticated algae', 'cyano- bacteria')
func_names <- c('excavator', 'scraper', 'grazer/detritivore', 'browser')
ss_names <- c('single', 'pair', 'single sp school', 'mixed-sp school')

base <- ggplot() +
  theme_classic(base_size = 12, base_family = 'Helvetica') + ylab(element_blank())
# Body size

A.body <- base +
  geom_ribbon(data = area.ci[[1]] %>% bind_cols(., Size = forage.data$Size), aes(ymax=upper, ymin=lower, x=Size),
              fill = 'transparent', color = 'black', linetype='dashed') +
  geom_point(data=forage.data, aes(y=Area, x = Size), size = 1.7, alpha = 0.8, shape =21) +
  geom_line(data = bind_cols(Fit = area.pred[[1]]$fit, Size = forage.data$Size), aes(x=Size, y=Fit), color = 'black') +
  xlab('Total length (cm)')

A.sp <- base +
  geom_hline(aes(yintercept = mean(forage.data$Area)), linetype='dashed', alpha = 0.6, size=0.6) +
  geom_violin(data = forage.data, aes(x=Species, y=Area), fill = 'white', color = 'grey30', size = 0.25, trim=F) +
  geom_jitter(data = forage.data, aes(x=Species, y=Area), width = 0.1, size = 1, fill = 'black', alpha = 0.5, shape = 21) +
  geom_errorbar(data = bind_cols(Fit = area.pred[[2]]$fit, Species = forage.data$Species) %>% distinct(), 
             aes(ymax=Fit, ymin=Fit, x=Species), size=1, color = 'black') +
  xlab('Species') + scale_x_discrete(labels=sp_names) +
  theme(axis.text.x=element_text(angle = 30, hjust = 1, face = 'italic'))

A.ss <- base +
  geom_hline(aes(yintercept = mean(forage.data$Area)), linetype='dashed', alpha = 0.6, size=0.6) +
  geom_violin(data = forage.data, aes(x=SS, y=Area), fill = 'white', color = 'grey30', size = 0.25, trim=F) +
  geom_jitter(data = forage.data, aes(x=SS, y=Area), width = 0.1, size = 1, fill = 'black', alpha = 0.5, shape = 21) +
  geom_errorbar(data = bind_cols(Fit = area.pred[[3]]$fit, SS = forage.data$SS) %>% distinct(), 
                aes(ymax=Fit, ymin=Fit, x=SS), size=1, color = 'black') +
  xlab('Social structure') + scale_x_discrete(labels=str_wrap(ss_names, width = 10))

A.diet <- base +
  geom_hline(aes(yintercept = mean(forage.data$Area)), linetype='dashed', alpha = 0.6, size=0.6) +
  geom_violin(data = forage.data, aes(x=Diet, y=Area), fill = 'white', color = 'grey30', size = 0.25, trim=F) +
  geom_jitter(data = forage.data, aes(x=Diet, y=Area), width = 0.1, size = 1, fill = 'black', alpha = 0.5, shape = 21) +
  geom_errorbar(data = bind_cols(Fit = area.pred[[4]]$fit, Diet = forage.data$Diet) %>% distinct(), 
                aes(ymax=Fit, ymin=Fit, x=Diet), size=1, color = 'black') +
  xlab('Diet') + scale_x_discrete(labels=str_wrap(diet_names, width = 10)) +
  theme(axis.text.x=element_text(angle = 30, hjust = 1))

A.func <- base +
  geom_hline(aes(yintercept = mean(forage.data$Area)), linetype='dashed', alpha = 0.6, size=0.6) +
  geom_violin(data = forage.data, aes(x=Func, y=Area), fill = 'white', color = 'grey30', size = 0.25, trim=F) +
  geom_jitter(data = forage.data, aes(x=Func, y=Area), width = 0.1, size = 1, fill = 'black', alpha = 0.5, shape = 21) +
  geom_errorbar(data = bind_cols(Fit = area.pred[[5]]$fit, Func = forage.data$Func) %>% distinct(), 
                aes(ymax=Fit, ymin=Fit, x=Func), size=1, color = 'black') +
  xlab('Functional group') + scale_x_discrete(labels=str_wrap(func_names, width = 10))

# Tort plot panels --------------------------------------------------------

T.body <- base +
  geom_ribbon(data = tort.ci[[1]] %>% bind_cols(., Size = forage.data$Size), aes(ymax=upper, ymin=lower, x=Size),
              fill = 'transparent', color = 'black', linetype='dashed') +
  geom_point(data=forage.data, aes(y=Tort, x = Size), size = 1.7, alpha = 0.8, shape=21) +
  geom_line(data = bind_cols(Fit = tort.pred[[1]]$fit, Size = forage.data$Size), aes(x=Size, y=Fit), color = 'black') +
  xlab('Total length (cm)')

T.sp <- base +
  geom_hline(aes(yintercept = mean(forage.data$Tort)), linetype='dashed', alpha = 0.6, size=0.6) +
  geom_violin(data = forage.data, aes(x=Species, y=Tort), fill = 'white', color = 'grey30', size = 0.25, trim=F) +
  geom_jitter(data = forage.data, aes(x=Species, y=Tort), width = 0.1, size = 1, fill = 'black', alpha = 0.5, shape = 21) +
  geom_errorbar(data = bind_cols(Fit = tort.pred[[2]]$fit, Species = forage.data$Species) %>% distinct(), 
                aes(ymax=Fit, ymin=Fit, x=Species), size=1, color = 'black') +
  xlab('Species') + scale_x_discrete(labels=sp_names) +
  theme(axis.text.x=element_text(angle = 30, hjust = 1, face = 'italic'))

T.ss <- base +
  geom_hline(aes(yintercept = mean(forage.data$Tort)), linetype='dashed', alpha = 0.6, size=0.6) +
  geom_violin(data = forage.data, aes(x=SS, y=Tort), fill = 'white', color = 'grey30', size = 0.25, trim=F) +
  geom_jitter(data = forage.data, aes(x=SS, y=Tort), width = 0.1, size = 1, fill = 'black', alpha = 0.5, shape = 21) +
  geom_errorbar(data = bind_cols(Fit = tort.pred[[3]]$fit, SS = forage.data$SS) %>% distinct(), 
                aes(ymax=Fit, ymin=Fit, x=SS), size=1, color = 'black') +
  xlab('Social structure') + scale_x_discrete(labels=str_wrap(ss_names, width = 10))

T.diet <- base +
  geom_hline(aes(yintercept = mean(forage.data$Tort)), linetype='dashed', alpha = 0.6, size=0.6) +
  geom_violin(data = forage.data, aes(x=Diet, y=Tort), fill = 'white', color = 'grey30', size = 0.25, trim=F) +
  geom_jitter(data = forage.data, aes(x=Diet, y=Tort), width = 0.1, size = 1, fill = 'black', alpha = 0.5, shape = 21) +
  geom_errorbar(data = bind_cols(Fit = tort.pred[[4]]$fit, Diet = forage.data$Diet) %>% distinct(), 
                aes(ymax=Fit, ymin=Fit, x=Diet), size=1, color = 'black') +
  xlab('Diet') + scale_x_discrete(labels=str_wrap(diet_names, width = 10)) +
  theme(axis.text.x=element_text(angle = 30, hjust = 1))

T.func <- base +
  geom_hline(aes(yintercept = mean(forage.data$Tort)), linetype='dashed', alpha = 0.6, size=0.6) +
  geom_violin(data = forage.data, aes(x=Func, y=Tort), fill = 'white', color = 'grey30', size = 0.25, trim=F) +
  geom_jitter(data = forage.data, aes(x=Func, y=Tort), width = 0.1, size = 1, fill = 'black', alpha = 0.5, shape = 21) +
  geom_errorbar(data = bind_cols(Fit = tort.pred[[5]]$fit, Func = forage.data$Func) %>% distinct(), 
                aes(ymax=Fit, ymin=Fit, x=Func), size=1, color = 'black') +
  xlab('Functional group') + scale_x_discrete(labels=str_wrap(func_names, width = 10))

(A.body + A.sp) / (A.ss + A.diet + A.func) & scale_y_continuous(trans='log', limits = c(1,800), breaks = c(0,1,5,10,50,100,500))
ggsave('Fig_univariate_area.svg', device = 'svg', width = 260, height = 160, units = 'mm')

(T.body + T.sp) / (T.ss + T.diet + T.func) & scale_y_continuous(trans='log', limits = c(1,80), breaks = c(1,5,10,20,50,100))
ggsave('Fig_univariate_tort.svg', device = 'svg', width = 260, height = 160, units = 'mm')

univar.patch <- (A.body / A.sp / A.ss / A.diet / A.func) * scale_y_continuous(trans='log', limits = c(1,1000), breaks = c(0,1,5,10,50,100,500,1000)) * ylab(expression('Foraging area ('~m^2~')')) | 
  (T.body / T.sp / T.ss / T.diet / T.func) * scale_y_continuous(trans='log', limits = c(1,80), breaks = c(1,5,10,20,50,100)) * ylab('Tortuosity')
univar.patch
ggsave('../figures/Fig2_univariate_areatort.pdf', device='pdf', width = 200, height = 400, units = 'mm')

# Earlier iteration
# library(beanplot)
# setEPS()
# postscript("Fig2Emmy.eps")
# par(mfrow =c(3,2))
# # setting up figure with 3x2 plots
# plot(forage.data$Size, forage.data$Area, log="y", xlab="size", ylab="foraging area", pch=16)
# points(sizemodepred$fit ~ forage.data$Size,type="l")
# points(sizeupr ~ forage.data$Size,type="l",lty=2)
# points(sizewr ~ forage.data$Size,type="l",lty=2)
# beanplot(forage.data$Area ~ forage.data$SS,bty="n",las=1,xaxt="n")
# beanplot(forage.data$Area ~ forage.data$Func,bty="n",las=1,xaxt="n")
# beanplot(forage.data$Area ~ sort(levels(forage.data$Species)),bty="n",las=1)
# beanplot(forage.data$Area ~ forage.data$Diet,bty="n",las=1,xaxt="n")
# dev.off()
