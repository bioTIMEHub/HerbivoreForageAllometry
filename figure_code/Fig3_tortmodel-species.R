

# Figure generation for selected tortuosity model
# For response variable: Tortuosity
# Written/updated 05/04/2021
# Author CC, similar to Area plot with CIs

## Clear anything old
rm(list=ls(all=TRUE)) 

require(tidyverse)
require(patchwork)
require(scales)

# make sure we load data
forage.data<-read.table('./src/SpForagingMetrics.csv',header=T, sep=',') # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)
# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, Phase, SG, Diet, Func), .fns = as.factor))

# model fit result from our analysis
tort.model <- glm(data=forage.data, formula=Tort ~ Size + Species, family = Gamma(link='log'))

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

#Establish CIs
critval <- 1.96 ## approx 95% CI

# create a name object to link the sp codes with their proper spelling
species <- c(
  'S. doliatus',
  'Sc. frenatus',
  'A. nigricauda', 
  'Sc. rivulatus', 
  'Z. scopas', 
  'Ch. spilurus',
  'C. striatus',
  'N. unicornis',
  'S. vulpinus'
)

names(species) <- unique(forage.data$Species)


# Create new data for predictions -----------------------------------------

sp.data <- as.list(rep('', 9))
pred <- as.list(rep('', 9))
ci <- as.list(rep('', 9))

names(sp.data) <- unique(forage.data$Species)
names(pred) <- unique(forage.data$Species)
names(ci) <- unique(forage.data$Species)

for (i in 1:9) {
  sp.data[[i]] <- data.frame(
    Species = rep(unique(forage.data$Species)[i], 100), 
    Size = runif(100, min = forage.data %>% filter(Species == unique(Species)[i]) %>% select(Size) %>% as_vector() %>% min(),
                 max = forage.data %>% filter(Species == unique(Species)[i]) %>% select(Size) %>% as_vector() %>% max())
  )
  sp.data[[i]] <- sp.data[[i]] %>% arrange(Size)
  
  pred[[i]] <- predict.glm(tort.model, type=c("response"), se.fit=TRUE, newdata=sp.data[[i]])
  ci[[i]] <- data.frame(upper = pred[[i]]$fit + (critval * pred[[i]]$se.fit), 
                    lower = pred[[i]]$fit - (critval * pred[[i]]$se.fit))
  
}

# predictions for species if body size was held at the mean

part.data <- forage.data %>% mutate(Size = mean(Size)) %>% select(Species, Size)
pred.part <- predict(tort.model, type=c("response"), se.fit=TRUE,
                     newdata=part.data)
# data frame of predictions, confidence intervals, and variables
partials <- data.frame(upper = pred.part$fit + (critval * pred.part$se.fit), 
                      lower = pred.part$fit - (critval * pred.part$se.fit),
                      pred = pred.part$fit) %>% bind_cols(., part.data) %>% distinct()
# order by descending 
tort_order <- partials %>% arrange(desc(pred)) %>% select(Species) %>% as_vector() %>% as.character.factor()
partials$Species <- factor(partials$Species, levels=tort_order)

# Panel loop --------------------------------------------------------------

# Base R option
# par(mfrow=c(3,3))
# par(mar=c(1.5,1.5,1.5,1.5))
# par(oma=c(5,5,0,0))

pal <- viridis(9, option='C', begin=0.1, end=0.9)
pal.a <- viridis(9, option='C', begin=0.1, end=0.9, alpha=0.3)

# the partial regression for species
all.panel <- ggplot(data = partials) + 
  geom_linerange(aes(x=Species, ymin=lower, ymax=upper), color = 'grey30', size=0.5) +
  geom_point(aes(x=Species, y=pred), fill='grey50', shape=21, size=4.5) +
  theme_classic(base_size = 12, base_family = 'Helvetica') +
  scale_y_continuous(trans=scales::pseudo_log_trans(base=exp(1)), limits = c(0.5,20), breaks = c(1,2,5,10,20)) +
  scale_x_discrete(labels=str_replace_all(tort_order, species)) +
  labs(x=NULL, y='Tortuosity') +
  theme(axis.text.x=element_text(angle=30, hjust = 1, face = 'italic'))
all.panel
ggsave(plot=all.panel, '../figures/Fig3b_tort_sp.pdf', device='pdf', width = 140, height = 50, units = 'mm')

# empty list to input plot objects into
panel <- as.list(rep('', 9))

for (i in 1:9) {
panel[[i]] <- ggplot() +
    geom_point(data = forage.data %>% filter(!Species == tort_order[i]),
               aes(y=Tort, x=Size), shape=21, fill='transparent', color='grey90', size=0.9) +
    geom_ribbon(data = ci[[tort_order[i]]] %>% bind_cols(., Size = sp.data[[tort_order[i]]]$Size), aes(ymax=upper, ymin=lower, x=Size), 
                fill='transparent', color = 'grey30', linetype='dashed', size=0.7) +
    geom_line(data = bind_cols(Fit = pred[[tort_order[i]]]$fit, Size = sp.data[[tort_order[i]]]$Size), aes(x=Size, y=Fit), color = 'black') +
    geom_point(data = forage.data %>% filter(Species == tort_order[i]), aes(y=Tort, x=Size, shape=Phase), color='black', fill='black', size=1.7) +
    annotate("text", label = species[tort_order[i]], x = 7, y = 100, fontface=3, hjust=0) +
    theme_classic(base_size = 12, base_family = 'Helvetica') + labs(x=NULL, y=NULL) +
    scale_y_continuous(trans=scales::pseudo_log_trans(base=exp(1)), limits = c(0,100), breaks = c(1,5,10,20,50,100)) +
    scale_shape_manual(values=c(21,2,24), guide=NULL)

  ## Base R option
  # plot(Tort ~ Size, data=forage.data %>% filter(!Species == unique(Species)[i]), las=1, type="p", bty="l",pch=1, col='#AAAAAA88',
  #      xlab="",ylab="",tck=0.06, xlim=c(5,35), ylim=c(1,100),log="y")
  # points(Tort ~ Size, data=forage.data %>% filter(Species == unique(Species)[i]), type="p",bty="l", pch=16, cex=1.2)
  # points(pred[[i]]$fit ~ sp.data[[i]]$Size, type="l")
  # points(ci[[i]]$upper ~ sp.data[[i]]$Size, type="l", lty=2)
  # points(ci[[i]]$lower ~ sp.data[[i]]$Size, type="l", lty=2)
  # mtext(species[i], side=3, line=-1, adj=0.05, family="", font=3)
}

for (i in c(2,3,5,6,8,9)) {
  panel[[i]] <- panel[[i]] + theme(axis.text.y=element_blank())
}
for (i in c(1:6)) {
  panel[[i]] <- panel[[i]] + theme(axis.text.x=element_blank())
}

(panel[[1]] + panel[[2]] + panel[[3]] + panel[[4]] + panel[[5]] + panel[[6]] + panel[[7]] + panel[[8]] + panel[[9]]) / all.panel + plot_layout(heights=c(4,1))
ggsave('../figures/Fig3_tort_model_rev.pdf', device='pdf', width = 175, height = 200, units = 'mm')

# title(xlab="Total length (cm)",ylab='Foraging tortuosity', outer=T,
#       cex.lab=1.7, family="")


