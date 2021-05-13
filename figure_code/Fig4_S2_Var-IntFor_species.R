##New plots using predict and model CIs
##For IntFor
##Written 22/5/2015

## Clear anything old
rm(list=ls(all=TRUE)) 
require(tidyverse)
require(patchwork)

# make sure we load data
forage.data<-read.table('../original/src/R-data_Lizard_ellipse_Area.txt',header=T) # path relative to repo project folder
forage.data<- forage.data %>% arrange(Species, Size)
# fix column data type
forage.data <- forage.data %>% mutate(across(c(Species, SS, Diet, Func), .fns = as.factor))

# order alphabetically
forage.data$Species <- factor(forage.data$Species, 
                              levels=c("nigricauda", "striatus","unicornis", "scopas", "sordidus", "frenatus", "rivulatus", "doliatus", "vulpinis"))
# Proper species label object
sp_names <- c("A. nigricauda", "C. striatus", "N. unicornis", 
              "Z. scopas","Ch. spilurus", "Sc. frenatus", "Sc. rivulatus", "S. doliatus", "S. vulpinus")

# load models
int.model <- glm(formula = IntFor ~ Size + Species, family=Gamma(link="log"), data=forage.data)
var.model <- glm(formula = VarInt ~ Size + Species, family=Gamma(link="log"), data=forage.data)

#Establish CIs
critval <- 1.96 ## approx 95% CI

# Create new data for predictions -----------------------------------------

sp.data <- as.list(rep('', 9))
int.pred <- as.list(rep('', 9))
int.ci <- as.list(rep('', 9))

var.pred <- as.list(rep('', 9))
var.ci <- as.list(rep('', 9))

names(sp.data) <- levels(forage.data$Species)
names(int.pred) <- levels(forage.data$Species)
names(int.ci) <- levels(forage.data$Species)
names(var.pred) <- levels(forage.data$Species)
names(var.ci) <- levels(forage.data$Species)


for (i in 1:9) {
   sp.data[[i]] <- data.frame(
      Species = rep(levels(forage.data$Species)[i], 100), 
      Size = runif(100, min = forage.data %>% filter(Species == levels(Species)[i]) %>% select(Size) %>% as_vector() %>% min(),
                   max = forage.data %>% filter(Species == levels(Species)[i]) %>% select(Size) %>% as_vector() %>% max())
   )
   sp.data[[i]] <- sp.data[[i]] %>% arrange(Size)
   
   int.pred[[i]] <- predict.glm(int.model, type=c("response"), se.fit=TRUE, newdata=sp.data[[i]])
   int.ci[[i]] <- data.frame(upper = int.pred[[i]]$fit + (critval * int.pred[[i]]$se.fit), 
                         lower = int.pred[[i]]$fit - (critval * int.pred[[i]]$se.fit))
   var.pred[[i]] <- predict.glm(var.model, type=c("response"), se.fit=TRUE, newdata=sp.data[[i]])
   var.ci[[i]] <- data.frame(upper = var.pred[[i]]$fit + (critval * var.pred[[i]]$se.fit), 
                             lower = var.pred[[i]]$fit - (critval * var.pred[[i]]$se.fit))
}

# Mean inter-foray panel loop --------------------------------------------------------------

# Base R option
# par(mfrow=c(3,3))
# par(mar=c(1.5,1.5,1.5,1.5))
# par(oma=c(5,5,0,0))

# empty list to input plot objects into
int.panel <- as.list(rep('', 9))

for (i in 1:9) {
   int.panel[[i]] <- ggplot() +
      geom_point(data = forage.data %>% filter(!Species == levels(forage.data$Species)[i]),
                 aes(y=IntFor, x=Size), shape=21, fill='transparent', color='grey90', size=0.9) +
      geom_ribbon(data = int.ci[[i]] %>% bind_cols(., Size = sp.data[[i]]$Size),
                  aes(ymax=upper, ymin=lower, x=Size), 
                  fill = 'transparent', color = 'black', linetype='dashed', size=0.5) +
      geom_line(data = bind_cols(Fit = int.pred[[i]]$fit, Size = sp.data[[i]]$Size),
                aes(x=Size, y=Fit), color = 'black') +
      geom_point(data = forage.data %>% filter(Species == levels(forage.data$Species)[i]), aes(y=IntFor, x=Size), color='black', size=1.7) +
      annotate("text", label = sp_names[i], x = 7, y = 12, fontface=3, hjust=0) +
      theme_classic(base_size = 12, base_family = 'Helvetica') + labs(x=NULL, y=NULL) +
      scale_y_continuous(trans=scales::pseudo_log_trans(base=exp(1)), limits = c(0,12), breaks = c(1,2,5,10))
   
   ## Base R option
   # plot(IntFor ~ Size, data=forage.data %>% filter(!Species == unique(Species)[i]), las=1, type="p", bty="l",pch=1, col='#AAAAAA88',
   #      xlab="",ylab="",tck=0.06, xlim=c(5,35), ylim=c(1,100),log="y")
   # points(IntFor ~ Size, data=forage.data %>% filter(Species == unique(Species)[i]), type="p",bty="l", pch=16, cex=1.2)
   # points(int.pred[[i]]$fit ~ sp.data[[i]]$Size, type="l")
   # points(int.ci[[i]]$upper ~ sp.data[[i]]$Size, type="l", lty=2)
   # points(int.ci[[i]]$lower ~ sp.data[[i]]$Size, type="l", lty=2)
   # mtext(species[i], side=3, line=-1, adj=0.05, family="", font=3)
}

for (i in c(2,3,5,6,8,9)) {
   int.panel[[i]] <- int.panel[[i]] + theme(axis.text.y=element_blank())
}
for (i in c(1:6)) {
   int.panel[[i]] <- int.panel[[i]] + theme(axis.text.x=element_blank())
}

# print and save
(int.panel[[1]] + int.panel[[2]] + int.panel[[3]] + int.panel[[4]] + int.panel[[5]] + int.panel[[6]] + int.panel[[7]] + int.panel[[8]] + int.panel[[9]])
ggsave('../figures/Fig4_intfor_model.eps', device='eps', width = 175, height = 160, units = 'mm')

# Mean inter-foray panel loop --------------------------------------------------------------

# Base R option
# par(mfrow=c(3,3))
# par(mar=c(1.5,1.5,1.5,1.5))
# par(oma=c(5,5,0,0))

# empty list to input plot objects into
var.panel <- as.list(rep('', 9))

for (i in 1:9) {
   var.panel[[i]] <- ggplot() +
      geom_point(data = forage.data %>% filter(!Species == levels(forage.data$Species)[i]),
                 aes(y=VarInt, x=Size), shape=21, fill='transparent', color='grey90', size=0.9) +
      geom_ribbon(data = var.ci[[i]] %>% bind_cols(., Size = sp.data[[i]]$Size),
                  aes(ymax=upper, ymin=lower, x=Size), 
                  fill = 'transparent', color = 'black', linetype='dashed', size=0.5) +
      geom_line(data = bind_cols(Fit = var.pred[[i]]$fit, Size = sp.data[[i]]$Size),
                aes(x=Size, y=Fit), color = 'black') +
      geom_point(data = forage.data %>% filter(Species == levels(forage.data$Species)[i]), aes(y=VarInt, x=Size), color='black', size=1.7) +
      annotate("text", label = sp_names[i], x = 7, y = 120, fontface=3, hjust=0) +
      theme_classic(base_size = 12, base_family = 'Helvetica') + labs(x=NULL, y=NULL) +
      scale_y_continuous(trans=scales::pseudo_log_trans(base=exp(1)), limits = c(0,120), breaks = c(1,5,10,20,50,100))
   
   ## Base R option
   # plot(VarInt ~ Size, data=forage.data %>% filter(!Species == unique(Species)[i]), las=1, type="p", bty="l",pch=1, col='#AAAAAA88',
   #      xlab="",ylab="",tck=0.06, xlim=c(5,35), ylim=c(1,100),log="y")
   # points(VarInt ~ Size, data=forage.data %>% filter(Species == unique(Species)[i]), type="p",bty="l", pch=16, cex=1.2)
   # points(var.pred[[i]]$fit ~ sp.data[[i]]$Size, type="l")
   # points(var.ci[[i]]$upper ~ sp.data[[i]]$Size, type="l", lty=2)
   # points(var.ci[[i]]$lower ~ sp.data[[i]]$Size, type="l", lty=2)
   # mtext(species[i], side=3, line=-1, adj=0.05, family="", font=3)
}

for (i in c(2,3,5,6,8,9)) {
   var.panel[[i]] <- var.panel[[i]] + theme(axis.text.y=element_blank())
}
for (i in c(1:6)) {
   var.panel[[i]] <- var.panel[[i]] + theme(axis.text.x=element_blank())
}

# print and save
(var.panel[[1]] + var.panel[[2]] + var.panel[[3]] + var.panel[[4]] + var.panel[[5]] + var.panel[[6]] + var.panel[[7]] + var.panel[[8]] + var.panel[[9]])
ggsave('../figures/FigS2_VarInt_model.eps', device='eps', width = 175, height = 160, units = 'mm')

