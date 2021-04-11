
# Quantile regression between foraging area and mean interforay distance
# M Dornelas 26.06.2016

## Clear anything old
rm(list=ls(all=TRUE)) 

require(quantreg) # quantile regression libary
require(tidyverse) # plotting
require(patchwork) # multipanel

## Import data ##
forage.data<-read.table('../original/src/R-data_Lizard_ellipse_Area.txt',header=T)
species <- c(
  'Sc. rivulatus',
  'S. doliatus',
  'A. nigricauda', 
  'Sc. frenatus', 
  'Ch. spilurus', # updated taxonomy split Sc. sordidus into two species
  'N. unicornis',
  'C. striatus',
  'S. vulpinus',
  'Z. scopas'
)
names(species) <- unique(forage.data$Species)
forage.data$SpLong <- forage.data$Species # make a duplicate of the row for replacement
forage.data$SpLong <- str_replace_all(forage.data$SpLong, species) # replace

areadistmed<- rq(Area ~ IntFor, tau=0.5, data=forage.data)
#quantile regression of Area as Function of Interforay distance, for median
areadisttop<- rq(Area ~ IntFor, tau=0.05, data=forage.data)
#quantile regression of Area as Function of Interforay distance, for bottom 0.05 centile
areadistbot<- rq(Area ~ IntFor, tau=0.95, data=forage.data)
#quantile regression of Area as Function of Interforay distance, for top 0.05 centile

# Generate Fig 5 comparing IntFor with Area
QR <- ggplot(data=forage.data) +
  geom_abline(data = NULL, slope = areadistbot$coefficients[2], intercept = areadistbot$coefficients[1], size = 0.5, color = 'grey70') +
  geom_abline(data = NULL, slope = areadistmed$coefficients[2], intercept = areadistmed$coefficients[1], size = 0.5, color = 'grey70') +
  geom_abline(data = NULL, slope = areadisttop$coefficients[2], intercept = areadisttop$coefficients[1], size = 0.5, color = 'grey70') +
  geom_point(aes(x=IntFor, y=Area, shape = SpLong), size = 2, alpha = 0.7) +
  scale_shape_manual(values=c(0:8,10), name='Species') +
  guides(shape = 'none') + theme_bw(base_size=13) + 
  labs(x='Mean inter-foray distance (m)', y=expression(Foraging~area~"("~m^2~")")) +
  theme(panel.grid=element_blank(), legend.text = element_text(face='italic'))

IntForArea <- ggplot(data=forage.data) +
  geom_point(aes(x=IntFor, y=Area, shape = SpLong), size = 2, alpha = 0.7) +
  scale_shape_manual(values=c(0:8,10), name='Species') +
  theme_bw(base_size=13) + 
  theme(panel.grid=element_blank(), legend.text = element_text(face='italic')) +
  labs(x='Mean inter-foray distance (m)', y=NULL) +
  scale_y_continuous(trans='log', breaks=c(1,5,10,50,100,500), limits=c(1,500))

(QR + IntForArea) + plot_layout(guides='collect')

ggsave('../figures/Fig_quantreg.svg', device = 'svg', width = 280, height = 110, unit = 'mm', dpi = 300)
