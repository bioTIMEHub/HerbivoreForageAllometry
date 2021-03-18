###########################################
#
# Supplementary Figures
# Author: Cher Chow
# Updated: Mar 18, 2021
#
###########################################

getwd()
require(tidyverse) # for ggplot, stringr, dplyr
require(patchwork) # for multiple panels
dt <- read.table('src/R-data_Lizard_ellipse_Area.txt', header=T) # read in data

View(dt)
# Prettier species names for plotting
species <- as.vector(rep('', length(unique(dt$Species))))
unique(dt$Species)
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
names(species) <- unique(dt$Species)
dt$SpLong <- dt$Species # make a duplicate of the row for replacement
dt$SpLong <- str_replace_all(dt$SpLong, species) # replace

SizeIntFor <- ggplot(data=dt, aes(x=Size, y=IntFor)) +
  geom_point(aes(shape=SpLong), size=2) +
  scale_shape_manual(values=c(0:8,10), name='Species') +
  labs(x=NULL, y='Mean inter-foray distance (m)') +
  scale_y_continuous(limits=c(0,10)) +
  theme_bw(base_size=14) +
  theme(panel.grid=element_blank(), legend.text = element_text(face='italic'))

SizeVarInt <- ggplot(data=dt, aes(x=Size, y=VarInt)) +
  geom_point(aes(shape=SpLong), size=2) +
  scale_shape_manual(values=c(0:8,10), name='Species') +
  labs(x='Total length (cm)', y='Variance of inter-foray distances (m)') +
  scale_y_continuous(limits=c(0,80)) +
  theme_bw(base_size=14) +
  theme(panel.grid=element_blank(), legend.text = element_text(face='italic'))

fig1_SizeInt <- SizeIntFor / SizeVarInt + plot_layout(guides='collect')
fig1_SizeInt



