
# Auxiliary benthic and fish assemblage data
# Repo: HerbivoreForageScaling
# Updated by CC 17/08/2021

require(tidyverse)
require(readxl)
require(ggdist)
require(ggbeeswarm)

data.benthic <- read_excel('../data/Palfrey_Benthic_2014-10.xlsx', col_names=T)
data.fish <- read_excel('../data/Palfrey_Herbivore_2014-10.xlsx', col_names=T, skip=6)


# Benthic relative cover --------------------------------------------------

# remove site date columns
data.benthic <- data.benthic[-c(1:2,24)]
# wide to long format
data.benthic <- pivot_longer(data.benthic, cols=!TRANSECT, names_to="Benthos", values_to="pointCount") %>% filter(pointCount != 0)

# we don't need resolution on hard corals
data.benthic$Benthos <- str_replace_all(data.benthic$Benthos, 'Pocillopora|Acropora|Porites|Montipora|Isopora|Platygyra|Favia|Goniastrea|Cyphastrea|Stylophora|Coscinarea|Other hard coral', 'Hard coral')
data.benthic <- data.benthic %>% group_by(TRANSECT, Benthos) %>% summarise(pointCount=sum(pointCount))
data.benthic$cover <- data.benthic$pointCount/81

ggplot(data.benthic %>% filter(TRANSECT <= 8), aes(y=Benthos, x=cover)) +
  geom_boxplot(width=0.4) +
  geom_jitter(size = 1.5, color='grey50', shape = 21, height=0.3) +
  theme_classic(base_size = 12) + 
  labs(y = 'Benthos type', x = 'Relative cover')
ggsave('../figures/FigS2_benthos.eps', device='eps', width = 100, height = 40, units = 'mm')


# Conspecific fish --------------------------------------------------------

# just conspecifics for our focal species
data.fish <- data.fish %>% filter(str_detect(Species, 'rivulatus|frenatus|doliatus|vulpinus|sordidus|striatus|scopas|unicornis|nigricaudus'))
data.fish$Species <- c('Ch. spilurus', 'Sc. frenatus', 'Sc. rivulatus', 'A. nigricauda', 'C. striatus', 'N. unicornis', 'Z. scopas', 'S. vulpinus', 'S. doliatus')
colnames(data.fish)[4:11] <- paste0('T', 1:8) # rename transect replicates
data.fish <- data.fish %>% select(!`Functional Group`)
data.fish <- pivot_longer(data.fish, cols=starts_with('T'), names_to="Transect", values_to="Count") %>% filter(Count > 0)

ggplot(data.fish, aes(y=Species, x=Count)) +
  geom_boxplot(width=0.3) +
  geom_jitter(size = 1.5, color='grey50', shape = 21, height=0.15) +
  theme_classic(base_size = 12) + 
  theme(axis.text.y = element_text(face='italic')) +
  labs(y = 'Herbivore species', x = 'Count')
ggsave('../figures/FigS3_conspecifics.eps', device='eps', width=100, height=100, units='mm')
