
# Auxiliary benthic and fish assemblage data
# Repo: HerbivoreForageScaling
# Updated by CC 17/08/2021

require(tidyverse)
require(readxl)
require(ggdist)
require(ggbeeswarm)

data.benthic <- read_excel('../data/Palfrey_Benthic_2014-10.xlsx', col_names=T)
data.fish <- read_excel('../data/Palfrey_Herbivore_2014-10.xlsx', col_names=T, skip=6)
data.fishB <- read_excel('../data/Palfrey_Herbivore_2014-10.xlsx', col_names=T, skip=6, sheet=4)


# Benthic relative cover --------------------------------------------------

# remove site date columns
data.benthic <- data.benthic[-c(1:2,24)]
# wide to long format
data.benthic <- pivot_longer(data.benthic, cols=!TRANSECT, names_to="Benthos", values_to="pointCount") %>% filter(pointCount != 0)

# we don't need resolution on hard corals
data.benthic$Benthos <- str_replace_all(data.benthic$Benthos, 'Pocillopora|Acropora|Porites|Montipora|Isopora|Platygyra|Favia|Goniastrea|Cyphastrea|Stylophora|Coscinarea|Other hard coral', 'Hard coral')
data.benthic <- data.benthic %>% group_by(TRANSECT, Benthos) %>% summarise(pointCount=sum(pointCount))
data.benthic$cover <- data.benthic$pointCount/81

# summary table by benthos type
data.benthic %>% filter(TRANSECT <= 8) %>% # only keep first 8 transects so that this lines up with fish conspecific transects
  group_by(Benthos) %>% summarise(mean=mean(cover), SD=sd(cover))

ggplot(data.benthic %>% filter(TRANSECT <= 8), aes(y=Benthos, x=cover)) +
  geom_boxplot(width=0.4) +
  geom_jitter(size = 1.5, color='grey50', shape = 21, height=0.3) +
  theme_classic(base_size = 12) + 
  labs(y = 'Benthos type', x = 'Relative cover')
ggsave('../figures/FigS2_benthos.eps', device='eps', width = 100, height = 40, units = 'mm')


# Conspecific fish --------------------------------------------------------

# just conspecifics for our focal species
data.fish <- data.fish %>% filter(str_detect(Species, 'rivulatus|frenatus|doliatus|vulpinus|sordidus|striatus|scopas|unicornis|nigricaudus'))
data.fishB <- data.fishB %>% filter(str_detect(Species, 'rivulatus|frenatus|doliatus|vulpinus|sordidus|striatus|scopas|unicornis|nigricaudus'))
data.fish$Species <- c('Ch. spilurus', 'Sc. frenatus', 'Sc. rivulatus', 'A. nigricauda', 'C. striatus', 'N. unicornis', 'Z. scopas', 'S. vulpinus', 'S. doliatus')
data.fishB$Species <- c('Ch. spilurus', 'Sc. frenatus', 'Sc. rivulatus', 'A. nigricauda', 'C. striatus', 'N. unicornis', 'Z. scopas', 'S. vulpinus', 'S. doliatus')

colnames(data.fish)[4:11] <- paste0('T', 1:8) # rename transect replicates
colnames(data.fishB)[4:11] <- paste0('T', 1:8)
data.fish <- data.fish %>% select(!`Functional Group`)
data.fishB <- data.fishB %>% select(!`Functional Group`)
data.fishB <- data.fishB %>% mutate(across(where(is.character) & !c(Species,Genus), .fns=as.numeric)) # fix column types
data.fish <- pivot_longer(data.fish, cols=starts_with('T'), names_to="Transect", values_to="Count") %>% filter(Count > 0)
data.fishB <- pivot_longer(data.fishB, cols=starts_with('T'), names_to="Transect", values_to="Biomass") %>% filter(Biomass > 0)

data.fish <- full_join(data.fish, data.fishB, by=c("Species", 'Transect')) %>% select(!starts_with('Genus'))

data.fish %>% group_by(Species) %>% 
  summarise(totalC=sum(Count), totalB=sum(Biomass), 
            meanC=mean(Count), sdC=sd(Count), 
            meanB=mean(Biomass), sdB=sd(Biomass)) %>% View()

ggplot(data.fish, aes(y=Species, x=Count)) +
  geom_boxplot(width=0.3) +
  geom_jitter(size = 1.5, color='grey50', shape = 21, height=0.15) +
  theme_classic(base_size = 12) + 
  theme(axis.text.y = element_text(face='italic')) +
  labs(y = 'Herbivore species', x = 'Count')
ggsave('../figures/FigS3_conspecifics.eps', device='eps', width=100, height=100, units='mm')

ggplot(data.fish, aes(y=Species, x=Biomass)) +
  geom_boxplot(width=0.3) +
  geom_jitter(size = 1.5, color='grey50', shape = 21, height=0.15) +
  theme_classic(base_size = 12) + 
  theme(axis.text.y = element_text(face='italic')) +
  labs(y = NULL, x = 'Biomass (g)') +
  scale_x_continuous(trans=scales::pseudo_log_trans(base=exp(1)), breaks = c(50,100,500,1000,5000,10000))
ggsave('../figures/FigS3_conspecificB.eps', device='eps', width=100, height=100, units='mm')