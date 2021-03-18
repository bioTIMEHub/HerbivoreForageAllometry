# HerbivoreSpatialForage
**Manuscript**: _Scaling of foraging area with body size in herbivorous reef fishes varies with species_  
**Authors**: Cher FY Chow, Emmy Wassenius, Andrew S Hoey,  Maria Dornelas
  
This repo contains data and code used for the analysis and figure generation of this paper on the foraging allometry of reef fish herbivory.
Here are some abbreviations are used throughout file naming and code comments:
- **SS** = Social Status (single=1, pair=2, single species chool=3, mixed-species school =4)
- **Diet** = Diet (EAM=1, macroalgae=2, Detritus=3, Corticated algae=4, Cyanobacteria=5)
- **Func** = Functional group (excavator=1, scraper=2, grazer/detritivore=3, browser=4)
- **Size** = Total length (cm)
- **IntFor** = Mean Inter-foray distance (m)
- **Area** = Foraging Area
- **Tort** = Tortuosity ratio (sum of inter-foray/start-finish)
- **VarInt** = Variance of inter-foray distances

## Requirements
Code in this repository is written in R (version 4.0.0 + is preferred).  
The following packages are required for executing analysis code: `quantreg`  
Figure generation code requires these packages: `beanplot`, `ggplot2`, `stringr`, and `patchwork`  
The relevant required packages are implemented in each file as `require(package)`.

## Repository structure
**src**: Data files used for this study.  
**analysis_code**: Contains most of the data exploration and processing scripts.  
**figure_code**: Contains figure generation code. The scripts in this folder are dependent on the two above.  

### Data
**`R-data_Lizard_ellipse_Area.txt`** contains all of the observation records for 119 fish individuals with 12 fields on species, functional traits, body size, foraging metrics (area, interforay, tortuosity), and observation time.

### Analysis
Each model response variable has its own R script.  
**`Lizard Area models_MD.R`** Foraging area models with result checks. Also contains beanplot generation code for univariate models.  
**`Lizard IntFor models.R`** Inter-foray distance models with AIC ranking  
**`Lizard VarInt models.R`** Inter-foray distance variance models with AIC ranking  
**`quantile regression area interforay distance.R`** Quantile regression analysis of foraging area ~ mean interforay distances

### Figures
**`Area plot with CIs.R`** For Foraging area ~ Size * Species model prediction figures separated by species with confidence intervals  
**`Univariate model plots.R`** Older iteration of the univariate model plots.
