# HerbivoreForageScaling
**Manuscript**: _Species differences drive spatial scaling of foraging patterns in herbivorous reef fishes_  
**Authors**: Cher FY Chow, Emmy Wassenius, Maria Dornelas, Andrew S Hoey  
**Corresponding author**: If you have any questions, please reach out to MD via [Github](https://github.com/maadd) or [email](mailto:maadd@st-andrews.ac.uk)    
  
This repo contains data and code used for the analysis and figure generation of this paper on the scaling of herbivorous reef fish foraging patterns with body size.  
  
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
Figure generation code requires these packages: `tidyverse`, `ggbeeswarm` and `patchwork`  
The package dependencies are declared in each file as `require(package name here)`.

## Repository structure
**analysis_code**: Contains analysis scripts separated by response variable and/or type of analysis.  
**figure_code**: Contains figure generation code. May be dependent on the RData object outputs from **analysis_code**.

### Data
The data file contains all of the observation records for 119 fish individuals with 12 fields on species, functional traits, body size, foraging metrics (area, interforay, tortuosity), and observation time. *** This is not hosted in the repo. Please contact authors for this.

### Analysis
- **`AreaModels_MD.R`** Foraging area multi-model inference with result checks and model comparisons.
- **`TortModels_CC.R`** Tortuosity multi-model inference with result checks and model comparisons.
- **`IntForModels.R`** Mean inter-foray distance multi-model inference with result checks and model comparisons.
- **`VarIntModels.R`** Variance in inter-foray distance multi-model inference with result checks and model comparisons.
- **`IntFor_QuantReg.R`** Quantile regression analysis assessing mean inter-foray distance as a metric for foraging extent.
- **`outlier_sensitivity_analyses.R`** Outlier omission sensitivity analyses for area, tort, and mean intfor. Generates a comparison table for coefficient estimates and standard error.

### Figures
- **`Fig2_univariates_areatort.R`** Multipanel univariate model plots for area and tortuosity.
- **`Fig3_tortmodel-species.R`** Multipanel model predictions with partial estimates for tortuosity ~ size + specie
- **`Fig4_S2_Var-IntFor_species`** Figures for the inter-foray distance figures. Model predictions with a panel for each species, with partial estimates.  
