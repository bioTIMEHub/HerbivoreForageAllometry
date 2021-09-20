# HerbivoreForageScaling
**Manuscript**: _Species differences drive spatial scaling of foraging patterns in herbivorous reef fishes_  
[https://doi.org/10.1111/oik.08713](https://doi.org/10.1111/oik.08713)  
**Authors**: Cher FY Chow, Emmy Wassénius, Maria Dornelas, Andrew S Hoey  
**Corresponding author**: If you have any questions, please reach out to MD via [Github](https://github.com/maadd) or [email](mailto:maadd@st-andrews.ac.uk)    
  
This repository contains the data and code used for the analysis and figure generation for _Species differences drive spatial scaling of foraging patterns in herbivorous reef fishes_. We used three metrics— foraging area, mean inter-foray distance, and tortuosity— to describe the spatial foraging patterns of 119 herbivorous reef fishes. Our study investigates the potential effects of body size and a range of species traits (feeding substrata, feeding social grouping, functional grouping) on spatial foraging patterns, with the specific aims to:

1. Identify the strongest predictors of foraging patterns in herbivorous reef fish,
2. Develop models that capture drivers of spatial foraging patterns
  
**Disclaimer**: This repository is archived both on GitHub and Zenodo as a release following acceptance to Oikos, however the code may undergo revisions and changes following this release.

## Requirements
All files were written in R 4.0.0. We recommend executing the repository using this version.  
**Package dependencies:**  
The package dependencies are declared in each file as `require(package name here)`
- `quantreg` (v 5.86) 
- `tidyverse` (v 1.3.1)
- `ggbeeswarm` (v 0.6.0)
- `patchwork` (v 1.1.1) 

## Repository structure
**`analysis_code`**: Contains analysis scripts separated by response variable and/or type of analysis.  
**`analysis_outputs`**: Relevant outputs from analysis code that may be used in **figure_code**  
**`figure_code`**: Contains figure generation code. May be dependent on the RData object outputs from **analysis_code**.  
**`src`**: This directory contains foraging pattern and species trait data

## Data
All data is contained in one file `SpForagingMetrics.csv`. It contains observation records for 119 fish individuals with 12 fields on species, functional traits, body size, foraging metrics (area, interforay, tortuosity), and observation time. For parrotfishes, we also include a field on their life stage (initial phase IP or terminal phase TP). These data arose from focal individual follow dive surveys conducted at Palfrey Island, Great Barrier Reef. All fields except for feeding substrata and function group come from in situ observations. These two traits were assigned according to literature. The benthic and conspecific data used in `aux_benthic_fish.R` is available on request.

### Abbreviations
Here are some abbreviations used in the data file and subsequent analyses:

- **SG** = Social grouping during foraging (single=1, pair=2, single species chool=3, mixed-species school =4)
- **Diet** = Feeding substrata/diet (EAM=1, macroalgae=2, Detritus=3, Corticated algae=4, Cyanobacteria=5)
- **Func** = Functional group (excavator=1, scraper=2, grazer/detritivore=3, browser=4)
- **Size** = Total length (cm)
- **IntFor** = Mean Inter-foray distance (m)
- **Area** = Foraging Area
- **Tort** = Tortuosity ratio (sum of inter-foray/start-finish)
- **VarInt** = Variance of inter-foray distances


## Analysis
For each foraging metric response variable, we constructed several candidate models that represent all possible combination of predictors with and without interactions. We then compared these candidate models in model selection and generate a comparison table each. They are split in three files per response variable. We also include three auxiliary analyses assessing the use of inter-foray distance as metric of foraging extent, sensitivity analyses to size outliers, and relative benthic composition.

- **`AreaModels_MD.R`** Foraging area multi-model inference with result checks and model comparisons.
- **`TortModels_CC.R`** Tortuosity multi-model inference with result checks and model comparisons.
- **`IntForModels.R`** Mean inter-foray distance multi-model inference with result checks and model comparisons.
- **`VarIntModels.R`** Variance in inter-foray distance multi-model inference with result checks and model comparisons.
- **`IntFor_QuantReg.R`** Quantile regression analysis assessing mean inter-foray distance as a metric for foraging extent.
- **`aux_benthic_fish.R`** Data wrangling and basic analyses of the benthic composition and presence of conspecifics. Also includes figure generation.
- **`outlier_sensitivity_analyses.R`** Outlier omission sensitivity analyses for area, tort, and mean intfor. Generates a comparison table for coefficient estimates and standard error.

## Figure generation
- **`Fig2_univariates_areatort.R`** Multipanel univariate model plots for area and tortuosity.
- **`Fig3_tortmodel-species.R`** Multipanel model predictions with partial estimates for tortuosity ~ size + specie
- **`Fig4_S2_Var-IntFor_species`** Figures for the inter-foray distance figures. Model predictions with a panel for each species, with partial estimates.  


