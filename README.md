# Catch-Per-Unit-Effort (CPUE), Banding, and Morphometric Data Collected During Nocturnal Mist-Netting Sessions to Evaluate Trends and Variability Among Storm-Petrels of the California Channel Islands (1994-2018)

This repository contains code for cleaning, QAQCing, and analyzing the Ashy Storm-Petrel (ASSP) mistnetting data from Channel Islands National Park, 1994 - 2018. 

### Table of Contents 

### [Code](./Code)

- [01_data_cleaning](./Code/01_data_cleaning): code used to clean and QAQC raw data files
- [02_cpue_analysis](./Code/02_cpue_analysis): code used to calculate CPUE and other relevant metrics
- [03_figures](./Code/03_figures): code used to generate figures
- [04_misc_data_exploration](./Code/04_misc_data_exploration): code used for miscellaneous data exploration
- [5_wx_analysis](./Code/05_wx_analysis): code used to explore relationship between environmental conditions and ASSP CPUE (incomplete)
 
### [Data](./Data) 

- [raw_data](./Data/raw_data): raw data files prior to cleaning/QAQC
- [cleaned_data](./Data/cleaned_data): cleaned data files post cleaning/QAQC
- [wx_data](./Data/wx_data): data files used in weather analysis

### [Output](./output)

Contains .Rmd files of relevant code. 

### [Figures](./figures)

Contains versions of all figures. 

### Required Packages and Versions Used 

R version 4.4.1 (2024-06-14)  
Platform: aarch64-apple-darwin20  
Running under: macOS 15.6.1  

### Details of Article 

Information regarding this USGS data release can be found [here](https://www.usgs.gov/data/catch-unit-effort-cpue-banding-and-morphometric-data-collected-during-nocturnal-mist-netting).

### How to Use this Repository 

(Provide some guidance here to users, is there an order in which they should run things, for example?)  
