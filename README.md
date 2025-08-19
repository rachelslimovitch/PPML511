# PPML511
 
Trends in  sexual behavior and sexual activity. Pulling state and national data from YRBS, and population/births data from CDC WONDER.

Utizing a random forest model to impute outcome variables for states missing 4 or fewer years of data.

## DATA
acs2010.csv-acs2023.csv (population data)
natality_2024_15to19.csv, natality_2007_2023_15to19.csv;  (birth data)
2007_2023_ages15to17.csv, 2024_ages15to17.csv;  (birth data)
2007_2023_ages18to19.csv, 2024_ages18to19.csv (birth data)

births_bridged2006_2019_single2020_2022.csv (birth data by race/ethnicity)
pop_bridged2006_2019_single2020_2022.csv (pop data by race/ethnicity)

## SCRIPTS
00_libraries.R
- reads in all libraries needed for project

01_births_pop.R
- compiles all births and population data (ages 15-19, ages 15-17, ages 18-19)
- FINAL FILES: births_data (births, pop data by state, 2007-2024); births_younger(same, but ages 15-17), births_older (same, but ages 18-19)

02_national_data.R
- reads in population and birth data overall, by race/ethnicity, and by age group
- reads in raw national YRBS files and cleans them
- FINAL FILES: national_dataset_final (PE for each outcome variable for 3yr groupings: overall, by age group, by race/ethnicity); national_by_yr (same, but by yr)

03_YRBS_statedata.R
- reads in raw state YRBS files and cleans them; imputes RF value for PE if state missing <5 yrs
- creates region values by year and 3yr groupings for each variable
- FINAL FILES: region_byyr_noCI (outcome values for each region for each yr), region_yrgroups (outcome values for each region for 3 yr groupings), fig4_data (cleaned dataset for figure of US maps)

04_YRBS_state_rf_ci.R
- Computes 95% CI with RF imputation (for states missing <5 yrs of data) and bootstrapping
- FINAL FILES: qn60_ci, no_condom_ci, no_larc_ci, forced_intercourse_ci, no_protection_ci, no_hm_effective_ci
- *Note*: needed to save these as RData files earlier to reduce time needed to compute

05_tables_figures.R
