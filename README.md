# Grinnell-Mammal-Community-Shifts

This project explores how small mammal communities are responding to climate change across elevational gradients in California's Sierra Nevada.

## Data

-   Species Occurrence Data: GBIF Occurrence Download <https://doi.org/10.15468/dl.7x7sex> Accessed from R via rgbif ([https://github.com/ropensci/rgbif)](https://github.com/ropensci/rgbif)) on 2023-06-29.

## Code

### Data cleaning and Tidying

The following scripts are meant to be ran in the following order:

1.  **Occurrence_cleaning_function.R**: This script provides a function to clean the occurrence data (DOI: <https://doi.org/10.15468/dl.7x7sex>) file as downloaded from GBIF. Returns a clean *.csv* file for each species (*speciesname*\_occurrence_cleaned.csv).
2.  **ClimateNA_extraction.Rmd**: This scripts collates all occurrence data for all species in the study into a single data frame (). Returns a .csv file that is to be used in ClimateNA to extract climate data for each occurrence. After extraction, this code measures species' climate preferences. Returns a .csv file with the climate preferences of each species (species_climate_preferences.csv).
3.  **elev_site_data_cleaning.Rmd**: This script (occurrence_data_merged.csv) cleans and tidys the elevational data for each species (presented as pf_data), and site_data from Rowe et al. 2015. This script also calculates the community temperature index and community precipitation index for all historical sites in the historical and modern eras. Additionally, site climate marginality is calculated for each site. Returns clean elevational data as a *.csv* (pfa_data_clean.csv), and clean site data as a .csv (site_data_clean.csv).

## Citation

**Abstract**

**Aknowledgments**
