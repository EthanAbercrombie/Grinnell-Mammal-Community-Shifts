#Author: Ethan Abercrombie
#####

#packages
require(tidyverse)
require(svMisc)

#The folding code below contains information needed to extract climate values using R. However,
#it is not currently working - troubleshooting needed. Potential Problem: R is not writing the csv
#as 'UTF-8' despite argument included in write.csv.
#Set Working Directory to climateNA folder.
setwd('C:\\Users\\Ethan\\ClimateNA');getwd()

#This script collects climate data for species occurrence records within climateNA.
#Please see the help pafe for climateNA for appropriate data formatting.

#load occurrence data
#Here, my file contains multiple species (we can disentangle later)
occurrence_data <- readr::read_csv('C:/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_merged.csv')

# #Select appropriate columns in the following order.
# #Column 1 - your choice (written as ID1)
# #Column 2 - your choice (written as ID2)
# #Column 3 - latitude (written as 'lat')
# #Column 4 - longitude (written as 'long')
# #Column 5 - elevation (written as 'el')
# 
# #Replace all "NA" with "."
# 
# climate_na_data_input <- occurrence_data %>%
#   transmute(ID1 = record_number,
#             ID2 = species_name,
#             lat = as.numeric(decimalLatitude),
#             long = as.numeric(decimalLongitude),
#             el = as.numeric(elevation)) %>%
#   mutate_all(~replace(.,
#                       is.na(.),
#                       '.')) %>%
#   filter(ID1 != '.')
# 
# #ClimateNA cannot work with R object, so we must save our data to a comma delimited text file.
# 
# write.csv(climate_na_data_input,
#           file = 'C:/Users/Ethan/ClimateNA/climate_na_data_input.csv',
#           fileEncoding = 'UTF-8',
#           row.names = F)
# 
# ########
# #IMPORTANT
# #You must open this file in excel, and save it as UTF-8. For some reason, I cannot get R to export the merged file with UTF-8 encoding.
# ########
# 
# exe <- "ClimateNA_v7.20.exe"
# 
# inputFile = '/C:\\Users\\Ethan\\ClimateNA\\climate_na_data_input.csv'
# outputFile = '/C:\\Users\\Ethan\\ClimateNA\\climate_na_data_output.csv'
# yearPeriod = ''
# 
# system2(exe,
#         args= c('/Y', yearPeriod, inputFile, outputFile),
#         wait=T)

occurrence_climate_data <- read_delim("C:/Grinnell-Mammal-Community-Shifts/Data/Climate Data/climate_na_data_output.csv",
                                      delim = ",");colnames(occurrence_climate_data)[2:3] = c('record_number',
                                            'species_name')

occurrence_climate_data <- occurrence_climate_data %>% 
  group_by(record_number)
# #Data subset
# occurrence_climate_data <- occurrence_climate_data %>%
# filter(record_number %in% c(1:15))

#Remove unnessary files from memory.
rm(climate_na_data_input)

#######################

range = 15

occurrence_data <- occurrence_data %>% 
  filter(year <= 2020,
         year >= 1901) %>% 
  mutate(year_upper = NA,
         year_lower = NA) %>% 
  mutate(year_upper = ifelse(year <= 2020-range,
                             year + range,
                             2020),
         year_lower = ifelse(year >= 1901 + range,
                             year - range,
                             1901)) %>% 
  mutate(year_lower = ifelse(year > 2020 - range,
                             year_lower - (range-(2020 - year)),
                             year_lower),
         year_upper = ifelse(year < 1901 + range,
                             year_upper + ((1901+range)-year),
                             year_upper)) %>% 
  add_column(med_ann_temp = NA,
             med_ann_precip = NA) %>% 
  select('record_number',
         'year_lower',
         'year_upper')

#Write a function that calculates the median temperature and precipitation (any climate variable, really)
# for each occurrence.

#Step 1: extract appropriate rows from climate data - the number of rows equals 2x + 1 the range
#listed above.
occ_climate_extract <- function(record_ID){
    occurrence_climate_data %>% 
    filter(record_number == record_ID,
           between(Year,
                   year_lower,
                   year_upper))
}

#Set up containers to store climate data
temp <- vector()
precip <- vector()

#Run loop to extract data for each occurrence.
  #Clear up memory space.
  gc()
  
for (i in 1:max(occurrence_data$record_number)) {
  progress(i, progress.bar = TRUE)
  
  year_lower = occurrence_data[i, "year_lower"]
  year_upper = occurrence_data[i, "year_upper"]
  
  occ_subset <- occ_climate_extract(record_ID = occurrence_data$record_number[i])

  #Step 2: Calculate median of values. 
  temp[i] <- median(occ_subset$MAT)
  
  precip[i] <- median(occ_subset$MAP)
}

occurrence_data_with_climate <- occurrence_data %>% 
  mutate(med_ann_temp = temp,
         med_ann_precip = precip)

write_csv(occurrence_data_with_climate,
          file = 'C:/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_with_climate.csv')

#########
# Summarize climate parameters by species
#########

#Define species
species_list <- c('Sorex_ornatus',
                  'Dipodomys_heermanni',
                  'Microtus_californicus',
                  'Reithrodontomys_megalotis',
                  'Chaetodipus_californicus',
                  'Neotoma_fuscipes',
                  'Neotoma_macrotis',
                  'Peromyscus_truei',
                  'Sciurus_griseus',
                  'Dipodomys_agilis',
                  'Tamias_merriami',
                  'Peromyscus_boylii',
                  'Thomomys_bottae',
                  'Otospermophilus_beecheyi',
                  'Sorex_trowbridgii',
                  'Tamias_quadrimaculatus',
                  'Sorex_vagrans',
                  'Tamias_senex',
                  'Tamiasciurus_douglasii',
                  'Zapus_princeps',
                  'Microtus_montanus',
                  'Microtus_longicaudus',
                  'Thomomys_monticola',
                  'Neotoma_cinerea',
                  'Tamias_speciosus',
                  'Tamias_amoenus',
                  'Sorex_palustris',
                  'Marmota_flaviventris',
                  'Urocitellus_beldingi',
                  'Callospermophilus_lateralis',
                  'Sorex_monticolus',
                  'Ochotona_princeps',
                  'Tamias_alpinus')
# Peromyscus_maniculatus not included.

tibble(species_list) %>% 
  add_column(MAT = NA,
             MAP = NA)

for (i in species_list) {
  species_name <- species_list[i]
  
  sp_data <- filter(occurrence_data_with_climate,
         species_name = species_name)
  sp_MAT 
  
}

species_climate_preferences <- occurrence_data_with_climate %>% 
  select(species_name,
         med_ann_temp,
         med_ann_precip) %>% 
  group_by(species_name) %>% 
  summarise(MAT = median(med_ann_temp),
            MAP = median(med_ann_precip))

write_csv(species_climate_preferences,
          file = 'C:/Grinnell-Mammal-Community-Shifts/Data/Climate Data/species_climate_preferences.csv')


####################
#

occurrence_data <- occurrence_data %>%  
  mutate(year_upper = NA,
         year_lower = NA) %>% 
  mutate(year_upper = ifelse(year <= 2020-range,
                             year + range,
                             2020),
         year_lower = ifelse(year >= 1901 + range,
                             year - range,
                             1901)) %>% 
  mutate(year_lower = ifelse(year > 2020 - range,
                             year_lower - (range-(2020 - year)),
                             year_lower),
         year_upper = ifelse(year < 1901 + range,
                             year_upper + ((1901+range)-year),
                             year_upper)) %>% 
  add_column(med_ann_temp = NA,
             med_ann_precip = NA) %>% 
  select('record_number',
         'year_lower',
         'year_upper')

occurrence_climate_data <- occurrence_climate_data %>% 
  group_by(record_number) 

occurrence_data_with_climate <- bind_rows(occurrence_data,
                                      occurrence_climate_data) %>% 
    filter(between(Year,
                 year_upper,
                 year_lower)) %>% 
  summarise(med_ann_temp = median(MAT),
            med_ann_precip = median(MAP))

