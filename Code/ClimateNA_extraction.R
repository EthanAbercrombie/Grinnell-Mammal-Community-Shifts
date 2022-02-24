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

# #Data subset
# occurrence_climate_data <- occurrence_climate_data %>%
# filter(record_number %in% c(1:15))

#Remove unnecessary files from memory.
rm(climate_na_data_input)

#######################

range = 15

occurrence_data <- occurrence_data %>% 
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
                             year_upper))

for(i in 1:max(occurrence_data$record_number)){
  print(i)
  occurrence_climate_data$year_upper[occurrence_climate_data$record_number == i] = occurrence_data$year_upper[occurrence_data$record_number == i]
  occurrence_climate_data$year_lower[occurrence_climate_data$record_number == i] = occurrence_data$year_lower[occurrence_data$record_number == i]
}
occurrence_climate_data <- x
occurrence_climate_data <- occurrence_climate_data %>% 
  group_by(record_number) %>%
  filter(Year >= year_lower,
         Year <= year_upper) %>% 
  summarise(med_ann_temp = median(MAT),
            med_ann_precip = median(MAP))

occurrence_data_merged <- inner_join(occurrence_data,
                                     occurrence_climate_data,
                                     by = 'record_number') %>% 
  select(-recordNumber)

#Clear up space.
rm(coccurrence_data,
   occurrence_climate_data)

#Save occurrence data with climate data.
file_name <- 'occurrence_data_with_climate.csv'
write_csv(occurrence_data_merged,
          file = paste0('C:/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/',file_name))

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

species_climate_preferences <- occurrence_data_merged %>% 
  select(species_name,
         med_ann_temp,
         med_ann_precip) %>% 
  group_by(species_name) %>% 
  summarise(MAT = median(med_ann_temp),
            MAP = median(med_ann_precip))

write_csv(species_climate_preferences,
          file = 'C:/Grinnell-Mammal-Community-Shifts/Data/Climate Data/species_climate_preferences.csv')
