#Author: Ethan Abercrombie
#####

#packages
require(tidyverse)

#Please see the help page for climateNA for appropriate data formatting.

#load occurrence data
#Here, my file contains multiple species (we can disentangle later)
occurrence_data <- readr::read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_merged.csv')

#Select appropriate columns in the following order.
#Column 1 - your choice (written as ID1)
#Column 2 - your choice (written as ID2)
#Column 3 - latitude (written as 'lat')
#Column 4 - longitude (written as 'long')
#Column 5 - elevation (written as 'el')

#Replace all "NA" with ".". This is a requirement of ClimateNA.

climate_na_data_input <- occurrence_data %>%
  transmute(ID1 = record_number,
            ID2 = species_name,
            lat = as.numeric(decimalLatitude),
            long = as.numeric(decimalLongitude),
            el = as.numeric(elevation)) %>%
  mutate_all(~replace(.,
                      is.na(.),
                      '.'))

#ClimateNA uses a comma delimited text file.

write.csv(climate_na_data_input,
          file = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/climate_data/climate_na_data_input.csv',
          fileEncoding = 'UTF-8',
          row.names = F)
 

# #IMPORTANT
# #You must open this file in excel, and save it as UTF-8. For some reason, I cannot get R to export the merged file with UTF-8 encoding.
# ########

#Load output from climate NA

occurrence_climate_data <- read_delim("/Users/ethanabercrombie/Desktop/Grinnell-Mammal-Community-Shifts/Data/climate_data/climate_na_data_output_1901-2021Y.csv",
                                      delim = ",");colnames(occurrence_climate_data)[2:3] = c('record_number',
                                            'species_name')

#Remove unnecessary objects from memory.
rm(climate_na_data_input)

#######
#Calculate species' precipitation and temperature preferences.
#######

# For each occurrence, record the 31-year normal around the occurrence collection data (+-15 years).

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

#Remove unnecessary objects from memory.
rm(coccurrence_data,
   occurrence_climate_data)

#Save occurrence data with climate data.
file_name <- 'occurrence_data_with_climate.csv'
write_csv(occurrence_data_merged,
          file = paste0('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/',file_name))

#Calculate species' preferences.
species_climate_preferences <- occurrence_data_merged %>% 
  select(species_name,
         med_ann_temp,
         med_ann_precip) %>% 
  group_by(species_name) %>% #For each species, I kept those records within the 5th and 95th quantiles to avoid climate extremes.
  filter(between(med_ann_temp,
                 quantile(med_ann_temp, 0.05),
                 quantile(med_ann_temp, 0.95)) &
         between(med_ann_precip,
                 quantile(med_ann_precip, 0.05),
                 quantile(med_ann_precip, 0.95))) %>% 
  summarise(MAT = median(med_ann_temp),
            MAP = median(med_ann_precip),
            temp_min = min(med_ann_temp),
            temp_max = max(med_ann_temp),
            precip_min = min(med_ann_precip),
            precip_max = max(med_ann_precip))

#Save preferences in seperate file.
write_csv(species_climate_preferences,
          file = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/climate_data/species_climate_preferences.csv')