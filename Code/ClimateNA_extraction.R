#Author: Ethan Abercrombie
#####

#packages
require(tidyverse)

#The folding code below contains information needed to extract climate values using R. However,
#it is not currently working - troubleshooting needed. Potential Problem: R is not writing the csv
#as 'UTF-8' despite argument included in write.csv.
# #Set Working Directory to climateNA folder.
# setwd('C:\\Users\\Ethan\\ClimateNA');getwd()
# 
# #This script collects climate data for species occurrence records within climateNA.
# #Please see the help pafe for climateNA for appropriate data formatting.
# 
# #load occurrence data 
# #Here, my file contains multiple species (we can disentangle later)
# occurrence_data <- readr::read_csv('C:/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_merged.csv')
# 
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
# write.csv(climate_na_data_input,
#           file = 'C:/Users/Ethan/ClimateNA/climate_na_data_input.csv',
#           fileEncoding = 'UTF-8',
#           row.names = F)
# 
# exe <- "ClimateNA_v7.20.exe"
# 
# inputFile = 'C:\\Users\\Ethan\\ClimateNA\\climate_na_data_input.csv'
# outputFile = 'C:\\Users\\Ethan\\ClimateNA\\climate_na_data_output.csv'
# yearPeriod = '/Normal_1961_1990.nrm'
# 
# system2(exe,
#         args= c('/Y', yearPeriod, inputFile, outputFile),
#         wait=T)

occurrence_climate_data <- read_delim("C:/Grinnell-Mammal-Community-Shifts/Data/Climate Data/climate_na_data_output_1901-2020Y.csv",
                                      delim = ",")

colnames(occurrence_climate_data)[2:3] = c('record_number',
                                            'species_name')
#data_subset <- occurrence_climate_data %>% 
 # filter(record_number %in% c(1:15))

occurrence_data <- readr::read_csv('C:/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_merged.csv')

range = 5

occurrence_data <- occurrence_data %>% 
  filter(year <= 2020) 

occurrence_data <- occurrence_data %>% 
  mutate(year_upper = 1:nrow(occurrence_data),
         year_lower = 1:nrow(occurrence_data),
         record_number = 1:nrow(occurrence_data)) %>% 
  mutate(year_upper = ifelse(year <= 2020-range,
                             year + range,
                             2020),
         year_lower = ifelse(year >= 1901 + range,
                             year - range,
                             1901)) %>% 
  mutate(year_lower = ifelse(year > 2020 - range,
                             year_lower - (range-(2020 - year)),
                             year_lower)) %>% 
  add_column(med_ann_temp = NA,
             med_ann_precip = NA)

#Write a function that calculates the median temperature and precipitation (any climate variable, really)
# for each occurrence.

#Step 1: extract appropriate rows from climate data - the number of rows equals the 'range'
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
for (i in 1:max(occurrence_data$record_number)) {
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
