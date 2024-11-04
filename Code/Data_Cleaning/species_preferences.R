require(tidyverse)

#Load output from climate NA

occurrence_climate_data <- read_delim("/Users/ethanabercrombie/Desktop/Grinnell-Mammal-Community-Shifts/Data/climate_data/combined_climate_data_1901_2021.csv",
                                      delim = ",") %>% 
  mutate(year_upper = NA,
         year_lower = NA,
         year_upper_15 = NA,
         year_lower_15 = NA) %>% 
  rename('record_number' = ID1,
         'species_name' = ID2)

#######
#Calculate species' precipitation and temperature preferences.
#######
occurrence_data_merged <- readr::read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_merged.csv') %>% 
    filter(year >= 1901 &
             year <= 2021) #Remove occurrences not collected within ClimateNA range.

# num_missing <- is.na(occurrence_climate_data$med_ann_precip) #Check for missing data if needed.
# print(sum(num_missing))
# rm(num_missing)

# For each occurrence, record the 31-year normal around the occurrence collection data (+-15 years).
# year_upper_15 is the 15 years prior to collection only.
# year_lower_15 is the 15 years prior to collection only.
range = 15

occurrence_data <- occurrence_data_merged %>% 
  mutate(
    year_upper = ifelse(year <= 2021 - range, year + range, 2021),
    year_upper = pmin(year_upper, 2021),  # Ensure it doesn't exceed 2021
    year_lower = ifelse(year >= 1901 + range, year - range, 1901),
    year_lower = pmax(year_lower, 1901)   # Ensure it doesn't go below 1901
  ) %>% 
  mutate(
    year_lower = ifelse(year > 2021 - range, year_lower - (range - (2021 - year)), year_lower),
    year_upper = ifelse(year < 1901 + range, year_upper + ((1901 + range) - year), year_upper),
    year_upper_15 = ifelse(year >= 1901 + range, year, 1916),
    year_lower_15 = ifelse(year >= 1901 + range, year - range, 1901))

# Vectorized operation to update occurrence_climate_data
# occurrence_climate_data$year_upper[match(occurrence_data$record_number, occurrence_climate_data$record_number)] <- occurrence_data$year_upper
# occurrence_climate_data$year_lower[match(occurrence_data$record_number, occurrence_climate_data$record_number)] <- occurrence_data$year_lower
# occurrence_climate_data$year_lower_15[match(occurrence_data$record_number, occurrence_climate_data$record_number)] <- occurrence_data$year_lower_15
# occurrence_climate_data$year_upper_15[match(occurrence_data$record_number, occurrence_climate_data$record_number)] <- occurrence_data$year_upper_15

# NAs occur when occurrence_data does not have the records found in occurrence_climate_data
# This occurs when the record is not collected between 1901 and 2021.
occurrence_climate_data <- occurrence_climate_data %>%
  left_join(occurrence_data %>%
              dplyr::select(record_number, year_upper, year_lower, year_lower_15, year_upper_15),
            by = "record_number") %>%
  mutate(year_upper = coalesce(year_upper.y, year_upper.x),
         year_lower = coalesce(year_lower.y, year_lower.x),
         year_lower_15 = coalesce(year_lower_15.y, year_lower_15.x),
         year_upper_15 = coalesce(year_upper_15.y, year_upper_15.x)) %>%
  dplyr::select(-ends_with(".y"), -ends_with(".x"))

####

occurrence_climate_data_summary <- occurrence_climate_data %>%
  filter(between(Year,
                 year_lower,
                 year_upper)) %>% 
  group_by(record_number)  %>%
  mutate(
    med_ann_temp = median(MAT, na.rm = TRUE),
    med_ann_precip = median(MAP, na.rm = TRUE)
  ) %>%
  mutate(range_30 = length(Year),
  ) %>% 
  slice(1) %>%
  dplyr::select(record_number, med_ann_temp, med_ann_precip, range_30)

occurrence_climate_data_summary_15 <- occurrence_climate_data %>%
  filter(Year >= year_lower_15 & Year <= year_upper_15) %>% 
  group_by(record_number) %>%
  mutate(
    med_ann_temp_15 = median(MAT, na.rm = TRUE),
    med_ann_precip_15 = median(MAP, na.rm = TRUE)
  ) %>%
  slice(1) %>%
  dplyr::select(record_number, med_ann_temp_15, med_ann_precip_15)

occurrence_climate_data_summary <- left_join(occurrence_climate_data_summary,
                                             occurrence_climate_data_summary_15,
                                             by = join_by(record_number))

occurrence_data_merged <- inner_join(occurrence_data,
                                     occurrence_climate_data_summary,
                                     by = 'record_number') %>% 
  dplyr::select(-recordNumber)

plot(occurrence_data_merged$med_ann_temp ~ occurrence_data_merged$med_ann_temp_15)

#Remove unnecessary objects from memory.
rm(occurrence_data,
   occurrence_climate_data)

#Save occurrence data with climate data.
file_name <- 'occurrence_data_with_climate.csv'
write_csv(occurrence_data_merged,
          file = paste0('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/',file_name))

#Calculate species' preferences.
# species_climate_preferences <- occurrence_data_merged %>%
#   select(species_name,
#          med_ann_temp,
#          med_ann_precip) %>%
#   group_by(species_name) %>% #For each species, I kept those records within the 5th and 95th quantiles to avoid climate extremes.
#   filter(between(med_ann_temp,
#                  quantile(med_ann_temp, 0.05),
#                  quantile(med_ann_temp, 0.95)) &
#          between(med_ann_precip,
#                  quantile(med_ann_precip, 0.05),
#                  quantile(med_ann_precip, 0.95))) %>%
#   summarise(MAT = median(med_ann_temp),
#             MAP = median(med_ann_precip),
#             temp_min = min(med_ann_temp),
#             temp_max = max(med_ann_temp),
#             precip_min = min(med_ann_precip),
#             precip_max = max(med_ann_precip))

species_temperature_preferences <- occurrence_data_merged %>%
  dplyr::select(species,
         med_ann_temp_15,
         med_ann_precip_15) %>%
  group_by(species) %>% #For each species, I kept those records within the 5th and 95th quantiles to avoid climate extremes.
  filter(between(med_ann_temp_15,
                 quantile(med_ann_temp_15, 0.05),
                 quantile(med_ann_temp_15, 0.95))) %>%
  summarise(MAT = median(med_ann_temp_15),
            temp_min = min(med_ann_temp_15),
            temp_max = max(med_ann_temp_15))

species_precipitation_preferences <- occurrence_data_merged %>%
  dplyr::select(species,
         med_ann_precip_15,
         med_ann_precip_15) %>%
  group_by(species) %>% #For each species, I kept those records within the 5th and 95th quantiles to avoid climate extremes.
  filter(between(med_ann_precip_15,
                 quantile(med_ann_precip_15, 0.05),
                 quantile(med_ann_precip_15, 0.95))) %>%
  summarise(MAP = median(med_ann_precip_15),
            precip_min = min(med_ann_precip_15),
            precip_max = max(med_ann_precip_15))

# species_precipitation_preferences <- occurrence_data_merged %>%
#   select(species_name,
#          med_ann_precip,
#          med_ann_precip) %>%
#   group_by(species_name) %>%
#   filter(between(med_ann_precip,
#                  quantile(med_ann_precip, 0.05),
#                  quantile(med_ann_precip, 0.95)) &
#            between(med_ann_temp,
#                    quantile(med_ann_temp, 0.05),
#                    quantile(med_ann_temp, 0.95))) %>%
#   summarise(MAP = median(med_ann_precip),
#             precip_min = min(med_ann_precip),
#             precip_max = max(med_ann_precip),
#             MAT = median(med_ann_temp),
#             temp_min = min(med_ann_temp),
#             temp_max = max(med_ann_temp))

species_climate_preferences <- full_join(species_precipitation_preferences,
                                         species_temperature_preferences,
                                         by = 'species')

#Save preferences in separate file.
write_csv(species_climate_preferences,
          file = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/climate_data/species_climate_preferences.csv')

# Species preferences occurrences table
# This code essentially adds a column to the species_climate_preferences table above with the
# number of records used in calculating species preferences.
## Number of records
  nrow(occurrence_climate_data_summary_15)

## Table
  occurrence_summary_table <- occurrence_data_merged %>% 
    group_by(species) %>% 
    summarise(preferred_precip = median(),
              number_records = n())

  View(occurrence_summary_table)  

  occurrence_summary_table <- full_join(species_climate_preferences,
                                      occurrence_summary_table,
                                      by = 'species')
  View(occurrence_summary_table)

  library(clipr)
  write_clip(occurrence_summary_table$number_records)
