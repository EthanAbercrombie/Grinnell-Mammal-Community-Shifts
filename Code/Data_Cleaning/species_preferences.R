require(tidyverse)

#Load output from climate NA

occurrence_climate_data <- read_delim("/Users/ethanabercrombie/Desktop/Grinnell-Mammal-Community-Shifts/Data/climate_data/climate_na_data_output_1901-2021Y.csv",
                                      delim = ",") %>% 
  mutate(year_upper = NA,
         year_lower = NA) %>% 
  rename('record_number' = ID1,
         'species_name' = ID2)

#######
#Calculate species' precipitation and temperature preferences.
#######
occurrence_data_merged <- readr::read_csv('~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_merged.csv') %>% 
    filter(year >= 1901 &
             year <= 2021) #Remove occurrences not collected within ClimateNA range.

num_missing <- is.na(occurrence_climate_data$med_ann_precip) #Check for missing data if needed.
print(sum(num_missing))
rm(num_missing)

# For each occurrence, record the 31-year normal around the occurrence collection data (+-15 years).

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
    year_upper = ifelse(year < 1901 + range, year_upper + ((1901 + range) - year), year_upper)
  )

# Vectorized operation to update occurrence_climate_data
occurrence_climate_data$year_upper[match(occurrence_data$record_number, occurrence_climate_data$record_number)] <- occurrence_data$year_upper
occurrence_climate_data$year_lower[match(occurrence_data$record_number, occurrence_climate_data$record_number)] <- occurrence_data$year_lower

occurrence_climate_data_summary <- occurrence_climate_data %>%
  group_by(record_number) %>%
  filter(Year >= year_lower & Year <= year_upper) %>% 
  group_by(record_number) %>%
  summarise(
    med_ann_temp = median(MAT),
    med_ann_precip = median(MAP)
  )

occurrence_data_merged <- inner_join(occurrence_data,
                                     occurrence_climate_data_summary,
                                     by = 'record_number') %>% 
  select(-recordNumber)

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
  select(species_name,
         med_ann_temp,
         med_ann_precip) %>%
  group_by(species_name) %>% #For each species, I kept those records within the 5th and 95th quantiles to avoid climate extremes.
  filter(between(med_ann_temp,
                 quantile(med_ann_temp, 0.05),
                 quantile(med_ann_temp, 0.95))) %>%
  summarise(MAT = median(med_ann_temp),
            temp_min = min(med_ann_temp),
            temp_max = max(med_ann_temp))

species_precipitation_preferences <- occurrence_data_merged %>%
  select(species_name,
         med_ann_precip,
         med_ann_precip) %>%
  group_by(species_name) %>% #For each species, I kept those records within the 5th and 95th quantiles to avoid climate extremes.
  filter(between(med_ann_precip,
                 quantile(med_ann_precip, 0.05),
                 quantile(med_ann_precip, 0.95))) %>%
  summarise(MAP = median(med_ann_precip),
            precip_min = min(med_ann_precip),
            precip_max = max(med_ann_precip))

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
                                         by = 'species_name')

#Save preferences in seperate file.
write_csv(species_climate_preferences,
          file = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/climate_data/species_climate_preferences.csv')
