#load packages
require(sf)
require(tidyverse)

###
#load spatial data
###
load('C:/Grinnell-Mammal-Community-Shifts/Code/Occurence Data Cleaning/GADM Canada, USA, Mexico Level 0 WGS84.rda')
load('C:/Grinnell-Mammal-Community-Shifts/Code/Occurence Data Cleaning/GADM Canada, USA, Mexico Level 1 WGS84.rda')
load('C:/Grinnell-Mammal-Community-Shifts/Code/Occurence Data Cleaning/GADM Canada, USA, Mexico Level 2 WGS84.rda')

# remove Alaska and Hawaii because they make plotting much slower... but you may want to include Alaska if any species reside there
states <- nam1Sp[!(nam1Sp@data$NAME_1 %in% c('Hawaii')), ]

states <- st_transform(st_as_sf(states),
                       enmSdm::getCRS('albersNA'))
counties <- nam2Sp[!(nam2Sp@data$NAME_1 %in% c('Hawaii')), ]
counties <- st_transform(st_as_sf(counties),
                         enmSdm::getCRS('albersNA'))

rm(list = c('nam0Sp','nam1Sp','nam2Sp'))

#Define species in study.
species_list <- c('Chaetodipus_californicus',
                  'Callospermophilus_lateralis')
# Peromyscus_maniculatus not included.

#Create empty dataframe to store filtering results.
#The values are added in the occurrence_filtering function.
occurrence_metadata <- tibble(species = species_list,
                              filtered_records = NA,
                              within_range = NA,
                              within_buffer = NA,
                              outside_buffer = NA)

#Define export folders.
data_export_folder <- 'E:/Data/Species Occurrence Data/Species Occurrence Data Cleaned/'
plot_export_folder <- 'C:/Grinnell-Mammal-Community-Shifts/Figs and Tables/Occurrence_maps/Occurrence_maps_before_climate/'


for (i in 1:length(species_list)) {
  species_name <- species_list[i]
  occurrence_data <- read.delim(paste0('E:/Data/Species Occurrence Data/',species_name,'/occurrence.txt'))
  rangeMap <- st_transform(sf::st_read(paste0('E:/Data/IUCN Species Ranges/Species Ranges_Exports_QGIS/',
                                              species_name,
                                              '/',
                                              species_name,
                                              '.shp')),
                           enmSdm::getCRS('albersNA'))
  
  speciesSf_filtered <- occurrence_cleaning()
  
  occurrence_cleaning()
  occurrence_plot()
}

#Save metadata file
save(occurrence_metadata,
     file = 'C:/Grinnell-Mammal-Community-Shifts/Data/occurrence_metadata.Rdata')

###########
#Plotting function.
##########

occurrence_plot <- function(){
  occurrence_plot <- ggplot() +
    labs(title = paste(species_name)) +
    geom_sf(data = counties,
            fill = NA) +
    geom_sf(data = states,
            fill = NA,
            color = "darkred") +
    geom_sf(data = rangeMap,
            color = 'darkgreen',
            fill = 'green',
            alpha = 0.5,
            inherit.aes = FALSE) +
    geom_sf(data = range_buffer,
            color = 'yellow',
            fill = NA,
            inherit.aes = F) +
    geom_sf(data = speciesSf_filtered, 
            aes(color = within_buffer),
            inherit.aes = FALSE) +
    theme(legend.position = 'none') +
    coord_sf(xlim = c(pmin(occ_bounding_box[1],
                           rangeMap_bounding_box[1]),
                      pmax(occ_bounding_box[3],
                           rangeMap_bounding_box[3])),
             ylim = c(pmin(occ_bounding_box[2],
                           rangeMap_bounding_box[2]),
                      pmax(occ_bounding_box[4],
                           rangeMap_bounding_box[4])),
             expand = T)
  
  ggsave(filename = paste0(plot_export_folder,
                           species_name,
                           '.jpg'),
         plot = occurrence_plot,
         width = 5,
         height = 7,
         units = 'in')
}

#########
#Cleaning Function
########

occurrence_cleaning <- function(){
  species <- occurrence_data %>% 
    mutate(gbifID = as.character(gbifID)) %>% 
    subset(type=='PhysicalObject' |
             type=='specimen' |
             type=='PRESERVED_SPECIMEN' |
             type=='PreservedSpecimen' |
             type=='Occurrence' |
             type=="Collection" |
             type=='Physical Object' |
             type == 'Objeto fÃsico') %>% 
    filter(!is.na(decimalLatitude) & 
             !is.na(decimalLongitude))
  
  speciesSf <- st_as_sf(x = species,
                        coords = c(x = 'decimalLongitude',
                                   y = 'decimalLatitude'),
                        crs = 4326,
                        remove = FALSE)
  
  species_alb <- st_transform(speciesSf, 
                              enmSdm::getCRS('albersNA'))
  
  buffer_distance <<- units::as_units(80, "km")
  range_buffer <<- rangeMap %>% 
    st_buffer(dist = buffer_distance)
  
  speciesSf_filtered <- species_alb %>% 
    mutate(within_range = lengths(st_within(x = species_alb, 
                                            y = rangeMap)),
           within_buffer = lengths(st_within(x = species_alb, 
                                             y = range_buffer)),
           species_name = species_name) %>% 
    filter(year >= 1970) %>% 
    filter(coordinateUncertaintyInMeters <= 1000 | is.na(coordinateUncertaintyInMeters)) %>% 
    filter(!(coordinateUncertaintyInMeters == "NA" &
               within_range == 0)) %>%  
    filter(!(grepl('COORDINATE_ROUNDED',
                   issue)) &
             !(grepl('RECORDED_DATE_INVALID',
                     issue)) &
             !(grepl('COORDINATE_UNCERTAINTY_METERS_INVALID',
                     issue)) &
             !(grepl('PRESUMED_NEGATED_LONGITUDE',
                     issue))
    )
  
  write_csv(speciesSf_filtered,
            file = paste0(data_export_folder,
                          species_name,
                          '_occurrence_cleaned',
                          '.csv'))
  
  occ_bounding_box <<- as.numeric(st_bbox(speciesSf_filtered))
  rangeMap_bounding_box <<- as.numeric(st_bbox(range_buffer))
  
  occurrence_metadata[occurrence_metadata$species == species_name, 'filtered_records'] <<- nrow(speciesSf_filtered)
  occurrence_metadata[occurrence_metadata$species == species_name, 'within_range'] <<- nrow(filter(speciesSf_filtered,
                                                                                                   within_range == 1))
  occurrence_metadata[occurrence_metadata$species == species_name, 'within_buffer'] <<- nrow(filter(speciesSf_filtered,
                                                                                                    within_buffer == 1))
  occurrence_metadata[occurrence_metadata$species == species_name, 'outside_buffer'] <<- nrow(filter(speciesSf_filtered,
                                                                                                     within_buffer == 0))
  return(speciesSf_filtered)
}
     file = 'C:/Grinnell-Mammal-Community-Shifts/Data/occurrence_metadata.Rdata')
