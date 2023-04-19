#load packages
require(sf)
require(tidyverse)


###
#load spatial data
###
states <- st_transform(st_read('/Users/ethanabercrombie/Desktop/Spatial_Data/gadm404-levels.gpkg',
                      layer = 'level1'),
                      enmSdm::getCRS('albersNA')) %>% 
  filter(ID_0 == 'USA' |
           ID_0 == 'CAN' |
           ID_0 == 'MEX',
         NAME_1 != 'Alaska' &
           NAME_1 != 'Hawaii')

#Define species in study.

species_list <- c('Peromyscus_maniculatus_sonoriensis',
'Sorex_ornatus',
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

#Create empty dataframe to store filtering results.
#The values are added in the occurrence_filtering function.
occurrence_metadata <- tibble(species = species_list,
                              filtered_records = NA,
                              within_range = NA,
                              within_buffer = NA,
                              outside_buffer = NA)

#Define export folders.
data_export_folder <- '/Users/ethanabercrombie/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_clean/'
plot_export_folder <- '/Users/ethanabercrombie/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_clean_maps/'
for (i in 1:length(species_list)) {
  species_name <- species_list[i]
  occurrence_data <- read.delim(paste0('/Users/ethanabercrombie/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/',species_name,'/occurrence.txt'))
  rangeMap <- st_transform(sf::st_read(paste0('/Users/ethanabercrombie/Desktop/Grinnell-Mammal-Community-Shifts/Data/IUCN Species Ranges/Species Ranges_Exports_QGIS/',
                                              species_name,
                                              '/',
                                              species_name,
                                              '.shp')),
                           enmSdm::getCRS('albersNA'))
  
  speciesSf_filtered <- occurrence_cleaning()
  
  occurrence_plot()
}

 #Save metadata file
occurrence_metadata <- occurrence_metadata %>% 
  mutate(prop_inside = round(within_buffer/filtered_records,
                             digits = 2))
save(occurrence_metadata,
     file = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_metadata.Rdata')

###########
#Plotting function.
##########

occurrence_plot <- function(){
  occurrence_map <- ggplot() +
    labs(title = paste(species_name)) +
    geom_sf(data = states,
            fill = NA) +
    geom_sf(data = rangeMap,
            color = 'darkgreen',
            fill = 'green',
            alpha = 0.5,
            inherit.aes = FALSE) +
    geom_sf(data = range_buffer,
            color = 'yellow',
            fill = NA,
            inherit.aes = F) +
    theme() +
    coord_sf(xlim = c(pmin(occ_bounding_box[1],
                           rangeMap_bounding_box[1]),
                      pmax(occ_bounding_box[3],
                           rangeMap_bounding_box[3])),
             ylim = c(pmin(occ_bounding_box[2],
                           rangeMap_bounding_box[2]),
                      pmax(occ_bounding_box[4],
                           rangeMap_bounding_box[4])),
             expand = T)
  
  
  quartz(type = 'pdf',
         dpi = 144,
         antialias = T,
         file = paste0(plot_export_folder,
                       species_name))
  
  print(occurrence_map)
  
  graphics.off()
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
    mutate_if(is.character, list(~na_if(.,""))) %>% #This removes empty coordinates stored as a blankspace "".
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
  
  #Create a buffer 3x the distance of the rangemap buffer for automatic exclusion.
  exclusion_distance <<- units::as_units(240, "km")
  exclusion_buffer <<- rangeMap %>% 
    st_buffer(dist = exclusion_distance)
  
  speciesSf_filtered <- species_alb %>% 
    mutate(within_range = lengths(st_within(x = species_alb, 
                                            y = rangeMap)),
           within_buffer = lengths(st_within(x = species_alb, 
                                             y = range_buffer)),
           within_exclusion = lengths(st_within(x = species_alb, 
                                                y = exclusion_buffer)),
           species_name = species_name) %>% 
    # filter(year >= 1970) %>% #I have a feeling this should not be here as my climate extractio takes into consideration when the occurrence was collected.
    filter(coordinateUncertaintyInMeters <= 1000 | is.na(coordinateUncertaintyInMeters)) %>% 
    filter(!(coordinateUncertaintyInMeters == "NA" &
               within_range == 0),
           within_exclusion == 1) %>%  
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

#Save metadata
save(occurrence_metadata,
     file = '/Users/ethanabercrombie/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_metadata.Rdata')

occ_issue <- speciesSf_filtered %>% 
  filter(within_buffer == 0)
#number of occurrences with issues.
nrow(occ_issue)
#Unique issues.
unique(occ_issue$issue)
#View data
view(occ_issue)
