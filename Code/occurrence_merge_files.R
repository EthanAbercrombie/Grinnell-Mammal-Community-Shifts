#Author: Ethan Abercrombie

#This script merges the clean occurrence data into one large csv that can be used in Analyses. 
#########

#Define Path and set working directory
file_path <- '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_clean/'
setwd(file_path)

#' This function gets all files in a folder, loads each in turn as a data frame, and combines them into a single data frame.
#'
#' @param folder Name of the folder.
#' @param ... Other arguments to pass to list.files.
#'
#' @return A data frame

collateDfs <- function(folder, ...) {
  
  files <- list.files(folder, ...)
  
  if (length(files) == 0) {
    warning('No files found.')
  } else {
    
    # read first file
    df <- read_csv(files[1]) %>% 
      select(-c(eventDate,
                eventTime,
                georeferencedDate,
                verbatimElevation,
                fieldNumber,
                datasetID,
                collectionID,
                locationID))
    
    # if >1 files
    if (length(files > 1)) {
      
      for (countFile in 2:length(files)) {
        
        thisDf <- read_csv(files[countFile]) %>% 
          select(-c(eventDate,
                    eventTime,
                    georeferencedDate,
                    verbatimElevation,
                    fieldNumber,
                    datasetID,
                    collectionID,
                    locationID))
        df <- list(df, thisDf) %>% 
          bind_rows() %>% 
          type_convert()
        
      } # next file
      
    } # if more than one file
    
  }
  
  return(df)
  
}

#Save a file that merges all occurrence dataframes into one function.
#This will be useful in extracting climate data in one go.
occurrence_data_merged <- collateDfs(folder = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_clean/')
occurrence_data_merged <- occurrence_data_merged %>% 
  mutate(record_number = 1:nrow(occurrence_data_merged))

#Save merged csv file
readr::write_csv(occurrence_data_merged,
                 file = '~/Desktop/Grinnell-Mammal-Community-Shifts/Data/occurrence_data/occurrence_data_clean/occurrence_data_merged.csv')
