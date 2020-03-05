#' ph.R
#' 
#' This file contains function(s) for handling of pH data from flatbed scanners.
#' 
#' It makes following assumptions of files:
#' 
#'   * Data is on short form with one row for each well per plate.
#'   * pH data over time is at the end of the columns, with first column labeled "0"
#'   * Data set ends at the first column with no header.
#'

# Package requirements
require(tidyverse)
require(readxl)

ph_data <- function(path, plates){
  #' This function reads data from an Excel sheet. It picks out desired plates
  #' and returns pH data over time in long form.
  if(missing(plates)){
    message("No plates were given, so assuming plates 1-4")
    plates = c(1, 2, 3, 4)
  }
  
  # Read the data and remove columns at the end
  data <- read_excel(path, skip = 2, .name_repair = "minimal")
  data <- data[1:min(which(colnames(data) == "")) - 1]
  
  # Remove square brackets from column names
  for(i in 1:ncol(data)){
    if(grepl("^[[].*[]]$", colnames(data)[i])){
      col_name <- colnames(data)[i]
      colnames(data)[i] <- substr(col_name, 2, nchar(col_name) - 1)
    }
  }
  
  # Select desired plates
  data <- data %>% 
    filter(Plate %in% plates) %>%
  # Remove columns starting with first empty column
    gather(key = "time", value = "pH",
           colnames(.)[which(colnames(data) == "0"):length(.)]) %>%
    select(time, Chcc, Plate, pH, everything()) %>%
    mutate_at("time", as.numeric)
  
  # Update genus/species/subspecies from file
  updated <- read_excel(paste0("../static/", list.files("../static", pattern="[[:digit:]]{6}_names_.*[.]xlsx") %>% last())) %>%
    select(`Plate Setup`, Well, Genus, Species, Subspecies)

  data <-
    left_join(data, updated, by = c("Plate Setup", "Well"), suffix = c("_old", "_new")) %>%
    rename(Genus = Genus_new, Species = Species_new, Subspecies = Subspecies_new) %>%
    select(-Genus_old, -Species_old, -Subspecies_old)

  return(data)
}


ph_vmax <- function(path, plates){
  if(missing(plates)){
    message("No plates were given, so assuming plates 1-4")
    plates = c(1, 2, 3, 4)
  }
  
  # Read the data and remove columns at the end
  data <- read_excel(path, skip = 2, .name_repair = "unique")
  
  # Remove square brackets from column names
  for(i in 1:ncol(data)){
    if(grepl("^[[].*[]]$", colnames(data)[i])){
      col_name <- colnames(data)[i]
      colnames(data)[i] <- substr(col_name, 2, nchar(col_name) - 1)
    }
  }
  
  # Select desired plates
  data <- data %>%
    filter(Plate %in% plates) %>%
    mutate_at("Plate", as.character) %>%
    select(Chcc, Plate, Vmax = `Vmax, 1hr`)
  
  return(data)
}

ph_logger_data <- function(path){
  #' Load data from pH logger
  
  # Read data from file
  data <- read_excel(path)[-1,]
  
  # Remove trailing " h" from time
  data <- data %>%
    mutate(Time = str_replace(Time, ",", ".")) %>%
    mutate(Time = substr(Time, 1, nchar(Time) - 2)) %>%
    
  # Convert all columns to numeric
    mutate_all(as.numeric)
  
  return(data)
}
