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

ph_data <- function(path, plates){
  #' This function reads data from an Excel sheet. It picks out desired plates
  #' and returns pH data over time in long form.
  if(missing(plates)){
    print("No plates were given, so assuming plates 1-4")
    plates = c(1, 2, 3, 4)
  }
  require(tidyverse)
  require(readxl)
  
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
  
  return(data)
}


ph_vmax <- function(path, plates){
  if(missing(plates)){
    print("No plates were given, so assuming plates 1-4")
    plates = c(1, 2, 3, 4)
  }
  require(tidyverse)
  require(readxl)
  
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
