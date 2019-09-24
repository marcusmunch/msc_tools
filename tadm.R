#' TADM.R
#' 
#' This file contains a script/scripts to handle TADM data. It takes data with
#' following criteria:
#' 
#'  * Top left field contains an identifier for present plate
#'  * Rest of fields in top row contain well identifiers
#'  * Data beginning in 7th row with time in leftmost column
#'


tadm_area <- function(path, sheet){
  #' This function reads data from the given path and sheet. It sums all measurements
  #' that are below 0 and returns the data in long form
  
  require(tidyverse)
  require(readxl)
  
  message(paste("Loading TADM data from ", path))
  stopifnot(typeof(sheet) %in% c("double", "integer"))
  message(paste('Sheet:', excel_sheets(path)[sheet]))

  # Load the rest of the data
  data <- read_excel(path, sheet = sheet) %>%
    tail(-5)
  
  # Convert to long form
  data <- data %>%
    gather(key = "well", value = "tadm", -1) %>%
    mutate_at("tadm", as.numeric) %>%
    # Sum positive values for each well
    filter(tadm < 0) %>%
    group_by(well) %>%
    summarise(area = -sum(tadm)) %>%
    # Append setup from upper left field 
    mutate(setup = colnames(data)[1])
  
  return(data)
}
