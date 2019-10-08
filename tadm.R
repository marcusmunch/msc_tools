#' TADM.R
#' 
#' This file contains a script/scripts to handle TADM data. It takes data with
#' following criteria:
#' 
#'  * Top left field contains an identifier for present plate
#'  * Rest of fields in top row contain well identifiers
#'  * Data beginning in 7th row with time in leftmost column
#'


tadm_area <- function(path, sheets){
  #' This function reads data from the given path and sheet(s). It sums all measurements
  #' that are below 0 and returns the data in long form. If no sheet(s) is entered,
  #' all sheets will be read.

  require(tidyverse)
  require(readxl)

  # Set default parameter
  if(missing(sheets)){
    message("No 'sheets' entered, loading all")
    sheets <- 1:length(excel_sheets(path))
  }

  stopifnot(typeof(sheets) %in% c("double", "integer"))

  message(paste("Loading TADM data from ", path))
  
  data <- tibble()
  for(sheet in sheets){
    message(paste('Sheet:', excel_sheets(path)[sheet]))
    
    this_data <- read_excel(path, sheet = sheet) %>%
      tail(-5) %>%
      mutate(setup = colnames(.)[1]) %>%
      select(1, setup, everything())
    
    data <- data %>% bind_rows(this_data)
  }
  
  # Convert to long form
  data <- data %>%
    gather(key = "well", value = "tadm", -2) %>%
    mutate_at("tadm", as.numeric) %>%
    # Sum positive values for each well
    filter(tadm < 0) %>%
    group_by(well, setup) %>%
    summarise(area = -sum(tadm))
  
  return(data)
}
