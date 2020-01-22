anonymize <- function(input_tibble){
  require(tidyverse)
  known_chcc <- read_tsv("../known_chcc.txt") %>%
    mutate_all(as.character)

  CHCC_COLUMN_NAME <- "CHCC"

  if("Chcc" %in% colnames(input_tibble)){
    CHCC_COLUMN_NAME <- "Chcc"
    colnames(known_chcc)[1] <- "Chcc"
  }

  new_tibb <- input_tibble %>%
    mutate_at(CHCC_COLUMN_NAME, as.character) %>%
    left_join(known_chcc, by = CHCC_COLUMN_NAME) %>%
    mutate(CHCC_id = CHCC_COLUMN_NAME,
           !!CHCC_COLUMN_NAME := desc_short) %>%
    select(-desc_short)

  return(new_tibb)
}

anonymize_col <- function(in_tibble, in_column){
  require(tidyverse)

  # Define names for new columns
  id_colname <- paste0(in_column, "_id")
  long_colname <- paste0(in_column, "_long")

  # Load known CHCC list
  known_chcc <- read_tsv("../known_chcc.txt") %>%
    rename(!!long_colname := desc_long) %>%
    mutate_all(as.character)

  # Rename column header to allow for joining
  colnames(known_chcc)[1] <- in_column

  # Get descriptions and attack to input tibble
  new_tibble <-
    in_tibble %>%
    mutate_at(in_column, as.character) %>%
    left_join(known_chcc, by = in_column)

  # Create a column with old names...
  new_tibble[id_colname] <- select(new_tibble, in_column)

  # ...and replace input column header with descriptions
  new_tibble <- new_tibble %>%
    mutate(!!in_column := desc_short) %>%
    select(-desc_short)

  # If some matches were not found,
  new_tibble[in_column] <- ifelse(is.na(new_tibble[[in_column]]),
                                  new_tibble[[id_colname]],
                                  new_tibble[[in_column]])

  return(new_tibble)
}
