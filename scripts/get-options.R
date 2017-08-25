library(readxl)
library(stringr)
library(tidyverse)

"NtileByCohort912OPT0aug18v2.xlsx"

# Load file paths
filepaths <- read_csv("options-guide.csv",
  col_types = cols(
    option = col_character(),
    option_name = col_character(),
    filepath = col_character()
  ))


get_option <- function(link, cell_range, asset, option_name) {
  
  option <- read_excel(path = link, sheet = "dynasim912", col_names = FALSE, range = cell_range)
  
  names(option) <- c("Percentile", "Age", "low-1925", "1926-1930", "1931-1935", "1936-1940", "1941-1945", "1946-1950", 
                     "1951-1955", "1956-1960", "1961-1965", "1966-1970", "1971-1975", "1976-1985", "1986-1995", "1996-2005", "2006-2015", 
                     "2016-2025", "2026-2035", "2036-2045", "2046-2055", "2056-2065", "All")

  option <- option %>%
    mutate(Percentile = str_replace(Percentile, "the mean,    Mean", "100"),
           Percentile = str_replace_all(Percentile, "[^0-9/.]*", ""),
           Percentile = str_replace_all(Percentile, "\\.0*", ""),
           Percentile = ifelse(str_detect(Percentile, "^$"), NA, Percentile),
           Percentile = str_replace(Percentile, "100", "Mean")) %>%
    fill(Percentile) %>%
    filter(!Age %in% c("age", "All")) %>%
    mutate(Age = as.numeric(Age))
    
  option <- gather(option, key = "cohort", value = "value", -Age, -Percentile) %>%
    mutate(data_source = option_name, 
           Asset = asset)
  
  return(option)
  
}

iterate_option <- function(link, option_name) {

  option1 <- get_option(link = link, cell_range = "B2654:X3497", asset = "Retirement Account Assets", option_name = option_name)
  option2 <- get_option(link = link, cell_range = "B1799:X2642", asset = "Financial Assets", option_name = option_name)
  option3 <- get_option(link = link, cell_range = "B89:X932", asset = "Total Assets", option_name = option_name)
  option4 <- get_option(link = link, cell_range = "B944:X1787", asset = "Home Equity", option_name = option_name)

  ntiles_data <- bind_rows(option1, option2, option3, option4)
    
  rm(option1, option2, option3, option4)
  
  return(ntiles_data)
}  

ntiles_data <- map2(filepaths$filepath[1:14], filepaths$option_name[1:14], iterate_option) %>%
  reduce(bind_rows)

write_csv(ntiles_data, "data/options.csv")
