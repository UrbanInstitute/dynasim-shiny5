library(readxl)
library(stringr)
library(tidyverse)

ntiles <- "NtileByCohort912OPT0aug18v2.xlsx"

get_option <- function(cell_range, asset) {
  
  option <- read_excel(ntiles, sheet = "dynasim912", col_names = FALSE, range = cell_range)
  
  names(option) <- c("Percentile", "Age", "low-1925", "1926-1930", "1931-1935", "1936-1940", "1941-1945", "1946-1950", 
                     "1951-1955", "1956-1960", "1961-1965", "1966-1970", "1971-1975", "1976-1985", "1986-1995", "1996-2005", "2006-2015", 
                     "2016-2025", "2026-2035", "2036-2045", "2046-2055", "2056-2065", "All")

  option <- option %>%
    mutate(Percentile = str_replace(Percentile, "the mean,    Mean", "100"),
           Percentile = str_replace_all(Percentile, "[^0-9/.]*", ""),
           Percentile = str_replace_all(Percentile, "\\.0*", ""),
           Percentile = ifelse(str_detect(Percentile, "^$"), NA, Percentile)) %>%
    fill(Percentile) %>%
    filter(!Age %in% c("age", "All")) %>%
    mutate(Age = as.numeric(Age))
    
  option <- gather(option, key = "cohort", value = "value", -Age, -Percentile) %>%
    mutate(data_source = "DYNASIM", 
           Asset = asset)
  
  return(option)
  
}

option1 <- get_option(cell_range = "B89:X932", asset = "Retirement Account Assets")
option2 <- get_option(cell_range = "B1799:X2642", asset = "Financial Assets")
option3 <- get_option(cell_range = "B89:X932", asset = "Total Assets")
option4 <- get_option(cell_range = "B944:X1787", asset = "Home Equity")

ntiles_data <- bind_rows(option1, option2, option3, option4)

rm(option1, option2, option3, option4)

write_csv(ntiles_data, "data/options.csv")
