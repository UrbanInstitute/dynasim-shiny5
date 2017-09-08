library(readxl)
library(stringr)
library(tidyverse)

#ntiles <- "NtileByCohort912OPT0aug18v2.xlsx"
ntiles <- "X:/programs/run912/opt0/NtileByCohort912OPT0.xlsx"

# SCF
get_scf <- function(cell_range, asset) {
  scf <- read_excel(ntiles, sheet = "scf", col_names = FALSE, range = cell_range)
  
  names(scf) <- c("Age", "low-1925", "1926-1930", "1931-1935", "1936-1940", "1941-1945", "1946-1950", 
                  "1951-1955", "1956-1960", "1961-1965", "1966-1970", "1971-1975", "1976-1980", 
                  "1981+", "All")
  
  scf <- scf %>%
    filter(!str_detect(Age, "(^[^the0-9])"),
           Age != "18") %>%
    mutate(Percentile = ifelse(Age == "19", lag(Age), NA),
           Percentile = str_replace_all(Percentile, "[^0-9/.]*", ""),
           Percentile = str_replace(Percentile, "^$", "Mean")) %>%
    filter(str_detect(Age, "^[1-9]")) %>%
    fill(Percentile) %>%
    mutate(Percentile = str_replace(Percentile, "\\.0*", ""),
           Age = as.numeric(Age))

  scf <- gather(scf, key = "cohort", value = "value", -Age, -Percentile) %>%
    mutate(data_source = "SCF", 
           Asset = asset)
  
  return(scf)
}

# HRS
get_hrs <- function(cell_range, asset) {

  hrs <- read_excel(ntiles, sheet = "hrs", col_names = FALSE, range = cell_range)
  
  names(hrs) <- c("Percentile", "Age", "low-1920",	"1921-1925", "1926-1930",	"1931-1935", "1936-1940",	
                  "1941-1945", "1946-1950",	"1951-1955", "1956-1960",	"All")
  
  hrs <- hrs %>%
    mutate(Percentile = ifelse(Age == "age", Percentile, NA),
           Percentile = str_replace(Percentile, "the mean,    Mean", "100"),
           Percentile = str_replace_all(Percentile, "[^0-9/.]*", "")) %>%
    fill(Percentile) %>%
    mutate(Percentile = str_replace(Percentile, "100", "Mean"),
           Percentile = str_replace(Percentile, "\\.0*", "")) %>%
    filter(!Age %in% c("age", "All")) %>%
    mutate(Age = as.numeric(Age))
  
  hrs <- gather(hrs, key = "cohort", value = "value", -Age, -Percentile) %>%
    mutate(data_source = "HRS", 
           Asset = asset)
  
  return(hrs)
  
  }

# PSID
get_psid <- function(cell_range, asset) {
  psid <- read_excel(ntiles, sheet = "psid", col_names = FALSE, range = cell_range)
  
  names(psid) <- c("Age", "low-1925", "1926-1930", "1931-1935", "1936-1940", "1941-1945", "1946-1950", 
                   "1951-1955", "1956-1960", "1961-1965", "1966-1970", "1971-1975", "1976+", "All")
  
  psid <- psid %>%
    mutate(Percentile = ifelse(str_detect(Age, "^the"), Age, NA),
           Percentile = str_replace(Percentile, "the mean,    Mean", "100"),
           Percentile = str_replace_all(Percentile, "[^0-9/.]*", "")) %>%
    fill(Percentile) %>%
    mutate(Percentile = str_replace(Percentile, "100", "Mean"),
           Percentile = str_replace(Percentile, "\\.0*", "")) %>%
    filter(!str_detect(Age, "[a-z]")) %>%
    mutate(Age = as.numeric(Age))
  
  psid <- gather(psid, key = "cohort", value = "value", -Age, -Percentile) %>%
    mutate(data_source = "PSID",
           Asset = asset) 
  
  return(psid)
}

# SIPP
get_sipp <- function(cell_range, asset) {
  
  sipp <- read_excel(ntiles, sheet = "sipp", col_names = FALSE, range = cell_range)
  
  names(sipp) <- c("Age", "low-1925", "1926-1930", "1931-1935", "1936-1940", "1941-1945", "1946-1950", 
                   "1951-1955", "1956-1960", "1961-1965", "1966-1970", "1971-1975", "1976-1980", 
                   "1981+", "All")
  
  sipp <- sipp %>%
    mutate(Percentile = ifelse(str_detect(Age, "^the"), Age, NA),
           Percentile = str_replace(Percentile, "the mean,    Mean", "100"),
           Percentile = str_replace_all(Percentile, "[^0-9/.]*", "")) %>%
    fill(Percentile) %>%
    mutate(Percentile = str_replace(Percentile, "100", "Mean"),
           Percentile = str_replace(Percentile, "\\.0*", "")) %>%
    filter(!str_detect(Age, "[a-zA-z]")) %>%
    mutate(Age = as.numeric(Age))
  
  sipp <- gather(sipp, key = "cohort", value = "value", -Age, -Percentile) %>%
    mutate(data_source = "SIPP",
           Asset = asset)
  
  return(sipp)
  
}

# Run functions
hrs1 <- get_hrs(cell_range = "A2973:L3855", asset = "Retirement account assets")
hrs2 <- get_hrs(cell_range = "A2012:L2894", asset = "Financial assets")
hrs3 <- get_hrs(cell_range = "A90:L972", asset = "Total assets")
hrs4 <- get_hrs(cell_range = "A1051:L1933", asset = "Home equity")

psid1 <- get_psid(cell_range = "C7394:P8120", asset = "Retirement account assets")
psid2 <- get_psid(cell_range = "C5806:P6532", asset = "Financial assets")
psid3 <- get_psid(cell_range = "C6600:P7326", asset = "Total assets")
psid4 <- get_psid(cell_range = "C8188:P8914", asset = "Home equity")

scf1 <- get_scf(cell_range = "C6850:Q7901", asset = "Retirement account assets")
scf2 <- get_scf(cell_range = "C5706:Q6757", asset = "Financial assets")
scf3 <- get_scf(cell_range = "C3418:Q4469", asset = "Total assets")
scf4 <- get_scf(cell_range = "C4562:Q5613", asset = "Home equity")

sipp1 <- get_sipp(cell_range = "C7392:Q8118", asset = "Retirement account assets")
sipp2 <- get_sipp(cell_range = "C5804:Q6530", asset = "Financial assets")
sipp3 <- get_sipp(cell_range = "C246:Q972", asset = "Total assets")
sipp4 <- get_sipp(cell_range = "C8186:Q8912", asset = "Home equity")

# Combine data
ntiles_data <- bind_rows(hrs1, hrs2, hrs3, hrs4,
                         psid1, psid2, psid3, psid4,
                         scf1, scf2, scf3, scf4,
                         sipp1, sipp2, sipp3, sipp4)

rm(hrs1, hrs2, hrs3, hrs4,
   psid1, psid2, psid3, psid4,
   scf1, scf2, scf3, scf4,
   sipp1, sipp2, sipp3, sipp4)

write_csv(ntiles_data, "data/validation.csv")
