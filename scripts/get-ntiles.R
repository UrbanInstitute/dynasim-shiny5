library(readxl)
library(stringr)
library(tidyverse)



ntiles <- "NtileByCohort912OPT0aug18v2.xlsx"


# SCF Retirement Accounts
scf <- read_excel(ntiles, sheet = "scf", col_names = FALSE, range = "C6850:Q7902")

names(scf) <- c("Age", "low-1925", "1926-1930", "1931-1935", "1936-1940", "1941-1945", "1946-1950", 
                "1951-1955", "1956-1960", "1961-1965", "1966-1970", "1971-1975", "1976-1980", 
                "1981+", "All")

scf <- scf %>%
  filter(!str_detect(Age, "percentile"),
         !str_detect(Age, "RETACCT"),
         !str_detect(Age, "All"),
         Age != "18") %>%
  mutate(Percentile = ifelse(Age == "19", lag(Age), NA),
         Percentile = str_replace_all(Percentile, "[^0-9/.]*", ""),
         Percentile = str_replace(Percentile, "^$", "Mean")) %>%
  filter(str_detect(Age, "^[1-9]")) %>%
  fill(Percentile) %>%
  mutate(Percentile = str_replace(Percentile, "\\.0*", ""),
         Age = as.numeric(Age))

scf <- gather(scf, key = "cohort", value = "value", -Age, -Percentile) %>%
  mutate(data_source = "SCF")

filter(scf, cohort == "All") %>%
  ggplot(aes(Age, value, color = factor(Percentile))) +
    geom_line()


# HRS Retirement Accounts
hrs <- read_excel(ntiles, sheet = "hrs", col_names = FALSE, range = "A2973:L3855")

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
  mutate(data_source = "HRS")
  
filter(hrs, cohort == "All") %>%
  ggplot(aes(Age, value, color = factor(Percentile))) +
  geom_line()
 

# PSID Retirement Accounts
psid <- read_excel(ntiles, sheet = "psid", col_names = FALSE, range = "C7394:P8120")

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
  mutate(data_source = "PSID") 

filter(psid, cohort == "All") %>%
  ggplot(aes(Age, value, color = factor(Percentile))) +
  geom_line()




# SIPP Retirement Accounts
sipp <- read_excel(ntiles, sheet = "sipp", col_names = FALSE, range = "C7392:Q8118")

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
  filter(!str_detect(Age, "[a-z]")) %>%
  mutate(Age = as.numeric(Age))

sipp <- gather(sipp, key = "cohort", value = "value", -Age, -Percentile) %>%
  mutate(data_source = "SIPP")

filter(sipp, cohort == "All") %>%
  ggplot(aes(Age, value, color = factor(Percentile))) +
  geom_line()

# Combine data
bind_rows(hrs, psid, scf, sipp)



 
